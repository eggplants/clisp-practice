;; Grand Theft Onesp
(load "graph-util")

;; コンジェスチョンシティ定義
(defparameter *congestion-city-nodes* nil)
(defparameter *congestion-city-edges* nil)
(defparameter *visited-nodes* nil)
(defparameter *node-num* 30) ;; 30ノード
(defparameter *edge-num* 45) ;; 45パス
(defparameter *worm-num* 3)  ;; ツチボタル3チーム
(defparameter *cop-odds* 15) ;; 警察がいる確率1/n

;; node-num >= rand >= 1
(defun random-node ()
  (1+ (random *node-num*)))

;; a-bに双方向エッジを貼る
(defun edge-pair (a b)
  (unless (eql a b)
    (list (cons a b) (cons b a))))

;; edge-num本の双方向エッジをランダムの2ノード間に貼ったリスト生成
(defun make-edge-list ()
  (apply #'append (loop repeat *edge-num*
			collect (edge-pair (random-node) (random-node)))))

;; 孤立点

;; nodeからのエッジを列挙
(defun direct-edges (node edge-list)
  (remove-if-not (lambda (x)
		   (eql (car x) node))
		 edge-list))

;; nodeから到達可能なnode集合を列挙
;; 訪問済みリストを作って、未訪問ならそのノードをリストに追加
;; またそのnodeにつながっている全nodeに対して未訪問かチェック
;; 訪問済みリストが連結しているノード一覧になる
(defun get-connected (node edge-list)
  (let ((visited '()))
    (labels ((traverse (node)
	       (unless (member node visited)
		 (push node visited)
		 (mapc (lambda (edge)
			 (traverse (cdr edge)))
		       (direct-edges node edge-list)))))
      (traverse node))
    visited))

;; get-connectedで得たものと全nodesとの差集合でunconnectedをメモして、全nodeを探索し孤立点を列挙
;; nodes = '(*1..max-nodes)
(defun find-islands (nodes edge-list)
  (let ((islands nil))
    (labels ((find-island (nodes)
	       (let* ((connected (get-connected (car nodes) edge-list))
		      (unconnected (set-difference nodes connected)))
		 (push connected islands)
		 (when unconnected
		   (find-island unconnected)))))
      (find-island nodes))
    islands))

;; 島に橋をかける
(defun connect-with-bridges (islands)
  (when (cdr islands)
    (append (edge-pair (caar islands) (caadr islands))
	    (connect-with-bridges (cdr islands)))))

;; 全孤立点を検索しすべて連結して返す
;; nodes = '(*1..max-nodes)
(defun connect-all-islands (nodes edge-list)
  (append (connect-with-bridges (find-islands nodes edge-list)) edge-list))

;; 街のエッジを構築
(defun make-city-edges ()
  (let* ((nodes (loop for i from 1 to *node-num*
		      collect i))
	 (edge-list (connect-all-islands nodes (make-edge-list)))
	 (cops (remove-if-not (lambda (x)
				(zerop (random *cop-odds*)))
			      edge-list)))
    (add-cops (edges-to-alist edge-list) cops)))

;; エッジをalistへ;; ((1 . (2) (4) (6)) (2 . (1) (3) (9)) ...)
(defun edges-to-list (edge-list)
  (mapcar (lambda (node1)
	    (cons node1
		  (mapcar (lambda (edge)
			    (list (cdr edge)))
			  (remove-duplicates (direct-edges node1 edge-list)
					     :test #'equal))))
	  (remove-duplicates (mapcar #'car edge-list))))

;; 検問所の配置
(defun add-cops (edge-alist edges-with-cops)
  (mapcar (lambda (x)
	    (let ((node1 (car x))
		  (node1-edges (cdr x)))
	      (cons node1
		    (mapcar (lambda (edge)
			      (let ((node2 (car edge)))
				(if (intersection (edge-pair node1 node2)
						  edges-with-cops
						  :test #'equal)
				    (list node2 'cops)
				    edge)))
			    node1-edges))))
	  edge-alist))

;; 8.3===
