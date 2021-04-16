;; Land of Lisp: 5章

;; TODO:
;; - 周囲を見渡す
;; - 他の場所へ移動
;; - オブジェクトを拾う
;; - オブジェクトでなにかする

;; 場所の説明 - (assoc 'key *nodes*) で説明を呼び出す
(defparameter *nodes* '((living-room (you are in the living-room.
				      a wizard id ddnoring loudly on the couch.))
                        (garden (you are in a beautiful garden.
				 there is a well in front of you.))
                        (attic (you are in the attic.
				there is a giant welding torch
				in the cornar.))))


;; 通り道の説明 - (assoc 'key *edges*) で説明を呼び出す - (destination direction thing)
(defparameter *edges* '((living-room (garden west door)
                                     (attic upstairs ladder))
                        (garden (living-room east door))
                        (attic  (living-room downstairs ladder))))

;; 出現する物の一覧
(defparameter *objects* '(whiskey bucket chain frog))

;; 物の在り処
(defparameter *object-locations* '((whiskey living-room)
				   (bucket living-room)
				   (chain garden)
				   (frog garden)))

;; 現在の位置
(defparameter *location* 'living-room)

;; 説明の取得
(defun describe-location (location nodes)
  (cadr (assoc location nodes)))

;; エッジ名からパスの説明を生成するマクロ - 
(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

;; エッジの持つ複数のパスをnodesから検索し描写を作る
(defun describe-paths (location edges) ;; if: loc. = 'living-room;then returns: (append (THERE IS ...) (THERE IS ...))
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

;; 周囲にある物のリストを取得
;; (defun objects-at (loc obj-locs)
;;  (let ((d (delete-if-not #'(lambda (x) (eq loc (cadr x))) obj-locs)))
;;  (if (null d)
;;      d
;; (mapcar #'car d))))
(defun objects-at (loc objs obj-locs)
  (labels ((at-loc-p (obj)
	     (eq (cadr (assoc obj obj-locs)) loc))) ;; assocで確かめることで、マッチ中最新のpushされたalistが返る
    (remove-if-not #'at-loc-p objs)))

;; 周囲にある物の描写
(defun describe-objects (loc objs obj-locs)
  (let ((d (objects-at loc objs obj-locs)))
  (if (null d)
      d
      (apply #'append (mapcar #'(lambda (obj) `(you can see a ,obj on the floor.)) d)))))

;; 周囲を見渡す
(defun look ()
  (append (describe-location *location* *nodes*)
	  (describe-paths *location* *edges*)
	  (describe-objects *location* *objects* *object-locations*)))

;; 移動する
(defun walk (direction)
  (let ((next (find direction ;; find - 現在地の道を調べ、その中から$1が:keyに一致するものを取り出す
		    (cdr (assoc *location* *edges*))
		    :key #'cadr)))
    (if (null next) ;; pathが無く、移動できない場合
	'(you cannot go that way...)
	(progn (setf *location* (car next)) ;; 大域変数への変更はsetfを用いる
	       (look)))))

;; 手に取る
(defun pickup (object)
  (if (member object
		 (objects-at *location* *objects* *object-locations*))
      (progn
	(push (list object 'body) *object-locations*) ;; 同じ場所で同じものを再度pickupしても(obj 'body)が返るので削除しなくても良い
	 `(you are now carrying the ,object))
      '(you cannot get that.)))

;; 持ち物
(defun inventory ()
  (list 'items- (objects-at 'body *objects* *object-locations*)))

;; REPL
(defun game-repl ()
  (princ "game-cmd> ")
  (let ((cmd (game-read)))
    (unless (eq (car cmd) 'quit) ;; 'quitの時は終了
      (game-print (game-eval cmd))
      (game-repl))))

;; (cmd 'args) -> cmd args1 args2 ... とかけるようにするread
(defun game-read ()
  (let ((cmd (read-from-string (concatenate
				'string "(" (read-line) ")"))))
    ;; cmd部に argsにquote
    (cons (car cmd) (mapcar (lambda (x) (list 'quote x)) (cdr cmd)))))

;; 必要なcmd以外は実行しないeval
(defparameter *allowed-commands* '(look wark pickup inventory quit))

(defun game-eval (sexp)
  (if (member (car sexp) *allowed-commands*)
      (eval sexp)
      `(i do not know that command.
	cmds < ,*allowed-commands* >)))
;; [\!\?\.]で区切られた英文を正しいup/down caseでprint
(defun game-print (lst)
  (flet ((syml-to-chrl (lst)
		      (concatenate 'list (string-trim "() "
					   (prin1-to-string lst)))))
  (princ (concatenate 'string (tweak-text (syml-to-chrl lst)
			      t ;; 最初は大文字
			      nil))))
  (fresh-line))

(defun tweak-text (lst caps lit)
  (when lst ;; when lst is not nil, run block
    (let ((item (car lst))
	  (rest (cdr lst)))
      (cond ((eql item #\space) (cons item (tweak-text rest caps lit)))
	    ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
	    ((member item '(#\' #\")) (tweak-text rest caps (not lit)))
	    (lit (cons item (tweak-text rest nil lit)))
	    (caps (cons (char-upcase item) (tweak-text rest nil lit)))
	    (t (cons (char-downcase item) (tweak-text rest nil nil)))))))


	    
      
