;;場所の説明
;;開始位置
(defparameter *location* 'living-room)
(Defparameter *nodes* '((living-room (you are in the living-room.
                          a wizard id ddnoring loudly on the couch.))
                        (garden (you are in a beautiful garden.
                          there is a well in front of you.))
                        (attic (you are in the attic.
                          there is a giant welding torch
                          in the cornar.))))
;;通り道の構造を説明
(defparameter *edges* '((living-room (garden west door)
                                     (attic upstairs ladder))
                        (garden (living-room east door))
                        (attic  (living-room downstairs ladder))))
;;各場所の物を説明
(defparameter *objects* '(whiskey bucket frog chain))
(defparameter *object-locations* '((whiskey living-room)
                                   (bucket living-room)
                                   (chain garden)
                                   (frog garden))))
;;説明の取得(defname location a-data-list)
(defun describe-location (location nodes)
 (cadr (assoc location nodes)))
;;エッジ名からパスの説明を生成する
;;`準クオートはデータ埋め込み(`cmd`みたいな)
;;データと関数の境目には,カンマ
;;caddr->(先頭から３つ目), cadr->(先頭から２つ目)
(defun describe-path (edge)
 `(there is a ,(caddr edge) going ,(cadr edge) from here.))
;;エッジの持つ複数のパスをnodesから検索し説明を作る
;;append->
;;a new list that is the concatenation of the copies.
;;#'->
;;関数を引数として渡すとき、関数名をオブジェクトに変換
;;apply->
;;(apply #'append '((1 2) (3 4)))
(defun describe-paths (location edges)
 (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))
;;その場所にある物一覧のリストを取得
(defun objects-at (loc objs obj-locs)
 (labels ((at-loc-p (obj)
  (eq (second (assoc obj obj-locs)) loc)))
   (remove-if-not #'at-loc-p objs)))
;;(objects-at 'living-room *objects* *object-locations*)
;;=>(WHISKEY BUCKET)
;;objects-atの結果をテンプレ文に埋め込んで返したい
(defun describe-objects (loc objs obj-loc)
 (labels((describe-obj (obj)
          `(you see a ,obj on the floor.)))
  (apply #'append(mapcar #'describe-obj (objects-at loc objs obj-loc)))))
;;describe*をまとめる
;;グローバル変数を扱う関数は関数的ではない(独立していない)
;;ここがHaskellくんと違う(純粋な関数型言語ではない
(defun look ()
 (append
  (describe-location *location* *nodes*)
  (describe-paths *location* *edges*)
  (describe-objects *location* *objects* *object-locations*)))
(defun walk (direction)
 (let ((next (find direction
              (cdr (assoc *location* *edges*))
               :key #'cadr)))
   (if next
       (progn (setf *location* (car next))
              (look))
       '(you cannot go that way.))))
