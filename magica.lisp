;;場所の説明
(defparameter *nodes* '((living-room (you are in the living-room.
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

;;
;;




