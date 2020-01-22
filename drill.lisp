;;2.1.2
(defun poly-2-3-4 (x)
 (+ (* 2 (sqrt x 2)) (* 3 x) 4))
;;2.1.3
(defun en (r)
 (* pi (sqrt r 2)))
;;2.3.2
(let ((v 3))
 (list v
  (let ((v (* v v)))
   (+ v v))
   v
   (let ((v (+ v v)))
    (* v v)
   v)));;=>(3 18 3 6)
;;2.3.4
(defun my-evenp (n)
 (or (and (= (mod n 2)) t) nil))
;;2.4.2
(defun _ (x)
 (if (numberp x) (if (>= x 0) (<= x 100)) t)))
;;2.4.3
;;(or (foo x) (zerop x))と等価
;;(or &rest forms)は成功した時点でその式の値を返して終了
(defun hai (n)
 (let ((tmp (foo a)))
  (if tmp tmp (= n 0))))
;;2.6.1
;;(values &rest values)で、多値を返す関数の第一の値のみ取得する
(defun floor-1 (n div)
 (values (floor n div)))
;;2.6.2
(defun floor-2 (n div)
 (multiple-value-bind (f s) (floor n div) s))
;;3
;;(trace &rest specs)...ある関数の再帰とかの挙動を実行時トレース
;;-> (untrace トレースしている関数名)で脱出
;;(block name &rest forms)...progn等最後までに止められる
;;(return-from name &optional value)でblock名指定してbreak(returnだとname省略)
;;(loop &rest keywords-and-forms)...やばすぎ高級関数
;;(tagbody &rest statements)...tagと式を列挙し(go tag)で移動(goto文)
;;(endp obj)...objが式の終端かどうか判定
;;(dolist (var list &optional result) &body body)
;;...(dolist (iterator iterableobj) &actions)
;;(dotimes (var count &optional result) &body body)
;;...(dotimes (iterator count-int) &body body)

;;3.1.1
(defun my-evenp (n)
 (if (= 0 n) t (my-oddp (1- n))))
(defun my-oddp (n)
 (if (= 0 n) nil (my-evenp (1- n))))
(defun my-length (l)
 (if (eq l '()) 0 (1+ (my-length (cdr l)))))