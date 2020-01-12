;;;二分探索の数あて
;;グローバル変数はdefparameterかdefver
(defparameter *small* 1)
(defparameter *big* 100)
;;ashはビット演算の右シフト
(defun guess-my-num ()
 (print 'game-started!)
 (ash (+ *small* *big*) -1))
;;setqは変数を定義.setfはそれに加えcarのみとかの変更も可能
(defun bigger ()
 (setf *small* (1+ (guess-my-num)))
 (guess-my-num))
(defun smaller ()
 (setf *big* (1- (guess-my-num)))
 (guess-my-num))
(defun done! ()
 (defparameter *small* 1)
 (defparameter *big* 100)
 (print 'INITTED! ))

;;setqとsetfの違い
(setq abc '(1 2 3)) ;=> (1 2 3)
(setq (car abc) 3)  ;=> error!
(setf (car abc) 3)  ;=> 3
abc                 ;=> (3 2 3)

;;fletのスコープはflet内のみ
;動く
(defun tes ()
 (flet ((f (a b)
             (+ a b))
            (x (n)
             (random (* n 10)))
            (y (n)
             (random (* n 100))))
 (f (x 1) (y 2))))

;動く
(flet ((f (n) (+ n 10)))
 (f 2))

;動く
(defun test (n)
 (flet ((f (x) (+ x 100)))
  (f n)))

;;case
(defun judge-if (cmp)
 (case cmp
  ((t) (print "true"))
  ((nil) (print "false"))
  (otherwise (print "not bool it is!"))))

;;cond
(flet
 ((mzp (x)
  (concatenate 'string "odd" (if (minusp x) "minus" (if (plusp x) "plus" "zero")))))
 (defun mpeo (n)
  (cond
   ((oddp n) (mzp n))
   ((evenp n) (mzp n)))))


