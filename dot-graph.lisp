(defparameter *house* '((walls
			 (morter
				(cement)
			        (water)
				(sand))
			 (bricks))
			 (windows
			  (glass)
			  (frame)
			  (curtains))
			 (roof
			  (shingles)
			  (chimney))))

(defparameter *wizard-nodes* '((living-room
				(you are in the living-room.
				 a wizard is snoring loudly on the couch.))
			       (garden
				(you are in a beautiful garden.
				 there is a well in front of you.))
			       (attic
				(you are in the attic.
				 there is a giant welding torch in the corner.))))

(defparameter *wizard-edges* '((living-room
				(garden weat door)
				(attic upstairs ladder))
			       (garden
				(living-room east door))
			       (attic
				(living-room downstairs ladder))))

;; dotname - dot言語が扱える名前にノード名を変換( alphanumericp の補集合は _ に置換)
(defun dot-name (exp)
  (substitute-if #\_ (complement #'alphanumericp) (prin1-to-string exp)))

(defparameter *max-level-length* 30)

(defun dot-label (exp)
  (if exp
      (let ((s (prin1-to-string exp)))
	    (if (> (length s) *max-level-length*)
		(concatenate 'string (subseq s 0 (- *max-level-length* 3)) "...")
		s))
      ""))

;; NODE[label="..."] ;; mapc は mapcar の変更を返さない版
(defun nodes->dot (nodes)
  (mapc (lambda (node)
	  (fresh-line)
	  (princ (concatenate 'string
		       (dot-name (car node))
		       "[label=\""
		       (dot-label node)
		       "\"];")))
	nodes)
  nil)

;; NODE->NODE[label="..."]
(defun edges->dot (edges)
  (mapc (lambda (node)
	  (mapc (lambda (edge)
		  (fresh-line)
		  (princ (concatenate 'string
				      (dot-name (car node))
				      "->"
				      (dot-name (car edge))
				      "[label=\""
				      (dot-label (cdr edge))
				      "\"];")))
		(cdr node)))
	edges)
  nil)

;; digraph{nodeの列挙; edgeの列挙}
(defun graph->dot (nodes edges)
  (princ "digraph{")
  (nodes->dot nodes)
  (edges->dot edges)
  (princ "}")
  nil)

(defun dot->png (fname thunk) ;; thunk は無引数関数
  (with-open-file (*standard-output*     ;; 一時的な上書きでoutputをfnameへ
		   fname                 ;; ストリーム/ファイルの保存先
		   :direction :output    ;; write mode
		   :if-exists :supersede ;; ファイルを上書き
		   )
    (funcall thunk))
  ;; clisp
  ;; (ext:shell (concatenate 'string "dot -Tpng -O " fname))
  ;; sbcl
  (sb-ext:run-program "/usr/bin/env" (list "dot" "-Tpng" "-O" fname)))

;; 有向グラフ
(defun graph->png (fname nodes edges)
  (dot->png fname
	    (lambda () (graph->dot nodes edges))))

;; 無向グラフ; edgeの->を--にする
(defun uedges->dot (edges)
  (maplist (lambda (lst)
	     (mapc (lambda (edge)
		     (unless (assoc (car edge) (cdr lst))
		       (fresh-line)
		       (princ (concatenate 'string
					   (dot-name (caar lst))
					   "--"
					   (dot-name (car edge))
					   "[label=\""
					   (dot-label (cdr edge))
					   "\"];"))))
		       (cdar lst)))
		   edges))

(defun ugraph->dot (nodes edges)
  (princ "graph{")
  (nodes->dot nodes)
  (uedges->dot edges)
  (princ "};"))

(defun ugraph->png (fname nodes edges)
  (dot->png fname
	    (lambda ()
	      (ugraph->dot nodes edges))))

;;run
(defun main ()
  (graph->png "wizard.dot" *wizard-nodes* *wizard-edges*)
  (ugraph->png "u-wizard.dot" *wizard-nodes* *wizard-edges*))
