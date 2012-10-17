(defvar *wizard-nodes* '((living-room (you are in the living-room.
                                             a wizard is snoring loudly on the couch.))
                               (garden (you are in a beautiful garden.
                                        there is a well in front of you.))
                               (attic (you are in the attic.
                                       there is a giant welding torch in the corner.))))

(defvar *wizard-edges* '((living-room (garden west door)  
                                (attic upstairs ladder))
                               (garden (living-room east door))
                               (attic (living-room downstairs ladder))))

;; Enough now, not enough for further. Why [(-)] not work
(defun dot-name (exp)
  (replace-regexp-in-string "-" "_" (prin1-to-string exp)))

;; (dot-name 'living-room)

(defvar *max-label-length* 30)

;; There is no write-to-string function in elisp
(defun dot-label (exp)
  (if exp
      (let ((s exp))
        (if (> (length s) *max-label-length*)
            (concatenate 'string (subseq s 0 (- *max-label-length* 3)) "...")
            s))
      ""))

(defun nodes->dot (nodes)
  (mapc (lambda (node)
          (princ (dot-name (car node)))
          (princ "[label=\"")
          (princ (dot-label node))
          (princ "\"];\n"))
        nodes))

;; (nodes->dot *wizard-nodes*)

(defun edges->dot (edges)
  (mapc (lambda (node)
          (mapc (lambda (edge)
                  (princ (dot-name (car node)))
                  (princ "->")
                  (princ (dot-name (car edge)))
                  (princ "[label=\"")
                  (princ (dot-label (cdr edge)))
                  (princ "\"];\n"))
                (cdr node)))
        edges))

;; (edges->dot *wizard-edges*)

(defun graph->dot (nodes edges)
  (princ "digraph{\n")
  (nodes->dot nodes)
  (edges->dot edges)
  (princ "}"))

;; (graph->dot *wizard-nodes* *wizard-edges*)

(defun uedges->dot (edges)
  (maplist (lambda (lst)
             (mapc (lambda (edge)
                     (unless (assoc (car edge) (cdr lst))
                       (princ (dot-name (caar lst)))
                       (princ "--")
                       (princ (dot-name (car edge)))
                       (princ "[label=\"")
                       (princ (dot-label (cdr edge)))
                       (princ "\"];\n")))
                   (cdar lst)))
           edges))

;; (uedges->dot *wizard-edges*)
  
(defun ugraph->dot (nodes edges)
  (princ "graph{\n")
  (nodes->dot nodes)
  (uedges->dot edges)
  (princ "}"))

;; (ugraph->dot *wizard-nodes* *wizard-edges*)

(defun dot->png (fname thunk)
  (with-temp-file (concatenate 'string fname ".dot")
  (let ((standard-output (current-buffer)))
    (funcall thunk)))
  (start-process "graph" nil "/usr/bin/dot" "-Tpng" "-O" (concatenate 'string fname ".dot")))

(defun dgraph->png (fname nodes edges)
  (dot->png fname
            (lambda ()
              (graph->dot nodes edges))))

(defun ugraph->png (fname nodes edges)
  (dot->png fname
            (lambda ()
              (ugraph->dot nodes edges))))

(defun run ()
  (dgraph->png "wizard" *wizard-nodes* *wizard-edges*))

(run)

;; (start-process "graph" nil "/usr/bin/dot" "-Tpng" "-O" "wizard.dot")

;; (require 'find-lisp)
;; (with-output-to-temp-buffer "*my output*" 
;;   (mapc 'print (find-lisp-find-files "~/project/clisp" "\\.lisp$"))
;;   (setq prev-buffer (buffer-name))
;;   (switch-to-buffer "*my output*")
;;   (write-region nil nil "test")
;;   (switch-to-buffer prev-buffer)
;;   (kill-buffer "*my output*")
;; )

;; (buffer-name)

;; (ido-kill-buffer "*my output*")

;; (kill-buffer)

;; (with-open-file (*standard-output* "test" :direction :output :if-exists :supersede)
;;     (funcall thunk))

;; (require 'find-lisp)
;; (with-temp-file "test.txt"
;;   (let ((standard-output (current-buffer)))
;;   (mapc 'print (find-lisp-find-files "~/project/clisp" "\\.lisp$")))
;;   )