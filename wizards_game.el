(defvar *nodes* '((living-room (you are in the living-room.
                                    a wizard is snoring loudly on the couch.))
                  (garden (you are in a beautiful garden.
                               there is a well in front of you.))
                  (attic (you are in the attic.
                              there is a giant welding torch in the corner.))))

(defun describe-location (location nodes)
   (cadr (assoc location nodes)))

;; (describe-location 'garden *nodes*)

(defvar *edges* '((living-room (garden west door)  
                               (attic upstairs ladder))
                  (garden (living-room east door))
                  (attic (living-room downstairs ladder))))

(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

;; (describe-path '(garden west door))

(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

;; (describe-paths 'living-room *edges*)

(defvar *objects* '(whiskey bucket frog chain))

(defvar *object-locations* '((whiskey living-room)
                                   (bucket living-room)
                                   (chain garden)
                                   (frog garden)))

(defun objects-at (loc objs obj-loc)
  (labels ((is-at (obj)
                   (eq (cadr (assoc obj obj-loc)) loc)))
     (remove-if-not #'is-at objs)))

;; (objects-at 'living-room *objects* *object-locations*)

(defun describe-objects (loc objs obj-loc)
   (labels ((describe-obj (obj)
                `(you see a ,obj on the floor.)))
      (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))

;; (describe-objects 'living-room *objects* *object-locations*)

(defvar *location* 'living-room)

(defun look ()
  (append (describe-location *location* *nodes*)
          (describe-paths *location* *edges*)
          (describe-objects *location* *objects* *object-locations*)))

;; (look)

(defun walk (direction)
  (labels ((correct-way (edge)
             (eq (cadr edge) direction)))
    (let ((next (find-if #'correct-way (cdr (assoc *location* *edges*)))))
      (if next 
          (progn (setf *location* (car next)) 
                 (look))
        '(you cannot go that way.)))))

;; (walk 'west)

(defun pickup (object)
  (cond ((member object (objects-at *location* *objects* *object-locations*))
         (push (list object 'body) *object-locations*)
         `(you are now carrying the ,object))
        (t '(you cannot get that.))))

;; (walk 'east)
;; (look)
;; (pickup 'whiskey)


(defun inventory ()
  (cons 'items- (objects-at 'body *objects* *object-locations*)))

;; (inventory)

(defun have (object) 
    (member object (cdr (inventory))))

;; Use read or read-string instead of read-line, 
;; there is no read-line in elisp. Some improvment needed here. read-from-string?
(defun game-read ()
    (let ((cmd (read-from-string (concatenate 'string "(" (read-from-minibuffer "command: ") ")"))))
         (flet ((quote-it (x)
                    (list 'quote x)))
             (cons (caar cmd) (mapcar #'quote-it (cdar cmd))))))

;; (game-read)
;; (read-from-minibuffer "comand: ")
;; (cdar (read-from-string (concatenate 'string "(" "walk east" ")")))

(defvar *allowed-commands* '(look walk pickup inventory))

(defun game-eval (sexp)
    (if (member (car sexp) *allowed-commands*)
        (eval sexp)
        '(i do not know that command.)))

;; (read)

;; Use upcase and downcase instead of char-upcase and char-downcase
(defun tweak-text (lst caps lit)
  (when lst
    (let ((item (car lst))
          (rest (cdr lst)))
      (cond ((eql item ?\ ) (cons item (tweak-text rest caps lit)))
            ((member item '(?\! ?\? ?\.)) (cons item (tweak-text rest t lit)))
            ((eql item ?\") (tweak-text rest caps (not lit)))
            (lit (cons item (tweak-text rest nil lit)))
            (caps (cons (upcase item) (tweak-text rest nil lit)))
            (t (cons (downcase item) (tweak-text rest nil nil)))))))

(defun game-print (lst)
  (print (coerce (tweak-text (coerce (prin1-to-string lst) 'list) t nil) 'string)))

;; (game-print '(not only does this sentence have a "comma," and "iPad."))

;; the main function
(defun game-repl ()
    (let ((cmd (game-read)))
        (unless (eq (car cmd) 'quit)
          (game-print (game-eval cmd))
          ;; (game-eval cmd)
            (game-repl))))

;; maybe you need this when max deep length error occur
;; (setf max-lisp-eval-depth 5000)
(game-repl)

;; (coerce (prin1-to-string '(not)) 'list) 
;; (eql 40 ?\()

;; (member ?\. '(?\! ?\? ?\.))

;; (defun game-print (lst)
;; (print (coerce (tweak-text (coerce (replace-regexp-in-string "[(:)]" "" 
;; (prin1-to-string lst)) 'list) t nil) 'string)))

;; (replace-regexp-in-string "[()]" "" "( dd )")

;; (game-print '(hello . ! iiooo))

;; (coerce "hello hello" 'list)
;; (char-to-string ?\")
;; (defun game-print (lst)
;;   (message (replace-regexp-in-string "[(:)]" "" (prin1-to-string lst))))

