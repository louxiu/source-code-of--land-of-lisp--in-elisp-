;; The computer guess the number you choose between 1~100
(setq *small* 1)
(setq *big* 100)

(defun guess-my-number ()
     (ash (+ *small* *big*) -1))

(defun smaller ()
     (setq *big* (1- (guess-my-number)))
     (guess-my-number))

(defun bigger ()
     (setq *small* (1+ (guess-my-number)))
     (guess-my-number))

(defun start-over ()
   (setq *small* 1)
   (setq *big* 100)
   (guess-my-number))

(start-over)
(smaller)
(bigger)
