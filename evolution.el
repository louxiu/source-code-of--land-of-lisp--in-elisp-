;; (defclass record () ; No superclasses
;;   ((name :initarg :name
;;          :initform ""
;;          :type string
;;          :custom string
;;          :documentation "The name of a person.")
;;    (birthday :initarg :birthday
;;              :initform "Jan 1, 1970"
;;              :custom string
;;              :type string
;;              :documentation "The person's birthday.")
;;    (phone :initarg :phone
;;           :initform ""
;;           :documentation "Phone number."))
;;   "A single record for tracking people I know.")

;; (setq rec (record "rand" :name "Random Sample" :birthday "01/01/2000" :phone "555-5555"))

;; (record-name rec)

;; (oref rec :birthday)

;; (record-p rec)
(defun fresh-line ()
  (princ "\n"))

;; (fresh-line)

(defvar *width* 100)
(defvar *height* 30)

(defvar *jungle* '(45 10 10 10))
(defvar *plant-energy* 80)

(defvar *plants* (make-hash-table :test #'equal))

(defun random-plant (left top width height)
   (let ((pos (cons (+ left (random width)) (+ top (random height)))))
        (setf (gethash pos *plants*) t)))

(defun add-plants ()
   (apply #'random-plant *jungle*)
   (random-plant 0 0 *width* *height*))

(defstruct animal x y energy dir genes)

(defvar *animals* 
    (list (make-animal :x      (ash *width*  -1)
                       :y      (ash *height* -1)
                       :energy 1000
                       :dir    0
                       :genes  (loop repeat 8
                                     collecting (1+ (random 10))))))

(defun move (animal)
  (let ((dir (animal-dir animal))
        (x (animal-x animal))
        (y (animal-y animal)))
    (setf (animal-x animal) (mod (+ x
                                    (cond ((and (>= dir 2) (< dir 5)) 1)
                                          ((or (= dir 1) (= dir 5)) 0)
                                          (t -1))
                                    *width*)
                                 *width*))
    (setf (animal-y animal) (mod (+ y
                                    (cond ((and (>= dir 0) (< dir 3)) -1)
                                          ((and (>= dir 4) (< dir 7)) 1)
                                          (t 0))
                                    *height*)
                                 *height*))
    (decf (animal-energy animal))))

(defun turn (animal)
  (let ((x (random (apply #'+ (animal-genes animal)))))
    (labels ((angle (genes x)
               (let ((xnu (- x (car genes))))
                 (if (< xnu 0)
                     0
                     (1+ (angle (cdr genes) xnu))))))
        (setf (animal-dir animal)
              (mod (+ (animal-dir animal) (angle (animal-genes animal) x)) 8)))))

(defun eat (animal)
  (let ((pos (cons (animal-x animal) (animal-y animal))))
    (when (gethash pos *plants*)
      (incf (animal-energy animal) *plant-energy*)
      (remhash pos *plants*))))

(defvar *reproduction-energy* 200)

(defun reproduce (animal)
  (let ((e (animal-energy animal)))
    (when (>= e *reproduction-energy*)
      (setf (animal-energy animal) (ash e -1))
      (let ((animal-nu animal)
            (genes     (copy-list (animal-genes animal)))
            (mutation  (random 8)))
        (setf (nth mutation genes) (max 1 (+ (nth mutation genes) (random 3) -1)))
        (setf (animal-genes animal-nu) genes)
        (push animal-nu *animals*)))))

(defun update-world ()
  (setf *animals* (remove-if (lambda (animal)
                                 (<= (animal-energy animal) 0))
                             *animals*))
  (mapc (lambda (animal)
          (turn animal)
          (move animal)
          (eat animal)
          (reproduce animal))
        *animals*)
  (add-plants))

(defun draw-world ()
  (loop for y
        below *height*
        do (progn (fresh-line)
                  (princ "|")
                  (loop for x
                        below *width*
                        do (princ (cond ((some (lambda (animal)
                                                 (and (= (animal-x animal) x)
                                                      (= (animal-y animal) y)))
                                               *animals*)
                                         (char-to-string ?M)) ;; ?\M
                                        ((gethash (cons x y) *plants*) (char-to-string ?\*))
                                         (t (char-to-string ?\ )))))
                  (princ "|"))))

(defun evolution ()
  (draw-world)
  (fresh-line)
  (let ((str (read-from-minibuffer "days: ")))
    (cond ((equal str "quit") ())
          (t (let ((x (string-to-int str)))
               (if x
                   (loop for i
                      below x
                      do (update-world)
                      if (zerop (mod i 1000))
                      do (princ ?\.))
                   (update-world))
               (evolution))))))

;; (string-to-int "2a")

;; (princ (char-to-string ?M))
(evolution)

