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

;; (type-of rec)

;; (record-p rec)
;; (oref rec :birthday)
;; (oset rec :phone "555-5566")
;; (oref rec :phone)

;; (defmethod call-record ((rec record) &optional scriptname)
;;   "Dial the phone for the record REC.
;;    Execute the program SCRIPTNAME as to dial the phone."
;;    (message "Dialing the phone for %s"  (oref rec name))
;;    ;; to be implemented... 
;;    )

;; (call-record rec)

;; (defclass abroad-record (record)
;;   ((country :initarg :country
;;             :initform "DE"
;;             :documentation "Country this person is living in."))
;;   "Special class for people living abroad.")

;; (defmethod call-record :before ((rec abroad-record) &optional scriptname)
;;   "Prepend country code to phone number, then dial the phone for REC.
;;    Execute the program SCRIPTNAME as to dial the phone"
;;    (message "Prepending country code to phone number.")
;;    (unless (string-match "^00" (oref rec :phone))
;;     (let ((country (oref rec :country)))
;;      (cond 
;;        ;; just an example...
;;       ((string= country "IT")
;;        (oset rec :phone (concat "0043" (oref rec :phone)))))))
;;   )

;; (setq abroadrec (abroad-record "friend" :name "Good Friend" :birthday "01/01/2000" :phone "555-5555" :country "IT"))

;; (call-record abroadrec)

;; (call-next-method)

;; (eieio-customize-object rec)

;; Change defstruct to defclass

(defvar *player-health* nil)
(defvar *player-agility* nil)
(defvar *player-strength* nil)
(defvar *monsters* nil)
(defvar *monster-builders* nil)
(defvar *monster-num* 12)

(defun orc-battle ()
  (init-monsters)
  (init-player)
  (game-loop)
  (when (player-dead)
    (princ "You have been killed. Game Over."))
  (when (monsters-dead)
    (princ "Congratulations! You have vanquished all of your foes.")))

(defun game-loop ()
  (unless (or (player-dead) (monsters-dead))
    (show-player)
    (dotimes (k (1+ (truncate (/ (max 0 *player-agility*) 15))))
      (unless (monsters-dead)
        (show-monsters)
        (player-attack)))
    ;; (fresh-line)
    (map 'list
         (lambda(m)
           (or (monster-dead m) (monster-attack m)))
         *monsters*)
    (game-loop)))

(defun init-player ()
  (setf *player-health* 30)
  (setf *player-agility* 30)
  (setf *player-strength* 30))

;; (init-player)

(defun player-dead ()
  (<= *player-health* 0))

;; (player-dead)

(defun show-player ()
  (princ "\nYou are a valiant knight with a health of ")
  (princ *player-health*)
  (princ ", an agility of ")
  (princ *player-agility*)
  (princ ", and a strength of ")
  (princ *player-strength*))

;; (show-player)

(defun player-attack ()
;;  (fresh-line)
;;  (princ "Attack style: [s]tab [d]ouble swing [r]oundhouse:")
  (let ((command (read-from-minibuffer "Attack style: [s]tab [d]ouble swing [r]oundhouse:"))) 
    (cond 
        ((string-equal "s" command) (monster-hit (pick-monster)
                          (+ 2 (randval (ash *player-strength* -1)))))
        ((string-equal "d" command) (let ((x (randval (truncate (/ *player-strength* 6)))))
               (princ "Your double swing has a strength of ")
               (princ x)
               (monster-hit (pick-monster) x)
               (unless (monsters-dead)
                 (monster-hit (pick-monster) x))))
        ((string-equal "r" command) (dotimes (x (1+ (randval (truncate (/ *player-strength* 3)))))
                     (unless (monsters-dead)
                       (monster-hit (random-monster) 1)))))))

;; (setq temp (read-from-minibuffer "Attack style: [s]tab [d]ouble swing [r]oundhouse:") )
;; (cond 
;;  ((string-equal "s" temp) (message "a")))

;; (eql (read-from-minibuffer "Attack style: [s]tab [d]ouble swing [r]oundhouse:") "s")

(orc-battle)
;; (player-attack)

(defun randval (n)
  (1+ (random (max 1 n))))

(defun random-monster ()
  (let ((m (aref *monsters* (random (length *monsters*)))))
    (if (monster-dead m)
        (random-monster)
      m)))

(defun pick-monster ()
  (let ((x (string-to-number (read-from-minibuffer "Monster #:"))))
    (if (not (and (integerp x) (>= x 1) (<= x *monster-num*)))
        (progn (princ "That is not a valid monster number.")
               (pick-monster))
      (let ((m (aref *monsters* (1- x))))
        (if (monster-dead m)
            (progn (princ "That monster is already dead.")
                   (pick-monster))
          m)))))

(defun init-monsters ()
  (setf *monsters*
        (map 'vector
             (lambda (x)
               (funcall (nth (random (length *monster-builders*))
                             *monster-builders*)))
             (make-array *monster-num*))))

;; (init-monsters)

;; (random (length *monster-builders*))

(defun monster-dead (m)
  (<= (monster-health m) 0))

(defun monsters-dead ()
  (every #'monster-dead *monsters*))

(defun show-monsters ()
  (princ "Your foes:")
  (let ((x 0))
    (map 'list
         (lambda (m)
           (princ "   ")
           (princ (incf x))
           (princ ". ")
           (if (monster-dead m)
               (princ "**dead**")
             (progn (princ "(Health=")
                    (princ (monster-health m))
                    (princ ") ")
                    (monster-show m))))
         *monsters*)))

;; (show-monsters)

(defstruct monster (health (randval 10)))

;; (elt (make-monster) 0)

;; (defstruct test fun)

;; (setq dave (make-test :fun (lambda ()
;;                              (message "test"))))

;; (funcall (test-fun dave))

;; (message dave-fun)

;; (make-monster)

;; vector 0:name 1:healthy 3:.....

(defun monster-type-of (m)
  (if (vectorp m)
      (case (elt m 0) 
        ('cl-struct-monster "monster")
        ('cl-struct-orc "orc")
        ('cl-struct-hydra "hydra")
        ('cl-struct-slime-mold "slime-mold")
        ('cl-struct-brigand "brigand"))
    "unknow monster type"))

(defmethod monster-hit-monster (m x)
  (decf (monster-health m) x)
  (if (monster-dead m)
      (progn (princ "You killed the ")
             (princ (monster-type-of m))
             (princ "! "))
    (progn (princ "You hit the ")
           (princ (monster-type-of m))
           (princ ", knocking off ")
           (princ x)
           (princ " health points! "))))

(defmethod monster-hit (m x)
  (if (vectorp m)
      (case (elt m 0) 
        ('cl-struct-monster (monster-hit-monster m x))
        ('cl-struct-orc (monster-hit-monster m x))
        ('cl-struct-hydra (monster-hit-hydra m x))
        ('cl-struct-slime-mold (monster-hit-monster m x))
        ('cl-struct-brigand (monster-hit-monster m x)))
    "unknow monster type"))

(defmethod monster-show-monster (m)
  (princ "A fierce ")
  (princ (monster-type-of m)))

(defun monster-show (m)
  (if (vectorp m)
      (case (elt m 0) 
        ('cl-struct-monster (monster-show-monster m))
        ('cl-struct-orc (monster-show-orc m))
        ('cl-struct-hydra (monster-show-hydra m))
        ('cl-struct-slime-mold (monster-show-slime-mold m))
        ('cl-struct-brigand (monster-show-monster m)))
    "unknow monster type")

(defmethod monster-attack-monster (m))


(defmethod monster-attack (m)
  (if (vectorp m)
      (case (elt m 0) 
        ('cl-struct-monster (monster-attack-monster m))
        ('cl-struct-orc (monster-attack-orc m))
        ('cl-struct-hydra (monster-attack-hydra m))
        ('cl-struct-slime-mold (monster-attack-slime-mold m))
        ('cl-struct-brigand (monster-attack-brigand m)))
    "unknow monster type"))

(defstruct (orc (:include monster)) (club-level (randval 8)))

;; (setq dave (make-orc))

;; (monster-health dave)

;; (type-of (elt dave 0))

;; (monster-type-of dave)

;; (funcall (orc-monster_show dave) dave)

;; (make-orc)
;; (type-of (make-monster))
;; (type-of (make-orc))

(push #'make-orc *monster-builders*)

(defmethod monster-show-orc (m)
  (princ "A wicked orc with a level ")
  (princ (orc-club-level m))
  (princ " club"))

(defmethod monster-attack-orc (m)
  (let ((x (randval (orc-club-level m))))
    (princ "An orc swings his club at you and knocks off ")
    (princ x)
    (princ " of your health points. ")
    (decf *player-health* x)))

(defstruct (hydra (:include monster)))

(push #'make-hydra *monster-builders*)

(defmethod monster-show-hydra (hydra)
  (princ "A malicious hydra with ")
  (princ (monster-health m))
  (princ " heads."))

(defmethod monster-hit-hydra (m x)
  (decf (monster-health m) x)
  (if (monster-dead m)
      (princ "The corpse of the fully decapitated and decapacitated hydra falls to the floor!")
    (progn (princ "You lop off ")
           (princ x)
           (princ " of the hydra's heads! "))))

(defmethod monster-attack-hydra (m)
  (let ((x (randval (ash (monster-health m) -1))))
    (princ "A hydra attacks you with ")
    (princ x)
    (princ " of its heads! It also grows back one more head! ")
    (incf (monster-health m))
    (decf *player-health* x)))

(defstruct (slime-mold (:include monster)) (sliminess (randval 5)))

(push #'make-slime-mold *monster-builders*)

(defmethod monster-show-slime-mold (m)
  (princ "A slime mold with a sliminess of ")
  (princ (slime-mold-sliminess m)))

(defmethod monster-attack-slime-mold (m)
  (let ((x (randval (slime-mold-sliminess m))))
    (princ "A slime mold wraps around your legs and decreases your agility by ")
    (princ x)
    (princ "! ")
    (decf *player-agility* x)
    (when (zerop (random 2))
      (princ "It also squirts in your face, taking away a health point! ")
      (decf *player-health*))))

(defstruct (brigand (:include monster)))

(push #'make-brigand *monster-builders*)

(defmethod monster-attack-brigand (m)
  (let ((x (max *player-health* *player-agility* *player-strength*)))
    (cond ((= x *player-health*)
           (princ "A brigand hits you with his slingshot, taking off 2 health points! ")
           (decf *player-health* 2))
          ((= x *player-agility*)
           (princ "A brigand catches your leg with his whip, taking off 2 agility points! ")
           (decf *player-agility* 2))
          ((= x *player-strength*)
           (princ "A brigand cuts your arm with his whip, taking off 2 strength points! ")
           (decf *player-strength* 2)))))

;; (orc-battle)