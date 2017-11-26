;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                   Mersenne Twister                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :mersenne)

(export '*std-prng*)
(defvar *std-prng* nil)

(defstruct (mt (:constructor make-mt (seed)))
  (index 624)
  (ar (let ((a (make-array '(624))))
	(setf (aref a 0) seed)
	(loop for i from 1 to 623 do
	     (setf (aref a i)
		   (%mt-next (aref a (1- i)) i)))
	a)))

(export 'new-prng)
(defun new-prng (seed)
  (setq *std-prng* (make-mt seed)))

(defun %mt-next (prev i)
  (logand
   #xFFFFFFFF
   (+ (* #x6C078965 (logxor prev (ash prev -30))) i)))

(defun %mt-twist (mt)
  (loop for i from 0 to 623 do
     ;; get the msb and add it to lsb of next
       (let ((y (logand
		 #xFFFFFFFF
		 (logand
		  #x80000000
		  (+ (aref (mt-ar mt) i)
		     (logand
		      #x7FFFFFFF
		      (aref (mt-ar mt) (mod (1+ i) 624))))))))
	 (setf (aref (mt-ar mt) i)
	       (ash
		(logxor
		 (aref (mt-ar mt) (mod (+ i 397) 624))
		 y)
		-1))
	 (if (oddp y)
	     (setf (aref (mt-ar mt) i)
		   (logxor
		    (aref (mt-ar mt) i)
		    #x9908B0DF))
	     nil)
	 (setf (mt-index mt) 0)))
  mt)

(export 'mt-gen)
(defun mt-gen (&optional (mt *std-prng*))
  (when (null mt)
    (error "Mersenne Twister PRNG not yet generated"))
  (let ((mt (if (>= (mt-index mt) 624)
		(%mt-twist mt)
		mt)))
    (let ((y (aref (mt-ar mt) (mt-index mt))))
      (setf y (logxor y (ash y -11)))
      (setf y (logxor y (logand (ash y 7)  #x9D2C5680)))
      (setf y (logxor y (logand (ash y 15) #xEFC60000)))
      (setf y (logxor y (ash y -18)))
      (incf (mt-index mt))
      (setq *std-prng* mt)
      (values (logxor y #xFFFFFFFF)
	      mt))))

(export 'rndnum)
(defun rndnum (max &optional (rng *std-prng*))
  (multiple-value-bind (n g) (mt-gen rng)
    (values (mod n max) g)))

(export 'std-rndnum)
(defun std-rndnum (max)
  (multiple-value-bind (n g) (rndnum max *std-prng*)
    (setq *std-prng* g)
    n))

(export 'mshuffle)
(defun mshuffle (seq &optional (rng *std-prng*))
  (let* ((orig-type (type-of seq))
	 (len (length seq))
	 (*std-prng* rng)
	 (arr (make-array len :initial-contents seq)))
    (loop repeat len do
	 (let* ((i1 (std-rndnum len))
		(i2 (std-rndnum len))
		(tmp))
	   (format t "i1 ~D; i2 ~D~%" i1 i2)
	   (setf tmp (aref arr i1)
		 (aref arr i1) (aref arr i2)
		 (aref arr i2) tmp)
	   (format t "arr: ~S~%" arr)))
    (coerce arr orig-type)))
