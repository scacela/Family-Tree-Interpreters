;;;; -*- Mode: Common-Lisp; -*-

;;; Sam Cacela

;;; hash-table
(defparameter *p* (make-hash-table :test #'equal))

;;; person structures will be the values in the hash-table *p*
(defstruct person
  (name nil)
  (spouses nil)
  (parents nil)
  (children nil)
  (siblings nil)
  (halfsiblings nil)
  (ancestors nil)
  (descendants nil)
  (cousins nil))


;;; print each element of LIST on a line of its own.
;;; W
(defun formlist (list)
  (loop
    (when (null list) (return))
    (print (first list))
    (setq list (rest list))))


;;; creates a list containing all ancestors of person structure p
;;; E
(defun giveAncestors (p) 
  (let ((a (person-parents p)))  
    (when a
      (append a 
              (union (giveAncestors (gethash (first a) *p*))
                     (giveAncestors (gethash (second a) *p*)))))))      


;;; arguments: person structure, counter
;;; returns a list of lists. An inner list looks like:
;;; (<ancestor_name> <num_upward_traversals>)
;;; used in getCousinDegreeRemoval
(defun getTraversals (p ct)
  (when (person-parents p)
    (union (union(getTraversals (gethash (first (person-parents p)) *p*) (+ 1 ct))
                 (getTraversals (gethash (second (person-parents p)) *p*) (+ 1 ct))) 
           (list (list (first (person-parents p))(+ 1 ct)
                      (second (person-parents p)) (+ 1 ct))))))


;;; returns minimum value in list
;;; used in getCousinDegreeRemoval
(defun minlist (w)
  (if w (apply #'min w) 0))


;;; returns a list of the form:
;;; (<cousin_degree> <cousin_removal>)
;;; R
(defun getCousinDegreeRemoval(p1 p2)

  (setf cm_anc (intersection (giveAncestors p1) (giveAncestors p2)))

  (when cm_anc
  (setf p1_ancs (getTraversals p1 0))
  (setf p2_ancs (getTraversals p2 0))
  (setf p1_traversals nil)
  (setf p2_traversals nil)

  (loop for y in cm_anc
    do (loop for x in p1_ancs
    do (if (equal (first x) y)
    (setf p1_traversals (cons (second x) p1_traversals)))))

  (loop for y in cm_anc
    do (loop for x in p2_ancs
    do (if (equal (first x) y)
    (setf p2_traversals (cons (second x) p2_traversals)))))

  (list (- (min (minlist p1_traversals) (minlist p2_traversals)) 1) 
    (- (max (minlist p1_traversals) (minlist p2_traversals)) 
      (min (minlist p1_traversals) (minlist p2_traversals))))
  ))


;;; X, W
;;; returns true if <cousin_degree> and <cousin_removal> match up with
;;; those in X query
(defun compareCousinDegreeRemoval (cd cr p1 p2)
  (equal (getCousinDegreeRemoval p1 p2) (list cd cr)))


;;; is name1 married to name2? And vice-versa? (mutual)
;;; X, R, E
(defun marriedp (name1 name2)
  (setf m1 (gethash name1 *p*))
  (setf m2 (gethash name2 *p*))
  (when (or (not (typep m1 'person))
                 (not (typep m2 'person)))
            (error "~%m1 (~A) or m2 (~A) is not a person structure. Check married~%~%" m1 m2))
  (and (member name1 (person-spouses m2) :test #'eql)
       (member name2 (person-spouses m1) :test #'eql)))


;;; is name1 a parent of name2?
;;; X, R
(defun parentp (name1 name2)
  (setf m1 (gethash name1 *p*))
  (setf m2 (gethash name2 *p*))
  (when (or (not (typep m1 'person))
                 (not (typep m2 'person)))
            (error "~%m1 (~A) or m2 (~A) is not a person structure. Check parentp.~%~%" m1 m2))
  (member name1 (person-parents m2) :test #'eql))

;;; is name1 a child of name2?
;;; X, R
(defun childp (name1 name2)
  (setf m1 (gethash name1 *p*))
  (setf m2 (gethash name2 *p*))
  (when (or (not (typep m1 'person))
                 (not (typep m2 'person)))
            (error "~%m1 (~A) or m2 (~A) is not a person structure. Check childp.~%~%" m1 m2))
  (member name1 (person-children m2) :test #'eql))


;;; is name1 a sibling of name2? And vice-versa? (mutual)
;;; X, R
(defun siblingp (name1 name2)
  (setf m1 (gethash name1 *p*))
  (setf m2 (gethash name2 *p*))
  (when (or (not (typep m1 'person))
                 (not (typep m2 'person)))
            (error "~%m1 (~A) or m2 (~A) is not a person structure. Check siblingp.~%~%" m1 m2))
  (and (member name1 (person-siblings m2) :test #'eql)
       (member name2 (person-siblings m1) :test #'eql)))


;;; is name1 a half-sibling of name2? And vice-versa? (mutual)
;;; X, R
(defun halfsiblingp (name1 name2)
  (setf m1 (gethash name1 *p*))
  (setf m2 (gethash name2 *p*))
  (when (or (not (typep m1 'person))
                 (not (typep m2 'person)))
            (error "~%m1 (~A) or m2 (~A) is not a person structure. Check halfsiblingp.~%~%" m1 m2))
  (and (member name1 (person-halfsiblings m2) :test #'eql)
       (member name2 (person-halfsiblings m1) :test #'eql)))


;;; is name1 an ancestor of name2?
;;; X, R
(defun ancestorp (name1 name2)
  (setf m1 (gethash name1 *p*))
  (setf m2 (gethash name2 *p*))
  (when (or (not (typep m1 'person))
                 (not (typep m2 'person)))
            (error "~%m1 (~A) or m2 (~A) is not a person structure. Check ancestorp.~%~%" m1 m2))
  (member name1 (person-ancestors m2) :test #'eql))


;;; is name1 a descendant of name2?
;;; X, R
(defun descendantp (name1 name2)
  (setf m1 (gethash name1 *p*))
  (setf m2 (gethash name2 *p*))
  (when (or (not (typep m1 'person))
                 (not (typep m2 'person)))
            (error "~%m1 (~A) or m2 (~A) is not a person structure. Check descendantp.~%~%" m1 m2))
  (member name1 (person-descendants m2) :test #'eql))


;;; is name1 a cousin of of name2? And vice-versa? (mutual)
;;; X, R, W
(defun cousinp (name1 name2)
  (setf m1 (gethash name1 *p*))
  (setf m2 (gethash name2 *p*))
  (when (or (not (typep m1 'person))
                 (not (typep m2 'person)))
            (error "~%m1 (~A) or m2 (~A) is not a person structure. Check cousinp.~%~%" m1 m2))
  (and (member name1 (person-cousins m2) :test #'eql)
       (member name2 (person-cousins m1) :test #'eql)))



;;; list all who are the <relation> of name1
(defun handle-W (relation name1)
  (setf m1 (gethash name1 *p*))
  (when (not (typep m1 'person))
            (error "~%m1 (~A) is not a person structure. Check handle-W.~%~%" m1))
  (case relation
    ('spouse
      (formlist (sort (person-spouses m1) #'string-lessp))
      (format *standard-output* "~%~%"))

    ('parent
      (formlist (sort (person-parents m1) #'string-lessp))
      (format *standard-output* "~%~%"))

    ('child
      (formlist (sort (person-children m1) #'string-lessp))
      (format *standard-output* "~%~%"))

    ('sibling
      (formlist (sort (person-siblings m1) #'string-lessp))
      (format *standard-output* "~%~%"))

    ('half-sibling
      (formlist (sort (person-halfsiblings m1) #'string-lessp))
      (format *standard-output* "~%~%"))

    ('ancestor
      (formlist (sort (person-ancestors m1) #'string-lessp))
      (format *standard-output* "~%~%"))

    ('descendant
      (formlist (sort (person-descendants m1) #'string-lessp))
      (format *standard-output* "~%~%"))

    (t
      (setf stream_str_rel (make-string-input-stream (write-to-string relation)))
      (setf read_rel (read stream_str_rel nil nil))
      (setf cousin_str (write-to-string (first read_rel)))

      (when (equal cousin_str (string 'cousin))

      (setf cd_int (parse-integer (write-to-string (second read_rel))))
      (setf cr_int (parse-integer (write-to-string (third read_rel)))))

      ;;; only iterate through keys (names) in hash-table, not values (person structures)
      
      (setf everyone nil)
      (setf cous nil)
      (maphash (lambda (k v) (setf everyone (cons k everyone))) *p*)

      (dolist (e everyone)
        (when e
          (when (cousinp name1 e)
          (when (compareCousinDegreeRemoval cd_int cr_int (gethash name1 *p*) (gethash e *p*))
            (setf cous (cons e cous))))))

            (formlist (sort cous #'string-lessp))
            (format *standard-output* "~%~%"))
    ))



;;; how are name1 and name2 most closely related?
(defun handle-R (name1 name2)
  (if (marriedp name1 name2)
    (format *standard-output* "~%~A is married to ~A.~%~%" name1 name2)

  (if (parentp name1 name2)
    (format *standard-output* "~%~A is a parent of ~A.~%~%" name1 name2)

  (if (childp name1 name2) 
    (format *standard-output* "~%~A is a child of ~A.~%~%" name1 name2)

  (if (siblingp name1 name2)
    (format *standard-output* "~%~A is a sibling of ~A.~%~%" name1 name2)

  (if (halfsiblingp name1 name2)
    (format *standard-output* "~%~A is a half-sibling of ~A.~%~%" name1 name2)

  (if (ancestorp name1 name2)
    (format *standard-output* "~%~A is an ancestor of ~A.~%~%" name1 name2)

  (if (descendantp name1 name2)
    (format *standard-output* "~%~A is a descendant of ~A.~%~%" name1 name2)

  (if (cousinp name1 name2)
  (if (getCousinDegreeRemoval (gethash name1 *p*) (gethash name2 *p*))
    (and (setf cd_int (first (getCousinDegreeRemoval (gethash name1 *p*) (gethash name2 *p*))))
      (setf cr_int (second (getCousinDegreeRemoval (gethash name1 *p*) (gethash name2 *p*))))

      (case cd_int
          (1
            (format *standard-output* "~%~A is a 1st cousin ~A times removed from ~A.~%~%"
              name1 cr_int name2)
            )
          (2
            (format *standard-output* "~%~A is a 2nd cousin ~A times removed from ~A.~%~%"
              name1 cr_int name2)
            )
          (3
            (format *standard-output* "~%~A is a 3rd cousin ~A times removed from ~A.~%~%"
              name1 cr_int name2)
            )
          (8
            (format *standard-output* "~%~A is an 8th cousin ~A times removed from ~A.~%~%"
              name1 cr_int name2)
            )
          (11
            (format *standard-output* "~%~A is an 11th cousin ~A times removed from ~A.~%~%"
              name1 cr_int name2)
            )
          (18
            (format *standard-output* "~%~A is an 18th cousin ~A times removed from ~A.~%~%"
              name1 cr_int name2)
            )
          (t
            (format *standard-output* "~%~A is a ~Ath cousin ~A times removed from ~A.~%~%"
              name1 cd_int cr_int name2)
            ))) 
        (format *standard-output* "~%~A is not a ~Ath cousin ~A times removed from ~A.~%~%"
              name1 cd_int cr_int name2))

    (format *standard-output* "~%~A is unrelated to ~A.~%~%" name1 name2)

    )))))))))



;;; is name1 the <relation> of name2?
(defun handle-X (name1 relation name2)
(case relation 

  ('spouse
    (if (marriedp name1 name2)
      (format *standard-output* "~%~A is married to ~A.~%~%" name1 name2) 
      (format *standard-output* "~%~A is not married to ~A.~%~%" name1 name2)))

  ('parent
    (if (parentp name1 name2)
      (format *standard-output* "~%~A is a parent of ~A.~%~%" name1 name2) 
      (format *standard-output* "~%~A is not a parent of ~A.~%~%" name1 name2)))

  ('child
    (if (childp name1 name2)
      (format *standard-output* "~%~A is a child of ~A.~%~%" name1 name2) 
      (format *standard-output* "~%~A is not a child of ~A.~%~%" name1 name2)))

  ('sibling
    (if (siblingp name1 name2)
      (format *standard-output* "~%~A is a sibling of ~A.~%~%" name1 name2) 
      (format *standard-output* "~%~A is not a sibling of ~A.~%~%" name1 name2)))

  ('half-sibling
    (if (halfsiblingp name1 name2)
      (format *standard-output* "~%~A is a half-sibling of ~A.~%~%" name1 name2) 
      (format *standard-output* "~%~A is not a half-sibling of ~A.~%~%" name1 name2)))

  ('ancestor
    (if (ancestorp name1 name2)
      (format *standard-output* "~%~A is an ancestor of ~A.~%~%" name1 name2) 
      (format *standard-output* "~%~A is not an ancestor of ~A.~%~%" name1 name2)))

  ('descendant
    (if (descendantp name1 name2)
      (format *standard-output* "~%~A is a descendant of ~A.~%~%" name1 name2) 
      (format *standard-output* "~%~A is not a descendant of ~A.~%~%" name1 name2)))

  (t 
    (setf stream_str_rel (make-string-input-stream (write-to-string relation)))
    (setf read_rel (read stream_str_rel nil nil))
    (setf cousin_str (write-to-string (first read_rel)))

    (when (equal cousin_str (string 'cousin)) 

      (setf cd_int (parse-integer (write-to-string (second read_rel))))
      (setf cr_int (parse-integer (write-to-string (third read_rel))))

      (when (cousinp name1 name2)
      (if (compareCousinDegreeRemoval cd_int cr_int (gethash name1 *p*) (gethash name2 *p*))
        (case cd_int
          (1
            (format *standard-output* "~%~A is a 1st cousin ~A times removed from ~A.~%~%"
              name1 cr_int name2)
            )
          (2
            (format *standard-output* "~%~A is a 2nd cousin ~A times removed from ~A.~%~%"
              name1 cr_int name2)
            )
          (3
            (format *standard-output* "~%~A is a 3rd cousin ~A times removed from ~A.~%~%"
              name1 cr_int name2)
            )
          (8
            (format *standard-output* "~%~A is an 8th cousin ~A times removed from ~A.~%~%"
              name1 cr_int name2)
            )
          (11
            (format *standard-output* "~%~A is an 11th cousin ~A times removed from ~A.~%~%"
              name1 cr_int name2)
            )
          (18
            (format *standard-output* "~%~A is an 18th cousin ~A times removed from ~A.~%~%"
              name1 cr_int name2)
            )
          (t
            (format *standard-output* "~%~A is a ~Ath cousin ~A times removed from ~A.~%~%"
              name1 cd_int cr_int name2)
            ))

        ;;; cousin, but wrong type of cousin
        (format *standard-output* "~%~A and ~A are cousins, but of a different type.~%~%"
          name1 name2)
        ))

    (when (not (cousinp name1 name2))
       
        (case cd_int
          (1
            (format *standard-output* "~%~A is not a 1st cousin ~A times removed from ~A.~%~%"
              name1 cr_int name2)
            )
          (2
            (format *standard-output* "~%~A is not a 2nd cousin ~A times removed from ~A.~%~%"
              name1 cr_int name2)
            )
          (3
            (format *standard-output* "~%~A is not a 3rd cousin ~A times removed from ~A.~%~%"
              name1 cr_int name2)
            )
          (8
            (format *standard-output* "~%~A is not an 8th cousin ~A times removed from ~A.~%~%"
              name1 cr_int name2)
            )
          (11
            (format *standard-output* "~%~A is not an 11th cousin ~A times removed from ~A.~%~%"
              name1 cr_int name2)
            )
          (18
            (format *standard-output* "~%~A is not an 18th cousin ~A times removed from ~A.~%~%"
              name1 cr_int name2)
            )
          (t
            (format *standard-output* "~%~A is not a ~Ath cousin ~A times removed from ~A.~%~%"
              name1 cd_int cr_int name2)
            ))
      )))))



;;; event
(defun handle-E (name1 name2 name3) 
  (declare (special *p*))

  (let ((p1 nil) (p2 nil) (p3 nil))
    (setf p1 (gethash name1 *p*))
    (setf p2 (gethash name2 *p*))

    (when (null (gethash name1 *p*))
      (setf p1 (make-person :name name1))
      (setf (gethash name1 *p*) p1))


    (when (null (gethash name2 *p*))
      (setf p2 (make-person :name name2))
      (setf (gethash name2 *p*) p2))

      
    ;;; add spouses

    (when (not (marriedp name1 name2))
      (if (null (person-spouses p1)) (setf (person-spouses p1) (cons name2 nil))
      (and (setf (person-spouses p1) (cons name2 (person-spouses p1)))
            ))

      (if (null (person-spouses p2)) (setf (person-spouses p2) (cons name1 nil))
      (and (setf (person-spouses p2) (cons name1 (person-spouses p2))) 
        

        ;;; remove duplicate spouses
        (setf (person-spouses p1) (remove-duplicates (person-spouses p1)))
        (setf (person-spouses p2) (remove-duplicates (person-spouses p2)))

        )))


    ;;; add children, parents

    (when name3
      (setf p3 (make-person :name name3))
      (setf (gethash name3 *p*) p3)
      (setf (person-children p1) (cons name3 (person-children p1)))
      (setf (person-children p2) (cons name3 (person-children p2)))
      (setf (person-parents p3) (list name1 name2))


    ;;; add siblings

    (when (intersection (person-children p1) (person-children p2))
      ;;; ilist = list of children common to both parents
      (setf ilist (intersection (person-children p1) (person-children p2)))
      (when (set-difference ilist (person-siblings p3))

        ;;; gives child any siblings they don't have yet
        (setf (person-siblings p3)
          (append (set-difference ilist (person-siblings p3))))
        

        ;;; adds child to older siblings' lists
        (dolist (i ilist) (setf (person-siblings 
          (gethash i *p*)) (cons name3 (person-siblings (gethash i *p*)))))

        ;;; takes away child's name from own siblings list
        (setf (person-siblings p3) (remove name3 (person-siblings p3)))
        
        ;;; removes duplicate siblings
        (setf (person-siblings p3) (remove-duplicates (person-siblings p3)))

         ))


    ;;; add half-siblings

    ;;; from p1's side

    (when (set-difference (person-children p1) (person-children p2))
      ;;; ilist = list of p1's children who are not p2's children
      (setf ilist (set-difference (person-children p1) (person-children p2)))

      (when (set-difference (person-children p1) (person-halfsiblings p3))

        ;;; gives child any half-siblings they don't have yet
        (setf (person-halfsiblings p3)
          (append (set-difference ilist (person-halfsiblings p3))))
        

        ;;; adds child to older half-siblings' lists
        (dolist (i ilist) (setf (person-halfsiblings 
          (gethash i *p*)) (cons name3 (person-halfsiblings (gethash i *p*)))))

        ;;; takes away child's name from own half-siblings list
        (setf (person-halfsiblings p3) (remove name3 (person-halfsiblings p3)))
        
        ;;; removes duplicate half-siblings
        (setf (person-halfsiblings p3) (remove-duplicates (person-halfsiblings p3)))

        ))

    ;;; from p2's side

        (when (set-difference (person-children p2) (person-children p1))
      ;;; ilist = list of p2's children who are not p1's children
      (setf ilist (set-difference (person-children p2) (person-children p1)))

      (when (set-difference (person-children p2) (person-halfsiblings p3))

        ;;; gives child any half-siblings they don't have yet
        (setf (person-halfsiblings p3)
          (append (set-difference ilist (person-halfsiblings p3))))
        

        ;;; adds child to older half-siblings' lists
        (dolist (i (person-children p2)) (setf (person-halfsiblings 
          (gethash i *p*)) (cons name3 (person-halfsiblings (gethash i *p*)))))

        ;;; takes away child's name from own half-siblings list
        (setf (person-halfsiblings p3) (remove name3 (person-halfsiblings p3)))
        
        ;;; removes duplicate half-siblings
        (setf (person-halfsiblings p3) (remove-duplicates (person-halfsiblings p3)))

        ))


   ;;; add ancestors

   ;;; store return of giveAncestors into child's ancestors list
   (setf (person-ancestors p3) (append (giveAncestors p3) (person-ancestors p3)))


   ;;; add descendants

   ;;; iterate through ancestors in the return of giveAncestors, and add child to
   ;;; each of their descendant lists
   (dolist (i (giveAncestors p3))
      (when i
        (setf (person-descendants (gethash i *p*))
          (cons name3 (person-descendants (gethash i *p*))))))


    ;;; add cousins

    (setf everyone nil)
    (maphash (lambda (k v) (setf everyone (cons k everyone))) *p*)

      (dolist (e everyone)
            (when e

    #|
        if the sum of the list lengths of each of both of their ancestor lists are
        greater than the union of both of their ancestor lists, they share a common
        ancestor and are therefore cousins
    |#

      (when (> (+ (list-length (person-ancestors p3)) (list-length 
        (person-ancestors (gethash e *p*))))
                  (list-length (union (person-ancestors p3) 
                               (person-ancestors (gethash e *p*))))) 

        (setf (person-cousins p3) (cons e (person-cousins p3)))

        (setf (person-cousins (gethash e *p*)) (cons name3 (person-cousins (gethash e *p*)))))
      )))
    ))



;;; standard input is read and E, X, W, and R queries are handled
(defun familytree ()
  (let ((input nil))
    (setf input (read *standard-input* nil nil))
    (loop
      (when (null input) (return))
      (case (first input)
        ('E
          (format *standard-output* "~A~%~%" input)
          (handle-E (second input)
                    (third input)
                    (fourth input)))
        ('X
          (print input)
          (handle-X (second input)
                    (third input)
                    (fourth input)))
        ('W
          (print input)
          (handle-W (second input)
                    (third input)))
        ('R
          (print input)
          (handle-R (second input)
                    (third input))
          ))
      ;;; iterate to next line
      (setf input (read *standard-input* nil nil))
      )))

(familytree)