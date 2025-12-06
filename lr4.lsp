
(defvar *test-total* 0
  "Загальна кількість запущених тестів.")

(defvar *test-passed* 0
  "Кількість пройдених тестів.")

(defun report-test-result (name passed)
  (incf *test-total*)
  (when passed (incf *test-passed*))
  (format t "~&  ~A: ~:[FAILED~;PASSED~]~%" name passed)
  passed)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  BUBBLE SORT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bubble-sort-functional (lst &key (key #'identity) (test #'>))
  (if (null lst)
      nil
      (labels
          ((prepare-pairs (xs)
             (mapcar (lambda (x) (cons x (funcall key x)))
                     xs))

           (bubble-pass-pairs (pairs)
             (labels ((bubble-step (current rest)
                        (if (null rest)
                            (values (list current) nil)
                            (let ((next (car rest)))
                              (if (funcall test (cdr current) (cdr next))
                                  (multiple-value-bind (tail changed)
                                      (bubble-step current (cdr rest))
                                    (declare (ignore changed))
                                    (values (cons next tail) t))
                                  (multiple-value-bind (tail changed)
                                      (bubble-step next (cdr rest))
                                    (values (cons current tail) changed)))))))
               (if (null pairs)
                   (values nil nil)
                   (bubble-step (car pairs) (cdr pairs)))))

           (iterate (pairs)
             (multiple-value-bind (new-pairs changed)
                 (bubble-pass-pairs pairs)
               (if changed
                   (iterate new-pairs)
                   new-pairs))))

        (mapcar #'car (iterate (prepare-pairs lst))))))


;;; Тести bubble-sort
(defun check-bubble-sort-case (name input expected &key (key #'identity) (test #'>))
  (let* ((original (copy-list input))
         (result (bubble-sort-functional input :key key :test test)))
    (report-test-result
     name
     (and (equal result expected)
          (equal input original)))))


(defun test-bubble-sort ()
  (format t "~&=== ТЕСТУВАННЯ BUBBLE SORT ===~%")
  (setf *test-total* 0
        *test-passed* 0)

  (check-bubble-sort-case "empty list" '() '())
  (check-bubble-sort-case "single element" '(1) '(1))
  (check-bubble-sort-case "two elements reversed" '(2 1) '(1 2))
  (check-bubble-sort-case "random list" '(3 1 4 1 5 2) '(1 1 2 3 4 5))
  (check-bubble-sort-case "reverse sorted" '(5 4 3 2 1) '(1 2 3 4 5))
  (check-bubble-sort-case "already sorted" '(1 2 3 4 5) '(1 2 3 4 5))
  (check-bubble-sort-case "with negatives" '(0 -1 3 -2 2) '(-2 -1 0 2 3))
  (check-bubble-sort-case "all identical" '(1 1 1 1) '(1 1 1 1))

  ;; KEY
  (check-bubble-sort-case "sort by absolute value"
                          '(-3 1 -2 4) '(1 -2 -3 4)
                          :key #'abs)

  ;; TEST
  (check-bubble-sort-case "descending order"
                          '(1 3 2 5 4) '(5 4 3 2 1)
                          :test #'<)

  (check-bubble-sort-case "descending by abs"
                          '(-1 3 -4 2) '(-4 3 2 -1)
                          :key #'abs :test #'<)

  (format t "~&Bubble-sort tests: ~D / ~D passed.~%"
          *test-passed* *test-total*))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ЧАСТИНА 2 duplicate-elements-reducer 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun duplicate-elements-reducer (n &key (duplicate-p (constantly t)))
  (lambda (acc elem)
    (if (funcall duplicate-p elem)
        (append acc (make-list n :initial-element elem))
        (append acc (list elem)))))


(defun check-duplicate-reducer-case (name n list expected
                                      &key (predicate (constantly t)))
  (let* ((reducer (duplicate-elements-reducer n :duplicate-p predicate))
         (result  (reduce reducer list :initial-value '())))
    (report-test-result name (equal result expected))))


(defun test-duplicate-elements-reducer ()
  (format t "~&=== ТЕСТУВАННЯ DUPLICATE-ELEMENTS-REDUCER ===~%")
  (setf *test-total* 0
        *test-passed* 0)

  (check-duplicate-reducer-case
   "duplicate all elements ×2"
   2 '(1 2 3) '(1 1 2 2 3 3))

  (check-duplicate-reducer-case
   "duplicate even elements ×3"
   3 '(1 2 3) '(1 2 2 2 3)
   :predicate #'evenp)

  (check-duplicate-reducer-case
   "empty list"
   2 '() '())

  (check-duplicate-reducer-case
   "n = 1 (без дублювання)"
   1 '(a b c) '(a b c))

  (check-duplicate-reducer-case
   "predicate always NIL"
   5 '(1 2 3) '(1 2 3)
   :predicate (constantly nil))

  (check-duplicate-reducer-case
   "duplicate > 10"
   4 '(5 12 3 20)
   '(5 12 12 12 12 3 20 20 20 20)
   :predicate (lambda (x) (> x 10)))

  (format t "~&Duplicate-elements tests: ~D / ~D passed.~%"
          *test-passed* *test-total*))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Запуск тестів
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-all-tests ()
  (test-bubble-sort)
  (test-duplicate-elements-reducer))
  
