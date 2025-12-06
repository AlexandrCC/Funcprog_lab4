<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 4</b><br/>
"Робота з послідовностями та замиканнями"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right"><b>Студент</b>: Савельєв Олександр КВ-22</p>
<p align="right"><b>Рік</b>: 2025</p>

## Загальне завдання
Завдання складається з двох частин:
1. Переписати функціональну реалізацію алгоритму сортування з лабораторної
роботи 3 з такими змінами:
використати функції вищого порядку для роботи з послідовностями (де/якщо
це доречно, в разі, якщо функції вищого порядку не були використані при
реалізації л.р. No3);

додати до інтерфейсу функції (та використання в реалізації) два ключових
параметра: key та test , що працюють аналогічно до того, як працюють
параметри з такими назвами в функціях, що працюють з послідовностями (р.
12). При цьому key має виконатись мінімальну кількість разів.

2. Реалізувати функцію, що створює замикання, яке працює згідно із завданням за
варіантом (див. п 4.1.2). Використання псевдофункцій не забороняється, але, за
можливості, має бути зменшене до необхідного мінімуму.

## Варіант <22>
Написати функцію duplicate-elements-reducer , яка має один основний параметр n та
один ключовий параметр — функцію duplicate-p . duplicate-elements-reducer має
повернути функцію, яка при застосуванні в якості першого аргументу reduce робить
наступне: при обході списку, кожен елемент списка-аргумента reduce , для якого
функція duplicate-p повертає значення t (або не nil ), дублюється n разів. Якщо
користувач не передав функцію duplicate-p у duplicate-elements-fn , тоді
дублюються всі елементи вхідного списку. Обмеження, які накладаються на

використання функції-результату duplicate-elements-reducer при передачі у reduce
визначаються розробником (тобто, наприклад, необхідно чітко визначити, якими мають
бути значення ключових параметрів функції reduce from-end та initial-value ).

## Лістинг (функціональний підхід, `key`/`test`)
```lisp
  (if (null lst)
      nil
      (labels
          ((prepare-pairs (xs)
             (mapcar (lambda (x) (cons x (funcall key x))) xs))
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
```

### Тестові утиліти для bubble sort
```lisp
(defun check-bubble-sort-case (name input expected &key (key #'identity) (test #'>))
  (let* ((original (copy-list input))
         (result (bubble-sort-functional input :key key :test test))
         (passed (and (equal result expected)
                      (equal input original))))
    (report-test-result name passed)))

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
  (check-bubble-sort-case "sort by absolute value" '(-3 1 -2 4) '(1 -2 -3 4) :key #'abs)
  (check-bubble-sort-case "descending order" '(1 3 2 5 4) '(5 4 3 2 1) :test #'<)
  (check-bubble-sort-case "descending by abs" '(-1 3 -4 2) '(-4 3 2 -1) :key #'abs :test #'<)

  (format t "~&Bubble-sort tests: ~D / ~D passed.~%" *test-passed* *test-total*))
```

## Лістинг замикання `duplicate-elements-reducer`
```lisp
(defun duplicate-elements-reducer (n &key (duplicate-p (constantly t)))
  (lambda (acc elem)
    (if (funcall duplicate-p elem)
        (append acc (make-list n :initial-element elem))
        (append acc (list elem)))))
```

### Тестові утиліти для замикання
```lisp
(defun check-duplicate-reducer-case (name n list expected &key (predicate (constantly t)))
  (let* ((reducer (duplicate-elements-reducer n :duplicate-p predicate))
         (result (reduce reducer list :initial-value '()))
         (passed (equal result expected)))
    (report-test-result name passed)))

(defun test-duplicate-elements-reducer ()
  (format t "~&=== ТЕСТУВАННЯ DUPLICATE-ELEMENTS-REDUCER ===~%")
  (setf *test-total* 0
        *test-passed* 0)

  (check-duplicate-reducer-case "duplicate all elements ×2" 2 '(1 2 3) '(1 1 2 2 3 3))
  (check-duplicate-reducer-case "duplicate even elements ×3" 3 '(1 2 3) '(1 2 2 2 3) :predicate #'evenp)
  (check-duplicate-reducer-case "empty list" 2 '() '())
  (check-duplicate-reducer-case "n = 1" 1 '(a b c) '(a b c))
  (check-duplicate-reducer-case "predicate always NIL" 5 '(1 2 3) '(1 2 3) :predicate (constantly nil))
  (check-duplicate-reducer-case "duplicate only numbers > 10" 4 '(5 12 3 20) '(5 12 12 12 12 3 20 20 20 20) :predicate (lambda (x) (> x 10)))

  (format t "~&Duplicate-elements tests: ~D / ~D passed.~%" *test-passed* *test-total*))
```

## Запуск тестів
```sh
sbcl --load lr4.lsp --eval "(run-all-tests)" --eval "(quit)"
```

## Результат
Усі наявні тести проходять успішно.
