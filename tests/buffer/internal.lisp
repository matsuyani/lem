(defpackage :lem-tests/buffer/internal
  (:use :cl
        :rove))
(in-package :lem-tests/buffer/internal)

(defun check-corruption (buffer)
  (handler-case (lem/buffer/internal:check-buffer-corruption buffer)
    (lem/buffer/internal:corruption-warning ()
      (fail "corruption"))))

(defun collect-line-plist (buffer)
  (loop :for line := (lem/buffer/internal::point-line (lem:buffer-start-point buffer))
        :then (lem/buffer/line:line-next line)
        :while line
        :collect (lem/buffer/line:line-plist line)))

(deftest insert-newline-test
  ;; Arrange
  (let* ((buffer (lem:make-buffer "test" :temporary t))
         (point (lem:buffer-point buffer)))
    (lem:insert-string point "a" :key1 100)
    (lem:insert-string point "bcdefg" :key2 200)
    (lem:insert-string point "hijklmnopqrstuvwxyz" :key3 300)

    ;; Act
    (lem:move-to-line point 1)
    (lem:move-to-column point 2)
    (lem:insert-character point #\newline)

    (lem:move-to-line point 2)
    (lem:move-to-column point 4)
    (lem:insert-character point #\newline)

    (lem:move-to-line point 3)
    (lem:move-to-column point 10)
    (lem:insert-character point #\newline)

    ;; Assertions
    (check-corruption buffer)
    (ok (= 4 (lem:buffer-nlines buffer)))
    (ok (equal "ab
cdef
ghijklmnop
qrstuvwxyz"
               (lem:buffer-text buffer)))
    (ok (equal '((:KEY3 NIL :KEY2 ((1 2 200)) :KEY1 ((0 1 100 NIL)))
                 (:KEY3 NIL)
                 (:KEY3 ((1 10 300)))
                 NIL)
               (collect-line-plist buffer)))))

(deftest undo-redo
  (let* ((buffer (lem:make-buffer "test" :temporary t))
         (point (lem:buffer-point buffer)))
    (lem:insert-string point "Hello")
    (lem:buffer-undo-boundary buffer)
    (lem:insert-string point " World")
    (lem:buffer-undo-boundary buffer)
    (lem:buffer-undo point)
    (ok (equal "Hello" (lem:buffer-text buffer)))
    (lem:buffer-redo point)
    (ok (equal "Hello World" (lem:buffer-text buffer)))

    (check-corruption buffer)))

(deftest |`buffer-end-point` points to the end of the buffer|
  ;; Arrange
  (let* ((buffer (lem:make-buffer "test" :temporary t))
         (point (lem:buffer-point buffer)))
    (lem:insert-string point "aaaaaaaaaa")

    ;; Act
    (lem:move-to-line point 1)
    (lem:move-to-column point 5)
    (lem:delete-character point 10)

    ;; Assertion
    (let ((end-point (lem:buffer-end-point buffer)))
      (ok (= 5 (lem:point-charpos end-point)))
      (ok (= 1 (lem:line-number-at-point point))))
    (check-corruption buffer)))
