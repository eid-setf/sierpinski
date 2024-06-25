(defpackage #:sierpinski
  (:use :cl :cl-raylib :3d-vectors))

(in-package #:sierpinski)

(defconstant screen-width 600)
(defconstant screen-height 400)
(defconstant fps 60)

(defvar *target* nil)

;;;; A `tri' is three vectors in a list
(defvar first-tri
  (list (vec screen-width screen-height)
        (vec (/ screen-width 2) 0)
        (vec 0 screen-height)))
(defvar *next-triangles* nil)

;;;; Helper functions
;;; these are actually from the prespective of the triangle, not yours
(defun right-of (tri) (first tri))
(defun mid-of (tri) (second tri))
(defun left-of (tri) (third tri))

(defun draw-tri (tri color)
  (destructuring-bind (rt mt lt) tri
    (draw-triangle rt mt lt color)))

(defun draw-render-texture ()
  (with-drawing
    (clear-background :white)
    (draw-texture-rec (render-texture-texture *target*)
                      (make-rectangle
                       :x 0 :y 0
                       :width (texture-width
                               (render-texture-texture *target*))
                       :height (- (texture-height
                                   (render-texture-texture *target*))))
                      (vec 0 0)
                      :white)))

(defun midpoint (lst)
  (v/ (v+ (car lst) (cadr lst)) 2))

(defun get-combs (tri)
  (list (list (left-of tri) (mid-of tri))   ;left point
        (list (right-of tri) (left-of tri)) ;mid point
        (list (right-of tri) (mid-of tri)))) ;right point

(defun draw-next-gen ()
  (let ((acc nil))
    (dolist (parent *next-triangles*)
      (let ((child (mapcar #'midpoint (get-combs parent))))
        (with-texture-mode (*target*)
          (draw-tri child :white))
        ;; note that the upside-down triangle's left and right operators are reversed because
        ;; it's upside-down
        (push (list (right-of parent) (left-of child) (mid-of child)) acc) ;right small tri
        (push (list (left-of child) (mid-of parent) (right-of child)) acc) ;mid small tri
        (push (list (mid-of child) (right-of child) (left-of parent)) acc))) ;left small tri
    (setf *next-triangles* acc)))

(defun main ()
  (with-window (screen-width screen-height "Sierpinski!")
    (set-target-fps fps)
    (setf *target* (load-render-texture screen-width screen-height))
    (setf *next-triangles* (list first-tri))
    (with-texture-mode (*target*)
      (clear-background :white)
      (draw-tri first-tri :black))
    (loop until (window-should-close)
          for frame = 1 then (1+ frame) ;1 so the triangle starts full
          do (when (= (mod frame 90) 0)
               (draw-next-gen))
             (draw-render-texture))))
