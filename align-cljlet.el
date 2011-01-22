;;; align-cljlet.el  -- Align clojure let functions

;; Copyrigth (C) 2011  Glen Stampoultzis

;; Author: Glen Stampoultzis <gstamp(at)gmail.com>
;; Version: $Id:$
;; Keywords; clojure, align, let
;; URL: https://github.com/gstamp/align-cljlet
;;

;; This file is *NOT* part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;; Description:
;;
;; This program exists because I was tired of manually aligning let
;; statements in clojure.  This program is designed to quickly and
;; easily allow let forms to be aligned.  This is my first emacs
;; lisp program and as a result if probably less than optimal.  Feel
;; free to suggest improvements or send in patches.
;;
;; This program was inspired by align-let.el although does not share
;; any of it's code.  I had considered altering align-let.el to
;; work correctly with Clojure however it was easiler to simply
;; start from scratch.
;; 
;;
;;; Known limitations:
;;
;; * This program requires clojure mode to be running in order to
;;   function correctly.
;;
;;; Installation:
;;
;;   To use align-cljlet.el, put it in your load-path and add
;;   the following to your .emacs
;;
;;   (require 'align-cljlet)
;;
;;; Usage:
;;
;; To invoke simply position anywhere inside the let statement and
;; invoke:
;;
;; M-x align-cljlet
;;
;; You may wish to bound this to a specific key.
;;


(defun try-go-up ()
  (condition-case nil
      (up-list -1)
    (error "Form does not match"))
  t)

(defun get-width ()
  (save-excursion
    (let ((p (point)))
      (forward-sexp)
      (- (point) p))))

(defun find-start (regexp)
  (while (if (looking-at regexp)
             nil
           (try-go-up)))
  t)

(defun goto-next-row (next-row)
  (interactive)
  (condition-case nil
      (progn
        (funcall next-row)
        t)
    (error nil)))

(defun next-sexp ()
  (forward-sexp)
  (forward-sexp)
  (backward-sexp))

(defun calc-width (test next-row)
  (save-excursion
    (loop collect (if (funcall test)
                      (loop collect (get-width) while (next-sexp)))
          while (goto-next-row next-row))))

(defun respace-row (widths test)
  (if (funcall test)
      (save-excursion
        (loop for w in widths do
              (let ((p (point)))
                (next-sexp)
                (let* ((current-width (- (- (point) p) 1))
                       (difference    (- w current-width)))
                  (cond ((> difference 0)
                         (insert (make-string difference 32))) 
                        ((< difference 0)
                         (delete-backward-char (abs difference))))))))))

(defun respace-rows (widths test next-row)
  (loop do (respace-row widths test) while (goto-next-row next-row)))

(defun max-widths (widths)
  (loop for n from 0 to (- (length (first widths)) 1)
        collecting (apply #'max (mapcar (lambda (coll)
                                          (if (< (length coll) (+ 1 n))
                                              1
                                            (nth n coll)))
                                        widths))))

(defun align-section (start goto-start test next-row)
  (interactive)
  (save-excursion
    (if (find-start start)
        (progn (funcall goto-start)
               (respace-rows (butlast (max-widths (calc-width test next-row)))
                             test next-row)))))

(defmacro* defalign (name
                     &key
                     start
                     next-row
                     (test (lambda () t))
                     (goto-start (lambda ()
                                   (down-list 2))))
  `(defun ,(intern (concat "align-" (symbol-name name)))
     ()
     (interactive)
     (align-section ,start ,goto-start ,test ,next-row)))

(defalign cljlet
  :start "\\s(let"
  :next-row (lambda ()
              (forward-sexp)
              (forward-sexp)
              (forward-sexp)
              (backward-sexp)))

(defalign defroutes
  :start "\\s(defroutes"
  :next-row (lambda ()
              (backward-up-list)
              (forward-sexp)
              (forward-sexp)
              (backward-sexp)
              (down-list))
  :test (lambda ()
          (memq (symbol-at-point) '(GET POST))))

(provide 'align-cljlet)

