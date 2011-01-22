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


(defun found-let ()
  (looking-at "\\s(let"))
  

(defun try-go-up ()
  (condition-case nil
      (up-list -1)
    (error
     (error "Not in a \"let\" form")))
  t)

(defun find-let ()
  (while
      (if (found-let)
          nil
        (try-go-up)
        ))
  t)

(defun goto-next-pair ()
  (interactive)
  (condition-case nil
      (progn
        (forward-sexp)
        (forward-sexp)
        (forward-sexp)
        (backward-sexp)
        t)
    (error nil)))

(defun get-width ()
  (save-excursion
    (let ((p (point)))
      (forward-sexp)
      (- (point) p))))

(defun calc-width ()
  (save-excursion
    (let ((width 0))
      (while (progn
               (if (> (get-width) width)
                   (setq width (get-width)))
               (goto-next-pair)))
      width)))

(defun respace-single-let (max-width)
  (save-excursion
    (let (p current-width difference)
      (setq p (point))
      (forward-sexp)
      (forward-sexp)
      (backward-sexp)
      (setq current-width (- (- (point) p) 1)
            difference    (- max-width current-width))
      
      (cond ((> difference 0)
             (insert (make-string difference ? )))
            ((< difference 0)
             (delete-backward-char (abs difference))))
      
      )))

(defun respace-let (width)
  (while (progn
           (respace-single-let width)
           (goto-next-pair))))

(defun align-let ()
  ;; move to start of [
  (down-list 2)
  (let ((w (calc-width)))
    (respace-let w)
    ))

(defun align-cljlet ()
  (interactive)
  (save-excursion
    (if (find-let)
        (align-let))))

(defun find-defroutes ()
  (while
      (if (looking-at "\\s(defroutes")
          nil
        (try-go-up)))
  t)

(defun respace-single-route (widths)
  (if (test-fn)
      (save-excursion
        (loop for w in widths do
              (let ((p (point)))
                (forward-sexp)
                (forward-sexp)
                (backward-sexp)
                (let* ((current-width (- (- (point) p) 1))
                       (difference    (- w current-width)))
                  (cond ((> difference 0)
                         (insert (make-string difference 32))) 
                        ((< difference 0)
                         (delete-backward-char (abs difference))))))))))

(defun goto-next-route ()
  (interactive)
  (condition-case nil
      (progn
        (backward-up-list)
        (forward-sexp)
        (forward-sexp)
        (backward-sexp)
        (down-list)
        t)
    (error nil)))

(defun goto-next-route-part ()
  (interactive)
  (condition-case nil
      (progn
        (forward-sexp)
        (forward-sexp)
        (backward-sexp)
        t)
    (error nil)))

(defun respace-defroutes (max-widths)
  (loop do (respace-single-route max-widths) while (goto-next-route)))

(defun test-fn ()
  (memq (symbol-at-point) '(GET POST)))

(defun calc-width-defroutes ()
  (save-excursion
    (loop collect (if (test-fn)
                      (loop collect (get-width) while (goto-next-route-part)))
          while (goto-next-route))))

(defun max-widths (widths)
  (loop for n from 0 to (- (length (first widths)) 1)
        collecting (apply #'max (mapcar (lambda (coll)
                                          (if (< (length coll) (+ 1 n))
                                              1
                                            (nth n coll)))
                                        widths))))

(defun do-align-defroutes ()
  (down-list 2)
  (respace-defroutes (butlast (max-widths (calc-width-defroutes)))))

(defun align-defroutes ()
  (interactive)
  (save-excursion
    (if (find-defroutes)
        (do-align-defroutes))))

(provide 'align-cljlet)

