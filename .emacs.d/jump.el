;;; jump.el -- jump to source of what ever is at point

;; $Revision: 713 $
;; $Date: 2005-11-19 17:14:14 +0000 (Sat, 19 Nov 2005) $

;; This file is not part of Emacs

;; Author: Phillip Lord <p.lord@russet.org.uk>
;; Maintainer: Phillip Lord <p.lord@russet.org.uk>
;; Maintainer (XEmacs): Martin Kuehl (martin.kuehl@gmail.com)
;; Website: http://www.russet.org.uk

;; COPYRIGHT NOTICE
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA. 

;;; Code:

(require 'thingatpt)

(defun jump()
  (interactive)
  (let* ((sexp 
          (symbol-at-point))
         (fun (fboundp sexp))
         (var (boundp sexp)))
    (cond ((and fun var)
           (progn 
             (message "There is also a variable \"%s\"" sexp)
             (find-function sexp)))
          (fun 
           (find-function sexp))
          ((and var sexp)
           (find-variable sexp))
          ((not sexp)
           (message "Nothing at point"))
          (t
           (message "There is no function or variable called \"%s\"" sexp)))))



(define-key emacs-lisp-mode-map  "\C-c\C-j" 'jump)

