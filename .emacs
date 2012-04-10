;; Enviroment 
;; ============================================================================

(defun is-osx()
  (eq system-type 'darwin))

(defun is-linux()
  (eq system-type 'gnu/linux))

(if window-system
    (let ()
      (cond ((is-osx)
             (set-face-font 'default "-apple-inconsolata-medium-r-normal--14-140-72-72-m-140-iso10646-1")
             (setq mac-allow-anti-aliasing nil))
            ((is-linux)
             (set-default-font "Inconsolata-10")))))

;; Standard Configurationn
;; ============================================================================

(custom-set-variables
 '(case-fold-search t)
 '(current-language-environment "UTF-8")
 '(default-input-method "rfc1345")
 '(global-font-lock-mode t nil (font-lock))
 '(linum ((t (:inherit shadow :background "grey7"))))
 '(load-home-init-file t t)
 '(mouse-wheel-mode t nil (mwheel))
 '(safe-local-variable-values
   '((Syntax . COMMON-LISP)
     (Package . FLEXI-STREAMS)
     (Base . 10)))
 '(show-paren-mode t)
 '(transient-mark-mode t))

;; Path Setting Code
;; ============================================================================
(defmacro add-to-load-path (path)
  `(add-to-list 'load-path  ,path))

(defun include-path (folder)
  (add-to-load-path (concat emacs-directory "/" folder)))

;; Paths
;; ============================================================================

(setq home (getenv "HOME"))
(setq emacs-directory (concat home "/.emacs.d"))

(add-to-load-path emacs-directory)

(include-path "yasnippet")
(include-path "erc")
(include-path "django-mode")
;(include-path "auctex")
;(include-path "slime")

;; Requires
;; ============================================================================

(require 'tramp)
(require 'django-html-mode)
(require 'django-mode)
(require 'color-theme)
(require 'mic-paren)
(require 'yaml-mode)
(require 'generic-x)
(require 'pabbrev)
(require 'wide-column)
(require 'linum)
(require 'redo+)
(require 'pabbrev)
(require 'undo-tree)
(require 'highline)


;; Help Functions
;; ============================================================================

(defun send-buffer-to-open ()
  (interactive)
  (let
      ((pg (cond ((is-linux) "kde-open ") ((is-osx) "open"))))
    (unless
        (string= ""
               (shell-command-to-string
                (concat pg buffer-file-name)))
    (message "Open file...")
    (start-process (concat pg buffer-file-name)
                   nil pg buffer-file-name)
    (message "Open file... done"))))

(defun sublist (start end x)
  (if (= start 0)
	(head (1+ end) x)
    (sublist (1- start) (1- end) (cdr x))))

; (head 2 '("a" "b" "c")) -> ("a" "b")
; (head 0 '(1 2 3))
; (head -1 '(1 2 3))
(defun head(n str)
  "Takes first n elements from the list str."
  (if (= n 0)
      nil
    (if (> 0 n)
	(head (+ (length str) n) str)
      (cons (car str) (head (1- n)(cdr str))))))

(defun open-including-folder ()
  (interactive)
  (let
      ((pg (cond ((is-linux) "kde-open ") (is-osx) "open"))
       (directory
        (concat "/"
                (reduce
                 (lambda (x y) (concat x "/" y))
                 (let ((lst (split-string buffer-file-name "/")))
                   (sublist 1 (- (list-length  lst) 2) lst))))))
  (unless
      (string= ""
               (shell-command-to-string
                (concat pg directory)))
    (message "Open directory...")
    (start-process (concat pg directory)
                   nil pg directory)
    (message "Open directory... done"))))


(defun comments-in-lisp (map)
    (define-key map (kbd ";") (lambda ()
                              (interactive)
                              (insert ";; "))))

(defun ide-mode ()
  (interactive)
  (let ((s-f (selected-frame)))
    (save-excursion
      (set-frame-height s-f 50)
      (set-frame-width s-f 185)
      (set-frame-position s-f 50 0)
      (split-window-vertically)
      (split-window-horizontally)
      (other-window 1)
      (other-window 1)
      (split-window-horizontally)
      (other-window 1)
      (other-window 1))))

(defun mac-toggle-max-window ()
	  (interactive)
	  (set-frame-parameter nil 'fullscreen (if (frame-parameter nil 'fullscreen)
	                                           nil
	                                           'fullboth)))
(defun fullscreen ()
      (interactive)
      (mac-toggle-max-window))

;; Settings
;; ============================================================================
(setq user-full-name "Artur Ventura")
(setq user-mail-address "artur.ventura@surf-the-edge.com")

(line-number-mode      t)
(column-number-mode    t)
(fset 'yes-or-no-p 'y-or-n-p)
(setq inhibit-startup-message t)
(tool-bar-mode 0)
(setq visible-bell t)
(highline-mode t)
(global-undo-tree-mode)

;; Override CUA behavior
(setq mouse-drag-copy-region nil)
(setq x-select-enable-primary nil)  
(setq x-select-enable-clipboard t)  
(setq select-active-regions t) 
(cua-mode t)

(paren-activate)
(setq paren-sexp-mode t)


;; Keybindings
;; ============================================================================

(global-set-key [(M z)] 'undo)
(global-set-key  [(M shift z)] 'redo) 

(global-set-key  [(M up)] 'scroll-down)
(global-set-key  [(M down)] 'scroll-up)

(global-set-key  [(M left)] 'beginning-of-line)
(global-set-key [(M right)] 'end-of-line)
(global-set-key [?\C-x ?\C-o] 'send-buffer-to-open)
(global-set-key [?\C-x ?\C-d] 'open-including-folder)
(global-set-key [?\C-x ?\C-i] 'ide-mode)

(global-set-key "\M-2" (lambda ()
                              (interactive)
                              (insert "@")))

(global-set-key [(meta f)] 'fullscreen)



(defun hl-off () (highline-mode 0))

(pabbrev-mode t)
(add-hook 'find-file-hook '(lambda () (linum-mode 1)))
(defun go-black ()
  (interactive)
  (let ()
    (set-face-background 'paren-face-match "ForestGreen")
    (color-theme-dark-laptop)
    (set-face-background 'highline-face "purple4")))

(defun go-blue ()
  (interactive)
  (let ()
    (set-face-background 'paren-face-match "ForestGreen")
    (color-theme-charcoal-black)
    (set-face-background 'highline-face "purple4")))

(defun go-white ()
  (interactive)
  (let ()
    (set-face-background 'paren-face-match "PaleTurquoise1")
    (color-theme-emacs-21)
        (set-face-background 'highline-face "lemon chiffon")))

(set-face-background 'paren-face-match "PaleTurquoise1")



(load-library "autoinsert")
(setq auto-insert-copyright (user-full-name))
(setq auto-insert-query nil)

(setq auto-insert-alist
    '(
      ((perl-mode . "Perl Program")
      nil
      "#! /usr/bin/perl -w\n\n"
      "# -*- Mode: Perl -*-\n"
      "# -*- coding: UTF-8 -*-\n"
      "# Copyright (C) " (substring (current-time-string) -4)
      " by " auto-insert-copyright "\n#\n"
      "# File: " (file-name-nondirectory buffer-file-name) "\n"
       "# Time-stamp: "(current-time-string)"\n#\n"
      
     
      "# Author: "(user-full-name) "\n#\n"
      ""
      )

      ((python-mode . "Python Program")
      nil
      "#! /usr/bin/python\n\n"
      "# -*- Mode: Python -*-\n"
      "# -*- coding: UTF-8 -*-\n"
      "# Copyright (C) " (substring (current-time-string) -4)
      " by " auto-insert-copyright "\n#\n"
      "# File: " (file-name-nondirectory buffer-file-name) "\n"
       "# Time-stamp: "(current-time-string)"\n#\n"
      
     
      "# Author: "(user-full-name) "\n#\n"
      ""
      )

      ((lisp-mode . "Lisp Program")
       nil
       ";; -*- Mode: Lisp -*-\n"	;
       ";; -*- coding: UTF-8 -*-\n"
       ";; Copyright (C) " (substring (current-time-string) -4)
       "by " auto-insert-copyright "\n;;\n"
       ";; File: " (file-name-nondirectory buffer-file-name) "\n"
       ";; Time-stamp: "(current-time-string)"\n;;\n"
       
       ";; Author: "(user-full-name) "\n;;\n"
       ""
       )

      ((emacs-lisp-mode . "Emacs Lisp Program")
       nil
       ";; -*- Mode: Emacs-Lisp -*-\n"
       ";; -*- coding: UTF-8 -*-\n"       
       ";; Copyright (C) " (substring (current-time-string) -4)
       " by " auto-insert-copyright "\n ;;\n"
       ";; File: " (file-name-nondirectory buffer-file-name) "\n"
       ";; Time-stamp: "(current-time-string)"\n ;;\n"
       
       ";; Author: "(user-full-name) "\n ;;\n"
       ""
       )

      ((c-mode . "C Program")
       nil
       "/* -*- Mode: C -*-\n"
       " * -*- coding: UTF-8 -*-\n"
       " * Copyright (C) " (substring (current-time-string) -4)
       " by " auto-insert-copyright "\n *\n"
       " * File: " (file-name-nondirectory buffer-file-name) "\n"
       " * Time-stamp: "(current-time-string)"\n *\n"
       
       " * Author: "(user-full-name) "\n *\n"
       " */"
       ""
       )

      ((c++-mode . "C++ Program")
       nil
       "// -*- Mode: C++ -*-\n"
       "// -*- coding: UTF-8 -*-\n"
       "/* Copyright (C) " (substring (current-time-string) -4)
       " by " auto-insert-copyright "\n *\n"
       " * File: " (file-name-nondirectory buffer-file-name) "\n"
       " * Time-stamp: "(current-time-string)"\n *\n"
       
       " * Author: "(user-full-name) "\n *\n"
       " */"
       )
      ((javascript-generic-mode . "Js Program")
       nil
       "/* -*- Mode: Javascript -*-\n"
       " * -*- coding: UTF-8 -*-\n"
       " * Copyright (C) " (substring (current-time-string) -4)
       " by " auto-insert-copyright "\n *\n"
       " * File: " (file-name-nondirectory buffer-file-name) "\n"
       " * Time-stamp: "(current-time-string)"\n *\n"
       
       " * Author: "(user-full-name) "\n"
       " */"
       )
      
      ((java-mode . "java Program")
       nil
       "// -*- Mode: Java -*-\n"
       "// -*- coding: UTF-8 -*-\n"
       "/* Copyright (C) " (substring (current-time-string) -4)
       " by " auto-insert-copyright "\n *\n"
       " * File: " (file-name-nondirectory buffer-file-name) "\n"
       " * Time-stamp: "(current-time-string)"\n *\n"
       
       " * Author: "(user-full-name) "\n *\n"
       " */\n"
       )
))
(add-hook 'python-mode-hook
          (lambda ()
            (hs-minor-mode t) (pabbrev-mode t)
            (set (make-variable-buffer-local 'beginning-of-defun-function)
                 'py-beginning-of-def-or-class)
            (setq outline-regexp "def\\|class ")))

