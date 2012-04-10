;;; lua-mode.el -- Major mode for editing Lua sources
;;; Copyright (C) 2001 Ed Sinjiashvili

;;; Author:	Ed Sinjiashvili (slim@encred.ru)
;;; Created:	April 2001
;;; Keywords:	lua tools

;;; $Id: lua-mode.el,v 1.5 2001/04/10 11:22:49 slim Exp $

;;; Some code snippets were taken from python-mode.el which is
;;; Copyright (C) 1992,1993,1994  Tim Peters
;;; Author: 1995-1998 Barry A. Warsaw
;;;         1992-1994 Tim Peters

;;; Other sources of inspiration are ruby-mode.el and 
;;; inf-ruby.el by Yukihiro Matsumoto.

;;; This software is provided as-is, without express or implied
;;; warranty.  Permission to use, copy, modify, distribute or sell this
;;; software, without fee, for any purpose and by any individual or
;;; organization, is hereby granted, provided that the above copyright
;;; notice and this paragraph appear in all copies.

;;; TODO: smart indenting

;;; Usage:
;;; (0) check lua-program-name variable 
;;; (1) modify .emacs to use lua-mode 
;;;     for example :
;;;
;;;    (autoload 'lua-mode "lua-mode"
;;;      "Mode for editing lua source files")
;;;    (setq auto-mode-alist
;;;          (append '(("\\.lua$" . lua-mode)) auto-mode-alist))
;;;    (setq interpreter-mode-alist (append '(("lua" . lua-mode))
;;;    				     interpreter-mode-alist))
;;;    

(require 'comint)

(defvar lua-temp-directory
  (let ((ok '(lambda (x)
	       (and x
		    (setq x (expand-file-name x)) ; always true
		    (file-directory-p x)
		    (file-writable-p x)
		    x))))
    (or (funcall ok (getenv "TMPDIR"))
	(funcall ok "/usr/tmp")
	(funcall ok "/tmp")
	(funcall ok  ".")
	(error
	 "Couldn't find a usable temp directory -- set `lua-temp-directory'")))
  "*Directory used for temp files created by a *lua* process.
By default, the first directory from this list that exists and that you
can write into:  the value (if any) of the environment variable TMPDIR,
/usr/tmp, /tmp, or the current directory.")
     

(defvar lua-program-name "lua"
  "*Program invoked by run-lua command")

(defvar inferior-lua-mode-hook nil
  "*Hook for customizing inferior lua mode.")

(defvar inferior-lua-mode-map nil
  "*Mode map for inferior lua mode.")

(cond ((not inferior-lua-mode-map)
       (setq inferior-lua-mode-map 
	     (copy-keymap comint-mode-map))
       (define-key inferior-lua-mode-map "\C-c\C-l" 'lua-load-file)))

;;;###autoload 
(defun inf-lua-keys ()
  "Set inf lua keys definitiions into lua mode map"

  (define-key lua-mode-map "\M-\C-x" 'lua-send-definition)
  (define-key lua-mode-map "\C-c\C-e" 'lua-send-definition)
  (define-key lua-mode-map "\C-c\M-e" 'lua-send-definition-and-go)
  (define-key lua-mode-map "\C-c\C-r" 'lua-send-region)
  (define-key lua-mode-map "\C-c\M-r" 'lua-send-region-and-go)
  (define-key lua-mode-map "\C-c\C-c" 'lua-send-buffer)
  (define-key lua-mode-map "\C-c\C-v" 'lua-send-line)
  (define-key lua-mode-map "\C-c\C-z" 'switch-to-lua)
  (define-key lua-mode-map "\C-c\C-l" 'lua-load-file)
  (define-key lua-mode-map "\C-c\C-s" 'run-lua))

(defvar lua-buffer nil
  "current lua buffer")

(defvar inferior-lua-prompt-pattern "^> *"
  "prompt regexp for lua shell")

(defun inferior-lua-mode ()
  "Major mode for interacting with inferior lua process.
The following commands are available:
\\{inferior-lua-mode-map}

A Lua process can be fired up with M-x run-lua.

Customization: Entry to this mode runs the hooks on comint-mode-hook and
inferior-lua-mode-hook (in that order).

You can send text to the inferior Lua process from other buffers containing
Lua source.  
    switch-to-lua switches the current buffer to the Lua process buffer.
    lua-send-definition sends the current definition to the Lua process.
    lua-send-region sends the current region to the Lua process.

    lua-send-definition-and-go, lua-send-region-and-go switch 
    to the Lua process buffer after sending their text.


Commands:
Return after the end of the process' output sends the text from the 
    end of process to point.
Delete converts tabs to spaces as it moves back.
Tab indents for Lua; with argument, shifts rest
    of expression rigidly with the current line.
C-M-q does Tab on each line starting within following expression.
Paragraphs are separated only by blank lines.  Semicolons start comments.
If you accidentally suspend your process, use \\[comint-continue-subjob]
to continue it."
  (interactive)
  (comint-mode)

  (setq comint-prompt-regexp inferior-lua-prompt-pattern)
  (lua-mode-variables)
  (setq major-mode 'inferior-lua-mode)
  (setq mode-name "Inferior Lua")
  (setq mode-line-process '(":%s"))
  (use-local-map inferior-lua-mode-map)
  (setq comint-input-filter (function lua-input-filter))
  (run-hooks 'inferior-lua-mode-hook))

(defvar inferior-lua-filter-regexp "\\`\\s *\\S ?\\S ?\\s *\\'"
  "*Input matching this regexp are not saved on the history list.
Defaults to a regexp ignoring all inputs of 0, 1, or 2 letters.")

(defun lua-input-filter (str)
  "Don't save anything matching inferior-lua-filter-regexp"
  (not (string-match inferior-lua-filter-regexp str)))

(defun lua-args-to-list (string)
  (let ((where (string-match "[ \t]" string)))
    (cond ((null where) (list string))
	  ((not (= where 0))
	   (cons (substring string 0 where)
		 (lua-args-to-list (substring string (+ 1 where)
						 (length string)))))
	  (t (let ((pos (string-match "[^ \t]" string)))
	       (if (null pos)
		   nil
		 (lua-args-to-list (substring string pos
						 (length string)))))))))

;;;###autoload
(defun run-lua (cmd)
  "Run an inferior Lua process, input and output via buffer *lua*.
If there is a process already running in `*lua*', switch to that buffer.
With argument, allows you to edit the command line (default is value
of `lua-program-name').  Runs the hooks `inferior-lua-mode-hook'
\(after the `comint-mode-hook' is run).
\(Type \\[describe-mode] in the process buffer for a list of commands.)"
  (interactive (list (if current-prefix-arg
			 (read-string "Run Lua: " lua-program-name)
			 lua-program-name)))
  (if (not (comint-check-proc "*lua*"))
      (let ((cmdlist (lua-args-to-list cmd)))
	(set-buffer (apply 'make-comint "lua" (car cmdlist)
			   nil (cdr cmdlist)))
	(inferior-lua-mode)))
  (setq lua-program-name cmd)
  (setq lua-buffer "*lua*")
  (pop-to-buffer "*lua*"))

(defun inf-lua-execute-file (proc filename)
  "Send to Lua interpreter process PROC \"dofile('FILENAME')\".
Make that process's buffer visible and force display.  Also make
comint believe the user typed this string so that
`kill-output-from-shell' does The Right Thing."
  (let ((curbuf (current-buffer))
	(procbuf (process-buffer proc))
;	(comint-scroll-to-bottom-on-output t)
	(msg (format "-- working on region in file %s...\n" filename))
	(cmd (format "dofile('%s')\n" filename)))
    (unwind-protect
	(save-excursion
	  (set-buffer procbuf)
	  (goto-char (point-max))
	  (move-marker (process-mark proc) (point))
	  (funcall (process-filter proc) proc msg))
      (set-buffer curbuf))
    (process-send-string proc cmd)))

(defun lua-send-region (start end)
  "Send the current region to the inferior Lua process."
  (interactive "r")
  (let* ((temp (make-temp-name "lua-"))
       (file (expand-file-name temp lua-temp-directory)))
       
    (write-region start end file nil 'nomsg)
    (inf-lua-execute-file (lua-proc) file)
))

(defun lua-send-buffer ()
  "Send whole buffer to inferior lua"
  (interactive)
  (lua-send-region (point-min) (point-max)))

(defun lua-send-line()
  "Send current line to inferior lua process"
  (interactive)
  (lua-send-region (point-at-bol) (point-at-eol)))

(defconst lua-block-beg-re "function\\|if\\|while\\|for\\|do")
(defconst lua-block-end-re "end")  

(defun lua-end-of-defun (&optional arg)
  "Move forward to next end of defun.
An end of a defun is found by moving forward from the beginning of one."
  (interactive "p")
  (and (re-search-forward (concat "^\\(" lua-block-end-re "\\)\\($\\|\\b[^_]\\)")
			  nil 'move (or arg 1))
       (progn (beginning-of-line) t))
  (forward-line 1))

(defun lua-beginning-of-defun (&optional arg)
  "Move backward to next beginning-of-defun.
With argument, do this that many times.
Returns t unless search stops due to end of buffer."
  (interactive "p")
  (and (re-search-backward (concat "^\\(" lua-block-beg-re "\\)\\b")
			   nil 'move (or arg 1))
       (progn (beginning-of-line) t)))

(defun lua-send-definition ()
  "Send the current definition to the inferior Lua process."
  (interactive)
  (save-excursion
    (lua-end-of-defun)
    (let ((end (point)))
      (lua-beginning-of-defun)
      (lua-send-region (point) end))))

(defun switch-to-lua (eob-p)
  "Switch to the Lua process buffer.
With argument, positions cursor at end of buffer."
  (interactive "P")
  (if (get-buffer lua-buffer)
      (pop-to-buffer lua-buffer)
      (error "No current process buffer. See variable lua-buffer."))
  (cond (eob-p
	 (push-mark)
	 (goto-char (point-max)))))

(defun lua-send-region-and-go (start end)
  "Send the current region to the inferior Lua process.
Then switch to the process buffer."
  (interactive "r")
  (lua-send-region start end)
  (switch-to-lua t))

(defun lua-send-definition-and-go ()
  "Send the current definition to the inferior Lua. 
Then switch to the process buffer."
  (interactive)
  (lua-send-definition)
  (switch-to-lua t))

(defvar lua-source-modes '(lua-mode)
  "*Used to determine if a buffer contains Lua source code.
If it's loaded into a buffer that is in one of these major modes, it's
considered a lua source file by lua-load-file.
Used by these commands to determine defaults.")

(defvar lua-prev-l/c-dir/file nil
  "Caches the last (directory . file) pair.
Caches the last pair used in the last lua-load-file command.
Used for determining the default in the 
next one.")

(defun lua-load-file (file-name)
  "Load a lua file into the inferior Lua process."
  (interactive (comint-get-source "Load lua file: " lua-prev-l/c-dir/file
				  lua-source-modes t)) ; T because LOAD 
                                                          ; needs an exact name
  (comint-check-source file-name) ; Check to see if buffer needs saved.
  (setq lua-prev-l/c-dir/file (cons (file-name-directory    file-name)
				       (file-name-nondirectory file-name)))
  (inf-lua-execute-file (lua-proc) file-name))


(defun lua-proc ()
  "Returns the current lua process. See variable lua-buffer."
  (let ((proc (get-buffer-process (if (eq major-mode 'inferior-lua-mode)
				      (current-buffer)
				    lua-buffer))))
    (or proc
	(error "No current process. See variable lua-buffer"))))

;;; Do the user's customization...

(defvar inf-lua-load-hook nil
  "This hook is run when inf-lua is loaded in.
This is a good place to put keybindings.")
	
(run-hooks 'inf-lua-load-hook)

(defvar lua-mode-hook nil
  "*Hook for customizing lua-mode")

(defvar lua-mode-map nil
  "Keymap used in lua mode")
(unless lua-mode-map
  (let ((map (make-sparse-keymap)))
    (setq lua-mode-map map)
    (inf-lua-keys)))

(defvar lua-mode-syntax-table nil
  "Syntax table for lua code buffers")
(if lua-mode-syntax-table
    ()
  (setq lua-mode-syntax-table (make-syntax-table))
  ;; strings
  (modify-syntax-entry ?\' "\"" lua-mode-syntax-table)
  (modify-syntax-entry ?\" "\"" lua-mode-syntax-table)
  ;; escape sequence
  (modify-syntax-entry ?\\ "\\" lua-mode-syntax-table)
  ;; comments
  (modify-syntax-entry ?- ". 12" lua-mode-syntax-table)
  (modify-syntax-entry ?# "<" lua-mode-syntax-table)
  (modify-syntax-entry ?\n ">" lua-mode-syntax-table)
  ;; _ is the only synbol allowed in words
  (modify-syntax-entry ?\_ "_" lua-mode-syntax-table)
  ;; parens, etc.
  (modify-syntax-entry ?\( "()" lua-mode-syntax-table)
  (modify-syntax-entry ?\) ")(" lua-mode-syntax-table)
  (modify-syntax-entry ?\{ "(}" lua-mode-syntax-table)
  (modify-syntax-entry ?\} "){" lua-mode-syntax-table)
  (modify-syntax-entry ?\[ "(]" lua-mode-syntax-table)
  (modify-syntax-entry ?\] ")[" lua-mode-syntax-table)
  ;; punctuation
  (modify-syntax-entry ?\+ "." lua-mode-syntax-table)
  (modify-syntax-entry ?\* "." lua-mode-syntax-table)
  (modify-syntax-entry ?\/ "." lua-mode-syntax-table)
  (modify-syntax-entry ?\= "." lua-mode-syntax-table)
  (modify-syntax-entry ?\> "." lua-mode-syntax-table)
  (modify-syntax-entry ?\< "." lua-mode-syntax-table)
  (modify-syntax-entry ?\~ "." lua-mode-syntax-table)
  (modify-syntax-entry ?\; "." lua-mode-syntax-table))

(defun lua-mode-variables ()
  (set-syntax-table lua-mode-syntax-table)
  (make-local-variable 'require-final-newline)
  (setq	require-final-newline t)
  (make-variable-buffer-local 'comment-start)
  (setq comment-start "-- ")
  (make-variable-buffer-local 'comment-start-skip)
  (setq comment-start-skip "-- *")
  (make-variable-buffer-local 'commend-end)
  (setq comment-end ""))

(defvar lua-font-lock-keywords
  (let ((kw (mapconcat 'identity
		       '("and" "break" "do" "else" "elseif"
			 "end" "for" "function" "if" "in"
			 "local" "nil" "not" "or" "repeat"
			 "return" "then" "until" "while")
		       "\\|")))
    (list
     ;; keywords
     (cons (concat "\\b\\(" kw "\\)\\b[ \n\t(]") 1)	
     ;; double-bracketed strings
     '("\\[\\[\\(.*\\)\\]\\]" 1 font-lock-string-face)
     ))
  "Additional expressions to highlight in lua mode")

(put 'lua-mode 'font-lock-defaults '(lua-font-lock-keywords))

;;;###autoload
(defun lua-mode ()
  "Major mode for editing lua code

COMMANDS
\\{lua-mode-map}
VARIABLES
Use M-x run-lua to start inferior lua. Then you can use 
`\\[lua-send-line]' lua-send-line, `\\[lua-send-region]' lua-send-region,
`\\[lua-send-definition]' lua-send-definition and 
`\\[lua-send-buffer]' lua-send-buffer to interact with inferior lua process"

  (interactive)
  (kill-all-local-variables)

  (setq major-mode 'lua-mode
	mode-name "Lua"
	font-lock-defaults '(lua-font-lock-keywords))
	

  (use-local-map lua-mode-map)
  (lua-mode-variables)

  (run-hooks 'lua-mode-hook))


(defun lua-font-lock-bracket-string ()
  (if (re-search-forward "^=begin\\(\\s \\|$\\)" limit t)
	(let (beg)
	  (beginning-of-line)
	  (setq beg (point))
	  (forward-line 1)
	  (if (re-search-forward "^=end\\(\\s \\|$\\)" limit t)
	      (progn
		(set-match-data (list beg (point)))
		t)))))

(provide 'lua-mode)

;;;
;;; $Log: lua-mode.el,v $
;;; Revision 1.5  2001/04/10 11:22:49  slim
;;; some info on usage added
;;;
;;; Revision 1.4  2001/04/10 10:44:27  slim
;;; should work in FSF Emacs now too
;;;
;;; Revision 1.3  2001/04/10 10:18:29  slim
;;; lua-send-definition, lua-send-buffer, lua-send-line added
;;;
;;; Revision 1.2  2001/04/10 09:17:06  slim
;;; inferior lua interpreter support added
;;;
;;; Revision 1.1  2001/03/30 13:08:06  slim
;;; initial revision - simple font locking of keywords
;;;
