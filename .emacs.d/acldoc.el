;;; ACLDOC: An emacs interface the the Franz Allegro Common Lisp
;;; documentation tree.

;Copyright:
;Copyright © 2000, Lawrence Hunter (Larry.Hunter@uchsc.edu)
;Copyright © 2003, University of Colorado. 
;Copyright © 2004, University of Colorado. 

;This library is free software; you can redistribute it and/or
;modify it under the terms of the GNU Lesser General Public
;License as published by the Free Software Foundation; either
;version 2.1 of the License, or (at your option) any later version.
;This library is distributed in the hope that it will be useful,
;but WITHOUT ANY WARRANTY; without even the implied warranty of
;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;Lesser General Public License for more details.

;You should have received a copy of the GNU Lesser General Public
;License along with this library; if not, write to the Free Software
;Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

;; Changelog:

;;  11/1/04 Changed messages displayed while building index, and made
;;   default ACL version 7.0.  Added "content.htm" to the pages that
;;   should be indexed (makes it possible to find Allegro specific
;;   stuff not in the index, e.g. allegroserve.  Modified index-regexp
;;   to not create stack overflows when parsing that page.

;;  5/26/03 If url.el is available, goes to Franz web site by default.
;;   Takes longer to start up (has to download the indices from the
;;   web), but is always current.  Also, now shuts off autofontify (if
;;   it was on) for the index pages, which speeds parsing.  Modified to
;;   use built-in symbol-at-point and defines a customization group.

;; Requires FSF emacs 21 or XEmacs 21 or later; Franz ACL 6.0 or greater

;; To use this program, place this file in your load path, optionally
;; byte-compile it, and bind the function ACLDOC to a convenient key.
;; You may need to customize the group `acldoc' .  

;; Pick your favorite key binding.  Here's mine (commented out).

; (define-key lisp-mode-map [F12] 'acldoc)

;; Customization stuff

(defgroup acldoc nil "A Franz Allegro Common Lisp Documentation Browser")

(defcustom acldoc-allegro-version "7.0"
  "The version of ACL that the documentation browser should assume.  
Not used if ACLDOC-USE-LOCAL is true."
  :group 'acldoc
  :type 'string)

(defcustom acldoc-franz-url "http://www.franz.com/support/documentation"
  "The URL for the top of the Franz documentation tree; do not include version numbers"
  :group 'acldoc
  :type 'string)

;; If url is provided, use franz web site for the documentation.
;; Otherwise, or if ACLDOC-USE-LOCAL is true, we use ACLDOC-LOCAL-ROOT
;; as the acl install directory instead.

;; Have to do a separate defvar, since defcustom doesn't evaluate it's
;; arguments.  This defaults to use-local if the url package isn't
;; available.

(defvar acldoc-use-local (not (ignore-errors (require 'url))))

(defcustom acldoc-use-local nil
  "If true, use the local documentation. 
If you don't have the URL.el package (part of W3), then you must use local documentation
(You may need to set ACLDOC-LOCAL-ROOT, too)"
  :group 'acldoc
  :type 'boolean)

;; ACLDOC-LOCAL-ROOT is the root directory of the ACL documentation hierarchy.

(defcustom acldoc-local-root "/usr/local/src/acl62"
  "The root directory of the local ACL documentation hierarchy"
  :group 'acldoc
  :type 'directory)

;; Default is not to index common graphics entries.  If you want them indexed, set this.

(defcustom acldoc-index-common-graphics nil
  "When true, index the ACL Common Graphics entries (Windows implementation only)."
  :group 'acldoc
  :type 'boolean)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; No user servicable parts inside.  Should only need changing when Allegro
;; changes the way they provide documentation.  

;; ACLDOC Global variables

(defvar acldoc-index-alist nil
  "An alist associating index entries with html files that document them.")

(defvar acldoc-index-regexp "<a href=\"\\([^#][^\"]*\\)\"[^>]*>\\([^/]*\\)</a>"
  "A regular expression used to parse non-local references in the ACL
documentation index file.  Has two subexpressions, the first corresponding
to the html file, and the second to the label used for the hyperlink.")

(defvar acldoc-last-page nil
  "The last page viewed by ACLDOC.  Used to cycle through multiple entries.")

;; ACLDOC is the user interface.  It will pop up a browser window on the ACL
;; documentation for the argument (which can be read with completion from
;; the minibuffer, with default being the symbol near point).

(defun acldoc (entry)
  "View the ACL documenation on ENTRY.
If there is more than one page of documentation on ENTRY, successive calls
to ACLDOC will cycle through them."
  (interactive
   (let ((default (symbol-at-point)))
     (unless acldoc-index-alist (acldoc-build-index))
     (list (completing-read "ACL Documentation on: "
                            acldoc-index-alist nil t default default))))
  (let* ((pages (cdr (assoc entry acldoc-index-alist)))
         (last-same (member acldoc-last-page pages))
         (page (if (and last-same (cdr last-same)) ; if we looked at the
                   (cadr last-same)                ; last, just use the first.
                   (car pages))))
    (unless page (setq page (concat acldoc-franz-index-top "/contents.htm")))
    (setq acldoc-last-page page)
    (browse-url (acldoc-make-url page) '(t))))

;; Create the right url to browse a page, depending on settings

(defun acldoc-make-url (page)
  (if acldoc-use-local
      (concat "file://" acldoc-local-root "/" page)
    (concat acldoc-franz-url "/" acldoc-allegro-version "/" page)))

;; As of ACL6, the index is split into pieces.  Don't fontify these
;; files (default is on in XEmacs), but ensure that fontification is
;; returned to previous status.

(defun acldoc-build-index ()
  (message "ACLDOC-SETUP: Retrieving and parsing ACL documentation indices.")
  (setq acldoc-index-alist nil)
  (let ((autofontify? (if (boundp 'font-lock-auto-fontify) font-lock-auto-fontify))
	(ansi-pages (copy-sequence acldoc-ansicl-index-pages))
	(franz-pages (copy-sequence (if acldoc-index-common-graphics
                                        acldoc-cg-index-pages
				      acldoc-nocg-index-pages)))
        page)
    (unwind-protect 
	(progn
	  (if autofontify? (setq font-lock-auto-fontify nil))
	  (while (setq page (pop ansi-pages))
	    (acldoc-parse-index-page page acldoc-ansicl-index-top))
	  (acldoc-cleanup-ansi-entries)
	  (while (setq page (pop franz-pages))
	    (acldoc-parse-index-page page acldoc-franz-index-top)))
      (if autofontify? (setq font-lock-auto-fontify autofontify?))
      (message "ACLDOC indices built"))))


  
;; ACLDOC-PARSE-INDEX-PAGE extracts all of the (non-local) hyperlinks from
;; an index page, and builds an alist from them.

(defun acldoc-parse-index-page (filename subdir)
  "Parse an acldoc index page to create acldoc index alist."
  (let ((index-buffer (acldoc-get-index-buffer filename subdir)))
    (if index-buffer
        (with-current-buffer index-buffer
          (save-match-data
            (goto-char (point-min))
            (while (re-search-forward acldoc-index-regexp nil t)
              (let* ((key (acldoc-remove-markups (match-string 2)))
                     (url (concat subdir "/" (match-string 1)))
                     (previous (assoc key acldoc-index-alist)))
                (unless (or (acldoc-index-entry-p key url)
                            (string= "" key))
                  (if previous
                      (unless (member url (cdr previous))
                        (setcdr previous (cons url (cdr previous))))
                      (setq acldoc-index-alist
                            (cons (list key url) acldoc-index-alist)))))))
          (message "Parsed index page: %s" filename))
        (error "Can't find ACL documentation indices. Customize group `acldoc' to fix this."))))

(defun acldoc-get-index-buffer (filename subdir)
  (if acldoc-use-local
      (find-file-noselect (concat acldoc-local-root "/" subdir "/" filename) t)
    (let ((buffer (get-buffer-create (format " *acl-download-%s-%s*" subdir filename))))
      (setq url-working-buffer buffer)
      (url-retrieve (concat acldoc-franz-url "/" acldoc-allegro-version "/" subdir "/" filename) t)
      (setq url-working-buffer nil)
      buffer)))
      

;; Some utility functions:

;; ACLDOC-REMOVE-MARKUPS removes any html markups and .htm or .html suffixes
;; from the key strings.  The index file does things like change the fonts,
;; etc.  The suffix removal is because the relatively few files referenced
;; in the index have reasonably explanatory names (e.g. introduction.htm).
;; It would also be reasonable not to include links that used filenames as
;; keys. 

(defun acldoc-remove-markups (string)
  "Remove any html markups and/or .htm or .html suffixes from string"
  (save-match-data
    (while (string-match "<[^>]*>" string)
      (setq string (concat (substring string 0 (match-beginning 0))
                           (substring string (match-end 0)))))
    (when (string-match ".html*" string)
      (setq string (subseq string 0 (match-beginning 0))))
    (while (string-match "&gt;" string)
      (setq string (concat (substring string 0 (match-beginning 0))
                           ">" (substring string (match-end 0)))))
    (while (string-match "&lt;" string)
      (setq string (concat (substring string 0 (match-beginning 0))
                           "<" (substring string (match-end 0)))))
    string))


;; ACLDOC-CLEANUP-ANSI-ENTRIES turns coma-separated keys into multiple
;; entries and makes URLs relative to the ACLROOT.  It assumes all elements
;; of the acldoc-index-alist are ansi entries.

(defun acldoc-cleanup-ansi-entries ()
  (save-match-data
    (let ((old acldoc-index-alist)
          (new nil)
          entry)
      (while (setq entry (pop old))
        (let ((key (first entry))
              (url (second entry)))
          (while (string-match ", +" key)
            (push (list (substring key 0 (match-beginning 0)) url) new)
            (setq key (substring key (match-end 0))))
          (push (list key url) new)))
      (setq acldoc-index-alist (nreverse new)))))



;; ACLDOC-INDEX-ENTRY? tests to see if a key and URL are really just
;; pointers to other parts of the index.  The test is that the word index
;; appears in the url, but not in the key.

(defun acldoc-index-entry-p (key url)
  (and (string-match "index" url)
       (not (string-match "index" key))))

;; ACLDOC index-pages

(defvar acldoc-franz-index-top "doc")

(defvar acldoc-nocg-index-pages
  '("nocg-index-a.htm" "nocg-index-b.htm" "nocg-index-c.htm"
    "nocg-index-d.htm" "nocg-index-e.htm" "nocg-index-g.htm"
    "nocg-index-h.htm" "nocg-index-i.htm" "nocg-index-j.htm"
    "nocg-index-l.htm" "nocg-index-m.htm" "nocg-index-n.htm"
    "nocg-index-o.htm" "nocg-index-q.htm" "nocg-index-r.htm"
    "nocg-index-s.htm" "nocg-index-t.htm" "nocg-index-v.htm"
    "nocg-index-w.htm" "nocg-index-xyz.htm" "nocg-index-other.htm"))

(defvar acldoc-cg-index-pages
  '("index-a.htm" "index-b.htm" "index-c.htm" "index-d.htm"
    "index-e.htm" "index-f.htm" "index-g.htm" "index-h.htm"
    "index-i.htm" "index-j.htm" "index-k.htm" "index-l.htm"
    "index-m.htm" "index-n.htm" "index-o.htm" "index-p.htm"
    "index-q.htm" "index-r.htm" "index-s.htm" "index-t.htm"
    "index-u.htm" "index-v.htm" "index-w.htm" "index-xyz.htm"
    "index-other.htm"))

(defvar acldoc-ansicl-index-top "ansicl/section")

(defvar acldoc-ansicl-index-pages
  '("dictio10.htm" "dictio11.htm" "dictio12.htm" "dictio13.htm"
    "dictio14.htm" "dictio15.htm" "dictio16.htm" "dictio17.htm"
    "dictio18.htm" "dictio19.htm" "dictio20.htm" "dictio21.htm"
    "diction0.htm" "diction1.htm" "diction2.htm" "diction3.htm"
    "diction4.htm" "diction5.htm" "diction6.htm" "diction7.htm"
    "diction8.htm" "diction9.htm" "dictiona.htm"))


(provide 'acldoc)
