;;; po-mode+.el --- Extensions to GNU gettext's `po-mode.el'.
;;
;; Filename: po-mode+.el
;; Description: Extensions to GNU gettext's `po-mode.el'.
;; Author: Gaute Hvoslef Kvalnes <gaute@verdsveven.com>
;; Copyright (C) 2006, Gaute Hvoslef Kvalnes.
;; Created: Thu Jun 22 13:42:15 CEST 2006
;; Version: 0.2
;; Last-Updated: Sat Jul  1 18:20:05 2006 (7200 CEST)
;;           By: Gaute Hvoslef Kvalnes
;;     Update #: 57
;; URL: http://www.emacswiki.org/cgi-bin/wiki/po-mode+.el
;; Keywords: i18n, gettext
;; Compatibility: GNU Emacs 22.x
;;
;; Features that might be required by this library:
;;
;;   `cl', `po-mode'
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This package is an extension to `po-mode.el', GNU Gettext's major
;; mode for editing PO files.
;;
;; Although this is written, tested and used in Emacs 22.x, it might
;; work in other versions too.
;;
;; You may adjust some variables, below, by defining them in your
;; `.emacs' file, either directly or through command 'M-x customize'.
;;
;; Installation:
;;
;;  Put this file where Emacs can find it, and place the following in
;;  your `~/.emacs':
;;
;;  (autoload 'po-mode "po-mode+"
;;   "Major mode for translators to edit PO files" t)
;;
;;  (Don't load both `po-mode' and `po-mode+', just `po-mode+'.)
;;
;; Extensions to po-mode:
;;
;;  `po-update-header' is a generalization of the function that
;;  updates 'PO-Revision-Date'. It now updates 'Last-Translator',
;;  'Language-Team' and 'X-Generator' too. `po-translator',
;;  `po-language-team' and `po-x-generator' are customizable
;;  variables.
;;
;;  `po-msgid-to-msgstr' is changed so that it ignores KDE-style
;;  context comments: The first line begins with '_:' and ends with
;;  '\n'. These should never appear in the translation.
;;
;;  `po-subedit-insert-next-tag' and `po-subedit-insert-next-arg' are
;;  useful functions taken from KBabel. Since (XML) tags and arguments
;;  normally should be kept verbatim in the translation, it's nice to
;;  have an easy way to insert them. C-c C-t inserts the next tag,
;;  while C-c C-a inserts the next argument.
;;
;;  `po-auto-select-mode' provides two alternative workflows.
;;  po-mode's original behaviour is invoked by setting this variable
;;  to 'by-type, so that `po-auto-select' (SPC) first cycles through
;;  the untranslated entries before starting on the fuzzy ones. Since
;;  I find it more convenient to translate both untranslated and fuzzy
;;  entries in the same run, I prefer 'by-order instead.
;;
;;  `po-select-entry-number' is bound to 'g'. It asks for an entry
;;  number and selects it.
;;
;; Bugfixes to po-mode:
;;
;;  The fuzzy counter is fixed. Entries that are fuzzy at the same
;;  time as being untranslated or obsolete were incorrectly counted as
;;  both fuzzy and untranslated or obsolete. These should not be
;;  counted as fuzzy. This change makes po-mode agree with
;;  msgfmt. (I've never seen fuzzy+untranslated entries, but a FIXME
;;  comment in the source code warned against it. Fuzzy+obsolete
;;  entries happens all the time, at least in the projects I work on.)
;;
;; Bugs introduced:
;;
;;  The customizable variable `po-auto-replace-revision-date' is now
;;  meaningless, being replaced by the more general
;;  `po-auto-update-header'. I don't know how to remove it without
;;  editing `po-mode.el'. Perhaps it should be kept, made into a
;;  non-customizable variable? The way I do it now overrides the old
;;  preference.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'po-mode)

;; Some Common Lisp macros are used:
;;   loop
(eval-when-compile (require 'cl))

(defconst po-mode-+-version-string "0.2" "\
Version number of this version of po-mode+.el.")

;; REPLACES ORIGINAL in `po-mode.el'
;; Use `po-x-generator'.
(defun po-mode-version ()
  "Show and return Emacs PO mode version."
  (interactive)
  (let ((msg (concat "Emacs " emacs-version 
		     ", po-mode " po-mode-version-string
		     "+" po-mode-+-version-string)))
    (message msg)
    msg))

;; Replaces `po-replace-revision-date' with `po-update-header' on
;; `write-contents-hook'.
;; Adds keybindings.
(defun po-mode+ ()
  "To be run on `po-mode-hook'."
  (remove-hook 'write-contents-hooks 'po-replace-revision-date)
  (add-hook 'write-contents-hooks 'po-update-header)
  (define-key po-mode-map "g" 'po-select-entry-number)
  (define-key po-subedit-mode-map "\C-c\C-a" 'po-subedit-insert-next-arg)
  (define-key po-subedit-mode-map "\C-c\C-t" 'po-subedit-insert-next-tag))

(add-hook 'po-mode-hook 'po-mode+)

;;; Customisation.

(defcustom po-auto-select-mode 'by-type
  "*Workflow preference.
Set this to 'by-type to translate all the untranslated entries
before starting on the fuzzy ones. Select 'by-order to translate
untranslated and fuzzy entries in the same run."
  :type '(choice 
	  (const by-type)
	  (const by-order))
  :group 'po)

(defcustom po-auto-update-header t
  "*Automatically update the header.  Value is nil, t, or ask."
  :type '(choice (const nil)
		 (const t)
		 (const ask))
  :group 'po)

(defcustom po-x-generator (po-mode-version)
  "*X-Generator header to identify the editor."
  :type 'string
  :group 'po)

(defconst po-translator-default "FULL NAME <EMAIL@ADDRESS>"
  "Default, empty value for Last-Translator.")

(defcustom po-translator po-translator-default
  "*The translator's name and email address."
  :type 'string
  :group 'po)

(defconst po-language-team-default "LANGUAGE <LL@li.org>"
  "Default, empty value for Language-Team.")

(defcustom po-language-team po-language-team-default
  "*Language name and address of mailing list."
  :type 'string
  :group 'po)

;; REPLACES ORIGINAL in `po-mode.el'
;; Added "g".
(defconst po-help-display-string
  (_"\
PO Mode Summary           Next Previous            Miscellaneous
*: Later, /: Docum        n    p    Any type       .     Redisplay
                          t    T    Translated     /v    Version info
Moving around             f    F    Fuzzy          ?, h  This help
<    First if any         o    O    Obsolete       =     Current index
>    Last if any          u    U    Untranslated   0     Other window
/SPC Auto select                                   V     Validate
g    Entry number                                  M     Mail officially
                        Msgstr Comments            _     Undo
Modifying entries         RET  #    Call editor    E     Edit out full
TAB   Remove fuzzy mark   k    K    Kill to        Q     Forceful quit
DEL   Fuzzy or fade out   w    W    Copy to        q     Confirm and quit
LFD   Init with msgid     y    Y    Yank from      

gettext Keyword Marking                            Position Stack
,    Find next string     Compendiums              m  Mark and push current
M-,  Mark translatable    *c    To compendium      r  Pop and return
M-.  Change mark, mark    *M-C  Select, save       x  Exchange current/top

Program Sources           Auxiliary Files          Lexicography
s    Cycle reference      a    Cycle file          *l    Lookup translation
M-s  Select reference     C-c C-a  Select file     *M-l  Add/edit translation
S    Consider path        A    Consider PO file    *L    Consider lexicon
M-S  Ignore path          M-A  Ignore PO file      *M-L  Ignore lexicon
")
  "Help page for PO mode.")

;; REPLACES ORIGINAL in `po-mode.el'
;; Added `po-subedit-insert-next-arg' and `po-subedit-insert-next-tag'.
(defconst po-subedit-mode-menu-layout
  `("PO-Edit"
    ["Ediff and merge translation variants" po-subedit-ediff
      ,@(if (featurep 'xemacs) '(t)
	  '(:help "Call `ediff' for merging variants"))]
    ["Cycle through auxiliary files" po-subedit-cycle-auxiliary t]
    ["Insert next argument" po-subedit-insert-next-arg t]
    ["Insert next tag" po-subedit-insert-next-tag t]
    "---"
    ["Abort edit" po-subedit-abort
     ,@(if (featurep 'xemacs) '(t)
	  '(:help "Don't change the translation"))]
    ["Exit edit" po-subedit-exit
     ,@(if (featurep 'xemacs) '(t)
	 '(:help "Use this text as the translation and close current edit buffer"))])
  "Menu layout for PO subedit mode.")


;; REPLACES ORIGINAL in `po-mode.el'
;; Fixed the fuzzy counter.
(defun po-compute-counters (flag)
  "Prepare counters for mode line display.  If FLAG, also echo entry position."
  (and flag (po-find-span-of-entry))
  (setq po-translated-counter 0
	po-fuzzy-counter 0
	po-untranslated-counter 0
	po-obsolete-counter 0)
  (let ((position 0) (total 0) current here)
    ;; FIXME 'here' looks obsolete / 2001-08-23 03:54:26 CEST -ke-
    (save-excursion
      (po-find-span-of-entry)
      (setq current po-start-of-msgstr)
      (goto-char (point-min))
      ;; While counting, skip the header entry, for consistency with msgfmt.
      (po-find-span-of-entry)
      (if (string-equal (po-get-msgid nil) "")
	  (goto-char po-end-of-entry))
      (if (re-search-forward "^msgid" (point-max) t)
	  (progn
	    ;; Start counting
	    (while (re-search-forward po-any-msgstr-regexp nil t)
	      (and (= (% total 20) 0)
		   (if flag
		       (message (_"Position %d/%d") position total)
		     (message (_"Position %d") total)))
	      (setq here (point))
	      (goto-char (match-beginning 0))
	      (setq total (1+ total))
	      (and flag (eq (point) current) (setq position total))
	      (cond ((eq (following-char) ?#)
		     (setq po-obsolete-counter (1+ po-obsolete-counter)))
		    ((looking-at po-untranslated-regexp)
		     (setq po-untranslated-counter (1+ po-untranslated-counter)))
		    (t (setq po-translated-counter (1+ po-translated-counter))))
	      (goto-char here))

	    ;; Make another pass just for the fuzzy entries, kind of kludgey.
	    (goto-char (point-min))
	    (while (re-search-forward po-fuzzy-regexp nil t)
	      (po-find-span-of-entry)
	      (unless (or (eq po-entry-type 'translated)
			  (eq po-entry-type 'obsolete))
		(setq po-fuzzy-counter (1+ po-fuzzy-counter))))
	    (setq po-translated-counter (- po-translated-counter po-fuzzy-counter)))
	'()))

    ;; Push the results out.
    (if flag
	(message (_"\
Position %d/%d; %d translated, %d fuzzy, %d untranslated, %d obsolete")
		 position total po-translated-counter po-fuzzy-counter
		 po-untranslated-counter po-obsolete-counter)
      (message "")))
  (po-update-mode-line-string))


;;; Processing the PO file header entry.
;; This replaces `po-replace-revision-date'.

(defmacro po-replace-header-field (condition field replacement)
  "Replace a header field."
  `(if ,condition
       (save-excursion
	 (goto-char (point-min))
	 (if (re-search-forward (concat "^\"" ,field ":.*") nil t)
	     (let ((buffer-read-only po-read-only))
	       (replace-match
		(concat "\"" ,field ": " ,replacement "\\n\"")
		t t))))))

(defun po-update-header ()
  "Update fields in the PO file header."
  (if (or (eq po-auto-update-header t)
	  (and (eq po-auto-update-header 'ask)
	       (y-or-n-p (_"May I update the header? "))))
      (save-excursion
	(let* ((time (current-time))
	       (seconds (or (car (current-time-zone time)) 0))
	       (minutes (/ (abs seconds) 60))
	       (zone (format "%c%02d%02d"
			     (if (< seconds 0) ?- ?+)
			     (/ minutes 60)
			     (% minutes 60))))
	  (po-replace-header-field 
	   (fboundp 'format-time-string)
	   "PO-Revision-Date"
	   (concat (format-time-string "%Y-%m-%d %H:%M" time) zone))
	  (po-replace-header-field
	   (not (equal po-translator po-translator-default))
	   "Last-Translator" po-translator)
	  (po-replace-header-field
	   (not (equal po-language-team po-language-team-default))
	   "Language-Team" po-language-team)
	  (po-replace-header-field t "X-Generator" po-x-generator)))))

(defun po-remove-context-comment (msg)
  "Removes any KDE-style context comment from MSG."
  (if (string-match "^_:.*\n" msg)
      (replace-match "" nil nil msg)
      msg))

;; REPLACES ORIGINAL in `po-mode.el'
;; Modified to remove KDE-style context comments before copying.
;; Fixed a typo, too :-)
(defun po-msgid-to-msgstr ()
  "Initialize msgstr with msgid."
  (interactive)
  (po-find-span-of-entry)
  (if (or (eq po-entry-type 'untranslated)
	  (eq po-entry-type 'obsolete)
	  (y-or-n-p (_"Really lose previous translation? ")))
      (po-set-msgstr (po-remove-context-comment (po-get-msgid nil))))
  (message ""))


;;; Moving around.

;; REPLACES ORIGINAL in `po-mode.el'
(defun po-auto-select-entry ()
  "Select the next entry according to the workflow preference
`po-auto-select-mode'."
  (interactive)
  (case po-auto-select-mode
    ('by-type (po-auto-select-entry-by-type))
    ('by-order (po-auto-select-entry-by-order))))

(defun po-auto-select-entry-by-type ()
  "Select the next entry having the same type as the current one.
If none, wrap from the beginning of the buffer with another type,
going from untranslated to fuzzy, and from fuzzy to obsolete.
Plain translated entries are always disregarded unless there are
no entries of the other types."
  (po-find-span-of-entry)
  (goto-char po-end-of-entry)
  (if (and (= po-untranslated-counter 0)
	   (= po-fuzzy-counter 0)
	   (= po-obsolete-counter 0))
      ;; All entries are plain translated.  Next entry will do, or
      ;; wrap around if there is none.
      (if (re-search-forward po-any-msgstr-regexp nil t)
	  (goto-char (match-beginning 0))
	  (goto-char (point-min)))
      ;; If over a translated entry, look for an untranslated one first.
      ;; Else, look for an entry of the same type first.
      (let ((goal (if (eq po-entry-type 'translated)
		      'untranslated
		      po-entry-type)))
	(while goal
	  ;; Find an untranslated entry, or wrap up for a fuzzy entry.
	  (if (eq goal 'untranslated)
	      (if (and (> po-untranslated-counter 0)
		       (re-search-forward po-untranslated-regexp nil t))
		  (progn
		    (goto-char (match-beginning 0))
		    (setq goal nil))
		  (goto-char (point-min))
		  (setq goal 'fuzzy)))
	  ;; Find a fuzzy entry, or wrap up for an obsolete entry.
	  (if (eq goal 'fuzzy)
	      (if (and (> po-fuzzy-counter 0)
		       (re-search-forward po-fuzzy-regexp nil t))
		  (progn
		    (goto-char (match-beginning 0))
		    (setq goal nil))
		  (goto-char (point-min))
		  (setq goal 'obsolete)))
	  ;; Find an obsolete entry, or wrap up for an untranslated entry.
	  (if (eq goal 'obsolete)
	      (if (and (> po-obsolete-counter 0)
		       (re-search-forward po-obsolete-msgstr-regexp nil t))
		  (progn
		    (goto-char (match-beginning 0))
		    (setq goal nil))
		  (goto-char (point-min))
		  (setq goal 'untranslated))))))
  ;; Display this entry nicely.
  (po-current-entry))

(defun po-auto-select-entry-by-order ()
  "Select the next entry that should be translated, either an
fuzzy or untranslated entry. Select obsolete entries if there are
no more fuzzy or untranslated ones, or if an obsolete entry is
already selected. Plain translated entries are always disregarded
unless there are no entries of the other types."
  (po-find-span-of-entry)
  (goto-char po-end-of-entry)
  (if (and (= po-untranslated-counter 0)
	   (= po-fuzzy-counter 0)
	   (= po-obsolete-counter 0))
      ;; All entries are plain translated.  Next entry will do, or
      ;; wrap around if there is none.
      (if (re-search-forward po-any-msgstr-regexp nil t)
	  (goto-char (match-beginning 0))
	(goto-char (point-min)))
    ;; If over an obsolete entry, continue looking for obsolete ones.
    ;; Else, look for an untranslated or fuzzy entry.
      (let ((goal (if (eq po-entry-type 'obsolete)
		      'obsolete
		      'untranslated-or-fuzzy)))
	(while goal
	  ;; Find an untranslated or fuzzy entry, or wrap up for an
	  ;; obsolete entry.
	  (if (eq goal 'untranslated-or-fuzzy)
	      (if (and (or (> po-fuzzy-counter 0)
			   (> po-untranslated-counter 0))
		       (re-search-forward 
			(format "\\(%s\\|%s\\)" 
				po-fuzzy-regexp
				po-untranslated-regexp)
			nil t))
		  (progn
		    (goto-char (match-beginning 0))
		    (setq goal nil))
		  (goto-char (point-min))
		  (setq goal 'obsolete)))
	  ;; Find an obsolete entry, or wrap up for an untranslated or
	  ;; fuzzy entry.
	  (if (eq goal 'obsolete)
	      (if (and (> po-obsolete-counter 0)
		       (re-search-forward po-obsolete-msgstr-regexp nil t))
		  (progn
		    (goto-char (match-beginning 0))
		    (setq goal nil))
		  (goto-char (point-min))
		  (setq goal 'untranslated-or-fuzzy))))))
  ;; Display this entry nicely.
  (po-current-entry))

(defun po-select-entry-number (num)
  "Go to entry number NUM."
  (interactive "nEntry number: ")
  (po-first-entry)
  (loop for i from 1 to num
       do (po-next-entry)))


;;; Editing management and submode.

;; REPLACES ORIGINAL in `po-mode.el'
;; Added calls to `po-find-args' and `po-find-tags'.
(defun po-edit-string (string type expand-tabs)
  "Prepare a pop up buffer for editing STRING, which is of a given TYPE.
TYPE may be 'comment or 'msgstr.  If EXPAND-TABS, expand tabs to spaces.
Run functions on po-subedit-mode-hook."
  (run-hooks 'po-before-subedit-mode-hook)
  (let ((marker (make-marker)))
    (set-marker marker (cond ((eq type 'comment) po-start-of-msgid)
			     ((eq type 'msgstr) po-start-of-msgstr)))
    (if (po-check-for-pending-edit marker)
	(let ((edit-buffer (generate-new-buffer
			    (concat "*" (buffer-name) "*")))
	      (edit-coding buffer-file-coding-system)
	      (buffer (current-buffer))
	      overlay slot)
	  (if (and (eq type 'msgstr) po-highlighting)
	      ;; ;; Try showing all of msgid in the upper window while editing.
	      ;; (goto-char (1- po-start-of-msgstr))
	      ;; (recenter -1)
	      (save-excursion
		(goto-char po-start-of-entry)
		(re-search-forward po-any-msgid-regexp nil t)
		(let ((end (1- (match-end 0))))
		  (goto-char (match-beginning 0))
		  (re-search-forward "msgid +" nil t)
		  (setq overlay (po-create-overlay))
		  (po-highlight overlay (point) end buffer))))
	  (po-find-args (po-get-msgid nil))
	  (po-find-xml-tags (po-get-msgid nil))
	  (setq slot (list marker edit-buffer overlay)
		po-edited-fields (cons slot po-edited-fields))
	  (pop-to-buffer edit-buffer)
	  (set (make-local-variable 'po-subedit-back-pointer) slot)
	  (set (make-local-variable 'indent-line-function)
	       'indent-relative)
	  (setq buffer-file-coding-system edit-coding)
	  (setq local-abbrev-table po-mode-abbrev-table)
	  (erase-buffer)
	  (insert string "<")
	  (goto-char (point-min))
	  (and expand-tabs (setq indent-tabs-mode nil))
	  (use-local-map po-subedit-mode-map)
	  (if (fboundp 'easy-menu-define)
	      (progn
		(easy-menu-define po-subedit-mode-menu po-subedit-mode-map ""
		  po-subedit-mode-menu-layout)
		(and po-XEMACS (easy-menu-add po-subedit-mode-menu))))
	  (set-syntax-table po-subedit-mode-syntax-table)
	  (run-hooks 'po-subedit-mode-hook)
	  (message po-subedit-message)))))

(defvar po-xml-tags-in-msgid '()
  "List of XML tags in a msgid, found by `po-find-xml-tags'.")

(defvar po-xml-tag-regexp "\\(<[^>]+>\\|&[a-z]+;\\)"
  "Matches XML tags and entities.")

(defun po-find-xml-tags (msgid)
  "Find any XML tags in msgid and put them in `po-xml-tags-in-msg'."
  (setq po-xml-tags-in-msgid (po-find-matches msgid po-xml-tag-regexp)))

(defvar po-args-in-msgid '()
  "List of arguments in a msgid, found by `po-find-args'.")

(defvar po-args-regexp "\\(%[-+# ]?[0-9*]+?\\(\\.[0-9*]\\)?[hlL]?[cdieEfgGosuxXpn]\\|%L?[0-9]\\|\\$\\[[a-z]+\\]\\|%[A-Z_]+\\|\\$[a-z_]+\\$\\)"
  "Matches various argument types.
   %[-+ #]?[0-9*]?\\(\\.[0-9*]\\)?[hlL]?[cdieEfgGosuxXpn]
                              C-style printf arguments
   %L?[0-9]             %1      Qt
   \\$[[a-z]+]         $[arg]  OpenOffice.org
   %[A-Z_]            %ARG    OpenOffice.org
   \\$[a-z_]+]\\$       $arg$   OpenOffice.org")

(defun po-find-args (msgid)
  "Find any arguments in msgid and put them in `po-args-in-msg'."
  (setq po-args-in-msgid (po-find-matches msgid po-args-regexp)))

(defun po-find-matches (s regexp)
  "Return a list of all occurences of regexp found in s."
  (loop for pos = 0 then (match-end 0)
     while (string-match regexp s pos)
     collect (match-string 0 s)))

(defun po-subedit-insert-next-tag ()
  "Insert the next XML tag or entity that occurs in the msgid."
  (interactive)
  (if po-xml-tags-in-msgid
      (insert (pop po-xml-tags-in-msgid))
      (error (_"No more tags."))))

(defun po-subedit-insert-next-arg ()
  "Insert the next argument that occurs in the msgid."
  (interactive)
  (if po-args-in-msgid
      (insert (pop po-args-in-msgid))
      (error (_"No more arguments."))))

(provide 'po-mode+)

;;; po-mode+.el ends here
