;;; po-mode+.el --- Extensions to GNU gettext's `po-mode.el'.
;;
;; Filename: po-mode+.el
;; Description: Extensions to GNU gettext's `po-mode.el'.
;; Author: Gaute Hvoslef Kvalnes <gaute@verdsveven.com>
;; Copyright (C) 2006, Gaute Hvoslef Kvalnes.
;; Created: Thu Jun 22 13:42:15 CEST 2006
;; Version: 0.4
;; Last-Updated: Wed Sep 16 15:26:12 2009 (7200 CEST)
;;           By: Gaute Hvoslef Kvalnes
;;     Update #: 182
;; URL: http://github.com/gaute/po-mode-plus
;; Keywords: i18n, gettext
;; Compatibility: GNU Emacs 22.x
;;
;; Features that might be required by this library:
;;
;;   `cl', `longlines', `po-mode'
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This package is an extension to `po-mode.el', GNU Gettext's major
;; mode for editing PO files. It is made for po-mode version 2.01, and
;; is not likely to work with later versions (yet).
;;
;; Although written, tested and used in Emacs 22.x, po-mode+ might
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
;;  Make sure `po-mode.el' is available for Emacs, but don't load it
;;  explicitly.
;;
;; Extensions to po-mode:
;;
;;  po-mode+ 0.1, 0.2
;;
;;  `po-update-header' is a generalization of the function that
;;  updates 'PO-Revision-Date'. It now updates 'Last-Translator',
;;  'Language-Team' and 'X-Generator' too. `po-translator',
;;  `po-language-team' and `po-x-generator' are customizable
;;  variables. The function will insert fields that don't already
;;  exist.
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
;;  po-mode+ 0.3
;;
;;  `po-find-msg' (C-c C-m) searches for a message containing a
;;  string. The pattern in `po-ignore-in-search' defines characters to
;;  ignore.  This is useful for accelerators, which are typically
;;  marked by '&' or '~'.
;;
;;  `po-find-msgstr' (C-c C-s) searches for a message where the msgstr
;;  contains a string. The pattern in `po-ignore-in-search' defines
;;  characters to ignore.
;;
;;  `po-find-msgid' (C-c C-i) searches for a message where the msgid
;;  contains a string. The pattern in `po-ignore-in-search' defines
;;  characters to ignore.
;;
;;  `po-replace-in-msgstrs' (C-c C-r) is a function that replaces a
;;  string with another in all the msgstrs from the current point to
;;  the end of the file.
;;
;;  po-mode+ 0.4
;;
;;  If `longlines-mode' is available, it will be used for linewrapping
;;  in the subedit buffer.
;;
;;  Lookup
;;
;;    There were some unimplemented "lexicography" functions in
;;    po-mode, probably intended for dictionary lookups. (They've been
;;    left unimplemented for years, as far as I can tell.)  I adapted
;;    these to my purpose: A generic lookup feature for auxiliary
;;    files, translation memories, dictionaries, etc. The search
;;    result is displayed in `po-lookup-buffer', which is suggested to
;;    be left open in a separate frame. (`po-setup-lookup-frame' will
;;    set up such a frame.)
;;
;;    So far, only the auxiliary search is implemented. It resembles
;;    KBabel's auxiliary feature, but is more flexible when it comes
;;    to picking the right file. It partly duplicates po-mode's
;;    existing auxiliary feature, but I find it more convenient to
;;    work with.
;;
;;    (The lookup features are still very much a work in progress.)
;;
;;    `po-lookup' (l) looks up the current msgid using the selected
;;    lookup method.
;;
;;    `po-copy-from-lookup' (L) copies the selected lookup result to
;;    the current entry. `po-subedit-copy-from-lookup' (C-c C-l) does
;;    the same thing while editing a message.
;;
;;    `po-edit-lookup-entry' (M-l) opens a buffer where you can edit
;;    the lookup result, if that is possible.
;;
;;    `po-select-lookup-method' (M-L) will cycle between the lookup
;;    methods as soon as I implement more than just auxiliary search.
;;
;;  po-mode+ 0.5
;;
;;  The lookup now supports more than one match, with scores up to 100
;;  and an optional status (like "fuzzy"). Move between results with
;;  `po-select-next-lookup-result' (M-n) and
;;  `po-select-previous-lookup-result' (M-p).
;;
;;  Lookup methods may now be specified freely, not limited to
;;  auxiliary searches.
;;
;;  `po-filter-msgid' and `po-filter-msgstr' run the msgid or msgstr
;;  through an external command and puts the result in msgstr. This is
;;  useful for example when you have a command-line translation tool.
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
;;  `po-mode-version' is probably not as nice in XEmacs.
;;
;;  I have no idea how well the new features work in terminal mode.
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
;;   case loop pop
(eval-when-compile (require 'cl))

(defconst po-mode+-version-string "0.4" 
  "Version number of this version of po-mode+.el.")

;; REPLACES ORIGINAL in `po-mode.el'
;; Added emacs-version and po-mode+-version-string.
;; Now returns the string, to be used in the 'X-Generator' header.
(defun po-mode-version ()
  "Show and return Emacs PO mode version."
  (interactive)
  (let ((msg (concat "Emacs " emacs-version 
                     ", po-mode " po-mode-version-string
                     "+" po-mode+-version-string)))
    (message msg)
    msg))

(defun po-mode+ ()
  "Run on `po-mode-hook' to initialize the po-mode extensions:

- Replace `po-replace-revision-date' with `po-update-header' on
  `write-contents-hook'.
- Turn on wrapping with `longlines-mode' in the subedit buffer.
- Add keybindings.
"
  (remove-hook 'write-contents-hooks 'po-replace-revision-date)
  (add-hook 'write-contents-hooks 'po-update-header)
  (when (featurep 'longlines)
    ;; Turn on and off longlines-mode to wrap when editing a message
    ;; and unwrap before putting it back.
    (add-hook 'po-subedit-mode-hook '(lambda () (longlines-mode 1)))
    (add-hook 'po-subedit-exit-hook '(lambda () (longlines-mode 0))))
  (define-key po-mode-map "g" 'po-select-entry-number)
  (define-key po-mode-map "l" 'po-lookup)
  (define-key po-mode-map "L" 'po-copy-from-lookup)
  (define-key po-mode-map "\C-c\C-m" 'po-find-msg)
  (define-key po-mode-map "\C-c\C-s" 'po-find-msgstr)
  (define-key po-mode-map "\C-c\C-i" 'po-find-msgid)
  (define-key po-mode-map "\C-c\C-r" 'po-replace-in-msgstrs)
  (define-key po-mode-map "\M-l" 'po-edit-lookup-entry)
  (define-key po-mode-map "\M-L" 'po-select-lookup-method)
  (define-key po-mode-map "\M-n" 'po-select-next-lookup-result)
  (define-key po-mode-map "\M-p" 'po-select-previous-lookup-result)
  (define-key po-subedit-mode-map "\C-c\C-a" 'po-subedit-insert-next-arg)
  (define-key po-subedit-mode-map "\C-c\C-l" 'po-subedit-copy-from-lookup)
  (define-key po-subedit-mode-map "\C-c\C-t" 'po-subedit-insert-next-tag)
  (when po-always-setup-lookup-frame
    (po-setup-lookup-frame)))

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

(defcustom po-lookup-replace ""
  "*A regexp representing the part of the full filename to replace
when generating the name of the file to search in. Replaced by
`po-lookup-replace-with'."
  :type 'string
  :group 'po)

(defcustom po-lookup-replace-with ""
  "*The string that replaces `po-lookup-replace' when generating
the name of the file to search in."
  :type 'string
  :group 'po)

(defcustom po-always-setup-lookup-frame nil
  "*Set this to make the lookup frame open automatically when
PO-mode starts."
  :type 'boolean
  :group 'po)

(defcustom po-lookup-frame-width 59
  "*The width of the lookup frame, in characters."
  :type 'integer
  :group 'po)

(defcustom po-lookup-frame-height 16
  "*The height of the lookup frame, in lines."
  :type 'integer
  :group 'po)

(defcustom po-lookup-frame-font ""
  "*The font used for displaying the lookup frame.
Set this to the empty string to use the standard font. Otherwise, select a
font that `set-frame-font' will accept. (If the lookup frame is
small, it might be a good idea to select a compact sans-serif
font.)"
  :type 'string
  :group 'po)


;; REPLACES ORIGINAL in `po-mode.el'
;; Added "g" and "Search and replace" section.
;; Changed "Lexicography" into "Lookup".
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
                                                   Position Stack
gettext Keyword Marking                            m  Mark and push current
,    Find next string     Compendiums              r  Pop and return
M-,  Mark translatable    *c    To compendium      x  Exchange current/top
M-.  Change mark, mark    *M-C  Select, save       
                                                   Lookup
Program Sources           Auxiliary Files          l    Lookup translation
s    Cycle reference      a    Cycle file          M-l  Edit search result
M-s  Select reference     C-c C-a  Select file     L    Insert search result
S    Consider path        A    Consider PO file    *M-L  Cycle lookup
M-S  Ignore path          M-A  Ignore PO file      M-n  Select next result
                                                   M-p  Select prev. result
Search and replace
C-c C-m  Find message
C-c C-s  Find msgstr
C-c C-i  Find msgid
C-c C-r  Replace in msgstrs
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

;; List the variables we use from po-mode.el, to avoid byte-compiler
;; warnings.
(defvar po-translated-counter)
(defvar po-untranslated-counter)
(defvar po-fuzzy-counter)
(defvar po-obsolete-counter)
(defvar po-start-of-entry)
(defvar po-end-of-entry)
(defvar po-start-of-msgid)
(defvar po-start-of-msgstr)
(defvar po-entry-type)
(defvar po-read-only)
(defvar po-edited-fields)
(defvar po-subedit-back-pointer)


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

(defun po-replace-header-field (field new-value &optional add)
  "Replace the value of header field FIELD with NEW-VALUE. 
If ADD is t, add the field if it's missing."
  (goto-char (point-min))
  (po-find-span-of-entry)
  (when (string-equal (po-get-msgid nil) "")
    (if (re-search-forward (concat "^\"" field ":.*") po-end-of-entry t)
        (let ((buffer-read-only po-read-only))
          (replace-match
           (concat "\"" field ": " new-value "\\n\"")
           t t))
        (when add
          (let ((buffer-read-only po-read-only))
            (goto-char po-end-of-entry)
            (insert (concat "\"" field ": " new-value "\\n\"\n")))))))

(defun po-get-translator ()
  "Returns `po-translator', for use in the 'Last-Translator'
  header field. May be overridden to provide different values
  based on, for instance, `buffer-file-name'."
  po-translator)

(defun po-get-language-team ()
  "Returns `po-language-team', for use in the 'Language-Team'
  header field. May be overridden to provide different values
  based on, for instance, `buffer-file-name'."
  po-language-team)

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
          (when (fboundp 'format-time-string)
            (po-replace-header-field
             "PO-Revision-Date"
             (concat (format-time-string "%Y-%m-%d %H:%M" time) zone) t))
          (po-replace-header-field "Last-Translator" (po-get-translator) t)
          (po-replace-header-field "Language-Team" (po-get-language-team) t)
          (po-replace-header-field "X-Generator" po-x-generator t)))))

(defun po-remove-context-comment (msg)
  "Remove any KDE-style context comment from MSG."
  (if (string-match "^_n?:.*\n" msg)
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
`po-auto-select-mode':

'by-type: Select the next entry having the same type as the
current one. If none, wrap from the beginning of the buffer with
another type, going from untranslated to fuzzy, and from fuzzy to
obsolete.

'by-order: Select the next entry that should be translated,
either an fuzzy or untranslated entry. Select obsolete entries if
there are no more fuzzy or untranslated ones, or if an obsolete
entry is already selected.

Plain translated entries are always disregarded unless there are
no entries of the other types."
  (interactive)
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
      (let ((goal (case po-auto-select-mode
                    ;; If over a translated entry, look for an
                    ;; untranslated one first.  Else, look for an
                    ;; entry of the same type first.
                    ('by-type (if (eq po-entry-type 'translated)
                                  'untranslated
                                  po-entry-type))
                    ;; If over an obsolete entry, continue looking for
                    ;; obsolete ones.  Else, look for an untranslated
                    ;; or fuzzy entry.
                    ('by-order (if (eq po-entry-type 'obsolete)
                                   'obsolete
                                   'untranslated-or-fuzzy)))))
        (while goal
          (case goal
            ('untranslated
             ;; Find an untranslated entry, or wrap up for a fuzzy entry.
             (if (and (> po-untranslated-counter 0)
                      (re-search-forward po-untranslated-regexp nil t))
                 (progn
                   (goto-char (match-beginning 0))
                   (setq goal nil))
                 (goto-char (point-min))
                 (setq goal 'fuzzy)))
            ('fuzzy
             ;; Find a fuzzy entry, or wrap up for an obsolete entry.
             (if (and (> po-fuzzy-counter 0)
                      (re-search-forward po-fuzzy-regexp nil t))
                 (progn
                   (goto-char (match-beginning 0))
                   (setq goal nil))
                 (goto-char (point-min))
                 (setq goal 'obsolete)))
            ('untranslated-or-fuzzy
             ;; Find an untranslated or fuzzy entry, or wrap up for an
             ;; obsolete entry.
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
            ('obsolete
             ;; Find an obsolete entry, or wrap up for an untranslated entry.
             (if (and (> po-obsolete-counter 0)
                      (re-search-forward po-obsolete-msgstr-regexp nil t))
                 (progn
                   (goto-char (match-beginning 0))
                   (setq goal nil))
                 (goto-char (point-min))
                 (if (eq 'po-auto-select-mode 'by-type)
                     (setq goal 'untranslated)
                     (setq goal 'untranslated-or-fuzzy))))
            (t (error (_"Unknown entry type")))))))
  ;; Display this entry nicely.
  (po-current-entry))

(defun po-select-entry-number (num)
  "Go to entry number NUM."
  (interactive "nEntry number: ")
  (loop for i from 1 to num
       initially (po-first-entry)
       do (po-next-entry)
       when (= 0 (mod i 10)) do (message "%i" i)
       finally (progn (po-current-entry)
                      (message "%i" num))))


;;; Editing management and submode.

;; REPLACES ORIGINAL in `po-mode.el'
;; Added calls to `po-find-args' and `po-find-tags'.
(defun po-edit-string (string type expand-tabs)
  "Prepare a pop up buffer for editing STRING, which is of a given TYPE.
TYPE may be 'comment or 'msgstr.  If EXPAND-TABS, expand tabs to spaces.
Run functions on po-subedit-mode-hook."
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
              ;; Try showing all of msgid in the upper window while editing.
              (goto-char po-start-of-msgstr)
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

(defvar po-args-in-msgid '()
  "List of arguments in a msgid, found by `po-find-args'.")

(defvar po-xml-tag-regexp "\\(<[^>]+>\\|&[a-z]+;\\)"
  "*Matches XML tags and entities.")

(defvar po-args-regexp "\\(%[-+# ]?[0-9*]+?\\(\\.[0-9*]\\)?[hlL]?[cdieEfgGosuxXpn]\\|%L?[0-9]\\|\\$\\[[a-z]+\\]\\|%[A-Z_]+\\|\\$[a-z_]+\\$\\|\\$[A-Z_]+\\|\\$([A-Z0-9]+)\\)"
  "*Matches various argument types:
   %[-+ #]?[0-9*]?\\(\\.[0-9*]\\)?[hlL]?[cdieEfgGosuxXpn]
                              C-style printf arguments
   %L?[0-9]           %1      Qt
   \\$[[a-z]+]         $[arg]  OpenOffice.org
   %[A-Z_]            %ARG    OpenOffice.org
   \\$[a-z_]+\\$       $arg$   OpenOffice.org
   \\$[A-Z_]+         $ARG    OpenOffice.org
   \\$([A-Z0-9]+)     $(ARG1)  OpenOffice.org")

(defun po-find-xml-tags (msgid)
  "Find any XML tags in MSGID and put them in `po-xml-tags-in-msg'."
  (setq po-xml-tags-in-msgid (po-find-matches msgid po-xml-tag-regexp)))

(defun po-find-args (msgid)
  "Find any arguments in MSGID and put them in `po-args-in-msg'."
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
      (error (_"No more tags"))))

(defun po-subedit-insert-next-arg ()
  "Insert the next argument that occurs in the msgid."
  (interactive)
  (if po-args-in-msgid
      (insert (pop po-args-in-msgid))
      (error (_"No more arguments"))))


;;; Search and replace.

(defvar po-ignore-in-search "[&~]"
  "*Regexp to ignore when searching, inserted between every
character. (Useful for accelerators.)")

(defun po-add-ignores (s)
  "Return S with the ignore pattern `po-ignore-in-search' added
between every character."
  (interactive "s")
  (cond ((eql (length s) 0) s)
        (t (concat "\\(" po-ignore-in-search "\\)?" (substring s 0 1)
                   (po-add-ignores (substring s 1))))))

(defun po-find-msg (s)
  "Find an entry containing S, ignoring `po-ignore-in-search'."
  (interactive "sFind: ")
  (po-next-entry-with-regexp (po-add-ignores s) t))

(defun po-find-msgstr (s)
  "Find a msgstr containing S, starting from the current
position, ignoring `po-ignore-in-search'."
  (interactive "sFind: ")
  (loop for msgstr = (progn (po-next-entry)
                            (po-find-span-of-entry)
                            (po-get-msgstr nil))
     until (string-match (po-add-ignores s) msgstr)))

(defun po-find-msgid (s)
  "Find a msgid containing S, starting from the current
position, ignoring `po-ignore-in-search'."
  (interactive "sFind: ")
  (loop for msgid = (progn (po-next-entry)
                           (po-find-span-of-entry)
                           (po-get-msgid nil))
     until (string-match (po-add-ignores s) msgid)))

(defun po-first-word (s)
  "Return the first word of S: Anything up to a space, newline,
punctuation or > (end of an XML tag).

This is used to speed up searches by jumping to potential
matches. Multiline messages don't match if the linewrapping is
different, so we try searching for the first word instead,
assuming that it's not wrapped.

FIXME: Escape any regexp syntax. Here or in the caller method?"
  (string-match "^[^ >\n]+" s)
  (match-string 0 s))

(defun po-replace-in-msgstrs (s r)
  "Replace S by R in all msgstrs. Preserves capitalization.
 (We cannot ignore characters here, since we don't know where to
insert them again.)"
  (interactive "sFind: \nsReplace with: ")
  (let ((first-word (po-first-word s)))
    (while (and (re-search-forward "^msgstr" nil t) 
                (re-search-forward first-word nil t))
      ;; `re-search-forward' may find matches outside the msgstr, but
      ;; `po-set-msgstr' does nothing in those cases.
      (po-find-span-of-entry)
      (po-set-msgstr (replace-regexp-in-string s r (po-get-msgstr nil)))
      (po-current-entry))))

(defun po-filter-string-externally (string command)
  "Pass STRING to COMMAND and put the output in msgstr.
%s in COMMAND will be replaced by STRING."
  (interactive "sCommand: ")
  (po-find-span-of-entry)
  (let ((coding-system-for-read 'utf-8)
        (coding-system-for-write 'utf-8))
    (po-set-msgstr 
     (shell-command-to-string
      (format command string)))))


;;; Lookup

;; I need a general interface for lookup methods, so that the user can
;; add his own. 
;;
;; Wanted features:
;;
;; - Interactive lookups, defaulting to the msgid or current-word.
;;
;; - Combined lookup in several auxiliaries, or perhaps any
;;   combination?
;;
;; - Hide/show original string. Useful for near matches, but a waste
;;   of space for exact matches.

;; (po-deflookup "Dansk" 
;;               (lambda ()
;;                 (po-lookup-in-file (replace-regexp-in-string
;;                                     "/nn/" "/da/" (buffer-file-name)))))

;; (po-deflookup "Bokmål" 
;;               (lambda ()
;;                 (po-lookup-in-file (replace-regexp-in-string
;;                                     "/nn/" "/nb/" (buffer-file-name)))))

;; (po-deflookup "Fellesordlista"
;;               (lambda ()
;;                 (po-lookup-in-slxdict)))

;; (defmacro po-add-lookup (name function)
;;   "Add a lookup function named NAME to `po-lookup-functions'.
;; FUNCTION should put a list of lookup results in
;; `po-lookup-list'."  )

;; (defmacro po-remove-lookup (name)
;;   "Remove the lookup function NAME from `po-lookup-functions'."
;;   (setq po-lookup-list (remove-if (lambda (n)
;;                                     (string-equal name (name n))))))

(defvar po-lookup-buffer "*po-lookup*"
  "The buffer where search results should show up. This is
suggested to be in a separate frame that is always open.")

(defvar po-lookup-list nil
  "A list that holds the lookup results.")

(defstruct po-lookup-result
  (original-string)
  (string)
  (score)
  (status)
  (location)
  (method))

(defun po-setup-lookup-frame ()
  "Set up a separate frame to display the lookup result, and
switch back again to the current frame."
  (interactive)
  (let ((current-frame (selected-frame))
        (lookup-frame 
         (select-frame (make-frame `((name . "PO lookup")
                                     (width . ,po-lookup-frame-width)
                                     (height . ,po-lookup-frame-height)
                                     (minibuffer . nil))))))
    (switch-to-buffer (get-buffer-create po-lookup-buffer))
    (delete-other-windows)
    (set-fill-column po-lookup-frame-width)
    (unless (string-equal po-lookup-frame-font "")
      (set-frame-font po-lookup-frame-font))
    (set-frame-position lookup-frame -1 -1)
    (select-frame-set-input-focus current-frame)
    (message "Lookup frame opened.")))

(defun po-lookup-file ()
  "Return the name of the file to look up in, generated by
replacing `po-lookup-replace' with `po-lookup-replace-with' in
the full filename. Returns nil if the file doesn't exist."
  (replace-regexp-in-string po-lookup-replace
                            po-lookup-replace-with
                            (buffer-file-name)))

(defun po-lookup-in-file (name method)
  "Search the PO file NAME for a msgid equal to the current
msgid. Add the results to the lookup list.

This function is heavily based on `po-seek-equivalent-translation'. 
It might be possible to merge them."
  (unless (file-exists-p name)
    (error (format "File not found: %s" name)))
  (po-find-span-of-entry)
  (let ((msgid (po-get-msgid nil))
;;        (string (buffer-substring po-start-of-msgid po-start-of-msgstr))
        (current (current-buffer))
        (buffer (find-file-noselect name))
        (case-fold-search nil))
    (set-buffer buffer)
    (unless (string-equal msgid "")
      (let (found
            (first-word (po-first-word msgid)))
        (goto-char (point-min))
        ;; EXPERIMENTAL SPEEDUP ATTEMPT :-)
        ;; Try first to find an exact match with the quoted msgid.
        ;; This is fast, but will miss entries with different
        ;; word-wrapping.
        ;; WOW! Disabling case-fold-search is even more efficient.
;;         (while (and (not found) (search-forward string nil t))
;;           ;; Screen out longer 'msgid's.
;;           (if (looking-at "^msgstr ")
;;               (progn
;;                 (po-find-span-of-entry)
;;                 ;; Ignore an untranslated entry.
;;                 (or (string-equal
;;                      (buffer-substring po-start-of-msgstr po-end-of-entry)
;;                      "msgstr \"\"\n")
;;                     (setq found t)))))
        ;; Find potential matches by searching for `first-word'. This
        ;; is faster than looking at every msgid, since the exact
        ;; comparison is slow. The search for "^msgid" is done to make
        ;; sure we move to a new message before searching for new
        ;; matches.
        (while (and (not found)
                    (re-search-forward "^msgid" nil t)
                    (re-search-forward first-word nil t))
          (po-find-span-of-entry)
          (let ((looked-up (po-get-msgid nil)))
            ;; See if the potential match is a real match and make sure
            ;; it's not untranslated.
            (if (and (string-equal msgid looked-up)
                     (not (eq 'untranslated po-entry-type)))
                (setq found t))))
        (if found
            (progn
              (set-buffer buffer)
              ;;               (po-empty-lookup)
              (if (eq 'fuzzy po-entry-type)
                  (po-add-lookup (po-get-msgstr nil) 100 "f" name method)
                  (po-add-lookup (po-get-msgstr nil) 100 "t" name method)))
                                        ;(po-empty-lookup)
            (message (_"Not found")))
        found))))

(defface po-lookup-score-face
    '((t (:foreground "black"
          :background "khaki")))
  "Face for displaying score values."
  :group 'po)

(defface po-lookup-status-face
    '((t (:foreground "blue1"
          :background "white")))
  "Face for displaying the status of a lookup result."
  :group 'po)

(defface po-lookup-odd-face
    '((t (:background "white"
          :foreground "black")))
  "Face for displaying the odd lines of the lookup results."
  :group 'po)

(defface po-lookup-even-face
    '((t (:background "lavender"
          :foreground "black")))
  "Face for displaying the even lines of the lookup results."
  :group 'po)

(defun po-lookup ()
  "Lookup the current string using the selected lookup method."
  (interactive)
  (po-empty-lookup)
  (po-lookup-in-file (po-lookup-file) "Bokmål")
  (po-display-lookup po-lookup-list))

(defun po-lookup ()
  "Lookup the current string using the selected lookup method."
  (interactive)
  (po-empty-lookup)
  (po-lookup-in-files)
  (po-display-lookup po-lookup-list))

(defun po-lookup-in-files ()
  "Lookup the current string in several files."
  ;; FIXME: Handle missing files.
  (let ((po-lookup-replace-with "/da/"))
    (save-excursion
      (po-lookup-in-file (po-lookup-file) "Dansk")))
  (let ((po-lookup-replace-with "/nb/"))
    (save-excursion
      (po-lookup-in-file (po-lookup-file) "Bokmål"))))


(defun po-empty-lookup ()
  "Empty the lookup list."
  (setq po-lookup-list nil)
  (po-display-lookup po-lookup-list))

(defun po-add-lookup (string score &optional status location method)
  "Add the search result STRING to `po-lookup-list', and update
the display. The SCORE for an exact match should be 100."
;  (add-to-list 'po-lookup-list (list score status location string))
  (add-to-list 'po-lookup-list (make-po-lookup-result
                                :original-string nil
                                :string string
                                :score score
                                :status status
                                :location location
                                :method method))
  ;  (po-display-lookup po-lookup-list)
  )


(defun po-truncate-start (s width)
  "Truncate the beginning of the string S to WIDTH."
  (if (> (length s) width)
      (substring s (- width))
      s))

(defun po-basename (path)
  "Return the filename part of PATH."
  (string-match "[^/]+$" path)
  (match-string 0 path))

(require 'button)

(define-button-type 'po-location-button
    'filename nil
    'action #'po-edit-from-lookup-button)

(defun po-display-lookup (result-list)
  "Place search results into `po-lookup-buffer'."
  ;; TODO: Handle missing values.
  (let ((display-buffer-reuse-frames t)
        (line-face 'po-lookup-odd-face))
    (save-excursion
      (set-buffer (get-buffer-create po-lookup-buffer))
      (delete-region (point-min) (point-max))
      (mapc (lambda (result)
              (let ((score (po-lookup-result-score result))
                    (status (or (po-lookup-result-status result) ""))
                    (location (or (po-lookup-result-location result) ""))
                    (method (or (po-lookup-result-method result) ""))
                    (string (po-lookup-result-string result)))
                (insert
                 (propertize (format " %3s " score)
                             'face 'po-lookup-score-face)
                 (propertize (format " %s " status)
                             'face  'po-lookup-status-face)
                 (propertize (format "%s " method)
                             'face  'po-lookup-status-face))
                (insert-text-button (po-basename location)
                                    'filename location
                                    'help-echo location
                                    :type 'po-location-button)
                (insert "\n" (propertize string 'face line-face) "\n")
                (if (eq line-face 'po-lookup-odd-face)
                    (setq line-face 'po-lookup-even-face)
                    (setq line-face 'po-lookup-odd-face))))
            result-list)
      (when (featurep 'longlines)
        (longlines-mode 1))
      (display-buffer po-lookup-buffer))))

(defun po-get-lookup ()
  "Return the selected search result."
  ;; Currently returns the first result only.
  ;; How about a prefix argument to copy entry number X?
  (po-lookup-result-string (car po-lookup-list)))

(defun po-copy-from-lookup ()
  "Set the msgid of the current message to the selected lookup
result."
  (interactive)
  (po-find-span-of-entry)
  (let ((buffer-read-only po-read-only)
        (entry-type po-entry-type)
        replacement)
    (save-excursion
      (if (or (eq entry-type 'untranslated)
              (eq entry-type 'obsolete)
              (y-or-n-p (_"Really lose previous translation? ")))
          (setq replacement (po-get-lookup))))
    (when replacement
      (po-set-msgstr replacement))
    (message "")))

(defun po-subedit-copy-from-lookup ()
  "Insert the selected search result into the buffer."
  (interactive)
  (delete-region (point-min) (point-max))
  (insert (po-get-lookup)))

(defun po-edit-lookup-entry ()
  "Open the looked-up entry in a buffer where you can edit it."
  (interactive)
  (find-file (po-lookup-result-location (car po-lookup-results))))

(defun po-edit-from-lookup-button (button)
  "Open the entry that BUTTON points to."
  (find-file-other-frame (button-get button 'filename)))

(defun po-select-lookup-method ()
  "Cycle the available lookup methods."
  (interactive)
  (error "Not yet implemented"))

(defun po-select-next-lookup-result ()
  "Select the next lookup result."
  (interactive)
  (save-excursion
    (set-buffer (get-buffer-create po-lookup-buffer))
    (re-search-forward "^ [0-9]")))

(defun po-select-previous-lookup-result ()
  "Select the previous lookup result."
  (interactive)
  (error "Not yet implemented"))

(provide 'po-mode+)

;;; po-mode+.el ends here
