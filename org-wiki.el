;;; org-wiki.el --- Desktop wiki extension for org-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Caio Rodrigues Soares Silva

;; Author: Caio Rodrigues       <caiorss DOT rodrigues AT gmail DOT com>
;; Maintainer: Caio Rordrigues  <caiorss DOT rodrigues AT gmail DOT com>
;; Keywords: org-mode, wiki, notes, notebook
;; Version: 4.2
;; URL: https://www.github.com/caiorss/org-wiki'
;; Package-Requires: ((helm-core "2.0") (org "9") (cl-lib "0.5"))


;; This is free and unencumbered software released into the public domain.
;;
;; Anyone is free to copy, modify, publish, use, compile, sell, or
;; distribute this software, either in source code form or as a compiled
;; binary, for any purpose, commercial or non-commercial, and by any
;; means.
;;
;; In jurisdictions that recognize copyright laws, the author or authors
;; of this software dedicate any and all copyright interest in the
;; software to the public domain. We make this dedication for the benefit
;; of the public at large and to the detriment of our heirs and
;; successors. We intend this dedication to be an overt act of
;; relinquishment in perpetuity of all present and future rights to this
;; software under copyright law.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
;; OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
;; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;; OTHER DEALINGS IN THE SOFTWARE.
;;
;; For more information, please refer to <http://unlicense.org/>

;;; Commentary:

;; Org-wiki is a org-mode extension that provides tools to manage and
;; build a desktop wiki where each wiki page is an org-mode file.
;;

;;; Code:

;; external libraries
(require 'ox-html)
(require 'helm)

;; built-in Emacs lib
(require 'cl-lib)     ;; Common-lisp emulation library
(require 'easymenu)
(require 'subr-x)     ;; Provides string trim functions.

;; ****************** U S E R - S E T T I N G S ************************ ;;

(defgroup org-wiki nil
  "Org-wiki Settings"
  :group 'tools
  )

(defcustom org-wiki-location "~/org/wiki"
  "Org-wiki directory where all wiki pages files *.org are stored.
Default value ~/org/wiki."
  :type 'directory
  :group 'org-wiki
  )

(defcustom org-wiki-default-read-only nil
  "If this variable is true all org-wiki pages will open as read-only by default.
You can toggle read-only mode with M-x read-only-mode or C-x C-q."
  :type  'boolean
  :group 'org-wiki
  )

;;; =======  Python Webserver Settings =========== ;;

(defcustom org-wiki-server-port "8000"
  "Default port to server org-wiki static files server."
  :type  'string
  :group 'org-wiki
  )

(defcustom org-wiki-server-host "0.0.0.0"
  "Default address that the server listens to."
  :type  'string
  :group 'org-wiki
  )

;; ======== Async export settings ================ ;;

(defcustom org-wiki-emacs-path "emacs"
  "Path to Emacs executable. Default value 'emacs'."
  :type 'file
  :group 'org-wiki
  )

;;; Path to init file like init.el used by function org-wiki-html-export
;; The user can set for a more lightweight file in order to speed up the
;; exporting speed.
;;
(defcustom org-wiki-user-init-file (concat (file-name-as-directory user-emacs-directory) "init.el")
  "Path to init.el file used for asynchronous export."
  :type 'file
  :group 'org-wiki
  )


;; ====== Optional Clip.jar image pasting app =========== ;;
(defcustom org-wiki-backup-location nil
  "Path to backup directory."
  :type 'directory
  :group 'org-wiki
  )

;; Optional Clip.jar image pasting app
(defcustom org-wiki-clip-jar-path "~/bin/Clip.jar"
  "Path to Clip.jar utility to paste images from clipboard."
  :type 'file
  :group 'org-wiki 
  )



;;; Default index page (index.org) accessed with M-x org-wiki-index
;;
(defvar org-wiki-index-file-basename "index")


;;; Additional publishing options
(defcustom org-wiki-publish-plist '()
  "Additional options passed to `org-publish'."
  :type 'plist
  :group 'org-wiki)


(defcustom org-wiki-template
  (concat "#+TITLE: %n\n"
          "#+DESCRIPTION:\n"
          "#+KEYWORDS:\n"
          "#+STARTUP:  content\n"
          "\n\n"
          "- [[wiki:index][Index]]\n\n"
          "- Related: \n\n"
          "* %n\n"
          )
  "Default template used to create org-wiki pages/files.
- %n - is replaced by the page name.
- %d - is replaced by current date in the format year-month-day."

  :type 'string
  :group 'org-wiki
  )

;; ********************** I N T E R N A L - F U N C T I O N S ************************* ;;
;;

;; @SECTION: Internal functionsq
;;
(defun org-wiki--concat-path (base relpath)
  "Concat directory path (BASE) and a relative path (RELPATH)."
  (concat (file-name-as-directory base) relpath))



(defun org-wiki--get-buffers ()
  "Return all org-wiki page buffers (.org) files in `org-wiki-location`."
 (cl-remove-if-not (lambda (p)
                 (let* ((fp (buffer-file-name p))
                        (fpath (if fp (expand-file-name fp) nil))
                        )
                         ;; path test if file exists (if fpath not nil)
                   (and  fpath
                         ;; test if buffer file is in wiki location 
                         (string-prefix-p (expand-file-name org-wiki-location) fpath)
                         ;; test if buffer file has extension .org 
                         (string-suffix-p ".org" fpath)
                    )))
               (buffer-list)))

(defun org-wiki--normalize-path (path)
  "Replace double slashes for a single slash and remove slash at the end of a PATH."
  (replace-regexp-in-string
   "//"
   "/"
   (replace-regexp-in-string "/$" "" (expand-file-name path))))

(defun  org-wiki--path-equal (p1 p2)
  "Test if paths P1 and P2 are equal."
  (equal (org-wiki--normalize-path p1) (org-wiki--normalize-path p2)))


(defun org-wiki--file->page (filename)
  "Get a wiki page name from a FILENAME.
Example:
ELISP> (file->org-wiki--page  \"Spanish.org\")
   \"Spanish\""
  (file-name-base filename))

(defun org-wiki--replace-extension (filename extension)
  "Replace a FILENAME extension by an new EXTENSION.
Example:
ELISP> (org-wiki/replace-extension \"file.org\" \"html\" )
       \"file.html\""
  (concat (car (split-string filename "\\."))
          "."
          extension
          ))


(defun org-wiki--page->file (pagename)
  "Get the corresponding wiki file (*.org) to the wiki PAGENAME.
Example:

ELISP> (org-wiki--page->file \"Linux\")
  \"~/org/wiki/Linux.org\""

  (concat (file-name-as-directory org-wiki-location)
          pagename
          ".org"
          ))

(defun org-wiki--current-page ()
  "Get current org-wiki page's name bound to current buffer."
  (org-wiki--file->page (buffer-file-name)))

(defun org-wiki--current-page-asset-dir ()
  "Get current org-wiki page's asset directory"
  (interactive)
  (concat (file-name-as-directory org-wiki-location)
          (file-name-base (buffer-file-name))))

(defun org-wiki--current-page-asset-file (filename)
  "Get current page's asset file path given its name.
Example: If the current page is 'Smalltalk programming'

ELISP> (org-wiki--current-page-asset-file \"manual.pdf\")
\"Smalltalk programming/manual.pdf\"
ELISP>"
  (concat (file-name-as-directory (file-name-base (buffer-file-name)))
          filename))

(defun org-wiki--buffer-file-in-wiki-p ()
  "Return true if current buffer file name is inside wiki directory."
  (file-exists-p
   (org-wiki--concat-path
    org-wiki-location
    (file-name-nondirectory (buffer-file-name)))))

(defun org-wiki--list-pages ()
  "Return a list containing all pages files *.org."
  (directory-files org-wiki-location))


(defun org-wiki--page->html-file (pagename)
  "Convert a wiki PAGENAME to html file name."
  (concat (file-name-as-directory (expand-file-name org-wiki-location))
          pagename
          ".html"
          ))

(defun org-wiki--page-files (&optional abspath)
  "Return a list containing all files in the wiki directory.

\(org-wiki--page-files &optional ABSPATH)

if abspath is null returns relative path, otherwise returns the absolute path.

Example:

ELISP> (remove-if-not #'file->org-wiki/page (org-wiki/page-files))
  (\"Abreviations_Slangs.wiki.org\" \"Android.wiki.org\" \"Bash_Script.wiki.org\")"

  (cl-remove-if-not
   (lambda (s)
     (let ((b (file-name-base s)))
       (not (or
             (string-prefix-p ".#" b)
             (string-suffix-p "~"  b )
             (string-prefix-p "#" b)
             (string-suffix-p "#" b)
           ))))
   (directory-files org-wiki-location abspath "\\.org$")))


(defun org-wiki--page-list ()
  "Return a list containing all wiki pages.
Example: '(\"Linux\" \"BSD\" \"Bash\"  \"Binary_Files\")"
  (mapcar #'org-wiki--file->page (org-wiki--page-files)))


;; @REVIEW: Function for future use.
;;
;; (defun org-wiki--get-page (wikipage)
;;   (org-wiki--concat-path org-wiki-location
;;                     (replace-regexp-in-string "\s" "_"
;;                     (replace-regexp-in-string "%20" "_"
;;                      (concat wikipage ".org")))))


(defun org-wiki--assets-get-dir (pagename)
  "Get path to asset directory of given PAGENAME."
  (org-wiki--concat-path org-wiki-location pagename))


(defun org-wiki--assets-make-dir (pagename)
  "Create the asset directory of a wiki page (PAGENAME) if it doesn't exist.
Example: (org-wiki--assets-make-dir \"Bash\")

It will crate the directory ~/wiki-location/Bash/
corresponding to the file ~/wiki-location/Bash.org
if it doesn't exist yet."
  (let ((assets-dir (org-wiki--assets-get-dir pagename)))
    (if (not (file-exists-p assets-dir))
        (make-directory assets-dir t))))


(defun org-wiki--assets-buffer-make-dir ()
  "Create asset directory of current buffer page if it doesn't exit."
  (if (org-wiki--buffer-file-in-wiki-p)
      (progn
        (org-wiki--assets-make-dir
         (file-name-base (buffer-file-name))))
    (message "Error: Not in a wiki page.")))


(defun org-wiki--is-buffer-in (b)
  "Check if buffer is an org-wiki buffer.
It returns true (non nil) if buffer directory is a subdirectory of
org-wiki-location."
  (string-prefix-p
   (expand-file-name org-wiki-location)
   (expand-file-name (with-current-buffer b
                       default-directory))))

;;=============== Org-mode custom protocol ===============;;
;;
;; @SECTION: Protocol

(defun org-wiki--org-link (path desc backend)
  "Creates an html org-wiki pages when  exporting to html.
Example: The hyperlink [[wiki:Linux][Dealing with Linux]]
will be exported to <a href='Linux.html'>Dealing with Linux</a>"
   (cl-case backend
     (html (format
            "<a href='%s.html'>%s</a>"
            path
            (or desc path)))))

(defun org-wiki--make-link (pagename)
  "Return a string containing a wiki link [[wiki:PAGENAME][PAGENAME]].
Example: if PAGENAME is Linux it will return [[wiki:Linux][Linux]]"
  (format "[[wiki:%s][%s]]" pagename pagename))

(defun org-wiki--open-page (pagename)
  "Open or create new a org-wiki page (PAGENAME) by name.
Example:  (org-wiki--open-page \"Linux\")
Will open the the wiki file Linux.org in
`org-wiki-location`"
  (let ((org-wiki-file (org-wiki--page->file pagename)))
    (if (not (file-exists-p org-wiki-file))
        ;; Action executed if file doesn't exist.
        (progn (find-file  org-wiki-file)
               ;; Insert header at top of page
               (org-wiki-header)
               ;; Save current page buffer
               (save-buffer)
               ;; Create assets directory
               (org-wiki--assets-make-dir pagename))

      ;; Action executed if file exists.
      (if org-wiki-default-read-only
          ;; open file in read-only mode. 
          (progn  (find-file  org-wiki-file)
                  (read-only-mode 1))
          ;; open file in writable mode. 
          (find-file  org-wiki-file))
        ))) 


(defun org-wiki--assets-get-file (pagename filename)
  "Return a path to an asset file FILENAME in given PAGENAME."
  (org-wiki--concat-path (org-wiki--assets-get-dir pagename) filename))

(defun org-wiki--assets-open-file-emacs (pagename filename)
  "Open an asset file FILENAME of a PAGENAME with Emacs.

Example: (org-wiki--assets-open-file-emacs \"Python\" \"example1.py\")
It will open the file <wiki path>/Python/example1.py related to the page Python.org."
  (find-file  (org-wiki--assets-get-file pagename filename)))


(defun org-wiki-xdg-open (filename)
  "Open a file FILENAME with default system application.
This function is operating system independent.

Running in Linux or BSD invokes the script xdg-open
Running in Windows invokes  cmd.exe
Running in Mac OSX invokes open"
  (cl-case system-type

    ;;; Linux
    (gnu/linux      (let ((process-connection-type  nil))

                      (start-process
                          "proc"
                          nil
                         ;; Command
                          "xdg-open" (expand-file-name filename))))
    ;;; Free BSD OS
    (gnu/kfreebsd    (let ((process-connection-type  nil))

                       (start-process
                          "proc"
                          nil
                           ;; Command
                          "xdg-open" (expand-file-name filename))))

    ;; Mac OSX - Tested. Ok.
    (darwin        (start-process
                    "proc"
                    nil
                    ;; Command
                    "open" (concat  (expand-file-name filename))))

    ;; Windows 7, 8, 10 - Kernel NT
    (windows-nt   (start-process
                   "proc"
                   nil
                   ;; Command
                   "cmd"  "/C"  "start" "" (expand-file-name filename))

       ))) ;; End of org-wiki/xdg-open


(defun org-wiki--protocol-open-assets-with-sys (link)
  "Org-mode protocol handler to open an asset with default system app.
Example: it will turn a hyperlink LINK of syntax Blueprint;box1.dwg that
points to the file <org wiki location>/Blueprint/box1.dwg."

  (let* ((a     (split-string link ";"))
        (pagename  (car a))
        (filename  (cadr a))
        )
    (org-wiki-xdg-open
     (org-wiki--assets-get-file pagename filename))))


;;  @DONE: Implement html exporting to org-wiki asset files
;;
(defun org-wiki--asset-link (path desc backend)
  "Creates an html org-wiki pages html exporting."
  (let* ((a    (split-string path ";"))
        (page  (car a))
        (asset (cadr a))
        (file-path (concat page "/"  asset))
        )
   (cl-case backend
     (html (format
            "<a href='%s'>%s</a>"
            file-path
            (or desc asset))))))

;;; Custom Protocols
(add-hook 'org-mode-hook
          (lambda ()
            (org-add-link-type  "wiki"
                                #'org-wiki--open-page
                                #'org-wiki--org-link )
            (org-add-link-type  "wiki-asset-sys"
                                #'org-wiki--protocol-open-assets-with-sys
                                #'org-wiki--asset-link)))

(defun org-wiki--helm-selection (callback)
  "Open a helm menu to select the wiki page and invokes the CALLBACK function."
  (helm :sources `((
                      (name . "Wiki Pages")
                      (candidates . ,(delete-dups (org-wiki--page-list)))
                      (action . ,callback)
                      ))))


(defun org-wiki--asset-page-files (pagename)
  "Get all asset files from a given PAGENAME."
  (org-wiki--assets-make-dir pagename)
  (directory-files (org-wiki--assets-get-dir pagename)))


(defun org-wiki--asset-helm-selection (callback)
  "Higher order function to deal with page assets.

org-wiki-asset-helm-selection (CALLBACK)

This function opens a helm menu to select a wiki page and then
passes the result of selection to a callback function that takes
a asset file as argument.

Example: If the user selects the file freebsdref1.pdf it inserts the
file name at current point.

> (org-wiki--asset-helm-selection (lambda (file) (insert file)))
  freebsdref1.pdf"
  (helm :sources `((
                      (name . "Wiki Pages")
                      (candidates . ,(org-wiki--asset-page-files
                                      (org-wiki--current-page)))
                      (action .  (lambda (file)
                                   (,callback (org-wiki--current-page-asset-file file))
                                   ))
                      ))))


(defun org-wiki--asset-download-hof (callback)
  "Higher order function to download a file.
Callback is a function with this signature:
 (callback <pagename> <filename>)

How this function works:
1. Ask the user for the URL suggesting the URL extracted from the clipboard.
2. Ask the user for the file name to be downloaded suggesting the filename extracted from
the URL.
3. Calls the callback function passing the current page name and the file name.

If the URL is: http://www.myurl.com/Manual1.pdf, the current page is Unix and
the callback function is:

  (lambda (p f) (insert (format \"%s/%s\" p f)))

if the user doesn't change the suggested file name It will insert at current
point: 'Unix/Manual.pdf'."
  (let*
      ((pagename (file-name-base (buffer-file-name)))

       ;; Get the URL suggestion from clibpoard
       (text (with-temp-buffer
              (clipboard-yank)
              (buffer-substring-no-properties (point-min)
                                              (point-max))))
       (url (read-string "Url: " text))
       (default-directory (org-wiki--assets-get-dir pagename))

       (output-file  (read-string "File name: "
                                  (car  (last (split-string url "/"))))))

    (org-wiki--assets-make-dir pagename)
    (url-copy-file url output-file)
    (funcall callback pagename output-file)))


;;************** U S E R -  M-X - C O M M A N D S ********************* ;;;
;;
;; @SECTION: User commands

(defun org-wiki-help ()
  "Show org-wiki commands."
  (interactive)
  (command-apropos "org-wiki-"))


(defun org-wiki-index ()
  "Open the index page: <org-wiki-location>/index.org.

   The file index.org is created if it doesn't exist."
  (interactive)
  (org-wiki--open-page org-wiki-index-file-basename))


(defun org-wiki-index-html ()
  "Open the Wiki (Index) in the default web browser."
  (interactive)
  (browse-url (concat "file://"
                      (org-wiki--page->html-file
                       org-wiki-index-file-basename))))

(defun org-wiki-index-frame ()
  "Open the index page in a new frame."
  (interactive)
  (with-selected-frame (make-frame)
    (org-wiki-index)))

(defun org-wiki-dired-all ()
  "Open the wiki directory in ‘dired-mode’ showing all files."
  (interactive)
  (dired org-wiki-location)
  (dired-hide-details-mode))

(defun org-wiki-dired ()
  "Open the wiki directory showing only the wiki pages."
  (interactive)
  (dired (org-wiki--concat-path org-wiki-location "*.org"))
  (dired-hide-details-mode))

(defun org-wiki-make-page ()
  "Create a new wiki page."
  (interactive)
  (find-file (org-wiki--page->file (read-string "Page Name: "))))

(defun org-wiki-asset-dired ()
  "Open the asset directory of current wiki page."
  (interactive)
  (let ((pagename (file-name-base (buffer-file-name))))
    (org-wiki--assets-make-dir pagename)
    (dired (org-wiki--assets-get-dir pagename))))

(defun org-wiki-asset-insert ()
  "Insert link wiki-asset-sys:<page>;<file> to an asset file of current page..
It inserts a link of type wiki-asset-sys:<Wiki-page>;<Asset-File>
Example:  [[wiki-asset-sys:Linux;LinuxManual.pdf]]"
  (interactive)
  (org-wiki--asset-helm-selection
   (lambda (file)
     (insert (format "[[wiki-asset-sys:%s;%s][%s]]"
                     (file-name-base (org-wiki--current-page-asset-dir))
                     (file-name-nondirectory file)
                     (read-string "Description: " (file-name-nondirectory file))
                     )))))

(defun org-wiki-asset-insert-file ()
  "Insert link file:<page>/<file> to asset file of current page at point.
Use this command to insert link to files that can be opened with
Emacs like source codes. It will insert a link like this
- [[file:Python/GpsScript.py][GpsScript.py]]."
  (interactive)
  (org-wiki--asset-helm-selection
    (lambda (file)
      (save-excursion
        (insert (org-make-link-string
                 (concat "file:" file)
                 (file-name-nondirectory file)
                 ))))))


(defun org-wiki-asset-insert-image ()
  "Insert link file:<page>/<file> to images asset file at point.
This command is similar to org-wiki-asset-insert-file but it inserts a link
in this way: [[file:Linux/logo.png][file:Linux/logo.png/]]."
  (interactive)
  (org-wiki--asset-helm-selection
    (lambda (file)
      (save-excursion
        (insert (org-make-link-string
                 (concat "file:" file)
                 (concat "file:" file)
                 ))))))



(defun org-wiki-asset-insert-block ()
  "Insert code block with contents of some asset file."
  (interactive)
  (org-wiki--asset-helm-selection
    (lambda (file)
      (save-excursion
        (insert (concat " - File: " (org-make-link-string (concat "file:" file))))
        (insert "\n\n")
        (insert "#+BEGIN_SRC text\n")
        (insert "  ")
        (insert (replace-regexp-in-string
                 "\n"
                 "\n  "
                 (with-temp-buffer
                   (insert-file-contents file)
                   (buffer-substring-no-properties (point-min) (point-max)))))
        (insert "\n#+END_SRC")
        ))))


(defun org-wiki-asset-find-file ()
  "Open a menu to select an asset file of current page and open it with Emacs.
Note: see 'org-wiki-asset-find-sys'

Example: If the current page is 'Smalltalk programming' and the user select the
file 'extendingClasses-number1.gst' it will open the file below with Emacs.

 - Smalltalk programming/'extendingClasses-number1.gst"
  (interactive)
  (org-wiki--asset-helm-selection #'find-file))

(defun org-wiki-asset-find-sys ()
  "Open a menu to select an asset file of current page and open it with system's app.
Example: If the current page is 'Smalltalk programming' and the
user select the file 'numerical-methods-in-smalltalk.pdf' it will
be opened with the default system's application like Foxit PDF or
Okular reader."
  (interactive)
  (org-wiki--asset-helm-selection #'org-wiki-xdg-open))

(defun org-wiki-asset-create ()
  "Prompts the user for a file name that doesn't exist yet and insert it at point.
Unlike the commands `org-wiki-asset-insert` or ` org-wiki-asset-insert-file` this command
asks the user for a file that doesn't exist yet and inserts a hyperlink to it at point.

It is useful to add links to scripts that will be stored in the
page directory.

Example: If the user enter this command and is in the page Linux
and enters scriptDemoQT.py it will insert a link at point like
this file:Linux/scriptDemoQT.py .

- Page:       <org-wiki-location>/Linux.org
- Directory:  <org-wiki-location>/Linux/"
  (interactive)
  (let ((filename (read-string "File: ")))
    (save-excursion
      (insert (org-make-link-string
               (concat "file:"
                       (org-wiki--current-page-asset-file filename))
               filename
               )))))


(defun org-wiki-asset-download-insert1 ()
  "Download a file from a URL in the clibpoard and inserts a link wiki-asset-sys:.
Note: This function is synchronous and blocks Emacs. If Emacs is stuck
type C-g to cancel the download."
  (interactive)
  (org-wiki--asset-download-hof
   (lambda (pagename output-file)
     (save-excursion (insert (format "[[wiki-asset-sys:%s;%s][%s]]"
                                     pagename output-file output-file))))))

(defun org-wiki-asset-download-insert2 ()
  "Download a file from a URL in the clibpoard and inserts a link file:<page>/<asset-file>.
Note: This function is synchronous and blocks Emacs. If Emacs gets frozen type C-g
to cancel the download."
  (interactive)
  (org-wiki--asset-download-hof
   (lambda (pagename output-file)
     (save-excursion (insert (format "file:%s/%s" pagename output-file ))))))

(defun org-wiki-helm ()
  "Browser the wiki files using helm."
  (interactive)
  (org-wiki--helm-selection #'org-wiki--open-page))


(defun org-wiki-helm-read-only ()
  "Open wiki page in read-only mode."
  (interactive)
  (org-wiki--helm-selection (lambda (pagename)
                             (find-file-read-only
                              (org-wiki--page->file pagename)
                              ))))

(defun org-wiki-helm-frame ()
  "Browser the wiki files using helm and opens it in a new frame."
  (interactive)
  (org-wiki--helm-selection  (lambda (act)
                               (with-selected-frame (make-frame)
                                 (set-frame-name (concat "Org-wiki: " act))
                                 (org-wiki--open-page act)
                                ))))


(defun org-wiki-switch ()
  "Switch between org-wiki page buffers."
  (interactive)
  (helm :sources `((
                   (name . "Wiki Pages")
                   (candidates . ,(mapcar (lambda (b)
                                            (cons (org-wiki--file->page (buffer-file-name b))
                                                  b
                                                  ))
                                          (org-wiki--get-buffers)))
                   (action . switch-to-buffer)
                   ))))

;;  @TODO: Implement org-wiki/helm-html
;;
(defun org-wiki-helm-html ()
  "Browser the wiki files using helm."
  (interactive)
    (helm :sources `((
                      (name . "Wiki Pages")

                      (candidates . ,(delete-dups (org-wiki--page-list)))

                      (action . org-wiki--open-page)
                      ))))

(defun org-wiki-close ()
  "Close all opened wiki pages buffer and save them."
  (interactive)
  (mapc (lambda (b)
          (with-current-buffer b
            (when (org-wiki--is-buffer-in b)
              ;; save the buffer if it is bound to a file
              ;; and it is not read-only
              (when (and (buffer-file-name b)
                         (not buffer-read-only))
                (save-buffer))
              (kill-this-buffer))))
        (buffer-list))
  (message "All wiki files closed. Ok."))


(defun org-wiki-close-image ()
  "Close all image/picture buffers which files are in org-wiki directory."
  (interactive)
  (mapc (lambda (b)
          (with-current-buffer b
            (when (and (org-wiki--is-buffer-in b)
                       (equal major-mode 'image-mode))
              (kill-this-buffer))))        
        (buffer-list))
  (message "All wiki images closed. Ok."))

(defun org-wiki-insert ()
  "Insert a Wiki link at point for a existing page."
  (interactive)
  (org-wiki--helm-selection
   (lambda (page) (insert (org-wiki--make-link page)))))

(defun org-wiki-create ()
  "Create a new org-wiki page asking the user its name."
  (interactive)
  (let ((page-name (read-string  "Page: ")))
    (save-excursion (insert (org-make-link-string (concat "wiki:" page-name)
                                                  page-name
                                                  )))))


(defun org-wiki-html-page ()
  "Open the current wiki page in the browser.  It is created if it doesn't exist yet."
  (interactive)
  (let ((html-file   (org-wiki--replace-extension (buffer-file-name) "html")))
    (if (not (file-exists-p html-file))
        (org-html-export-to-html))
  (browse-url html-file)))

(defun org-wiki-html-page2 ()
  "Exports the current wiki page to html and opens it in the browser."
  (interactive)
  (org-html-export-to-html)
  (browse-url (org-wiki--replace-extension (buffer-file-name) "html")))

(defun org-wiki-search ()
  "Search all wiki pages that contains a pattern (regexp or name)."
  (interactive)
  (rgrep (read-string "org-wiki - Search for: ")
         "*.org"
         org-wiki-location
         nil))

(defun org-wiki-open ()
  "Opens the wiki repository with system's default file manager."
  (interactive)
  (org-wiki-xdg-open org-wiki-location))

(defun org-wiki-asset-open ()
  "Open asset directory of current page with system's default file manager."
  (interactive)
  (org-wiki--assets-buffer-make-dir)
  (org-wiki-xdg-open (file-name-base (buffer-file-name))))

(defun org-wiki-assets-helm ()
  "Open the assets directory of a wiki page."
  (interactive)
  (org-wiki--helm-selection
   (lambda (page)
     (org-wiki--assets-make-dir page)
     (dired (org-wiki--assets-get-dir page)))))


(defun org-wiki-make-org-publish-plist (org-exporter)
  "Prepare plist for use with `org-publish'."
  (let ((plist-base
         `("html"
           :base-directory        ,org-wiki-location
           :base-extension        "org"
           :publishing-directory  ,org-wiki-location
           :publishing-function   ,org-exporter)))
    (setcdr plist-base
            (org-combine-plists (cdr plist-base) org-wiki-publish-plist))
    plist-base))

(defun org-wiki-export-with (org-exporter)
  "Export all pages to a given format. See full doc.
ORG-EXPORTER is a function that exports an org-mode page to a specific format like html.
It can be for instance: 

- org-html-publish-to-thml 
- org-latex-publish-to-pdf
- org-latex-publish-to-latex

WARN: This is a synchronous function and can freeze Emacs. Emacs will freeze while 
the exporting doesn't finish. Type C-g to abort the execution."
  (interactive)
  (let ((org-html-htmlize-output-type 'css)
        (org-html-htmlize-font-prefix "org-")
        (pub-plist (org-wiki-make-org-publish-plist org-exporter))
        )
    (org-publish pub-plist t)))


(defun org-wiki-export-html-sync ()
  "Export all pages to html in synchronous mode."
  (interactive)
  (let ((org-html-htmlize-output-type 'css)
        (org-html-htmlize-font-prefix "org-")
        (pub-plist (org-wiki-make-org-publish-plist 'org-html-publish-to-html))
        )
    (org-publish pub-plist t)))

(defun org-wiki-export-html ()
  "Export all pages to html.
Note: This function doesn't freeze Emacs since it starts another Emacs process."
  (interactive)
  (compile (mapconcat 'identity
                      `(,org-wiki-emacs-path
                        "--batch"
                        "-l" ,org-wiki-user-init-file
                        "-f" "org-wiki-export-html-sync"
                        "--kill"
                        )
                      " "
                      )))

(defun org-wiki-make-menu ()
  "Optional command to build an utility menu."
  (interactive)
  (easy-menu-define org-wik-menu global-map "Org-wiki"

    `("org-wiki"
      ("Main"
       ["Go to Index page \nM-x org-wiki-index" (org-wiki-index)]

       ["---" nil]
       ["Browsing" nil]
       ["Browse page \nM-x org-wiki-helm" (org-wiki-helm)]
       ["Browse page in other frame \nM-x org-wiki-helm-frame" (org-wiki-helm-frame)]
       ["Browse pages in read-only mode \nM-x org-wiki-helm-read-only" (org-wiki-helm-read-only)]
       ["---" nil]
       ["Wiki Directory" nil]
       ["Open org-wiki directory \nM-x org-wiki-dired" (org-wiki-dired)]
       ["Open org-wiki directory with system's file manager.\nM-x org-wiki-open" (org-wiki-open)]
       ["Close all pages \nM-x org-wiki-close" (org-wiki-close)]

       ["---" nil]
       ["Html export" nil]
       ["Open index page (html) in the browser \nM-x org-wiki-index-html" (org-wiki-index-html)]
       ["Export all pages to html \nM-x org-wiki-export-html" (org-wiki-export-html)]
       ["Help - Show all org-wiki commands \nM-x org-wiki-help" (org-wiki-help)]
       )
      ["---"  nil]
      ("Page Commands"
       ["Browse current page asset directory.\nM-x org-wiki-asset-dired"
        (org-wiki-asset-dired)]
       ["Browse current page asset directory with system's file manager.\nM-x org-wiki-asset-open"
        (org-wiki-asset-open)]

        ["Insert a link to a wiki page \nM-x org-wiki-insert" (org-wiki-insert)]
        ["Insert a link of type wiki-asset-sys at point.\nM-x org-wiki-asset-insert"
        (org-wiki-asset-insert)]
        ["Insert a link of type file:<page>/<asset> at point.\nM-x org-wiki-asset-insert-file"
         (org-wiki-asset-insert-file)
         ]
        ["Download an asset file and insert a wiki-asset-sys link at point.\nM-x org-wiki-asset-download-insert1"
         (org-wiki-asset-download-insert1)
         ]

        ["Download an asset file and insert a link at point of type file:<page>/<file.pdf>.\nM-x org-wiki-asset-download-insert2"
         (org-wiki-asset-download-insert2)]

        )
       ["---"  nil]
       ("Org-mode"
       ["Filter headings     \nM-x helm-org-in-buffer-headings" (helm-org-in-buffer-headings)]
       ["Hem occur           \nM-x helm-occur"                   (helm-occur)]
       ["Toggle Read only    \nM-x read-only-mode"               (read-only-mode 'toggle)]
       ["Toggle Images       \nM-x org-toggle-inline-images"     (org-toggle-inline-images)]
       ["Toggle Link display \nM-x org-toggle-link-display"      (org-toggle-link-display)]
      ))))


;;
;; Despite this function was implemented as a interface to 
;; Python3 simple http server, it can be refactored to work
;; with another more powerful http server such as Nginx.
;;
(defun org-wiki-server-toggle ()
  "Start/stop org-wiki http server. It requires Python3.
Note: This command requires Python3 installed."
  (interactive)
  (let (
        ;; Process name 
        (pname  "org-wiki-server")
        ;; Buffer name - Display process output (stdout)
        (bname   "*org-wiki-server*")
        ;; Set current directory to org-wiki repository.
        (default-directory org-wiki-location))

 
    (if (not (get-buffer bname))
        
        (progn

          (sit-for 0.1)
          (switch-to-buffer bname)

          
          (save-excursion ;; Save cursor position
            
           (insert "Server started ...\n\n")                               
           (message "\nServer started ...\n")

           ;; Show machine network cards' IP addresses.
           (cl-case system-type
                                        ;;; Linux
             (gnu/linux       (insert (shell-command-to-string "ifconfig")))
                                        ;;; Free BSD OS
             (gnu/kfreebsd    (insert (shell-command-to-string "ifconfig")))
                                        ;; Mac OSX - (Not tested )
             (darwin          (insert (shell-command-to-string "ifconfig")))
                                        ;; Windows 7, 8, 10 - Kernel NT
             (windows-nt      (insert (shell-command-to-string "ipconfig"))))          )
          
          (start-process pname
                         bname
                         "python3"
                         "-m"
                         "http.server"
                         "--bind"
                         org-wiki-server-host
                         org-wiki-server-port)                                                               
    
                
                (when (y-or-n-p "Open server in browser ?")
                  (browse-url (format "http://localhost:%s" org-wiki-server-port))))
      
        (progn  (switch-to-buffer bname)
                (kill-process (get-process pname))
                (message "Server stopped.")

                ))))



(defun org-wiki-paste-image ()  
  "Paste a image asking the user for the file name."
  (interactive)


  (let* ((dir   (file-name-as-directory
                   (file-name-base
                    (buffer-file-name))))
           (image-name (read-string "Image name: " )))
      
    (org-wiki--assets-make-dir dir)

    (insert "#+CAPTION: ")
    (save-excursion
      (insert image-name)
      (insert "\n")
      (insert
       (org-make-link-string
        (concat "file:"
                (string-trim
                 (shell-command-to-string
                  (mapconcat #'identity
                             `("java"
                               "-jar"
                               ;;,(expand-file-name "~/bin/Clip.jar")
                               ,(expand-file-name  org-wiki-clip-jar-path)
                               "--name"
                               ,(concat "\"" image-name "\"")
                               ,(concat "\"" dir "\"")
                               )
                             " "
                             )))))))))

(defun org-wiki-paste-image-uuid ()
  "Paste a image with automatic generated name (uuid)."
  (interactive)
  (let* ((dir   (file-name-base
                    (buffer-file-name))))
    
    (org-wiki--assets-make-dir dir)

    (insert "#+CAPTION: ")
    (save-excursion
      (insert "\n")
      (insert
       (org-make-link-string
        (concat "file:"
                  (string-trim
                   (shell-command-to-string
                    (mapconcat #'identity
                               `("java"
                                 "-jar"
                                        ;;,(expand-file-name "~/bin/Clip.jar")
                                 ,(expand-file-name  org-wiki-clip-jar-path)
                                 "--uuid"
                                 ,(concat "\"" dir "\""))

                               " "
                               )))))))))


;; Custom Minor Mode
(define-minor-mode org-wiki-panel-minor-mode
  "Ishell multi line mode"
  ;; The initial value - Set to 1 to enable by default
  nil
  ;; The indicator for the mode line.
  nil 
  ;; The minor mode keymap
  `(
    ;; Commands to Open Index page:
    (,(kbd "bii")        .  org-wiki-index)
    (,(kbd "bfi")        .  org-wiki-index-frame)
    (,(kbd "bbi")        .  org-wiki-index-html)

    ;; ==== Commands to browse pages ==========
    (,(kbd "hh")        .  org-wiki-helm )
    (,(kbd "hj")        .  org-wiki-switch )
    (,(kbd "hr")        .  org-wiki-helm-read-only)
    (,(kbd "hf")        .  org-wiki-helm-frame)
    (,(kbd "kk")        .  org-wiki-close)
    
    ;; ==== Commands to browse directories =====
    (,(kbd "dw")        .  org-wiki-dired )
    (,(kbd "do")        .  org-wiki-open)

    ;; ==== Install Menu =======================
    (,(kbd "smm")        .  org-wiki-make-menu)
    (,(kbd "see")        .  org-wiki-export-html)
    (,(kbd "sec")        . (lambda () (interactive) (customize-group "org-wiki")))
    (,(kbd "seg")        . (lambda () (interactive) (browse-url "http://www.github.com/caiorss/org-wiki")))

    ;; === Toggle commands =====================
    (,(kbd "tts")        .  org-wiki-server-toggle)
    ;; (,(kbd "ttl")        .  org-toggle-link-display)
    ;; (,(kbd "tti")        .  org-toggle-inline-images)

    (,(kbd "tty")        . (lambda () (interactive)
                             (tool-bar-mode 'toggle)
                             (menu-bar-mode 'toggle)
                             ))

    (,(kbd "ttb")        . (lambda () (interactive) (tool-bar-mode 'toggle)))
    (,(kbd "ttm")        . (lambda () (interactive) (menu-bar-mode 'toggle)))
    
    (,(kbd "q")         . (lambda () (interactive) (kill-buffer)))
    )
   ;; Make mode local to buffer rather than global 
   ;; :global t
)



(defun org-wiki-rgrep (pattern)
  "Search org-wiki with a regex pattern."
  (interactive "sSearch: ")
  (rgrep pattern "*.org" org-wiki-location nil))

(defun org-wiki-keywords ()
  "Display all org-wiki files with '#+KEYWORDS:' field."
  (interactive)
  (org-wiki-rgrep "^#+KEYWORDS:"))

(defun org-wiki-desc ()
  "Search all org-wiki files with '#+DESCRIPTION:' field."
  (interactive)
  (org-wiki-rgrep "^#+DESCRIPTION:"))

(defun org-wiki-find-dired ()
  "Show all org-wiki files in all subdirectories of org-wiki-location."
  (interactive)
  (find-dired org-wiki-location
              (mapconcat #'identity
                         '(
                           "-not -path '*/.git*'"         ;; Exclude .git Directory 
                           "-and -not -name '.#*'"        ;; Exclude temporary files starting with #
                           "-and -not -name '#*'"
                           "-and -not -name '*#'"
                           "-and -not -name '*~' "        ;; Exclude ending with ~ (tilde)
                           "-and -not -name '*.html' "    ;; Exclude html files 
                           )
                         
                         " "
                         )))

(defun org-wiki-website ()
  "Open org-wiki github repository."
  (interactive)
  (browse-url "http://www.github.com/caiorss/org-wiki"))

(defun org-wiki-header ()
  "Insert a header at the top of the file."
  (interactive)
  ;; Save current cursor location and restore it
  ;; after completion of block insider save-excursion.
  (save-excursion
    (let*
        ;; replace '%n' by page title
        ((text1 (replace-regexp-in-string
                 "%n"
                 (file-name-base (buffer-file-name))
                 org-wiki-template))
         ;; Replace %d by current date in the format %Y-%m-%d
         (text2 (replace-regexp-in-string
                 "%d"
                 (format-time-string "%Y-%m-%d")
                 text1
                 )))
      ;; Got to top of file
      (goto-char (point-min))
      (insert text2))))

(defun org-wiki-panel ()
  "Create a command panel for org-wiki."
  (interactive)
  (let ((buf (get-buffer-create "*org-wiki-panel*")))
    (switch-to-buffer buf)
    (kill-region (point-min) (point-max))
    (org-wiki-panel-minor-mode)
    (insert
     "                       Org-wiki command panel
Open Index Page

  [bii] - Go to index.                           - M-x org-wiki-index
  [bfi] - Go to index in a new frame.            - M-x org-wiki-index-frame
  [bbi] - Open index page in browser             - M-x org-wiki-index-html

Browse Pages

  [kk]  - Close all wiki buffers                 - M-x org-wiki-close
  [hh]  - Open a page.                           - M-x org-wiki-helm
  [hj]  - Switch between org-wiki buffers        - M-x org-wiki-switch
  [hr]  - Open a page in read-only mode.         - M-x org-wiki-helm-read-only
  [hf]  - Open a page in a new frame.            - M-x org-wiki-helm-frame


Open Directory

  [dw]  - Open wiki directory in dired buffer    - M-x org-wiki-index
  [do]  - Open wiki directory in file manager    - M-x org-wiki-open

Special Commands

  [q ]  - Quit.
  [sec] - Customize org-wiki                      - M-x customize-group org-wiki
  [see] - Export to all pages to html             - M-x org-wiki-export-html
  [smm] - Install org-wiki menu.                  - M-x org-wiki-make-menu
  [seg] - Go to org-wiki web site

Toggle

  [tts] - Toggle org-wiki-server                  - M-x org-wiki-server-toggle

  [tty] - Toggle Emacs toolbar and menu
  [ttb] - Toggle Emacs toolbar
  [ttm] - Toggle Emacs menu bar
"
     ))
  (read-only-mode))

;; =========== Copy Path Commands ============= ;;


(defun org-wiki-copy-location ()
  "Copy org-wiki location path to clipboard."
  (interactive)
  (let ((msg (expand-file-name org-wiki-location)))
   (with-temp-buffer
     (insert msg)
     (message (format "Copied to clipboard: %s" msg))
     (clipboard-kill-region (point-min) (point-max)))))


(defun org-wiki-copy-index-html ()
  "Copy org-wiki html index page to clipboard."
  (interactive)
  (let ((msg (expand-file-name (concat (file-name-as-directory org-wiki-location)
                                       "index.html"  ))))
   (with-temp-buffer
     (insert msg)
     (message (format "Copied to clipboard: %s" msg))
     (clipboard-kill-region (point-min) (point-max)))))


(defun org-wiki-copy-asset-path ()
  "Copy current page's asset directory to clipboard."
  (interactive)
  (let ((msg (expand-file-name (concat (file-name-as-directory org-wiki-location)
                                       (file-name-base (buffer-file-name))))))
   (with-temp-buffer
     (insert msg)
     (message (format "Copied to clipboard: %s" msg))
     (clipboard-kill-region (point-min) (point-max)))))


;; ============ Backup Commands =============== ;;


(defun org-wiki-backup-make ()
  "Make a org-wiki backup."
  (interactive)
  (let* ((zipfile            (concat "org-wiki-" (format-time-string "%Y-%m-%d") ".zip"))
         (destfile           (concat (file-name-directory org-wiki-backup-location) zipfile))
         (default-directory  org-wiki-location))
    (switch-to-buffer "*org-wiki-backup*")

    ;; Crate org-wiki backup location directory if doesn't exist.
    (if (not (file-exists-p org-wiki-backup-location))
        (make-directory org-wiki-backup-location t))

    (if (file-exists-p destfile) (delete-file destfile))
    (if (file-exists-p zipfile)  (delete-file zipfile))

    ;; Clear buffer removing all lines
    (delete-region (point-min) (point-max))
    (set-process-sentinel
     (start-process
      "org-wiki-backup" ;; Process name
      "*org-wiki-backup*" ;; Buffer where output is displayed.
      ;; Shell command
      "zip" "-r" "-9" zipfile ".")
     (lexical-let ((zipfile  zipfile)
                   (destfile destfile))
       (lambda (process state)
         (when (equal (process-exit-status process) 0)
           (switch-to-buffer "*org-wiki-backup*")
           (rename-file zipfile org-wiki-backup-location t)
           (message "Backup done. Ok.")
           (insert  "\nBackup done. Ok. Run M-x org-wiki-backup-dir to open backup directory.")
            ))))))

(defun org-wiki-backup-dir ()
    "Open org-wiki backup directory in dired mode."
    (interactive)
    ;; Create org-wiki backup location directory if doesn't exist.
    (if (not (file-exists-p org-wiki-backup-location))
        (make-directory org-wiki-backup-location t))
    ;; Open backup location
    (dired org-wiki-backup-location)
    ;; Update buffer
    (revert-buffer))

;; ============ Command Alias ================= ;;


(defun org-wiki-nav ()
  "Navigate through org-mode headings. Alias to helm-org-in-buffer-headings."
  (interactive)
  (helm-org-in-buffer-headings))

(defun org-wiki-occur ()
  "Search current buffer with helm-occur. Alias to helm-occur."
  (interactive)
  (helm-occur))

(defun org-wiki-toggle-images ()
  "Toggle inline images. Alias to M-x org-toggle-inline-images."
  (interactive)
  (org-toggle-inline-images))

(defun org-wiki-toggle-link ()
  "Toggle link display. Alias to M-x org-toggle-link-display"
  (interactive)
  (org-toggle-link-display))

(defun org-wiki-latex ()
  "Display latex formulas. Alias to M-x org-preview-latex-fragment"
  (interactive)
  (org-preview-latex-fragment))

(provide 'org-wiki)
;;; org-wiki.el ends here
