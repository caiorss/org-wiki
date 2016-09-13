;;
;;  Org-mode Wiki 
;;
;;
;; Dependencies:
;; -------------------------------------------
;;
;;   Emacs packages:
;;
;;       - helm 
;;
;;   External applications:
;;
;;       - xclip  - Necessary to paste images to Emacs 
;; 

(require 'cl)
(require 'ox-html)
(require 'helm)

; (require 'cl-lib)

;; Defines where the Wiki files is located
;;
(defvar org-wiki/location "~/org/wiki"
  "Org wiki directory location. Default value ~/org/wiki"
  )


(setq org-wiki/index-file-basename "Index")

;; ------- Internal functions ------------ ;; 

(defun org-wiki/concat-path (base relpath)
  
  "Concat directory path and a relative path"
  
  (concat (file-name-as-directory base) relpath))

(defun org-wiki/unique (xs)
  "Removes repeated elements from a list. 
 
  Example:
  
  > (unique '(x y a b 21 21 10 21 x y a ))
  (x y a b 21 10)
  "
  (let
    ((result nil))   

    (dolist (x xs)
      (if (not (member x result))
          (push x result)         
        ))
    (reverse result)
    ))


(defun org-wiki/normalize-path (path)

                   (replace-regexp-in-string
                    "//"
                    "/"   
                    (replace-regexp-in-string "/$" "" (expand-file-name path))
                    )


                   )
       
(defun  org-wiki/path-equal (p1 p2)
  (equal (org-wiki/normalize-path p1) (org-wiki/normalize-path p2))
)


(defun org-wiki/file->page (filename)
  "  Get a wiki page name from a file name.
   
  Example:

   ELISP> (file->org-wiki/page  \"Spanish.org\")
   \"Spanish\"

  "
  (car (split-string filename "\\."))

  )

(defun org-wiki/replace-extension (filename extension)
  " Replaces a file name by its extension 
   Example:  
  
  ELISP> (org-wiki/replace-extension \"file.org\" \"html\" )
  \"file.html\"

 "  
  (concat (car (split-string filename "\\."))
          "."
          extension
          ))


(defun org-wiki/page->file (pagename)
  "Get the corresponding wiki file (*.org) to the wiki page name.

  Example: 

  ELISP> (org-wiki/page->file \"Linux\")
  \"~/org/wiki/Linux.org\"

  "

  (concat (file-name-as-directory org-wiki/location)
          pagename
          ".org"
          ))

(defun org-wiki/list-pages ()
  
  "Returns a list containing all pages files *.org 
  "
  
  (directory-list org-wiki/location))


(defun org-wiki/page->html-file (pagename)
  "Converts a wiki page name to html file name."
  (concat (file-name-as-directory (expand-file-name org-wiki/location))
          pagename
          ".html"
          ))



(defun org-wiki/page-files (&optional abspath)
  " Return a list containing all files in the wiki directory 
 
  (org-wiki/page-files &optional abspath)
  
  if abspath is null returns relative path, otherwise returns the 
  absolute path. 

  Example:
 
  ELISP> (remove-if-not #'file->org-wiki/page (org-wiki/page-files))
  (\"Abreviations_Slangs.wiki.org\" \"Android.wiki.org\" \"Bash_Script.wiki.org\")

  "

  (remove-if-not
   
   (lambda (s)

     (let (
           (b (file-name-base s))
           )

     (not (or 
           (string-prefix-p ".#" b)
           (string-suffix-p "~"  b )
           (string-prefix-p "#" b)
           (string-suffix-p "#" b)

           ))))

   (directory-files org-wiki/location abspath ".org")))



(defun org-wiki/open-page (pagename)
  (find-file  (org-wiki/page->file pagename)))

(defun org-wiki/page-list ()
  (mapcar #'org-wiki/file->page (org-wiki/page-files)))



(defun org-wiki/get-page (wikipage)
  (org-wiki/concat-path org-wiki/location
                    (replace-regexp-in-string "\s" "_"
                    (replace-regexp-in-string "%20" "_"
                     (concat wikipage ".org")))))



(defun org-wiki/export-to-html (filename)
  "Export org-mode file to html without open a buffer"
 

  (with-temp-buffer 


    (let (
          (output-file  (concat (file-name-directory filename)
                                (concat (file-name-base filename) ".html")
                                ))

          (org-export-show-temporary-export-buffer  nil)

          (default-directory   (file-name-directory filename))
	  
          )


      (insert-file filename)

      (message (concat "Exporting " filename))
      
	  (org-html-export-as-html)
      
	  (with-current-buffer "*Org HTML Export*"

	    (if (file-exists-p output-file)
            (delete-file output-file))
	    
	    (append-to-file  (buffer-substring-no-properties (point-min) (point-max))
                         nil
                         output-file
			       )
	    

      )

    (kill-buffer "*Org HTML Export*")
    )))




(defun org-wiki/export-to-html2 (filename)
  (let*
      ;; Open a file without swith to the buffer and returns a buffer
      ((buf   (find-file-noselect filename)))

    (princ (format "Exporting : %s" filename))

    (with-current-buffer  buf
      (ignore-errors (org-html-export-to-html)))

    ;; Kill the buffer after export it to html
    (kill-buffer buf)
    (princ (format "Exported %s to html" filename))
    ))

(defun org-wiki/export-html-sync ()
  "Export all wiki pages to html"
  (interactive)
  (mapc #'org-wiki/export-to-html (org-wiki/page-files t)))

(defun org-wiki/export-html ()
  (interactive)
  
  (mapc #'org-wiki/export-to-html2
        (org-wiki/page-files t)))   

(defun  org-wiki/export-html-async ()
   "Export all wiki files to html launching an asynchronous
    Emacs Process.
    "
  (interactive)
  (set-process-sentinel
   (start-process "wiki-export"
                  "*wiki-export*"
                  "emacs"
                  "--batch"
                  "-l"
                  "~/.emacs.d/init.el"                  
                  "-f"
                  "org-wiki/export-html"
                  "--kill")
   (lambda (p e)
     (when (= 0 (process-exit-status p))
       (message "Wiki exported to html Ok."))))
    ;; End of set-process-sentinel

  (message "Exporting wiki to html")
  )



;;;------ Org-mode custom protocol ------------- ;;


(defun org-wiki/org-link (path desc backend)
   (cl-case backend
     (html (format
            "<a href='%s.html'>%s</a>"
            path
            (or desc path)))))

(defun org-wiki/make-link (org-wiki/page)
  "returns a string containing a wiki link 'wiki:<name of page>"
  (format "[[wiki:%s][%s]]" org-wiki/page org-wiki/page)  
  )


 ;;; Custom Protocols
(add-hook 'org-mode-hook
          (lambda ()    
            
            (org-add-link-type  "wiki"  #'org-wiki/open-page  #'org-wiki/org-link )
            ))
            

;; ---------------- User Commands ---------------- ;;

;; (defun org-wiki/dired ()
;;   "Open wiki directory in dired mode"

;;   (dired org-wiki/location))


(defun org-wiki/index ()
  "Open the index page: <org-wiki/location>/index.org. 

   The file index.org is created if it doesn't exist. 
   
  "
  (interactive)
  (org-wiki/open-page org-wiki/index-file-basename)

  )

(defun org-wiki/html ()  
  "Open the Wiki (Index) in the default web browser."  

  (interactive)  
  (browse-url (concat "file://" (org-wiki/page->html-file org-wiki/index-file-basename)))
)

(defun org-wiki/index-frame ()
  "Open the index page in a new frame."
  (interactive)
  
  (with-selected-frame (make-frame)
    (org-wiki/index)
    ))


(defun org-wiki/dired-all ()
  "Open the wiki directory in dired-mode showing all files.

   Usage: M-x org-wiki/dired-all 
  "

  (interactive)
  (dired org-wiki/location)
  (dired-hide-details-mode)
  )

(defun org-wiki/dired ()
  "Open the wiki directory showing only the wiki pages

   Usage: M-x org-wiki/dired 
   "
  
  (interactive)
  (dired (org-wiki/concat-path org-wiki/location "*.org"))
  (dired-hide-details-mode)
  )


(defun org-wiki/make-page ()
  "Creates a new wiki page."
  (interactive)
  (find-file (org-wiki/page->file (read-string "Page Name: "))))



(defun org-wiki/helm ()
  " Browser the wiki files using helm.

  Usage: M-x org-wiki/helm 
  "
  (interactive)  
    (helm :sources `((
                      (name . "Wiki Pages")

                      (candidates . ,(org-wiki/unique (org-wiki/page-list)))

                      (action . org-wiki/open-page)                    
                      ))))


(defun org-wiki/helm-frame ()
  " Browser the wiki files using helm and opens it in a new frame 
  
  Usage: M-x org-wiki/helm-frame 
  "
  (interactive)

  (helm :sources `((
                      (name . "Wiki Pages")

                      (candidates . ,(org-wiki/page-list))

                      (action .

                              (lambda (act)
                                (with-selected-frame (make-frame)
                                  (org-wiki/open-page act)
                                )
                              )                    
                              )))))


;;  @TODO: Implement org-wiki/helm-html 
;;
(defun org-wiki/helm-html ()
  " Browser the wiki files using helm.

  Usage: M-x org-wiki/helm 
  "
  (interactive)  
    (helm :sources `((
                      (name . "Wiki Pages")

                      (candidates . ,(org-wiki/unique (org-wiki/page-list)))

                      (action . org-wiki/open-page)                    
                      ))))


(defun org-wiki/close ()

  "Close all opened wiki pages buffer and save them"

  (interactive)
  
  (mapcar (lambda (b) 

            (when (and (buffer-file-name b) ;; test if is a buffer associated with file 

                     (org-wiki/path-equal  org-wiki/location
                                           (file-name-directory (buffer-file-name b)))
                )

                (save-buffer b)
                (kill-buffer b)

              ))


          (buffer-list)

          )

  (message "All wiki files closed. Ok.")
  )


(defun org-wiki/insert ()
    
  " Inserts a Wiki Page link at point.

  Usage: M-x org-wiki/insert 
 
  "
  (interactive)
  
    (helm :sources `((
                      (name       . "Wiki Pages")

                      (candidates . ,(org-wiki/unique (org-wiki/page-list)))

                      (action     .  (lambda (page) (insert (org-wiki/make-link page))))
                      
                      ))))



(defun org-wiki/html-page ()
  " Open the current wiki page in the browser. It is created if it doesn't exist yet.   
  "
  (interactive)

  (let ((html-file   (org-wiki/replace-extension (buffer-file-name) "html")))

    (if (not (file-exists-p html-file))
        (org-html-export-to-html)
        )

  (browse-url html-file)
  ))

(defun org-wiki/html-page2 ()
  " Exports the current wiki page to html and opens it in the browser.

  "
  (interactive)
  (org-html-export-to-html)
  (browse-url (org-wiki/replace-extension (buffer-file-name) "html"))   
  )




(provide 'org-wiki)
