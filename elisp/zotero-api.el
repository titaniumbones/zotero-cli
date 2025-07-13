;;; zotero-api.el --- Zotero Local API client for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Matt Price
;; Keywords: zotero, research, bibliography
;; Version: 0.5.0
;; Package-Requires: ((emacs "25.1") (request "0.3.2") (json "1.4"))

;;; Commentary:

;; This package provides functions to interact with Zotero's local API
;; for retrieving items, collections, and annotations from your Zotero library.
;; 
;; Key features:
;; - Retrieve items and collections from personal and group libraries
;; - Extract annotations from PDF attachments
;; - Format annotations as org-mode text
;; - Generate Zotero links with page numbers
;;
;; Usage:
;; (require 'zotero-api)
;; (zotero-get-items)
;; (zotero-get-annotations-for-item "ITEM_ID")

;;; Code:

(require 'request)
(require 'json)
(require 'url-util)

;;; Custom variables

(defgroup zotero nil
  "Zotero API integration for Emacs."
  :group 'external
  :prefix "zotero-")

(defcustom zotero-base-url "http://localhost:23119"
  "Base URL for Zotero local API."
  :type 'string
  :group 'zotero)

(defcustom zotero-default-user-id "0"
  "Default user ID for Zotero API calls (0 = current logged-in user)."
  :type 'string
  :group 'zotero)

;;; Core API functions

(defun zotero--make-request (endpoint &optional callback)
  "Make a GET request to Zotero API ENDPOINT.
If CALLBACK is provided, make asynchronous request, otherwise synchronous.
Returns parsed JSON response or nil on error."
  (let ((url (concat zotero-base-url "/api" endpoint)))
    (if callback
        ;; Asynchronous request
        (request url
          :type "GET"
          :parser 'json-read
          :success (cl-function
                    (lambda (&key data &allow-other-keys)
                      (funcall callback data)))
          :error (cl-function
                  (lambda (&key error-thrown &allow-other-keys)
                    (message "Zotero API error: %s" error-thrown)
                    (funcall callback nil))))
      ;; Synchronous request
      (condition-case err
          (with-current-buffer
              (url-retrieve-synchronously url t nil 10)
            (goto-char (point-min))
            (when (re-search-forward "^HTTP/[0-9\\.]+ 200" nil t)
              (re-search-forward "^$" nil t)
              (json-read)))
        (error
         (message "Zotero API request failed: %s" (error-message-string err))
         nil)))))

(defun zotero-get-item (item-id &optional library-id)
  "Get a single item by ITEM-ID.
If LIBRARY-ID is provided, get from group library, otherwise from personal library."
  (let ((endpoint (if library-id
                      (format "/groups/%s/items/%s" library-id item-id)
                    (format "/users/%s/items/%s" zotero-default-user-id item-id))))
    (zotero--make-request endpoint)))

(defun zotero-get-item-children (item-id &optional library-id)
  "Get all children of an item (attachments, notes, etc.) by ITEM-ID.
If LIBRARY-ID is provided, get from group library, otherwise from personal library."
  (let ((endpoint (if library-id
                      (format "/groups/%s/items/%s/children" library-id item-id)
                    (format "/users/%s/items/%s/children" zotero-default-user-id item-id))))
    (let ((response (zotero--make-request endpoint)))
      (if (vectorp response) (append response nil) response))))

(defun zotero-get-items (&optional library-id limit item-type)
  "Get items from a library.
LIBRARY-ID: Library/group ID (nil for personal library)
LIMIT: Maximum number of items to return (default 25)
ITEM-TYPE: Filter by item type (e.g., 'book', 'journalArticle')"
  (let* ((limit (or limit 25))
         (params (format "?limit=%d" limit))
         (params (if item-type
                     (concat params "&itemType=" item-type)
                   params))
         (endpoint (if library-id
                       (format "/groups/%s/items%s" library-id params)
                     (format "/users/%s/items%s" zotero-default-user-id params))))
    (let ((response (zotero--make-request endpoint)))
      (if (vectorp response) (append response nil) response))))

(defun zotero-get-top-level-items (&optional library-id limit)
  "Get top-level items (excluding child items like attachments).
LIBRARY-ID: Library/group ID (nil for personal library)
LIMIT: Maximum number of items to return (default 25)"
  (let* ((limit (or limit 25))
         (params (format "?limit=%d&top=1" limit))
         (endpoint (if library-id
                       (format "/groups/%s/items%s" library-id params)
                     (format "/users/%s/items%s" zotero-default-user-id params))))
    (let ((response (zotero--make-request endpoint)))
      (if (vectorp response) (append response nil) response))))

(defun zotero-get-collections (&optional library-id)
  "Get all collections from a library.
LIBRARY-ID: Library/group ID (nil for personal library)"
  (let ((endpoint (if library-id
                      (format "/groups/%s/collections" library-id)
                    (format "/users/%s/collections" zotero-default-user-id))))
    (let ((response (zotero--make-request endpoint)))
      (if (vectorp response) (append response nil) response))))

(defun zotero-get-libraries ()
  "Get all libraries (groups) available in Zotero."
  (let ((endpoint (format "/users/%s/groups" zotero-default-user-id)))
    (let ((response (zotero--make-request endpoint)))
      (if (vectorp response) (append response nil) response))))

(defun zotero-get-library-info (library-id)
  "Get detailed information about a specific library/group by LIBRARY-ID."
  (let ((endpoint (format "/groups/%s" library-id)))
    (zotero--make-request endpoint)))

(defun zotero-get-item-types ()
  "Get all available item types."
  (let ((response (zotero--make-request "/itemTypes")))
    (if (vectorp response) (append response nil) response)))

;;; PDF and annotation functions

(defun zotero-get-pdf-attachments (item-id &optional library-id)
  "Get all PDF attachments for a given item by ITEM-ID.
LIBRARY-ID: Library/group ID (nil for personal library)"
  (let* ((children (zotero-get-item-children item-id library-id))
         (pdf-attachments '()))
    (dolist (child children)
      (let* ((data (cdr (assq 'data child)))
             (item-type (cdr (assq 'itemType data)))
             (content-type (cdr (assq 'contentType data))))
        (when (and (string= item-type "attachment")
                   (string= content-type "application/pdf"))
          (push child pdf-attachments))))
    (nreverse pdf-attachments)))

(defun zotero-get-attachment-annotations (attachment-id &optional library-id)
  "Get all annotations for a PDF attachment by ATTACHMENT-ID.
LIBRARY-ID: Library/group ID (nil for personal library)"
  (let ((annotations '()))
    ;; First try the standard approach - get children of the attachment
    (let* ((endpoint (if library-id
                         (format "/groups/%s/items/%s/children" library-id attachment-id)
                       (format "/users/%s/items/%s/children" zotero-default-user-id attachment-id)))
           (response (zotero--make-request endpoint)))
      (when (vectorp response)
        (dolist (item (append response nil))
          (let* ((data (cdr (assq 'data item)))
                 (item-type (cdr (assq 'itemType data))))
            (when (string= item-type "annotation")
              (push item annotations))))))
    
    ;; If no annotations found as children, try alternative approach:
    ;; Look for annotation items where parentItem matches our attachment-id
    (when (null annotations)
      (let ((annotation-items (zotero-get-items library-id 1000 "annotation")))
        (dolist (item annotation-items)
          (let* ((data (cdr (assq 'data item)))
                 (parent-item (cdr (assq 'parentItem data))))
            (when (string= parent-item attachment-id)
              (push item annotations))))))
    
    (nreverse annotations)))

(defun zotero-get-all-annotations-for-item (item-id &optional library-id)
  "Get all annotations from all PDF attachments for a given item by ITEM-ID.
LIBRARY-ID: Library/group ID (nil for personal library)
Returns an alist with item info and all annotations organized by attachment."
  (let* ((item (zotero-get-item item-id library-id))
         (result '()))
    
    (if (not item)
        (list (cons 'error (format "Item %s not found" item-id)))
      
      (let* ((item-data (cdr (assq 'data item)))
             (item-title (or (cdr (assq 'title item-data)) "Unknown"))
             (item-type (or (cdr (assq 'itemType item-data)) "Unknown"))
             (pdf-attachments (zotero-get-pdf-attachments item-id library-id))
             (attachments '()))
        
        (dolist (attachment pdf-attachments)
          (let* ((attachment-id (cdr (assq 'key attachment)))
                 (attachment-data (cdr (assq 'data attachment)))
                 (attachment-title (or (cdr (assq 'title attachment-data)) "Unknown"))
                 (filename (or (cdr (assq 'filename attachment-data)) "Unknown"))
                 (annotations (zotero-get-attachment-annotations attachment-id library-id)))
            
            (push (list (cons 'attachment-id attachment-id)
                        (cons 'attachment-title attachment-title)
                        (cons 'filename filename)
                        (cons 'annotations-count (length annotations))
                        (cons 'annotations annotations))
                  attachments)))
        
        (list (cons 'item-id item-id)
              (cons 'item-title item-title)
              (cons 'item-type item-type)
              (cons 'attachments (nreverse attachments)))))))

;;; Org-mode formatting

(defun zotero-format-as-org-mode (annotations-data)
  "Format ANNOTATIONS-DATA as org-mode text.
ANNOTATIONS-DATA should be the result from `zotero-get-all-annotations-for-item'."
  (let ((error-msg (cdr (assq 'error annotations-data))))
    (if error-msg
        (format "# Error: %s\n" error-msg)
      
      (let* ((item-title (cdr (assq 'item-title annotations-data)))
             (item-type (cdr (assq 'item-type annotations-data)))
             (item-id (cdr (assq 'item-id annotations-data)))
             (attachments (cdr (assq 'attachments annotations-data)))
             (org-content '()))
        
        ;; Main header with item info
        (push (format "* %s" item-title) org-content)
        (push "  :PROPERTIES:" org-content)
        (push (format "  :ITEM_TYPE: %s" item-type) org-content)
        (push (format "  :ZOTERO_KEY: %s" item-id) org-content)
        (push "  :END:" org-content)
        (push "" org-content)
        
        ;; Process each PDF attachment
        (dolist (attachment attachments)
          (let* ((attachment-title (cdr (assq 'attachment-title attachment)))
                 (attachment-id (cdr (assq 'attachment-id attachment)))
                 (filename (cdr (assq 'filename attachment)))
                 (annotations (cdr (assq 'annotations attachment))))
            
            ;; PDF header
            (push (format "** %s" attachment-title) org-content)
            (push "   :PROPERTIES:" org-content)
            (push (format "   :ATTACHMENT_ID: %s" attachment-id) org-content)
            (push (format "   :FILENAME: %s" filename) org-content)
            (push "   :END:" org-content)
            (push "" org-content)
            
            (if (null annotations)
                (progn
                  (push "   No annotations found." org-content)
                  (push "" org-content))
              
              ;; Process each annotation
              (dolist (annotation annotations)
                (let* ((ann-data (cdr (assq 'data annotation)))
                       (ann-type (or (cdr (assq 'annotationType ann-data)) "unknown"))
                       (text (cdr (assq 'annotationText ann-data)))
                       (comment (cdr (assq 'annotationComment ann-data)))
                       (page-label (cdr (assq 'annotationPageLabel ann-data)))
                       (position (cdr (assq 'annotationPosition ann-data)))
                       (page-index (when (and position (listp position))
                                     (cdr (assq 'pageIndex position))))
                       ;; Create Zotero link
                       (zotero-link (concat "zotero://select/library/items/" attachment-id))
                       (page-info ""))
                  
                  ;; Add page parameter to link if available
                  (cond
                   (page-label
                    (setq zotero-link (concat zotero-link "?page=" page-label))
                    (setq page-info (format " (p. %s)" page-label)))
                   ((and page-index (numberp page-index))
                    (setq zotero-link (concat zotero-link "?page=" (number-to-string (1+ page-index))))
                    (setq page-info (format " (p. %d)" (1+ page-index)))))
                  
                  ;; Annotation header
                  (push (format "*** %s%s" (capitalize ann-type) page-info) org-content)
                  
                  ;; Annotation text
                  (when (and text (not (string-empty-p text)))
                    (push "#+BEGIN_QUOTE" org-content)
                    (push text org-content)
                    (push "#+END_QUOTE" org-content)
                    (push "" org-content))
                  
                  ;; Annotation comment
                  (when (and comment (not (string-empty-p comment)))
                    (push (format "*Comment:* %s" comment) org-content)
                    (push "" org-content))
                  
                  ;; Zotero link
                  (push (format "[[%s][Open in Zotero]]" zotero-link) org-content)
                  (push "" org-content))))))
        
        (mapconcat 'identity (nreverse org-content) "\n")))))

;;; Interactive functions

;;;###autoload
(defun zotero-insert-item-annotations (item-id)
  "Insert annotations for ITEM-ID at point in org-mode format."
  (interactive "sZotero Item ID: ")
  (let* ((annotations-data (zotero-get-all-annotations-for-item item-id))
         (org-content (zotero-format-as-org-mode annotations-data)))
    (insert org-content)))

;;;###autoload
(defun zotero-save-item-annotations-to-file (item-id filename)
  "Save annotations for ITEM-ID to FILENAME in org-mode format."
  (interactive "sZotero Item ID: \nFSave to file: ")
  (let* ((annotations-data (zotero-get-all-annotations-for-item item-id))
         (org-content (zotero-format-as-org-mode annotations-data)))
    (with-temp-file filename
      (insert org-content))
    (message "Annotations saved to %s" filename)))

;;;###autoload
(defun zotero-browse-libraries ()
  "Browse Zotero libraries and display basic information."
  (interactive)
  (let ((libraries (zotero-get-libraries)))
    (with-output-to-temp-buffer "*Zotero Libraries*"
      (princ (format "Found %d libraries:\n\n" (length libraries)))
      (dolist (library libraries)
        (let* ((id (cdr (assq 'id library)))
               (name (cdr (assq 'name library)))
               (type (cdr (assq 'type library))))
          (princ (format "ID: %s\nName: %s\nType: %s\n\n" id name type)))))))

;;;###autoload
(defun zotero-browse-items (&optional library-id limit)
  "Browse Zotero items and display basic information.
LIBRARY-ID: Library/group ID (nil for personal library)
LIMIT: Maximum number of items to show (default 25)"
  (interactive)
  (let* ((limit (or limit 25))
         (items (zotero-get-top-level-items library-id limit)))
    (with-output-to-temp-buffer "*Zotero Items*"
      (princ (format "Found %d items:\n\n" (length items)))
      (dolist (item items)
        (let* ((key (cdr (assq 'key item)))
               (data (cdr (assq 'data item)))
               (title (or (cdr (assq 'title data)) "Unknown"))
               (item-type (or (cdr (assq 'itemType data)) "Unknown")))
          (princ (format "Key: %s\nTitle: %s\nType: %s\n\n" key title item-type)))))))

(defun zotero-export-item-bibtex (item-id &optional library-id)
  "Export a single item as BibTeX.
ITEM-ID is the Zotero item ID.
LIBRARY-ID is optional library ID for group libraries.
Returns BibTeX string or nil if export failed."
  (let* ((endpoint (if library-id
                       (format "/groups/%s/items/%s" library-id item-id)
                     (format "/users/%s/items/%s" zotero-default-user-id item-id)))
         (url (format "%s%s?format=bibtex" zotero-base-url endpoint)))
    (condition-case err
        (let ((response (request url
                          :type "GET"
                          :sync t
                          :timeout 10)))
          (when (eq (request-response-status-code response) 200)
            (string-trim (request-response-data response))))
      (error nil))))

(provide 'zotero-api)

;;; zotero-api.el ends here
