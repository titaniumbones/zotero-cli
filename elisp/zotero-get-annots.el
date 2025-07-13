;;; zotero-get-annots.el --- Command-line interface for Zotero annotation retrieval -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: Claude
;; Keywords: zotero, research, bibliography, annotations
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.1") (zotero-api "1.0.0"))

;;; Commentary:

;; This package provides a command-line interface for retrieving annotations
;; from Zotero items, similar to the Python get-annots.py script.
;;
;; Usage from command line:
;; emacs --batch -l zotero-get-annots.el --eval "(zotero-cli-get-annotations \"ITEM_ID\")"
;; emacs --batch -l zotero-get-annots.el --eval "(zotero-cli-get-annotations \"ITEM_ID\" t)"
;;
;; Usage interactively:
;; M-x zotero-get-annotations-for-item

;;; Code:

(require 'zotero-api)
(require 'json)

(defun zotero-cli-print-annotations-summary (annotations-data)
  "Print a summary of ANNOTATIONS-DATA to stdout."
  (let ((error-msg (cdr (assq 'error annotations-data))))
    (if error-msg
        (message "Error: %s" error-msg)
      
      (let* ((item-title (cdr (assq 'item-title annotations-data)))
             (item-type (cdr (assq 'item-type annotations-data)))
             (item-id (cdr (assq 'item-id annotations-data)))
             (attachments (cdr (assq 'attachments annotations-data)))
             (total-annotations 0))
        
        (message "Item: %s (%s)" item-title item-type)
        (message "PDF Attachments: %d" (length attachments))
        
        ;; Count total annotations
        (dolist (attachment attachments)
          (let ((count (cdr (assq 'annotations-count attachment))))
            (setq total-annotations (+ total-annotations count))))
        
        (message "Total Annotations: %d" total-annotations)
        (message "")
        
        ;; Print detailed results
        (let ((attachment-num 1))
          (dolist (attachment attachments)
            (let* ((attachment-title (cdr (assq 'attachment-title attachment)))
                   (filename (cdr (assq 'filename attachment)))
                   (annotations (cdr (assq 'annotations attachment))))
              
              (message "--- Attachment %d: %s ---" attachment-num attachment-title)
              (message "File: %s" filename)
              (message "Annotations: %d" (length annotations))
              
              (let ((ann-num 1))
                (dolist (annotation annotations)
                  (let* ((ann-data (cdr (assq 'data annotation)))
                         (ann-type (or (cdr (assq 'annotationType ann-data)) "unknown"))
                         (text (cdr (assq 'annotationText ann-data)))
                         (comment (cdr (assq 'annotationComment ann-data))))
                    
                    (message "  %d. Type: %s" ann-num ann-type)
                    (when (and text (not (string-empty-p text)))
                      (let ((truncated-text (if (> (length text) 100)
                                                (concat (substring text 0 100) "...")
                                              text)))
                        (message "     Text: %s" truncated-text)))
                    (when (and comment (not (string-empty-p comment)))
                      (let ((truncated-comment (if (> (length comment) 100)
                                                   (concat (substring comment 0 100) "...")
                                                 comment)))
                        (message "     Comment: %s" truncated-comment)))
                    (setq ann-num (1+ ann-num))))
              
              (setq attachment-num (1+ attachment-num))))))))))

(defun zotero-cli-save-json (annotations-data item-id)
  "Save ANNOTATIONS-DATA to a JSON file for ITEM-ID."
  (let ((filename (format "annotations_%s.json" item-id)))
    (with-temp-file filename
      (insert (json-encode annotations-data)))
    (message "Full results saved to: %s" filename)))

(defun zotero-cli-save-org (annotations-data item-id)
  "Save ANNOTATIONS-DATA to an org file for ITEM-ID."
  (let* ((filename (format "annotations_%s.org" item-id))
         (org-content (zotero-format-as-org-mode annotations-data)))
    (with-temp-file filename
      (insert org-content))
    (message "Org-mode output saved to: %s" filename)
    (message "")
    (message "%s" (make-string 50 ?=))
    (message "ORG-MODE OUTPUT:")
    (message "%s" (make-string 50 ?=))
    (message "%s" org-content)))

;;;###autoload
(defun zotero-cli-get-annotations (item-id &optional org-mode)
  "Retrieve annotations for ITEM-ID and output results.
If ORG-MODE is non-nil, output in org-mode format, otherwise JSON."
  (message "Retrieving annotations for item: %s" item-id)
  (message "")
  
  (let ((annotations-data (zotero-get-all-annotations-for-item item-id)))
    (if (cdr (assq 'error annotations-data))
        (message "Error: %s" (cdr (assq 'error annotations-data)))
      
      ;; Print summary
      (zotero-cli-print-annotations-summary annotations-data)
      
      ;; Save output
      (if org-mode
          (zotero-cli-save-org annotations-data item-id)
        (zotero-cli-save-json annotations-data item-id)))))

;;;###autoload
(defun zotero-get-annotations-for-item (item-id)
  "Interactive function to retrieve annotations for ITEM-ID."
  (interactive "sZotero Item ID: ")
  (let ((choice (completing-read "Output format: " '("JSON" "Org-mode") nil t)))
    (zotero-cli-get-annotations item-id (string= choice "Org-mode"))))

;;;###autoload
(defun zotero-find-items-with-pdfs (&optional limit)
  "Find items in library that have PDF attachments.
LIMIT: Maximum number of items to check (default 50)"
  (interactive)
  (let* ((limit (or limit 50))
         (items (zotero-get-top-level-items nil limit))
         (items-with-pdfs '()))
    
    (message "=== Finding items with PDF attachments ===")
    (message "Getting top-level items...")
    (message "Found %d top-level items" (length items))
    
    (let ((item-num 1))
      (dolist (item items)
        (let* ((data (cdr (assq 'data item)))
               (item-id (cdr (assq 'key item)))
               (title (or (cdr (assq 'title data)) "Unknown"))
               (item-type (or (cdr (assq 'itemType data)) "Unknown")))
          
          (message "")
          (message "%d. %s..." item-num (substring title 0 (min 60 (length title))))
          (message "   Type: %s, ID: %s" item-type item-id)
          
          ;; Get children of this item
          (let* ((children (zotero-get-item-children item-id))
                 (pdf-count 0))
            (message "   Children: %d" (length children))
            
            ;; Count PDF attachments
            (dolist (child children)
              (let* ((child-data (cdr (assq 'data child)))
                     (child-type (cdr (assq 'itemType child-data)))
                     (content-type (cdr (assq 'contentType child-data))))
                (when (and (string= child-type "attachment")
                           (string= content-type "application/pdf"))
                  (setq pdf-count (1+ pdf-count)))))
            
            (if (> pdf-count 0)
                (progn
                  (message "   ✅ Has %d PDF attachment(s)" pdf-count)
                  (push (list (cons 'id item-id)
                              (cons 'title title)
                              (cons 'type item-type)
                              (cons 'pdf-count pdf-count))
                        items-with-pdfs))
              (message "   ❌ No PDF attachments"))
            
            (setq item-num (1+ item-num)))))
    
    (message "")
    (message "=== SUMMARY ===")
    (message "Items with PDF attachments: %d" (length items-with-pdfs))
    
    (if items-with-pdfs
        (progn
          (message "")
          (message "Items you can test for annotations:")
          (dolist (item (seq-take items-with-pdfs 10))
            (let ((id (cdr (assq 'id item)))
                  (title (cdr (assq 'title item))))
              (message "  (zotero-cli-get-annotations \"%s\")  ;; %s..."
                       id (substring title 0 (min 50 (length title)))))))
      (message "No items found with PDF attachments in the first %d items." limit)
      (message "You might need to:")
      (message "1. Add some PDFs to your Zotero library")
      (message "2. Increase the limit to search more items")))))

;; Command-line argument parsing for batch mode
(when noninteractive
  (defun zotero-cli-main ()
    "Main function for command-line usage."
    (let ((args command-line-args-left))
      (cond
       ((and (>= (length args) 1)
             (string= (car args) "--help"))
        (message "Usage: emacs --batch -l zotero-get-annots.el --eval \"(zotero-cli-get-annotations \\\"ITEM_ID\\\")\"")
        (message "       emacs --batch -l zotero-get-annots.el --eval \"(zotero-cli-get-annotations \\\"ITEM_ID\\\" t)\"")
        (message "")
        (message "Examples:")
        (message "  emacs --batch -l zotero-get-annots.el --eval \"(zotero-cli-get-annotations \\\"ABC123\\\")\"")
        (message "  emacs --batch -l zotero-get-annots.el --eval \"(zotero-cli-get-annotations \\\"ABC123\\\" t)\""))
       (t
        (message "Zotero annotation retrieval tool loaded.")
        (message "Use (zotero-cli-get-annotations \"ITEM_ID\") or (zotero-cli-get-annotations \"ITEM_ID\" t)")))))
  
  ;; Only run main if this file is being executed directly
  (when (string-match-p "zotero-get-annots\\.el$" (car command-line-args))
    (zotero-cli-main)))

(provide 'zotero-get-annots)

;;; zotero-get-annots.el ends here
