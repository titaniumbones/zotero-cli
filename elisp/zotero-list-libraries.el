;;; zotero-list-libraries.el --- List Zotero libraries and collections -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: Claude
;; Keywords: zotero, research, bibliography, libraries
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.1") (zotero-api "1.0.0"))

;;; Commentary:

;; This package provides functions to explore Zotero libraries and collections,
;; similar to the Python list-libraries.py script.

;;; Code:

(require 'zotero-api)

;;;###autoload
(defun zotero-list-libraries-cli ()
  "Command-line interface to list Zotero libraries and data."
  (interactive)
  (message "Zotero Library Explorer")
  (message "%s" (make-string 50 ?=))
  
  ;; Test connection by trying to get libraries
  (message "Connecting to Zotero local API...")
  (condition-case err
      (let ((libraries (zotero-get-libraries)))
        (message "Libraries API call completed. Found %d libraries." (length libraries))
        
        ;; Also try getting collections from personal library
        (message "Trying to get collections from personal library...")
        (condition-case err2
            (let ((collections (zotero-get-collections)))
              (message "Collections API call completed. Found %d collections." (length collections))
              
              ;; Try getting items from personal library
              (message "Trying to get items from personal library...")
              (condition-case err3
                  (let ((items (zotero-get-top-level-items nil 10)))
                    (message "Items API call completed. Found %d top-level items." (length items))
                    
                    ;; Get available item types
                    (message "Getting available item types...")
                    (condition-case err4
                        (let ((item-types (zotero-get-item-types)))
                          (message "Item types API call completed. Found %d item types." (length item-types))
                          
                          ;; Display results
                          (zotero-display-results libraries collections items item-types))
                      (error
                       (message "Error getting item types: %s" (error-message-string err4))
                       (zotero-display-results libraries collections items nil))))
                (error
                 (message "Error getting items: %s" (error-message-string err3))
                 (zotero-display-results libraries collections nil nil))))
          (error
           (message "Error getting collections: %s" (error-message-string err2))
           (zotero-display-results libraries nil nil nil))))
    (error
     (message "Error connecting to Zotero: %s" (error-message-string err))
     (message "Make sure Zotero is running and the local API is enabled."))))

(defun zotero-display-results (libraries collections items item-types)
  "Display results of library exploration.
LIBRARIES: List of group libraries
COLLECTIONS: List of collections
ITEMS: List of items
ITEM-TYPES: List of available item types"
  (message "")
  (message "RESULTS:")
  (message "%s" (make-string 50 ?=))
  
  ;; Libraries
  (if (null libraries)
      (progn
        (message "No group libraries found.")
        (message "This could mean:")
        (message "- You don't have any group libraries")
        (message "- The API endpoint returned empty data")
        (message "- There was an issue with the API call"))
    (message "Found %d group library/libraries" (length libraries)))
  
  ;; Collections
  (if collections
      (progn
        (message "Found %d collections in personal library" (length collections))
        (message "")
        (message "Personal Library Collections:")
        (let ((count 1))
          (dolist (collection (seq-take collections 5))
            (let* ((data (cdr (assq 'data collection)))
                   (name (or (cdr (assq 'name data)) "Unknown"))
                   (key (or (cdr (assq 'key collection)) "Unknown"))
                   (parent (cdr (assq 'parentCollection data))))
              (message "%d. %s" count name)
              (message "   Key: %s" key)
              (when parent
                (message "   Parent: %s" parent))
              (message "")
              (setq count (1+ count))))
          (when (> (length collections) 5)
            (message "... and %d more collections" (- (length collections) 5)))))
    (message "No collections found in personal library"))
  
  ;; Items
  (if items
      (progn
        (message "")
        (message "Found %d items in personal library" (length items))
        (message "")
        (message "Personal Library Items:")
        (let ((count 1))
          (dolist (item (seq-take items 5))
            (let* ((data (cdr (assq 'data item)))
                   (title (or (cdr (assq 'title data)) "Unknown Title"))
                   (item-type (or (cdr (assq 'itemType data)) "Unknown"))
                   (key (or (cdr (assq 'key item)) "Unknown"))
                   (creators (cdr (assq 'creators data))))
              (message "%d. %s" count title)
              (message "   Type: %s" item-type)
              (message "   Key: %s" key)
              (when creators
                (let ((author-names '()))
                  (dolist (creator (seq-take creators 3))
                    (let ((last-name (cdr (assq 'lastName creator))))
                      (when last-name
                        (push last-name author-names))))
                  (when author-names
                    (message "   Authors: %s" (mapconcat 'identity (nreverse author-names) ", ")))))
              (message "")
              (setq count (1+ count))))
          (when (> (length items) 5)
            (message "... and %d more items" (- (length items) 5)))))
    (message "No items found in personal library"))
  
  ;; Item types
  (if item-types
      (progn
        (message "")
        (message "Available item types (%d total):" (length item-types))
        (let ((count 1))
          (dolist (item-type (seq-take item-types 10))
            (let ((type-name (cdr (assq 'itemType item-type)))
                  (localized (cdr (assq 'localized item-type))))
              (message "%d. %s - %s" count type-name localized)
              (setq count (1+ count))))
          (when (> (length item-types) 10)
            (message "... and %d more item types" (- (length item-types) 10)))))
    (message "No item types found"))
  
  ;; Show group libraries if any
  (when libraries
    (message "")
    (message "Found %d library/libraries:" (length libraries))
    (message "")
    
    ;; Display summary of all libraries
    (let ((count 1))
      (dolist (library libraries)
        (let* ((id (cdr (assq 'id library)))
               (name (cdr (assq 'name library)))
               (type (cdr (assq 'type library)))
               (description (cdr (assq 'description library))))
          (message "%d. Library ID: %s" count (or id "Unknown"))
          (message "   Name: %s" (or name "Unknown"))
          (message "   Type: %s" (or type "Unknown"))
          (when description
            (let ((desc (if (> (length description) 100)
                            (concat (substring description 0 100) "...")
                          description)))
              (message "   Description: %s" desc)))
          (message "")
          (setq count (1+ count)))))
    
    ;; Get detailed information for each library
    (message "Detailed Library Information:")
    (message "%s" (make-string 50 ?=))
    
    (let ((count 1))
      (dolist (library libraries)
        (let* ((library-id (cdr (assq 'id library)))
               (name (cdr (assq 'name library))))
          (if (not library-id)
              (message "Library %d: No ID available, skipping detailed info" count)
            
            (message "")
            (message "--- Library %d: %s ---" count (or name "Unknown"))
            
            ;; Get detailed library info
            (condition-case err
                (let ((detailed-info (zotero-get-library-info (format "%s" library-id))))
                  (if detailed-info
                      (progn
                        (message "Detailed Information:")
                        (dolist (key '(id name type description owner members created modified))
                          (let ((value (cdr (assq key detailed-info))))
                            (when value
                              (message "  %s: %s" (capitalize (symbol-name key)) value))))
                        (message "")
                        (message "Raw data available for further processing."))
                    (message "  Could not retrieve detailed information")))
              (error
               (message "  Error getting detailed info: %s" (error-message-string err)))))
          (setq count (1+ count)))))))

;; Command-line support for batch mode
(when noninteractive
  (defun zotero-list-libraries-main ()
    "Main function for command-line usage."
    (zotero-list-libraries-cli)))

(provide 'zotero-list-libraries)

;;; zotero-list-libraries.el ends here