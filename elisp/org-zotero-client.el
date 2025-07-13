;;; org-zotero-client.el --- Zotero annotation extraction for org-mode -*- lexical-binding: t; -*-

(require 'bibtex)
(require 'zotero-api)
(require 'cl-lib)

;;; Variables

(defvar org-zotero--citation-mapping nil
  "Cached citation key to Zotero ID mapping.")

(defvar org-zotero--mapping-source nil
  "Source file for cached mapping.")

;;; Core functions

(defun org-zotero-list-libraries ()
  "List all available Zotero libraries."
  (interactive)
  (let ((libraries (zotero-get-libraries)))
    (if (called-interactively-p 'any)
        (progn
          (message "Available Zotero libraries:")
          (dolist (lib libraries)
            (let ((id (cdr (assq 'id lib)))
                  (name (cdr (assq 'name lib)))
                  (type (cdr (assq 'type lib))))
              (message "  %s: %s (%s)" id name type))))
      libraries)))

(defun org-zotero-set-library-for-buffer (library-id)
  "Set the ZOTERO_LIBRARY_ID property for the current buffer."
  (interactive 
   (list (read-string "Library ID: " 
                      (org-zotero-get-buffer-library-id))))
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^\\s-*#\\+ZOTERO_LIBRARY_ID:\\s-*\\(.+\\)\\s-*$" nil t)
        (replace-match (format "#+ZOTERO_LIBRARY_ID: %s" library-id))
      (goto-char (point-min))
      (insert (format "#+ZOTERO_LIBRARY_ID: %s\n" library-id))))
  (message "Set library ID to: %s" library-id))

(defun org-zotero-get-buffer-library-id ()
  "Get ZOTERO_LIBRARY_ID property from current buffer."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^\\s-*#\\+ZOTERO_LIBRARY_ID:\\s-*\\(.+\\)\\s-*$" nil t)
      (match-string 1))))

(defun org-zotero-get-bibliography-file ()
  "Get bibliography file path from #+BIBLIOGRAPHY keyword in current buffer."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^\\s-*#\\+BIBLIOGRAPHY:\\s-*\\(.+\\)\\s-*$" nil t)
      (let ((bib-path (match-string 1)))
        (if (file-name-absolute-p bib-path)
            bib-path
          (expand-file-name bib-path (file-name-directory (buffer-file-name))))))))

(defun org-zotero-find-citations-in-buffer ()
  "Find all Zotero citations in current buffer."
  (let ((citations '())
        (patterns '("@\\([A-Za-z0-9_-]+\\)"
                    "\\[cite:@\\([A-Za-z0-9_-]+\\)\\]")))
    (save-excursion
      (goto-char (point-min))
      (dolist (pattern patterns)
        (goto-char (point-min))
        (while (re-search-forward pattern nil t)
          (let ((start (match-beginning 0))
                (end (match-end 0))
                (item-id (match-string 1)))
            (push (list start end item-id) citations)))))
    (let ((seen (make-hash-table :test 'equal))
          (unique-citations '()))
      (dolist (citation citations)
        (let ((item-id (nth 2 citation)))
          (unless (gethash item-id seen)
            (puthash item-id t seen)
            (push citation unique-citations))))
      (sort unique-citations (lambda (a b) (< (car a) (car b)))))))

(defun org-zotero-build-citation-mapping (bib-file &optional library-id)
  "Build mapping from BibTeX citation keys to Zotero item IDs."
  (when (file-exists-p bib-file)
    (with-temp-buffer
      (insert-file-contents bib-file)
      (bibtex-mode)
      (let ((mapping '())
            (entry-count 0))
        (goto-char (point-min))
        (while (bibtex-skip-to-valid-entry)
          (setq entry-count (1+ entry-count))
          (let ((citation-key (bibtex-key-in-head))
                (zotero-id nil))
            (save-excursion
              (bibtex-beginning-of-entry)
              (let ((entry-end (save-excursion (bibtex-end-of-entry) (point))))
                (when (re-search-forward "^[ \\t]*key[ \\t]*=[ \\t]*{\\([^}]+\\)}" entry-end t)
                  (setq zotero-id (match-string 1)))))
            (when (and citation-key zotero-id)
              (push (cons citation-key zotero-id) mapping)))
          (bibtex-end-of-entry))
        (message "Successfully mapped %d citation keys to Zotero IDs" (length mapping))
        mapping))))

(defun org-zotero-resolve-citation-key (citation-key &optional library-id)
  "Resolve BibTeX citation key to Zotero item ID."
  (unless (and org-zotero--citation-mapping org-zotero--mapping-source)
    (let ((bib-file (org-zotero-get-bibliography-file)))
      (unless bib-file
        (error "No bibliography file found"))
      (unless (file-exists-p bib-file)
        (error "Bibliography file not found: %s" bib-file))
      (setq org-zotero--citation-mapping (org-zotero-build-citation-mapping bib-file library-id))
      (setq org-zotero--mapping-source bib-file)))
  (cdr (assoc citation-key org-zotero--citation-mapping)))

(defun org-zotero-get-org-keywords (filepath)
  "Get org-mode keywords from file (bibliography, cite_export, etc.).
FILEPATH is the path to the org-mode file.
Returns alist of keyword names to values."
  (let ((keywords '()))
    (condition-case err
        (with-temp-buffer
          (insert-file-contents filepath)
          (goto-char (point-min))
          (while (re-search-forward "^\\s-*#\\+\\([A-Z_]+\\):\\s-*\\(.+\\)\\s-*$" nil t)
            (let ((keyword (upcase (match-string 1)))
                  (value (string-trim (match-string 2))))
              (push (cons keyword value) keywords)))
          keywords)
      (error
       (message "Error reading file %s: %s" filepath (error-message-string err))
       nil))))

(defun org-zotero-extract-all-annotations-to-notes (&optional output-file)
  "Extract all annotations from citations in current buffer."
  (interactive)
  (let* ((buffer-library-id (org-zotero-get-buffer-library-id))
         (citations (org-zotero-find-citations-in-buffer)))
    
    (if (not citations)
        (message "No Zotero citations found in buffer")
      
      (when (called-interactively-p 'any)
        (unless output-file
          (setq output-file (read-file-name "Save annotations to file: ")))
        (unless (y-or-n-p (format "Found %d citations. Extract annotations? " (length citations)))
          (message "Cancelled by user")
          (cl-return-from org-zotero-extract-all-annotations-to-notes nil)))
      
      (let ((citation-key (substring-no-properties (nth 2 (car citations))))
            (zotero-id nil))
        (message "Processing citation: %s" citation-key)
        (setq zotero-id (org-zotero-resolve-citation-key citation-key buffer-library-id))
        
        (if (not zotero-id)
            (message "Citation key '%s' not found" citation-key)
          
          (message "Resolved: %s → %s" citation-key zotero-id)
          (let* ((item-data (zotero-get-item zotero-id buffer-library-id))
                 (title (or (cdr (assq 'title (cdr (assq 'data item-data)))) "Unknown Title"))
                 (annotations-data (zotero-get-all-annotations-for-item zotero-id buffer-library-id))
                 (attachments (cdr (assq 'attachments annotations-data)))
                 (total-annotations 0))
            
            (dolist (attachment attachments)
              (let ((count (cdr (assq 'annotations-count attachment))))
                (when count
                  (setq total-annotations (+ total-annotations count)))))
            
            (message "Found %d annotations in %d PDFs" total-annotations (length attachments))
            
            (when (> total-annotations 0)
              (let ((formatted (zotero-format-as-org-mode annotations-data citation-key))
                    (source-keywords (org-zotero-get-org-keywords (buffer-file-name))))
                (if output-file
                    (progn
                      (with-temp-file output-file
                        (insert (format "#+TITLE: %s\n" title))
                        ;; Include important org keywords from source file
                        (let ((bibliography (cdr (assoc "BIBLIOGRAPHY" source-keywords)))
                              (cite-export (cdr (assoc "CITE_EXPORT" source-keywords)))
                              (library-id (or buffer-library-id (cdr (assoc "ZOTERO_LIBRARY_ID" source-keywords)))))
                          (when bibliography
                            (insert (format "#+BIBLIOGRAPHY: %s\n" bibliography)))
                          (when cite-export
                            (insert (format "#+CITE_EXPORT: %s\n" cite-export)))
                          (when library-id
                            (insert (format "#+ZOTERO_LIBRARY_ID: %s\n" library-id))))
                        (insert "\n")
                        (insert formatted))
                      (message "✅ Annotations saved to: %s" output-file)
                      output-file)
                  (let ((notes-buffer (get-buffer-create "*Zotero Annotations*")))
                    (with-current-buffer notes-buffer
                      (erase-buffer)
                      (insert (format "#+TITLE: %s\n" title))
                      ;; Include important org keywords from source file
                      (let ((bibliography (cdr (assoc "BIBLIOGRAPHY" source-keywords)))
                            (cite-export (cdr (assoc "CITE_EXPORT" source-keywords)))
                            (library-id (or buffer-library-id (cdr (assoc "ZOTERO_LIBRARY_ID" source-keywords)))))
                        (when bibliography
                          (insert (format "#+BIBLIOGRAPHY: %s\n" bibliography)))
                        (when cite-export
                          (insert (format "#+CITE_EXPORT: %s\n" cite-export)))
                        (when library-id
                          (insert (format "#+ZOTERO_LIBRARY_ID: %s\n" library-id))))
                      (insert "\n")
                      (insert formatted)
                      (org-mode)
                      (goto-char (point-min)))
                    (display-buffer notes-buffer)
                    (message "✅ Annotations displayed in buffer")
                    notes-buffer))))))))

;;; Collection annotation functions

(defun org-zotero-select-library ()
  "Select a Zotero library interactively from available libraries.
Returns library ID or nil for personal library."
  (let* ((libraries (zotero-get-libraries))
         (choices '())
         (library-map '()))
    
    ;; Add personal library option
    (push "Personal Library" choices)
    (push (cons "Personal Library" nil) library-map)
    
    ;; Add group libraries
    (dolist (library libraries)
      (let* ((id (cdr (assq 'id library)))
             (data (cdr (assq 'data library)))
             (name (cdr (assq 'name data)))
             (id-string (if (numberp id) (number-to-string id) id))
             (display-name (format "%s (ID: %s)" name id)))
        (push display-name choices)
        (push (cons display-name id-string) library-map)))
    
    (let ((selected (completing-read "Select library: " (nreverse choices) nil t)))
      (cdr (assoc selected library-map)))))

(defun org-zotero-select-collection (library-id)
  "Select a collection from specified library.
LIBRARY-ID is the library to search (nil for personal library).
Returns collection ID."
  (let* ((collections (zotero-get-collections library-id))
         (choices '())
         (collection-map '()))
    
    (if (null collections)
        (progn
          (message "No collections found in library")
          nil)
      
      (dolist (collection collections)
        (let* ((id (cdr (assq 'key collection)))
               (data (cdr (assq 'data collection)))
               (name (cdr (assq 'name data)))
               (parent (cdr (assq 'parentCollection data)))
               (display-name (if parent
                                 (format "%s (subcollection)" name)
                               name)))
          (push display-name choices)
          (push (cons display-name id) collection-map)))
      
      (let ((selected (completing-read "Select collection: " (nreverse choices) nil t)))
        (cdr (assoc selected collection-map))))))

;;;###autoload
(defun org-zotero-extract-collection-annotations-interactive ()
  "Interactively extract all annotations from a Zotero collection.
Prompts user to select library, collection, and output file."
  (interactive)
  (let* ((library-id (org-zotero-select-library))
         (collection-id (org-zotero-select-collection library-id)))
    
    (if (not collection-id)
        (message "No collection selected or no collections available")
      
      (message "Retrieving collection information...")
      (let ((collection-info (zotero-get-collection-info collection-id library-id)))
        (if (not collection-info)
            (message "Error: Collection not found")
          
          (let* ((collection-data (cdr (assq 'data collection-info)))
                 (collection-name (cdr (assq 'name collection-data)))
                 (output-file (read-file-name 
                               (format "Save annotations to file: ")
                               nil 
                               (format "collection_%s.org" 
                                       (replace-regexp-in-string "[^a-zA-Z0-9_-]" "_" collection-name)))))
            
            (message "Extracting annotations from collection: %s" collection-name)
            (let ((collection-annotations (zotero-get-all-collection-annotations collection-id library-id)))
              
              (if (assq 'error collection-annotations)
                  (message "Error: %s" (cdr (assq 'error collection-annotations)))
                
                (let* ((items-count (cdr (assq 'items-count collection-annotations)))
                       (items-with-annotations (length (cdr (assq 'items collection-annotations))))
                       (formatted-content (zotero-format-collection-annotations-as-org collection-annotations)))
                  
                  (message "Found %d items in collection, %d with annotations" 
                           items-count items-with-annotations)
                  
                  (if (= items-with-annotations 0)
                      (message "No items with annotations found in this collection")
                    
                    ;; Save to file
                    (with-temp-file output-file
                      (insert (format "#+TITLE: Collection Annotations - %s\n" collection-name))
                      (when library-id
                        (insert (format "#+ZOTERO_LIBRARY_ID: %s\n" library-id)))
                      (insert "\n")
                      (insert formatted-content))
                    
                    (message "✅ Collection annotations saved to: %s" output-file)
                    
                    ;; Also display in buffer
                    (let ((notes-buffer (get-buffer-create "*Zotero Collection Annotations*")))
                      (with-current-buffer notes-buffer
                        (erase-buffer)
                        (insert (format "#+TITLE: Collection Annotations - %s\n" collection-name))
                        (when library-id
                          (insert (format "#+ZOTERO_LIBRARY_ID: %s\n" library-id)))
                        (insert "\n")
                        (insert formatted-content)
                        (org-mode)
                        (goto-char (point-min)))
                      (display-buffer notes-buffer))
                    
                    output-file)))))))))))

;;;###autoload  
(defun org-zotero-extract-collection-annotations (collection-id &optional library-id output-file)
  "Extract all annotations from a Zotero collection.
COLLECTION-ID is the Zotero collection ID.
LIBRARY-ID is optional library ID (nil for personal library).
OUTPUT-FILE is optional output file path."
  (interactive "sCollection ID: ")
  (let* ((collection-info (zotero-get-collection-info collection-id library-id)))
    (if (not collection-info)
        (message "Error: Collection %s not found" collection-id)
      
      (let* ((collection-data (cdr (assq 'data collection-info)))
             (collection-name (cdr (assq 'name collection-data))))
        
        (unless output-file
          (setq output-file (read-file-name 
                             "Save annotations to file: "
                             nil
                             (format "collection_%s.org" 
                                     (replace-regexp-in-string "[^a-zA-Z0-9_-]" "_" collection-name)))))
        
        (message "Extracting annotations from collection: %s" collection-name)
        (let ((collection-annotations (zotero-get-all-collection-annotations collection-id library-id)))
          
          (if (assq 'error collection-annotations)
              (message "Error: %s" (cdr (assq 'error collection-annotations)))
            
            (let* ((items-count (cdr (assq 'items-count collection-annotations)))
                   (items-with-annotations (length (cdr (assq 'items collection-annotations))))
                   (formatted-content (zotero-format-collection-annotations-as-org collection-annotations)))
              
              (message "Found %d items in collection, %d with annotations" 
                       items-count items-with-annotations)
              
              (if (= items-with-annotations 0)
                  (message "No items with annotations found in this collection")
                
                ;; Save to file
                (with-temp-file output-file
                  (insert (format "#+TITLE: Collection Annotations - %s\n" collection-name))
                  (when library-id
                    (insert (format "#+ZOTERO_LIBRARY_ID: %s\n" library-id)))
                  (insert "\n")
                  (insert formatted-content))
                
                (message "✅ Collection annotations saved to: %s" output-file)
                output-file)))))))))

(provide 'org-zotero-client)

;;; org-zotero-client.el ends here