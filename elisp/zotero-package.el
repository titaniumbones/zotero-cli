;;; zotero-package.el --- Zotero API package loader -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: Matt Price
;; Keywords: zotero, research, bibliography
;; Version: 0.5.0
;; Package-Requires: ((emacs "25.1") (request "0.3.2"))

;;; Commentary:

;; This is a convenience package that loads all Zotero API components.
;; Simply require this package to get access to all Zotero functionality.

;;; Code:

(require 'zotero-api)
(require 'zotero-get-annots)
(require 'zotero-list-libraries)
(require 'org-zotero-client)

;;;###autoload
(defun zotero-setup ()
  "Set up Zotero API with default configuration."
  (interactive)
  (message "Zotero API loaded successfully!")
  (message "Available functions:")
  (message "- (zotero-get-annotations-for-item \"ITEM_ID\")")
  (message "- (zotero-browse-libraries)")
  (message "- (zotero-browse-items)")
  (message "- (zotero-find-items-with-pdfs)")
  (message "- (zotero-list-libraries-cli)")
  (message "")
  (message "Org-mode integration:")
  (message "- M-x org-zotero-mode to enable in org buffers")
  (message "- C-c z a - Insert annotations at citation")
  (message "- C-c z f - Fetch annotations for item")
  (message "- C-c z b - Fetch annotations from all citations")
  (message "- C-c z e - Extract all annotations to temporary buffer")
  (message "- C-c z E - Extract all annotations to file")
  (message "- C-c z u - Update existing annotations")
  (message "- C-c z s - Set library ID for buffer")
  (message "")
  (message "Configuration:")
  (message "- zotero-base-url: %s" zotero-base-url)
  (message "- zotero-default-user-id: %s" zotero-default-user-id))

(provide 'zotero-package)

;;; zotero-package.el ends here
