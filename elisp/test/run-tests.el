;;; run-tests.el --- Test runner for org-zotero-client -*- lexical-binding: t; -*-

;;; Commentary:
;; Test runner script for org-zotero-client test suite

(require 'package)

;; Add current directory to load path
(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(add-to-list 'load-path (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name))))

;; Load test file
(require 'test-org-zotero-client)

;; Run tests
(ert-run-tests-batch-and-exit "test-org-zotero-")

;;; run-tests.el ends here