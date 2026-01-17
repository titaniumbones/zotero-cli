;;; test-org-zotero-client.el --- Unit tests for org-zotero-client -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for org-zotero-client.el functionality

(require 'ert)
(require 'org-zotero-client)

;;; Tests

(ert-deftest test-org-zotero-get-buffer-library-id ()
  "Test extracting library ID from buffer."
  (with-temp-buffer
    (insert "#+TITLE: Test Document\n")
    (insert "#+ZOTERO_LIBRARY_ID: 5895731\n")
    (insert "#+BIBLIOGRAPHY: test.bib\n")
    (should (string= (org-zotero-get-buffer-library-id) "5895731"))))

(ert-deftest test-org-zotero-get-buffer-library-id-missing ()
  "Test handling of missing library ID."
  (with-temp-buffer
    (insert "#+TITLE: Test Document\n")
    (insert "#+BIBLIOGRAPHY: test.bib\n")
    (should (null (org-zotero-get-buffer-library-id)))))

(ert-deftest test-org-zotero-get-bibliography-file ()
  "Test extracting bibliography file path."
  (with-temp-buffer
    (setq buffer-file-name "/tmp/test.org")
    (insert "#+TITLE: Test Document\n")
    (insert "#+BIBLIOGRAPHY: ./test.bib\n")
    (should (string= (org-zotero-get-bibliography-file) "/tmp/test.bib"))))

(ert-deftest test-org-zotero-find-citations-simple ()
  "Test finding simple @cite citations."
  (with-temp-buffer
    (insert "Some text with @smith2023 citation.\n")
    (insert "Another @jones2022 here.\n")
    (let ((citations (org-zotero-find-citations-in-buffer)))
      (should (= (length citations) 2))
      (should (string= (nth 2 (car citations)) "smith2023"))
      (should (string= (nth 2 (cadr citations)) "jones2022")))))

(ert-deftest test-org-zotero-find-citations-org-cite ()
  "Test finding org-cite style citations."
  (with-temp-buffer
    (insert "Text with [cite:@smith2023] citation.\n")
    (insert "And [cite:@jones2022] here.\n")
    (let ((citations (org-zotero-find-citations-in-buffer)))
      (should (= (length citations) 2))
      (should (string= (nth 2 (car citations)) "smith2023"))
      (should (string= (nth 2 (cadr citations)) "jones2022")))))

(ert-deftest test-org-zotero-find-citations-duplicates ()
  "Test deduplication of citations."
  (with-temp-buffer
    (insert "Text with @smith2023 and @smith2023 again.\n")
    (let ((citations (org-zotero-find-citations-in-buffer)))
      (should (= (length citations) 1))
      (should (string= (nth 2 (car citations)) "smith2023")))))

(ert-deftest test-org-zotero-resolve-citation-key ()
  "Test resolving citation key using cached mapping."
  (let ((org-zotero--citation-mapping '(("smith2023" . "ABCD1234")
                                        ("jones2022" . "EFGH5678")))
        (org-zotero--mapping-source "/tmp/test.bib"))
    (should (string= (org-zotero-resolve-citation-key "smith2023") "ABCD1234"))
    (should (string= (org-zotero-resolve-citation-key "jones2022") "EFGH5678"))
    (should (null (org-zotero-resolve-citation-key "nonexistent")))))

(ert-deftest test-org-zotero-set-library-for-buffer-new ()
  "Test setting library ID in buffer without existing ID."
  (with-temp-buffer
    (insert "#+TITLE: Test Document\n")
    (org-zotero-set-library-for-buffer "5895731")
    (goto-char (point-min))
    (should (re-search-forward "^#\\+ZOTERO_LIBRARY_ID: 5895731$" nil t))))

;;; Test Suite Runner

(defun org-zotero-run-tests ()
  "Run all org-zotero-client tests."
  (interactive)
  (ert-run-tests-batch-and-exit "test-org-zotero-"))

(provide 'test-org-zotero-client)

;;; test-org-zotero-client.el ends here