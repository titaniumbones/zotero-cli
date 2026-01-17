# Org-Zotero-Client Test Suite

This directory contains unit tests for the `org-zotero-client.el` package.

## Running Tests

### Command Line
```bash
# Run all tests
emacs -batch -L .. -l run-tests.el

# Run specific test
emacs -batch -L .. -l test-org-zotero-client.el -f ert-run-tests-batch-and-exit
```

### From Emacs
```elisp
;; Load the test file
(load-file "test-org-zotero-client.el")

;; Run all tests interactively
(ert "test-org-zotero-")

;; Run specific test
(ert-run-test 'test-org-zotero-get-buffer-library-id)
```

## Test Coverage

The test suite covers:

- **Buffer parsing functions**:
  - `org-zotero-get-buffer-library-id`
  - `org-zotero-get-bibliography-file`
  - `org-zotero-set-library-for-buffer`

- **Citation finding**:
  - `org-zotero-find-citations-in-buffer`
  - Support for `@cite` and `[cite:@cite]` formats
  - Deduplication of citations

- **BibTeX processing**:
  - `org-zotero-build-citation-mapping`
  - `org-zotero-resolve-citation-key`
  - Handling of missing keys

## Test Data

Tests use temporary buffers and files to avoid dependencies on external resources. Mock data is generated for:

- BibTeX entries with and without `key` fields
- Org-mode documents with various citation formats
- Buffer configurations with different library IDs

## Adding Tests

When adding new functionality to `org-zotero-client.el`, please add corresponding tests:

1. Create test functions with the `test-org-zotero-` prefix
2. Use `ert-deftest` macro
3. Include both positive and negative test cases
4. Test edge cases (missing data, malformed input, etc.)
5. Use `with-temp-buffer` and `with-temp-file` for isolation

Example:
```elisp
(ert-deftest test-org-zotero-new-function ()
  "Test description."
  (with-temp-buffer
    (insert "test data")
    (should (equal (org-zotero-new-function) expected-result))))
```