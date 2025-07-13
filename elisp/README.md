# Emacs Lisp Implementation

Emacs Lisp client for Zotero local API with org-mode integration. Provides seamless annotation extraction and citation processing within Emacs.

## 🚀 Quick Start

```elisp
;; Load the package
(load-file "/path/to/zotero-cli/elisp/org-zotero-client.el")

;; Extract annotations from all citations in current buffer
(org-zotero-extract-all-annotations-to-notes)

;; List available libraries
(org-zotero-list-libraries)

;; Set library ID for current buffer
(org-zotero-set-library-for-buffer "5895731")
```

## 📦 Installation

### Prerequisites

- Emacs 25.1+
- `request` package (install via MELPA)
- Zotero Desktop with local API enabled

### Using use-package (Recommended)

```elisp
(use-package request :ensure t)

(use-package org-zotero-client
  :load-path "/path/to/zotero-cli/elisp/"
  :ensure nil
  :after org
  :bind (("C-c z a" . org-zotero-extract-all-annotations-to-notes)
         ("C-c z l" . org-zotero-list-libraries)
         ("C-c z s" . org-zotero-set-library-for-buffer))
  :config
  ;; Optional: Set default library ID
  (setq org-zotero-default-library-id "5895731"))
```

### Manual Installation

```elisp
;; Install request package
(package-install 'request)

;; Add to load path
(add-to-list 'load-path "/path/to/zotero-cli/elisp/")

;; Load the package
(require 'org-zotero-client)

;; Optional key bindings
(define-key org-mode-map (kbd "C-c z a") 'org-zotero-extract-all-annotations-to-notes)
(define-key org-mode-map (kbd "C-c z l") 'org-zotero-list-libraries)
(define-key org-mode-map (kbd "C-c z s") 'org-zotero-set-library-for-buffer)
```

## 📋 Available Commands

### Interactive Commands

| Command | Description | Key Binding |
|---------|-------------|-------------|
| `org-zotero-extract-all-annotations-to-notes` | Extract annotations from all citations in buffer | `C-c z a` |
| `org-zotero-list-libraries` | List available Zotero libraries | `C-c z l` |
| `org-zotero-set-library-for-buffer` | Set library ID for current buffer | `C-c z s` |

### Core Functions

| Function | Purpose |
|----------|---------|
| `org-zotero-get-buffer-library-id` | Get library ID from current buffer |
| `org-zotero-get-bibliography-file` | Get bibliography file path |
| `org-zotero-find-citations-in-buffer` | Find all citations in current buffer |
| `org-zotero-resolve-citation-key` | Resolve BibTeX key to Zotero item ID |
| `org-zotero-build-citation-mapping` | Build citation key to item ID mapping |

### Low-Level API Functions

| Function | Purpose |
|----------|---------|
| `zotero-get-libraries` | Get all available libraries |
| `zotero-get-item` | Get single item by ID |
| `zotero-get-all-annotations-for-item` | Get annotations for an item |
| `zotero-format-as-org-mode` | Format annotations as org-mode text |

## 📝 Usage Examples

### Basic Usage

```elisp
;; In an org-mode buffer with citations
;; Set library ID first
(org-zotero-set-library-for-buffer "5895731")

;; Extract annotations for all citations
(org-zotero-extract-all-annotations-to-notes)

;; Or save to specific file
(org-zotero-extract-all-annotations-to-notes "/path/to/notes.org")
```

### Working with Citations

```elisp
;; Find all citations in current buffer
(setq citations (org-zotero-find-citations-in-buffer))

;; Resolve specific citation key
(org-zotero-resolve-citation-key "smith2023")

;; Get library ID from buffer
(org-zotero-get-buffer-library-id)
```

### Programmatic Usage

```elisp
;; Get annotations for specific item
(setq annotations (zotero-get-all-annotations-for-item "YW7IQA55" "5895731"))

;; Format as org-mode text
(setq org-text (zotero-format-as-org-mode annotations))

;; List all libraries
(setq libraries (zotero-get-libraries))
```

## 🔧 Configuration

### Buffer Setup

Add these headers to your org-mode files:

```org
#+BIBLIOGRAPHY: references.bib
#+ZOTERO_LIBRARY_ID: 5895731

Your content with citations @author2023 here.
```

### Citation Formats

The package supports multiple citation formats:

```org
# Simple citations
This paper @cowanMoreWorkMother1983 discusses findings.

# Org-cite format
See [cite:@smith2023] for methodology.

# Mixed usage
Both @author2023 and [cite:@jones2022] are supported.
```

### BibTeX Integration

The package automatically resolves BibTeX citation keys to Zotero item IDs:

1. Exports from Zotero must include the `key` field
2. The `#+BIBLIOGRAPHY:` header points to the BibTeX file
3. Citation keys in the org file match BibTeX entries

Example BibTeX entry:
```bibtex
@article{smith2023,
  author = {Smith, John},
  title = {Research Paper},
  key = {YW7IQA55},
  year = {2023}
}
```

## 🧪 Testing

Run the test suite:

```elisp
;; Load test file
(load-file "/path/to/zotero-cli/elisp/test/test-org-zotero-client.el")

;; Run all tests
(ert "test-org-zotero-")

;; Run specific test
(ert-run-test 'test-org-zotero-get-buffer-library-id)
```

Or from command line:
```bash
cd elisp/test
emacs -batch -L .. -l run-tests.el
```

## 🔍 Troubleshooting

### Common Issues

1. **"No bibliography file found"**
   - Add `#+BIBLIOGRAPHY: references.bib` to your org file
   - Ensure the BibTeX file exists and is readable

2. **"Citation key not found"**
   - Verify the BibTeX file contains the citation key
   - Check that the `key` field is present in BibTeX entries

3. **"Item not found in Zotero"**
   - Ensure Zotero Desktop is running
   - Check that the item exists in the specified library
   - Verify the library ID is correct

4. **"No annotations found"**
   - Confirm the item has PDF attachments
   - Check that PDFs contain highlighted text or notes

### Debug Information

Enable debug mode:
```elisp
(setq zotero-debug t)
```

This will show detailed API requests and responses in the `*Messages*` buffer.

## 📚 API Reference

### Buffer Analysis Functions

- `(org-zotero-get-buffer-library-id)` → string or nil
- `(org-zotero-get-bibliography-file)` → string or nil
- `(org-zotero-find-citations-in-buffer)` → list of citations

### Citation Resolution

- `(org-zotero-resolve-citation-key KEY &optional LIBRARY-ID)` → string or nil
- `(org-zotero-build-citation-mapping BIB-FILE &optional LIBRARY-ID)` → alist

### Annotation Processing

- `(org-zotero-extract-all-annotations-to-notes &optional OUTPUT-FILE)` → buffer or filename
- `(zotero-get-all-annotations-for-item ITEM-ID LIBRARY-ID)` → annotation data
- `(zotero-format-as-org-mode ANNOTATIONS-DATA)` → string

### Library Management

- `(org-zotero-list-libraries)` → list of libraries
- `(org-zotero-set-library-for-buffer LIBRARY-ID)` → updates buffer
- `(zotero-get-libraries)` → list of library objects

## 🤝 Contributing

When adding new features:

1. Update both the main functions and tests
2. Ensure compatibility with existing org-mode workflows
3. Add documentation for new commands
4. Test with various citation formats
5. Maintain API parity with Python implementation

## 📄 Files

- `org-zotero-client.el` - Main package with org-mode integration
- `zotero-api.el` - Core API client functions
- `test/test-org-zotero-client.el` - Unit tests
- `test/run-tests.el` - Test runner
- `test/README.md` - Testing documentation