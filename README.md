# Zotero CLI

> **DEPRECATED**: This repository has been merged into [mwp-zotero-tools](https://github.com/titaniumbones/mwp-zotero-tools).
> - Python code: `packages/zotero-cli/`
> - Emacs Lisp: `packages/zotero-elisp/`
>
> This repository is archived and will no longer receive updates.

---

A multi-language toolkit for retrieving and processing Zotero annotations via the local API. Provides both Python and Emacs Lisp implementations with API parity wherever possible.

## 🚀 Quick Start

### Python
```bash
cd python/
pip install requests
python get-annots.py ITEM_ID --org  # Get annotations in org-mode format
python org_zotero_client.py process notes.org  # Process org file citations
```

### Emacs Lisp
```elisp
(use-package org-zotero-client
  :load-path "/path/to/zotero-cli/elisp/"
  :ensure nil
  :after org)

(load-file "/path/to/zotero-cli/elisp/org-zotero-client.el")
(org-zotero-extract-all-annotations-to-notes)
```

## 📁 Project Structure

```
zotero-cli/
├── python/                    # Python implementation
│   ├── get-annots.py         # Main annotation retrieval
│   ├── org_zotero_client.py  # Org-mode file processing
│   ├── list-libraries.py     # Library exploration
│   └── README.md
├── elisp/                     # Emacs Lisp implementation
│   ├── zotero-api.el         # Core API client
│   ├── org-zotero-client.el  # Org-mode integration
│   ├── test/                 # Unit tests
│   └── README.md
└── test/                      # Test files and examples
```

## ✨ Core API Features

Both implementations provide identical functionality for:

- **Library Management**: List libraries, get library info, work with group libraries
- **Item Retrieval**: Get items, fetch individual items, retrieve child attachments
- **Annotation Processing**: Extract annotations from PDFs, format as org-mode text
- **Citation Resolution**: Map BibTeX keys to Zotero item IDs
- **Batch Processing**: Process multiple citations in org-mode files

### Key API Methods

| Function | Python | Emacs Lisp |
|----------|---------|------------|
| List libraries | `get_libraries()` | `zotero-get-libraries` |
| Get item | `get_item(item_id)` | `zotero-get-item` |
| Get annotations | `get_all_annotations_for_item()` | `zotero-get-all-annotations-for-item` |
| Format as org | `format_as_org_mode()` | `zotero-format-as-org-mode` |
| Process citations | `process_org_file()` | `org-zotero-extract-all-annotations-to-notes` |

## 🔧 Installation

### Python
```bash
cd python/
pip install requests  # Only dependency
```

### Emacs Lisp

**Using use-package (recommended):**
```elisp
(use-package request :ensure t)

(use-package org-zotero-client
  :load-path "/path/to/zotero-cli/elisp/"
  :ensure nil
  :after org
  :bind (("C-c z a" . org-zotero-extract-all-annotations-to-notes)
         ("C-c z l" . org-zotero-list-libraries)
         ("C-c z s" . org-zotero-set-library-for-buffer)))
```

**Manual installation:**
```elisp
(package-install 'request)  ; Install request package
(add-to-list 'load-path "/path/to/zotero-cli/elisp/")
(require 'org-zotero-client)
```

## 📖 Usage Examples

### Python Examples

```bash
# Get annotations for a specific item
python get-annots.py YW7IQA55

# Get annotations in org-mode format
python get-annots.py YW7IQA55 --org

# Process org file and insert annotations for all citations
python org_zotero_client.py process research-notes.org

# Set library ID for an org file
python org_zotero_client.py set-library research-notes.org

# List all libraries
python list-libraries.py
```

### Emacs Lisp Examples

```elisp
;; Interactive usage
M-x org-zotero-extract-all-annotations-to-notes  ; Process all citations in buffer
M-x org-zotero-list-libraries                    ; List available libraries
M-x org-zotero-set-library-for-buffer           ; Set library ID for current buffer

;; Programmatic usage
(org-zotero-extract-all-annotations-to-notes "/path/to/output.org")
(org-zotero-find-citations-in-buffer)
(org-zotero-resolve-citation-key "smith2023")
```

### Org-Mode Citation Formats

Both implementations support multiple citation formats:

```org
# Simple citations
This paper @cowanMoreWorkMother1983 discusses important findings.

# Org-cite format
See [cite:@smith2023] for methodology details.

# BibTeX integration
#+BIBLIOGRAPHY: references.bib
#+ZOTERO_LIBRARY_ID: 5895731

Citations like @author2023 are automatically resolved.
```

### Working with Group Libraries

Set a library ID for your org file to work with group libraries:

```org
#+ZOTERO_LIBRARY_ID: 123456
#+BIBLIOGRAPHY: references.bib

Your content with citations @author2023 here.
```

## 🔗 Requirements

- **Zotero Desktop**: Must be running with local API enabled
- **Local API**: Accessible at `http://localhost:23119`
- **Python**: 3.6+ with `requests` library
- **Emacs**: 25.1+ with `request` package

## 🛠️ Development

The project maintains API parity between Python and Emacs Lisp implementations wherever possible. Both provide equivalent functionality for:

- Library and item management
- Annotation extraction and formatting
- Citation resolution and processing
- Org-mode integration

### Key Components

- **Python**: `ZoteroLocalAPI` class provides core API functionality
- **Emacs Lisp**: Functions in `zotero-api.el` provide equivalent API methods
- **Org Integration**: Both support BibTeX citation resolution and org-mode formatting

## 📚 Documentation

- [Python Implementation README](python/README.md) - Detailed Python usage and API
- [Emacs Lisp Implementation README](elisp/README.md) - Emacs commands and configuration
- [API Reference](doc_resources/zotero_local_api_reference.md) - Complete API documentation

## 🎯 Use Cases

- **Research Writing**: Automatically insert Zotero annotations into org-mode documents
- **Literature Review**: Batch process multiple papers and collect all annotations
- **Note Organization**: Export annotations from Zotero into structured org-mode files
- **Cross-Reference**: Link between Zotero items and your own notes
- **Group Collaboration**: Work with shared Zotero group libraries

## 🤝 Contributing

This project emphasizes maintaining API parity between implementations. When adding features, ensure both Python and Emacs Lisp versions are updated consistently.

## 📄 License

This project is provided as-is for research and educational purposes.