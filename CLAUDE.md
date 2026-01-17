# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a multi-language Zotero CLI tool that interfaces with Zotero's local API to retrieve annotations from PDF attachments. The project provides both Python and Emacs Lisp implementations with equivalent functionality.

### Project Structure

```
zotero-cli/
├── python/          # Python implementation
│   ├── get-annots.py
│   ├── list-libraries.py
│   ├── org_zotero_client.py
│   └── debug-*.py
├── elisp/           # Emacs Lisp implementation
│   ├── zotero-api.el
│   ├── org-zotero-client.el
│   └── test/
├── doc_resources/   # API documentation
│   └── zotero_local_api_reference.md
└── CLAUDE.md
```

## Architecture

Both implementations follow the same architectural patterns:

### Python Implementation (`python/`)
- **ZoteroLocalAPI class**: Core API client using `requests` library
- **Command-line interfaces**: Scripts for annotation retrieval and library exploration
- **Type hints**: Full typing support using Python's typing module

### Emacs Lisp Implementation (`elisp/`)
- **zotero-api.el**: Core API client using `request` package
- **Interactive functions**: Emacs commands for annotation retrieval and library exploration
- **Native org-mode output**: Direct integration with Emacs org-mode

### Key Components (Both Implementations)

Core API client with methods for:
- Getting items and their children
- Filtering PDF attachments
- Retrieving annotations from attachments
- Aggregating all annotations for an item
- Formatting annotations as org-mode or markdown text

## Development Commands

### Python Implementation

```bash
cd python/

# Get annotations in JSON format
python get-annots.py <item_id>

# Get annotations in org-mode format  
python get-annots.py <item_id> --org

# Get annotations in markdown format
python get-annots.py <item_id> --markdown

# Extract all annotations from a collection
python get-collection-annots.py <collection_id>

# Extract collection annotations in org-mode format
python get-collection-annots.py <collection_id> --org

# Extract from group library collection
python get-collection-annots.py <collection_id> --library-id <library_id> --org

# Save to specific file
python get-collection-annots.py <collection_id> --output my_collection.org --org

# Explore libraries and collections
python list-libraries.py

# Find items with PDF attachments
python find-items-with-pdfs.py

# Debug annotation retrieval
python debug-annotations.py <item_id>

# Export PDF/EPUB attachments and convert to markdown
python export-attachments.py library
python export-attachments.py collection <collection_id>
```

#### Collection Annotation Examples

```bash
# Extract all annotations from "Research Papers" collection in personal library
python get-collection-annots.py ABC123DEF --org

# Extract from group library collection with custom output file
python get-collection-annots.py XYZ789GHI --library-id 12345 --output research_notes.org --org

# Get JSON output for programmatic processing
python get-collection-annots.py ABC123DEF --library-id 12345 --output collection_data.json
```

#### Attachment Export Examples

```bash
# Export all PDF/EPUB attachments from personal library
python export-attachments.py library

# Export from specific group library to custom folder
python export-attachments.py library --library-id 12345 --target my_papers

# Export from collection in personal library
python export-attachments.py collection ABC123DEF

# Export from group library collection with custom settings
python export-attachments.py collection XYZ789GHI --library-id 12345 --target research_collection

# Export only PDFs without markdown conversion
python export-attachments.py library --types pdf --no-convert

# Export with verbose output and save summary
python export-attachments.py collection ABC123DEF --verbose --output-summary export_summary.json

# Dry run to see what would be exported
python export-attachments.py --dry-run library
```

**Output Structure:**
```
target_folder/
├── originals/          # Original PDF/EPUB files (named with citation keys)
│   ├── smith2023.pdf
│   └── jones2024.epub
└── markdown/           # Converted markdown files with YAML frontmatter
    ├── smith2023.md
    └── jones2024.md
```

**Markdown File Format:**
```yaml
---
title: "Original Document Title"
author: "Author Name"
year: 2023
citation_key: "smith2023"
zotero_key: "ABC123DEF"
original_file: "../originals/smith2023.pdf"
item_type: "journalArticle"
publication: "Journal Name"
doi: "10.1000/123456"
---

# Document content converted to markdown by markitdown
...
```

### Emacs Lisp Implementation

#### Interactive Usage (within Emacs)
```elisp
;; Load the packages
(require 'zotero-api)
(require 'org-zotero-client)

;; Get annotations for an item
(zotero-get-all-annotations-for-item "ITEM_ID")

;; Insert annotations at point (org-mode format)
(zotero-insert-item-annotations "ITEM_ID")

;; Insert annotations at point (markdown format)
(zotero-insert-item-annotations-markdown "ITEM_ID")

;; Save annotations to file (org-mode format)
(zotero-save-item-annotations-to-file "ITEM_ID" "output.org")

;; Save annotations to file (markdown format)
(zotero-save-item-annotations-to-markdown-file "ITEM_ID" "output.md")

;; Extract collection annotations interactively (with prompts for library/collection selection)
(org-zotero-extract-collection-annotations-interactive)

;; Extract collection annotations directly
(org-zotero-extract-collection-annotations "COLLECTION_ID")

;; Extract from group library collection
(org-zotero-extract-collection-annotations "COLLECTION_ID" "LIBRARY_ID")

;; Browse libraries
(zotero-browse-libraries)

;; Browse items
(zotero-browse-items)

;; Select library interactively
(org-zotero-select-library)

;; Select collection from library
(org-zotero-select-collection "LIBRARY_ID")
```

#### Collection Annotation Workflow Examples

```elisp
;; Interactive workflow - prompts for everything
M-x org-zotero-extract-collection-annotations-interactive

;; Direct extraction to specific file
(org-zotero-extract-collection-annotations "ABC123DEF" nil "~/research/my-collection.org")

;; Extract from group library with interactive file selection
(org-zotero-extract-collection-annotations "XYZ789GHI" "12345")
```

#### Command-line Usage (batch mode)
```bash
cd elisp/

# Get annotations (requires emacs with request package)
emacs --batch -l zotero-api.el -l zotero-get-annots.el \
  --eval "(zotero-cli-get-annotations \"ITEM_ID\")"

# Get annotations in org-mode format
emacs --batch -l zotero-api.el -l zotero-get-annots.el \
  --eval "(zotero-cli-get-annotations \"ITEM_ID\" t)"

# Test the implementation
emacs --batch --script test-zotero.el
```

### Dependencies

#### Python
- `requests` for HTTP communication
- `json` for data serialization  
- `sys` for command-line arguments
- `typing` for type hints
- `urllib.parse` for URL handling

#### Emacs Lisp
- `request` package for HTTP communication
- `json` for data serialization
- Built-in Emacs functions for org-mode integration

## API Endpoints Used

- `/items/{item_id}` - Get item details
- `/items/{item_id}/children` - Get attachments and annotations
- Filters for `itemType: 'attachment'` and `contentType: 'application/pdf'`
- Filters for `itemType: 'annotation'`

## Output Format

The script outputs annotation data both to console and saves complete results to `annotations_{item_id}.json`.

## API Documentation

Complete API documentation is available in `doc_resources/zotero_local_api_reference.md`.

### Key API Learnings

- **User ID**: Use `0` as userID for current logged-in user (not `1`)
- **Base URL**: `http://localhost:23119/api/`
- **Authentication**: None required for local API
- **Limitations**: Read-only access, current user only, no group write access
- **Annotation Storage**: Annotations are stored as top-level items with `parentItem` field pointing to attachment, not as children of attachments

### Available Functions

The `ZoteroLocalAPI` class now includes:

**Library Management:**
- `get_libraries()` - Get all group libraries
- `get_library_info(library_id)` - Get specific library details
- `get_collections(library_id=None)` - Get collections from library

**Item Management:**
- `get_items(library_id=None, limit=25, item_type=None)` - Get items with filtering
- `get_top_level_items(library_id=None, limit=25)` - Get top-level items only
- `get_item_types()` - Get available item types
- `get_item(item_id)` - Get specific item
- `get_item_children(item_id)` - Get item's children (attachments, notes)

**Annotation Management:**
- `get_pdf_attachments(item_id)` - Get PDF attachments for item
- `get_attachment_annotations(attachment_id)` - Get annotations from attachment
- `get_all_annotations_for_item(item_id)` - Get all annotations for item
- `format_as_org_mode(annotations_data)` - Format annotations as org-mode text

**Collection Annotation Management:**
- `get_collection_items(collection_id, library_id=None, limit=100)` - Get items from a collection
- `get_collection_info(collection_id, library_id=None)` - Get collection metadata
- `get_all_collection_annotations(collection_id, library_id=None)` - Get all annotations from all items in a collection
- `format_collection_annotations_as_org(collection_data)` - Format collection annotations as org-mode text

**File Attachment Management:**
- `get_file_attachments(item_id, library_id=None, file_types=['pdf', 'epub'])` - Get file attachments for item
- `download_attachment_file(attachment_id, target_path, library_id=None)` - Download attachment to local filesystem
- `get_attachment_metadata(item_id, library_id=None)` - Extract metadata for YAML frontmatter

**Bulk Export Functions:**
- `export_library_attachments(library_id=None, target_folder='zotero_export', file_types=['pdf', 'epub'], convert_to_markdown=True)` - Export all attachments from library
- `export_collection_attachments(collection_id, library_id=None, target_folder='zotero_collection_export', file_types=['pdf', 'epub'], convert_to_markdown=True)` - Export all attachments from collection

### Testing Scripts

#### Python (`python/`)
- `list-libraries.py` - Comprehensive exploration of available data
- `get-annots.py` - Annotation retrieval with CLI interface
  - `python get-annots.py ITEM_ID` - JSON output
  - `python get-annots.py ITEM_ID --org` - Org-mode output
- `debug-annotations.py` - Debug annotation retrieval step by step
- `find-items-with-pdfs.py` - Find items that have PDF attachments
- `export-attachments.py` - Export PDF/EPUB attachments with markdown conversion
  - `python export-attachments.py library` - Export from personal library
  - `python export-attachments.py collection COLLECTION_ID` - Export from collection

#### Emacs Lisp (`elisp/`)
- `zotero-list-libraries.el` - Library and collection exploration
- `zotero-get-annots.el` - Annotation retrieval with interactive commands
- `test-zotero.el` - Test suite for all functionality
- `zotero-package.el` - Convenience package loader

### Common Endpoints

- `/api/users/0/items` - Personal library items
- `/api/users/0/collections` - Personal library collections
- `/api/users/0/groups` - User's groups
- `/api/groups/{groupID}/items` - Group library items
- `/api/itemTypes` - Available item types

## Project Structure and Modularity

- We are building a general-purpose module to explore the Zotero API
- Multiple scripts will be developed to explore different functionalities
- Basic functionality will be abstracted into files that can be loaded by an `__init__.py`
- After initial exploration, a proper module structure will be developed

## Local API References

- Zotero localapi docs are here: https://github.com/zotero/zotero/blob/c32e050c6ec0b27c153fd9f04f675620d2463e93/chrome/content/zotero/xpcom/server/server_localAPI.js

## Development Resources

- Save Zotero API documentation locally in the `doc_resources` folder to keep references and notes organized

## Annotation Storage and API Structures

- Annotations are typically stored as child items of PDF attachments
- Annotation objects contain metadata like type (highlight, note, etc.), text content, page number, and bounding coordinates
- API returns annotations with unique identifiers and relationships to parent documents
- Key annotation types include:
  - Text highlights (with color and text content)
  - Freeform notes
  - Ink/drawing annotations

## Elisp Code Formatting (MANDATORY AFTER EVERY CHANGE TO ELISP DIRECTORY)

**CRITICAL**: After EVERY code change, you MUST format the code before running tests. The formatter is NEVER wrong - if code looks wrongly formatted after running it, it's ALWAYS because of a mistake in the code.

```bash
# Format all Elisp files - RUN THIS AFTER EVERY CHANGE
Emacs -batch --eval "(progn (setq indent-tabs-mode nil) (dolist (file (directory-files \".\" t \"\\\\.el$\")) (find-file file) (setq indent-tabs-mode nil) (indent-region (point-min) (point-max)) (save-buffer) (kill-buffer)))"

# Alternative: Format a specific file
Emacs -batch --eval "(progn (find-file \"./elisp/org-zotero-client.el\") (setq indent-tabs-mode nil) (indent-region (point-min) (point-max)) (save-buffer))"

# Remove trailing whitespaces from all Elisp files
Emacs -batch --eval "(progn (dolist (file (directory-files \".\" t \"\\\\.el$\")) (find-file file) (delete-trailing-whitespace) (save-buffer) (kill-buffer)))"
```

**Debugging Syntax Errors**: If files fail to load due to syntax errors (missing parentheses, quotes, etc.), the formatter's indentation will reveal the problem:
1. Run the formatter on the broken file
2. Look for incorrectly indented lines - they indicate where parentheses/quotes are unbalanced
3. The formatter's indentation is ALWAYS correct, so trust it to find your syntax errors

**Trailing Whitespaces**: All code must be free of trailing whitespaces (spaces or tabs at the end of lines). Use the command above or configure your editor to automatically remove them on save.


### Self reference in code or commits

- **Important - Never self-reference in code or commits**: Do not mention Claude or include any self-referential messages in code or commit messages. Keep all content strictly professional and focused on the technical aspects.

## Linting and Code Quality (RUN AFTER FORMATTING)

**IMPORTANT**: After formatting your code, you MUST check for linting errors. Ignore "Cannot open load file" errors for dependencies.


```bash
# Run linting checks - RUN THIS AFTER FORMATTING
# Check byte-compilation warnings (excluding dependency errors)
emacs -batch -f batch-byte-compile *.el 2>&1 | grep -v "Cannot open load file" | grep -v "No such file or directory" | grep -E "(Warning|Error)"

# Check documentation strings
emacs -batch --eval "(progn (dolist (file (directory-files \".\" t \"\\\\.el$\")) (find-file file) (condition-case err (checkdoc-current-buffer t) (error (message \"Checkdoc error in %s: %s\" file err))) (kill-buffer)))"

# Check for specific warnings (useful for CI/CD)
emacs -batch -f batch-byte-compile *.el 2>&1 | grep -v "Cannot open load file" | grep -v "No such file or directory" | grep -v "clang-include-fixer.el" | grep -E "(Warning|Error)" && echo "Linting errors found!" && exit 1 || echo "No linting errors found"
```

**Common Linting Errors to Fix**:
- **Undefined functions/variables**: Add proper `require` statements or `declare-function`
- **Missing or incorrect docstrings**: Follow Emacs docstring conventions (first line < 80 chars, end with period)
- **Free variables**: Properly declare or bind all variables
- **Lexical binding**: Files should start with `;;; -*- lexical-binding: t -*-`
- **Obsolete functions**: Replace deprecated functions with modern alternatives

## Development Principles

- Always ensure the APIs of the two libraries (python and elisp) are perfectly aligned. If functionality is implemented in one library, ask to implement in the other.

## Known Quirks and Reminders

- You frequently forget the final parenthesis in elisp functions.

## Development Conventions

- Always preface pop and python commands with uv

## Development Guidelines

- Avoid creating new files unless absolutely necessary.
