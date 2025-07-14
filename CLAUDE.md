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
- Formatting annotations as org-mode text

## Development Commands

### Python Implementation

```bash
cd python/

# Get annotations in JSON format
python get-annots.py <item_id>

# Get annotations in org-mode format  
python get-annots.py <item_id> --org

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

### Emacs Lisp Implementation

#### Interactive Usage (within Emacs)
```elisp
;; Load the packages
(require 'zotero-api)
(require 'org-zotero-client)

;; Get annotations for an item
(zotero-get-all-annotations-for-item "ITEM_ID")

;; Insert annotations at point
(zotero-insert-item-annotations "ITEM_ID")

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

### Testing Scripts

#### Python (`python/`)
- `list-libraries.py` - Comprehensive exploration of available data
- `get-annots.py` - Annotation retrieval with CLI interface
  - `python get-annots.py ITEM_ID` - JSON output
  - `python get-annots.py ITEM_ID --org` - Org-mode output
- `debug-annotations.py` - Debug annotation retrieval step by step
- `find-items-with-pdfs.py` - Find items that have PDF attachments

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

## Development Principles

- Always ensure the APIs of the two libraries (python and elisp) are perfectly aligned. If functionality is implemented in one library, ask to implement in the other.

## Known Quirks and Reminders

- You frequently forget the final parenthesis in elisp functions.

## Development Conventions

- Always preface pop and python commands with uv

## Development Guidelines

- Avoid creating new files unless absolutely necessary.