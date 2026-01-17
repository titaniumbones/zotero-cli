# Collection Annotation Extraction

This feature allows you to extract all annotations from every item in a Zotero collection, providing a comprehensive overview of your annotated research within a specific topic or project.

## Overview

The collection annotation extraction feature:

- **Extracts all annotations** from all items with PDF attachments in a collection
- **Works with both personal and group libraries**
- **Provides interactive selection** of libraries and collections (Emacs Lisp)
- **Outputs in org-mode format** with proper metadata and citations
- **Maintains API parity** between Python and Emacs Lisp implementations

## Use Cases

- **Research projects**: Gather all annotations from papers related to a specific topic
- **Literature reviews**: Compile annotations from a curated collection of papers
- **Course materials**: Extract annotations from assigned readings
- **Collaborative research**: Work with annotations from group library collections

## Python Implementation

### Basic Usage

```bash
# Extract collection annotations in org-mode format
python get-collection-annots.py <collection_id> --org

# Extract from group library
python get-collection-annots.py <collection_id> --library-id <library_id> --org

# Save to specific file
python get-collection-annots.py <collection_id> --output research_notes.org --org
```

### Command Line Options

- `<collection_id>`: Required. The Zotero collection ID
- `--library-id <id>`: Optional. Group library ID (omit for personal library)
- `--output <file>`: Optional. Output file path (auto-generated if not specified)
- `--org`: Optional. Output in org-mode format (default is JSON)

### Examples

```bash
# Personal library collection
python get-collection-annots.py ABC123DEF --org

# Group library collection with custom output
python get-collection-annots.py XYZ789GHI --library-id 12345 --output my_research.org --org

# JSON output for programmatic processing
python get-collection-annots.py ABC123DEF --output collection_data.json
```

## Emacs Lisp Implementation

### Interactive Commands

#### `org-zotero-extract-collection-annotations-interactive`

The main interactive command that provides a guided workflow:

1. **Select library**: Choose from personal library or available group libraries
2. **Select collection**: Choose from collections in the selected library
3. **Choose output file**: Specify where to save the annotations

```elisp
M-x org-zotero-extract-collection-annotations-interactive
```

#### `org-zotero-extract-collection-annotations`

Direct function for programmatic use:

```elisp
;; Extract from personal library collection
(org-zotero-extract-collection-annotations "COLLECTION_ID")

;; Extract from group library collection
(org-zotero-extract-collection-annotations "COLLECTION_ID" "LIBRARY_ID")

;; Extract to specific file
(org-zotero-extract-collection-annotations "COLLECTION_ID" "LIBRARY_ID" "~/research/output.org")
```

### Helper Functions

```elisp
;; Select library interactively
(org-zotero-select-library)

;; Select collection from specific library
(org-zotero-select-collection "LIBRARY_ID")
```

## Output Format

The generated org-mode file includes:

### Collection Header
```org
* Collection: [Collection Name]
  :PROPERTIES:
  :COLLECTION_ID: ABC123DEF
  :LIBRARY_ID: 12345
  :TOTAL_ITEMS: 45
  :ITEMS_WITH_ANNOTATIONS: 12
  :END:
```

### Item Sections
```org
** [Item Title]
  :PROPERTIES:
  :ITEM_TYPE: journalArticle
  :ZOTERO_KEY: ITEM123
  :END:

*** [PDF Attachment Name]
   :PROPERTIES:
   :ATTACHMENT_ID: ATTACH456
   :FILENAME: paper.pdf
   :END:

#+BEGIN_QUOTE
[Annotation text with proper encoding fixes]

[cite:@citation_key, p.123]
#+END_QUOTE

*Comment:* [User's annotation comment if present]
```

## Finding Collection IDs

### Method 1: Using the Interactive Commands (Recommended)
Use `org-zotero-extract-collection-annotations-interactive` in Emacs, which will show you collection names and let you select them.

### Method 2: Using the API
```bash
# List all collections in personal library
curl "http://localhost:23119/api/users/0/collections" | jq '.[] | {name: .data.name, key: .key}'

# List collections in group library
curl "http://localhost:23119/api/groups/LIBRARY_ID/collections" | jq '.[] | {name: .data.name, key: .key}'
```

### Method 3: From Zotero Desktop
Collection IDs can be found in the Zotero desktop application URL when viewing a collection.

## API Reference

Both implementations provide equivalent functions:

### Python (`ZoteroLocalAPI` class)
- `get_collection_items(collection_id, library_id=None, limit=100)`
- `get_collection_info(collection_id, library_id=None)`
- `get_all_collection_annotations(collection_id, library_id=None)`
- `format_collection_annotations_as_org(collection_data)`

### Emacs Lisp
- `zotero-get-collection-items(collection-id &optional library-id limit)`
- `zotero-get-collection-info(collection-id &optional library-id)`
- `zotero-get-all-collection-annotations(collection-id &optional library-id)`
- `zotero-format-collection-annotations-as-org(collection-data)`

## Technical Notes

- **Collection filtering**: Uses the correct Zotero API endpoint `/collections/{id}/items`
- **Large collections**: Handles collections with hundreds of items efficiently
- **Progress reporting**: Shows item counts and processing status
- **Error handling**: Graceful handling of missing collections or access issues
- **Citation integration**: Automatically extracts BibTeX citation keys for org-cite format
- **Text encoding**: Applies comprehensive Unicode normalization for clean output

## Troubleshooting

### "Collection not found" Error
- Verify the collection ID is correct
- Ensure you have access to the specified library
- Check that Zotero is running with the local API enabled

### Empty Results
- Confirm the collection contains items with PDF attachments
- Check that the PDFs have annotations in Zotero
- Verify library permissions for group libraries

### Large Collections
- For very large collections (>1000 items), processing may take several minutes
- Consider using smaller collections or filtering items first
- Monitor system resources during processing