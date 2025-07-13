# Python Implementation

Python client for Zotero local API with command-line tools and org-mode file processing capabilities.

## 🚀 Quick Start

```bash
# Install dependencies
pip install requests bibtexparser

# Get annotations in JSON format
python get-annots.py ITEM_ID

# Get annotations in org-mode format
python get-annots.py ITEM_ID --org

# Explore your Zotero library
python list-libraries.py

# Process org-mode files
python org_zotero_client.py process notes.org output.org
```

## 📦 Installation

### Prerequisites

- Python 3.6+
- Zotero Desktop with local API enabled

### Install Dependencies

```bash
pip install requests bibtexparser
```

## 📋 Available Commands

### Core Scripts

| Script | Description | Usage |
|--------|-------------|-------|
| `get-annots.py` | Retrieve annotations for specific items | `python get-annots.py ITEM_ID [--org]` |
| `list-libraries.py` | List available libraries and collections | `python list-libraries.py` |
| `org_zotero_client.py` | Process org-mode files with citations | `python org_zotero_client.py COMMAND [args]` |

### Org-Mode File Processing

The `org_zotero_client.py` script provides several commands:

| Command | Description | Usage |
|---------|-------------|-------|
| `process` | Process all citations in org file | `python org_zotero_client.py process input.org output.org` |
| `list` | List all citations found in file | `python org_zotero_client.py list input.org` |
| `fetch` | Fetch annotations for specific item | `python org_zotero_client.py fetch ITEM_ID` |
| `set-library` | Set library ID for org file | `python org_zotero_client.py set-library input.org` |

## 📝 Usage Examples

### Basic Annotation Retrieval

```bash
# Get annotations in JSON format
python get-annots.py YW7IQA55

# Get annotations formatted as org-mode text
python get-annots.py YW7IQA55 --org

# Work with group libraries
python get-annots.py YW7IQA55 --library-id 5895731
```

### Library Exploration

```bash
# List all libraries
python list-libraries.py

# List items in specific library
python list-libraries.py --library-id 5895731

# Find items with PDF attachments
python find-items-with-pdfs.py --library-id 5895731
```

### Org-Mode File Processing

```bash
# Process org file with citations
python org_zotero_client.py process research-notes.org annotated-notes.org

# List all citations in file
python org_zotero_client.py list research-notes.org

# Set library ID for file (interactive)
python org_zotero_client.py set-library research-notes.org

# Fetch annotations for specific item
python org_zotero_client.py fetch YW7IQA55 --org
```

## 🔧 Configuration

### Working with Group Libraries

Set library ID in your org files:

```org
#+ZOTERO_LIBRARY_ID: 5895731
#+BIBLIOGRAPHY: references.bib

Your content with citations @author2023 here.
```

### Citation Formats

The tools support multiple citation formats:

```org
# Simple citations
This paper @cowanMoreWorkMother1983 discusses findings.

# Org-cite format
See [cite:@smith2023] for methodology.

# BibTeX integration
Citations are resolved using the bibliography file.
```

### BibTeX Integration

For citation key resolution:

1. Export BibTeX from Zotero with `key` field included
2. Reference the BibTeX file in your org document
3. Citation keys in org file match BibTeX entries

Example BibTeX entry:
```bibtex
@article{smith2023,
  author = {Smith, John},
  title = {Research Paper},
  key = {YW7IQA55},
  year = {2023}
}
```

## 🔍 Troubleshooting

### Common Issues

1. **Connection errors**
   - Ensure Zotero Desktop is running
   - Check that local API is enabled (default port 23119)

2. **Item not found**
   - Verify item ID is correct
   - Check library ID if working with group libraries

3. **No annotations found**
   - Confirm item has PDF attachments
   - Verify PDFs contain highlighted text or notes

4. **Citation key not resolved**
   - Check BibTeX file exists and is readable
   - Ensure `key` field is present in BibTeX entries
   - Verify `#+BIBLIOGRAPHY:` header in org file

### Debug Mode

Enable debug output:
```bash
python get-annots.py ITEM_ID --debug
```

This will show detailed API requests and responses.

## 📚 API Reference

### ZoteroLocalAPI Class

The core `ZoteroLocalAPI` class provides:

| Method | Description | Returns |
|--------|-------------|---------|
| `get_libraries()` | Get all available libraries | List of library objects |
| `get_items(library_id)` | Get items from library | List of item objects |
| `get_item(item_id, library_id)` | Get single item | Item object |
| `get_item_children(item_id, library_id)` | Get item attachments | List of attachment objects |
| `get_all_annotations_for_item(item_id, library_id)` | Get annotations | Annotation data |
| `format_as_org_mode(annotations_data)` | Format as org-mode | String |

### Command Line Arguments

#### get-annots.py
- `item_id` - Zotero item ID (required)
- `--org` - Output in org-mode format
- `--library-id ID` - Use specific library
- `--debug` - Enable debug output

#### org_zotero_client.py
- `process input.org output.org` - Process citations
- `list input.org` - List citations
- `fetch item_id` - Fetch specific item
- `set-library input.org` - Set library ID

## 🧪 Testing

Run individual scripts to test functionality:

```bash
# Test API connection
python list-libraries.py

# Test annotation retrieval
python get-annots.py YW7IQA55

# Test org-mode processing
python org_zotero_client.py list test/test.org
```

## 🤝 Contributing

When adding new features:

1. Update both command-line tools and library functions
2. Ensure compatibility with org-mode workflows
3. Add error handling and debug output
4. Test with various citation formats
5. Maintain API parity with Emacs Lisp implementation

## 📄 Files

- `get-annots.py` - Main annotation retrieval script
- `list-libraries.py` - Library and collection explorer  
- `org_zotero_client.py` - Org-mode file processing
- `find-items-with-pdfs.py` - Find items with PDF attachments
- `debug-annotations.py` - Debug annotation retrieval
- `test-specific-annotation.py` - Test specific annotations
- `debug-api.py` - API endpoint testing
- `explore-annotations.py` - Explore annotation structure
- `../doc_resources/zotero_local_api_reference.md` - Complete API documentation
