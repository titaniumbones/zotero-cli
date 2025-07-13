# Zotero Local API Reference

**Author**: Matt Price, 2025  
**Note**: Claude code was used to help write this code.

## Overview
- **Version**: 3
- **Base URL**: `http://localhost:23119/api/`
- **Access**: Local only, for logged-in user
- **Authentication**: None required (local access)

## Key Limitations
- Only data for the logged-in user is available locally
- Write access not supported
- Minimal group metadata access
- Atom format not supported
- Use userID `0` or the user's actual API user ID

## Supported Endpoints

### Core Information
- `GET /api/` - Root endpoint (returns "Nothing to see here.")
- `GET /api/schema` - JSON schema
- `GET /api/itemTypes` - List of item types with localized names
- `GET /api/itemFields` - List of fields with localized names
- `GET /api/itemTypeFields` - Fields for specific item types
- `GET /api/itemTypeCreatorTypes` - Creator types for item types
- `GET /api/creatorFields` - Available creator fields

### User Library Endpoints
- `GET /api/users/:userID/collections` - Collections in user library
- `GET /api/users/:userID/items` - Items in user library
- `GET /api/users/:userID/items/:itemKey` - Specific item
- `GET /api/users/:userID/items/:itemKey/children` - Item children (attachments, notes)
- `GET /api/users/:userID/tags` - Tags in user library
- `GET /api/users/:userID/searches` - Saved searches
- `GET /api/users/:userID/groups` - User's groups

### Group Library Endpoints
- `GET /api/groups/:groupID` - Group information
- `GET /api/groups/:groupID/collections` - Collections in group
- `GET /api/groups/:groupID/items` - Items in group
- `GET /api/groups/:groupID/items/:itemKey` - Specific item in group
- `GET /api/groups/:groupID/items/:itemKey/children` - Item children in group
- `GET /api/groups/:groupID/tags` - Tags in group
- `GET /api/groups/:groupID/searches` - Saved searches in group

## Query Parameters

### Pagination
- `start` - Starting index (default: 0)
- `limit` - Number of results (default: 25, max: 100)
- `sort` - Field to sort by
- `direction` - Sort order (asc/desc)

### Filtering
- `itemType` - Filter by item type
- `tag` - Filter by tag
- `collection` - Filter by collection
- `since` - Items modified since version
- `format` - Export format (bibtex, biblatex, json, ris, etc.)

### Special Parameters
- `top` - Top-level items only
- `trash` - Items in trash
- `include` - Include additional data (data, bib, citation)

## Export Formats
- `bibtex` - BibTeX format
- `biblatex` - BibLaTeX format
- `json` - CSL JSON
- `ris` - RIS format
- `coins` - COinS format
- `mods` - MODS XML
- `refer` - Refer format
- `rdf_bibliontology` - RDF Bibliontology
- `rdf_dc` - RDF Dublin Core
- `rdf_zotero` - RDF Zotero
- `tei` - TEI format
- `wikipedia` - Wikipedia citation

## Item Types
Common item types include:
- `book`, `bookSection`, `journalArticle`, `magazineArticle`, `newspaperArticle`
- `thesis`, `letter`, `manuscript`, `interview`, `film`, `artwork`
- `webpage`, `blogPost`, `forumPost`, `email`
- `conferencePaper`, `report`, `bill`, `case`, `hearing`
- `patent`, `statute`, `audioRecording`, `videoRecording`
- `tvBroadcast`, `radioBroadcast`, `podcast`, `computerProgram`
- `attachment`, `note`, `annotation`

## Accessing PDF Annotations

PDF annotations in Zotero are stored as **grandchildren** of the main library items. The hierarchy is:

1. **Main Item** (book, article, etc.) - Parent
2. **PDF Attachment** - Child of main item
3. **Annotation** - Child of PDF attachment (grandchild of main item)

### Annotation Access Workflow

1. **Get Main Item**: 
   ```
   GET /api/users/:userID/items/:itemKey
   ```

2. **Get PDF Attachments** (children of main item):
   ```
   GET /api/users/:userID/items/:itemKey/children
   ```
   Filter for `itemType: "attachment"` and `contentType: "application/pdf"`

3. **Get Annotations** (children of PDF attachment):
   ```
   GET /api/users/:userID/items/:pdfItemKey/children
   ```
   Filter for `itemType: "annotation"`

### Annotation Data Structure

Each annotation contains:
- `annotationType`: "highlight", "note", "image", "ink"
- `annotationText`: The highlighted text
- `annotationComment`: User's comment/note
- `annotationColor`: Color of the highlight
- `annotationPageLabel`: Page number
- `annotationPosition`: Position data (coordinates, etc.)
- `annotationSortIndex`: Sort order on page
- `dateAdded`: When annotation was created
- `dateModified`: When annotation was last modified

### Example API Calls

```bash
# Get main item
curl "http://localhost:23119/api/users/0/items/ABCD1234"

# Get PDF attachments
curl "http://localhost:23119/api/users/0/items/ABCD1234/children"

# Get annotations from PDF attachment
curl "http://localhost:23119/api/users/0/items/EFGH5678/children"
```

### Annotation Types

- **highlight**: Text highlighting with optional color
- **note**: Sticky note annotation
- **image**: Image/area annotation
- **ink**: Freehand drawing annotation

### Common Annotation Fields

- `annotationText`: Selected text (for highlights)
- `annotationComment`: User's note/comment
- `annotationColor`: Highlight color (hex format)
- `annotationPageLabel`: Page number as string
- `annotationPosition`: JSON position data
- `annotationSortIndex`: Order on page
- `parentItem`: Key of parent PDF attachment

## Usage Notes
- Pagination often unnecessary for local access due to speed
- Very fast local access compared to web API
- Supports executing saved searches locally
- Local API follows web API specification with adaptations
- Use userID `0` as wildcard for current logged-in user
- **Important**: Annotations are grandchildren, not direct children of main items