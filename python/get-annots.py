#!/usr/bin/env python3
"""
Zotero Local API Annotations Retriever

This script retrieves all annotations from PDF attachments for a given Zotero item
using the Zotero local API.
"""

import requests
import json
import sys
from typing import List, Dict, Any, Optional
from urllib.parse import urljoin


class ZoteroLocalAPI:
    """Class to interact with Zotero's local API"""
    
    def __init__(self, base_url: str = "http://localhost:23119"):
        """
        Initialize the Zotero Local API client
        
        Args:
            base_url: Base URL for Zotero local API (default: http://localhost:23119)
        """
        self.base_url = base_url.rstrip('/')
        self.session = requests.Session()
        
    def _make_request(self, endpoint: str) -> Optional[Dict[Any, Any]]:
        """
        Make a GET request to the Zotero local API
        
        Args:
            endpoint: API endpoint (will be joined with base_url)
            
        Returns:
            JSON response as dictionary, or None if request failed
        """
        url = urljoin(self.base_url + '/', endpoint.lstrip('/'))
        
        try:
            response = self.session.get(url)
            response.raise_for_status()
            return response.json()
        except requests.exceptions.RequestException as e:
            print(f"Error making request to {url}: {e}")
            return None
        except json.JSONDecodeError as e:
            print(f"Error parsing JSON response from {url}: {e}")
            return None
    
    def get_item(self, item_id: str, library_id: Optional[str] = None) -> Optional[Dict[Any, Any]]:
        """
        Get a single item by ID
        
        Args:
            item_id: Zotero item ID
            library_id: Library/group ID (if None, uses personal library)
            
        Returns:
            Item data as dictionary, or None if not found
        """
        if library_id:
            return self._make_request(f"/api/groups/{library_id}/items/{item_id}")
        else:
            return self._make_request(f"/api/users/0/items/{item_id}")
    
    def get_item_children(self, item_id: str, library_id: Optional[str] = None) -> List[Dict[Any, Any]]:
        """
        Get all children of an item (attachments, notes, etc.)
        
        Args:
            item_id: Zotero item ID
            library_id: Library/group ID (if None, uses personal library)
            
        Returns:
            List of child items
        """
        if library_id:
            response = self._make_request(f"/api/groups/{library_id}/items/{item_id}/children")
        else:
            response = self._make_request(f"/api/users/0/items/{item_id}/children")
        
        # Zotero API returns data directly as a list
        if response and isinstance(response, list):
            return response
        return []
    
    def get_pdf_attachments(self, item_id: str, library_id: Optional[str] = None) -> List[Dict[Any, Any]]:
        """
        Get all PDF attachments for a given item
        
        Args:
            item_id: Zotero item ID
            library_id: Library/group ID (if None, uses personal library)
            
        Returns:
            List of PDF attachment items
        """
        children = self.get_item_children(item_id, library_id)
        pdf_attachments = []
        
        for child in children:
            if (child.get('data', {}).get('itemType') == 'attachment' and 
                child.get('data', {}).get('contentType') == 'application/pdf'):
                pdf_attachments.append(child)
        
        return pdf_attachments
    
    def get_attachment_annotations(self, attachment_id: str, library_id: Optional[str] = None) -> List[Dict[Any, Any]]:
        """
        Get all annotations for a PDF attachment
        
        Args:
            attachment_id: Zotero attachment item ID
            library_id: Library/group ID (if None, uses personal library)
            
        Returns:
            List of annotation items
        """
        # First try the standard approach - get children of the attachment
        if library_id:
            response = self._make_request(f"/api/groups/{library_id}/items/{attachment_id}/children")
        else:
            response = self._make_request(f"/api/users/0/items/{attachment_id}/children")
        
        annotations = []
        if response and isinstance(response, list):
            # Filter for annotation items
            for item in response:
                if item.get('data', {}).get('itemType') == 'annotation':
                    annotations.append(item)
        
        # If no annotations found as children, try alternative approach:
        # Look for annotation items where parentItem matches our attachment_id
        if not annotations:
            # Get annotation items and filter by parent
            annotation_items = self.get_items(library_id=library_id, limit=1000, item_type="annotation")
            for item in annotation_items:
                if item.get('data', {}).get('parentItem') == attachment_id:
                    annotations.append(item)
        
        return annotations
    
    def get_libraries(self) -> List[Dict[str, Any]]:
        """
        Get all libraries (groups) available in Zotero
        
        Returns:
            List of library/group dictionaries
        """
        # Use user ID 0 (wildcard for current logged-in user) as per local API docs
        user_id = 0
        
        response = self._make_request(f"/api/users/{user_id}/groups")
        # Zotero API returns data directly as a list, not wrapped in a 'data' field
        if response and isinstance(response, list):
            return response
        return []
    
    def get_library_info(self, library_id: str) -> Optional[Dict[str, Any]]:
        """
        Get detailed information about a specific library/group
        
        Args:
            library_id: Library/group ID
            
        Returns:
            Library information dictionary, or None if not found
        """
        response = self._make_request(f"/api/groups/{library_id}")
        # Zotero API returns data directly, not wrapped in a 'data' field
        if response and isinstance(response, dict):
            return response
        return None
    
    def get_collections(self, library_id: Optional[str] = None) -> List[Dict[str, Any]]:
        """
        Get all collections from a library
        
        Args:
            library_id: Library/group ID (if None, uses personal library)
            
        Returns:
            List of collection dictionaries
        """
        if library_id:
            # Group library collections
            response = self._make_request(f"/api/groups/{library_id}/collections")
        else:
            # Personal library collections (use user ID 0)
            response = self._make_request(f"/api/users/0/collections")
        
        # Zotero API returns data directly as a list
        if response and isinstance(response, list):
            return response
        return []
    
    def get_items(self, library_id: Optional[str] = None, limit: int = 25, item_type: Optional[str] = None) -> List[Dict[str, Any]]:
        """
        Get items from a library
        
        Args:
            library_id: Library/group ID (if None, uses personal library)
            limit: Maximum number of items to return
            item_type: Filter by item type (e.g., 'book', 'journalArticle')
            
        Returns:
            List of item dictionaries
        """
        params = f"?limit={limit}"
        if item_type:
            params += f"&itemType={item_type}"
        
        if library_id:
            # Group library items
            response = self._make_request(f"/api/groups/{library_id}/items{params}")
        else:
            # Personal library items (use user ID 0)
            response = self._make_request(f"/api/users/0/items{params}")
        
        # Zotero API returns data directly as a list
        if response and isinstance(response, list):
            return response
        return []
    
    def get_item_types(self) -> List[Dict[str, Any]]:
        """
        Get all available item types
        
        Returns:
            List of item type dictionaries
        """
        response = self._make_request("/api/itemTypes")
        return response if response is not None else []
    
    def get_top_level_items(self, library_id: Optional[str] = None, limit: int = 25) -> List[Dict[str, Any]]:
        """
        Get top-level items (excluding child items like attachments)
        
        Args:
            library_id: Library/group ID (if None, uses personal library)
            limit: Maximum number of items to return
            
        Returns:
            List of top-level item dictionaries
        """
        params = f"?limit={limit}&top=1"
        
        if library_id:
            response = self._make_request(f"/api/groups/{library_id}/items{params}")
        else:
            response = self._make_request(f"/api/users/0/items{params}")
        
        # Zotero API returns data directly as a list
        if response and isinstance(response, list):
            return response
        return []
    
    def get_all_annotations_for_item(self, item_id: str, library_id: Optional[str] = None) -> Dict[str, Any]:
        """
        Get all annotations from all PDF attachments for a given item
        
        Args:
            item_id: Zotero item ID
            library_id: Library/group ID (if None, uses personal library)
            
        Returns:
            Dictionary containing item info and all annotations organized by attachment
        """
        # Get the main item
        item = self.get_item(item_id, library_id)
        if not item:
            return {"error": f"Item {item_id} not found"}
        
        # Get PDF attachments
        pdf_attachments = self.get_pdf_attachments(item_id, library_id)
        
        result = {
            "item_id": item_id,
            "item_title": item.get('data', {}).get('title', 'Unknown'),
            "item_type": item.get('data', {}).get('itemType', 'Unknown'),
            "attachments": []
        }
        
        for attachment in pdf_attachments:
            attachment_id = attachment['key']
            attachment_title = attachment.get('data', {}).get('title', 'Unknown')
            
            # Get annotations for this attachment
            annotations = self.get_attachment_annotations(attachment_id, library_id)
            
            attachment_data = {
                "attachment_id": attachment_id,
                "attachment_title": attachment_title,
                "filename": attachment.get('data', {}).get('filename', 'Unknown'),
                "annotations_count": len(annotations),
                "annotations": annotations
            }
            
            result["attachments"].append(attachment_data)
        
        return result
    
    def normalize_text_encoding(self, text: str) -> str:
        """
        Fix common UTF-8/Latin-1 encoding issues in annotation text.
        
        Args:
            text: Raw text that may have encoding issues
            
        Returns:
            Text with corrected character encoding
        """
        if not text:
            return text
            
        # Common character replacements for UTF-8 decoded as Latin-1
        replacements = {
            # Smart quotes and dashes - corrected mappings
            'â': '"',  # Corrupted left double quotation mark
            'â': '"',  # Corrupted right double quotation mark  
            'â': "'",  # Corrupted left single quotation mark
            'â': "'",  # Corrupted right single quotation mark
            'â': '—',  # Corrupted em dash
            'â': '–',  # Corrupted en dash
            
            # Accented characters
            'Ã¡': 'á',  # a with acute
            'Ã©': 'é',  # e with acute
            'Ã­': 'í',  # i with acute
            'Ã³': 'ó',  # o with acute
            'Ãº': 'ú',  # u with acute
            'Ã±': 'ñ',  # n with tilde
            'Ã': 'À',   # A with grave
            'Ã¨': 'è',  # e with grave
            'Ã¬': 'ì',  # i with grave
            'Ã²': 'ò',  # o with grave
            'Ã¹': 'ù',  # u with grave
            'Ã¤': 'ä',  # a with diaeresis
            'Ã«': 'ë',  # e with diaeresis
            'Ã¯': 'ï',  # i with diaeresis
            'Ã¶': 'ö',  # o with diaeresis
            'Ã¼': 'ü',  # u with diaeresis
            'Ã§': 'ç',  # c with cedilla
            
            # Other common issues
            'â¢': '•',  # Bullet point
            'â¦': '…',  # Horizontal ellipsis
            'Â°': '°',  # Degree symbol
            'Â±': '±',  # Plus-minus sign
            'Â²': '²',  # Superscript 2
            'Â³': '³',  # Superscript 3
            'Â½': '½',  # Fraction 1/2
            'Â¼': '¼',  # Fraction 1/4
            'Â¾': '¾',  # Fraction 3/4
            'Â©': '©',  # Copyright symbol
            'Â®': '®',  # Registered trademark
            'â¹': '‹',  # Single left-pointing angle quotation mark
            'âº': '›',  # Single right-pointing angle quotation mark
            'Â«': '«',  # Left-pointing double angle quotation mark
            'Â»': '»',  # Right-pointing double angle quotation mark
            
            # Additional problematic sequences seen in the data
            'peÂºple': 'people',
            'pe"ºple': 'people',     # Alternative corruption pattern
            'Ã©lite': 'élite',
            'Ã±res': 'fires',
            'dÃ©cor': 'décor',
            'signiÃ±cant': 'significant',
            'deÃ±ciencies': 'deficiencies',
            'proÃ±tability': 'profitability',
            'contempoâraries': 'contemporaries',
            'contempo"raries': 'contemporaries',  # Alternative corruption pattern
            'houseâhold': 'household',
            'âassociationistsâ': '"associationists"',
            'âplace"': '"place"',
            'âpurchasing"': '"purchasing"',
            'âmaintain-ing': '"maintain-ing',
            'âdecentlyâ': '"decently"',
            'âseparate spheres.\'â': '"separate spheres.\'"',
            'heartâ is': 'heart" is',  # Specific corrupted quote pattern
            'âdogs': '"dogs',  # Specific corrupted quote pattern
            
            # Additional corruption patterns found in the data
            'as;9': 'as-',           # Common corruption pattern
            'as;9 ': 'as- ',         # Common corruption pattern with space
            'pa5-checks': 'paychecks',  # Specific corruption
            'th%.': 'they.',       # Corruption with punctuation 
            'th% ': 'they ',       # Corruption with space
            'th%': 'they',         # Corruption without space
            'undenl': 'under',       # Truncated/corrupted word
            'peºple': 'people',      # Specific corruption with ordinal symbol
            'contempo—raries': 'contemporaries',  # Specific corruption
            'contempo"raries': 'contemporaries',  # Alternative corruption pattern
            
            # Smart quote corruption patterns (where " appears inappropriately)
            'ex"pected': 'expected',    # Specific corruption in text
            'house"hold': 'household',  # Specific corruption 
            'house"wives': 'housewives', # Specific corruption
            'single"family': 'single-family',  # Specific corruption
            'well"publicized': 'well-publicized',  # Specific corruption
            'ration-ali\'ze': 'rationalize',  # Specific corruption with apostrophe
            'car"ried': 'carried',     # Specific corruption
            'in"dustrialization': 'industrialization',  # Specific corruption
            'self"sufficient': 'self-sufficient',  # Specific corruption
            'water"cooled': 'water-cooled',  # Specific corruption
            '"women\'s work"': '"women\'s work"',  # Keep proper quotes in quotes
            'home"places': 'home places',  # Specific corruption - add space
            'work"places': 'work places',  # Specific corruption - add space
            'rules"to': 'rules—to',    # Em-dash corruption (rules" should be rules—)
            'ourselves"generate': 'ourselves—generate',  # Em-dash corruption
            'home"namely': 'home—namely',  # Em-dash corruption
            
            # UTF-8 corruption patterns with specific byte sequences
            'rules"\x80\x94to': 'rules—to',    # Specific UTF-8 corrupted em-dash
            'ourselves"\x80\x94generate': 'ourselves—generate',  # UTF-8 corrupted em-dash
            'home"\x80\x94namely': 'home—namely',  # UTF-8 corrupted em-dash
            
            # Additional UTF-8 corruption patterns found in real data
            'rules"‚"to': 'rules—to',    # Alternative corruption representation
            'ourselves"‚"generate': 'ourselves—generate',  # Alternative corruption
            'home"‚"namely': 'home—namely',  # Alternative corruption
        }
        
        # Apply replacements
        normalized_text = text
        for wrong, correct in replacements.items():
            normalized_text = normalized_text.replace(wrong, correct)
            
        return normalized_text
    
    def format_as_org_mode(self, annotations_data: Dict[str, Any], citation_key: Optional[str] = None) -> str:
        """
        Format annotation data as org-mode text
        
        Args:
            annotations_data: Result from get_all_annotations_for_item
            citation_key: Optional BibTeX citation key for org-cite format
            
        Returns:
            Formatted org-mode string
        """
        if "error" in annotations_data:
            return f"# Error: {annotations_data['error']}\n"
        
        org_content = []
        
        # Main header with item info
        item_title = self.normalize_text_encoding(annotations_data.get('item_title', 'Unknown'))
        item_type = annotations_data.get('item_type', 'Unknown')
        item_id = annotations_data.get('item_id', 'Unknown')
        
        org_content.append(f"* {item_title}")
        org_content.append(f"  :PROPERTIES:")
        org_content.append(f"  :ITEM_TYPE: {item_type}")
        org_content.append(f"  :ZOTERO_KEY: {item_id}")
        org_content.append(f"  :END:")
        org_content.append("")
        
        # Process each PDF attachment
        for attachment in annotations_data.get('attachments', []):
            attachment_title = self.normalize_text_encoding(attachment.get('attachment_title', 'Unknown PDF'))
            attachment_id = attachment.get('attachment_id', 'Unknown')
            filename = attachment.get('filename', 'Unknown')
            annotations = attachment.get('annotations', [])
            
            # PDF header
            org_content.append(f"** {attachment_title}")
            org_content.append(f"   :PROPERTIES:")
            org_content.append(f"   :ATTACHMENT_ID: {attachment_id}")
            org_content.append(f"   :FILENAME: {filename}")
            org_content.append(f"   :END:")
            org_content.append("")
            
            if not annotations:
                org_content.append("   No annotations found.")
                org_content.append("")
                continue
            
            # Collect all annotation texts first
            annotation_texts = []
            comments = []
            
            for i, annotation in enumerate(annotations, 1):
                ann_data = annotation.get('data', {})
                ann_type = ann_data.get('annotationType', 'unknown')
                text = self.normalize_text_encoding(ann_data.get('annotationText', ''))
                comment = self.normalize_text_encoding(ann_data.get('annotationComment', ''))
                
                # Get page number if available
                page_label = ann_data.get('annotationPageLabel', '')
                position = ann_data.get('annotationPosition', {})
                page_index = position.get('pageIndex', '') if isinstance(position, dict) else ''
                
                # Create Zotero link
                zotero_link = f"zotero://select/library/items/{attachment_id}"
                if page_label:
                    zotero_link += f"?page={page_label}"
                elif page_index:
                    zotero_link += f"?page={page_index + 1}"  # Page index is 0-based
                
                # Collect annotation text with citation
                if text:
                    annotation_text = text
                    
                    # Add org-cite citation
                    if citation_key:
                        page_info = page_label if page_label else str(page_index + 1) if page_index else "?"
                        annotation_text += f"\n\n[cite:@{citation_key}, p.{page_info}]"
                    
                    annotation_texts.append(annotation_text)
                
                # Collect annotation comment
                if comment:
                    comments.append(f"*Comment:* {comment}")
            
            # Add single quote block for all annotations from this PDF
            if annotation_texts:
                org_content.append(f"#+BEGIN_QUOTE")
                org_content.append("\n\n".join(annotation_texts))
                org_content.append(f"#+END_QUOTE")
                org_content.append("")
            
            # Add comments after the quote block
            if comments:
                org_content.extend(comments)
                org_content.append("")
        
        return "\n".join(org_content)
    
    def format_as_markdown(self, annotations_data: Dict[str, Any], citation_key: Optional[str] = None) -> str:
        """
        Format annotation data as markdown text
        
        Args:
            annotations_data: Result from get_all_annotations_for_item
            citation_key: Optional BibTeX citation key for citations
            
        Returns:
            Formatted markdown string
        """
        if "error" in annotations_data:
            return f"# Error: {annotations_data['error']}\n"
        
        md_content = []
        
        # Main header with item info
        item_title = self.normalize_text_encoding(annotations_data.get('item_title', 'Unknown'))
        item_type = annotations_data.get('item_type', 'Unknown')
        item_id = annotations_data.get('item_id', 'Unknown')
        
        md_content.append(f"# {item_title}")
        md_content.append("")
        md_content.append(f"**Item Type:** {item_type}")
        md_content.append(f"**Zotero Key:** {item_id}")
        md_content.append("")
        
        # Process each PDF attachment
        for attachment in annotations_data.get('attachments', []):
            attachment_title = self.normalize_text_encoding(attachment.get('attachment_title', 'Unknown PDF'))
            attachment_id = attachment.get('attachment_id', 'Unknown')
            filename = attachment.get('filename', 'Unknown')
            annotations = attachment.get('annotations', [])
            
            # PDF header
            md_content.append(f"## {attachment_title}")
            md_content.append("")
            md_content.append(f"**Attachment ID:** {attachment_id}")
            md_content.append(f"**Filename:** {filename}")
            md_content.append("")
            
            if not annotations:
                md_content.append("No annotations found.")
                md_content.append("")
                continue
            
            # Collect all annotation texts first
            annotation_texts = []
            comments = []
            
            for i, annotation in enumerate(annotations, 1):
                ann_data = annotation.get('data', {})
                ann_type = ann_data.get('annotationType', 'unknown')
                text = self.normalize_text_encoding(ann_data.get('annotationText', ''))
                comment = self.normalize_text_encoding(ann_data.get('annotationComment', ''))
                
                # Get page number if available
                page_label = ann_data.get('annotationPageLabel', '')
                position = ann_data.get('annotationPosition', {})
                page_index = position.get('pageIndex', '') if isinstance(position, dict) else ''
                
                # Create Zotero link
                zotero_link = f"zotero://select/library/items/{attachment_id}"
                if page_label:
                    zotero_link += f"?page={page_label}"
                elif page_index:
                    zotero_link += f"?page={page_index + 1}"  # Page index is 0-based
                
                # Collect annotation text with citation
                if text:
                    annotation_text = text
                    
                    # Add citation
                    if citation_key:
                        page_info = page_label if page_label else str(page_index + 1) if page_index else "?"
                        annotation_text += f"\n\n[cite:@{citation_key}, p.{page_info}]"
                    
                    annotation_texts.append(annotation_text)
                
                # Collect annotation comment
                if comment:
                    comments.append(f"**Comment:** {comment}")
            
            # Add single quote block for all annotations from this PDF
            if annotation_texts:
                md_content.append("::: .quote")
                md_content.append("\n\n".join(annotation_texts))
                md_content.append(":::")
                md_content.append("")
            
            # Add comments after the quote block
            if comments:
                md_content.extend(comments)
                md_content.append("")
        
        return "\n".join(md_content)
    
    def get_citation_key_for_item(self, item_id: str, library_id: Optional[str] = None) -> Optional[str]:
        """
        Get the BibTeX citation key for a Zotero item by exporting it as BibTeX.
        
        Args:
            item_id: Zotero item ID
            library_id: Optional library ID for group libraries
            
        Returns:
            BibTeX citation key or None if not found
        """
        try:
            # Export the item as BibTeX
            bibtex_data = self.export_item_bibtex(item_id, library_id)
            if not bibtex_data:
                return None
            
            # Parse the BibTeX to extract the citation key
            import re
            # Match @type{citation_key, ...
            match = re.search(r'@\w+\s*{\s*([^,\s]+)\s*,', bibtex_data)
            if match:
                return match.group(1)
            
            return None
            
        except Exception as e:
            print(f"Error getting citation key for item {item_id}: {e}")
            return None
    
    def export_item_bibtex(self, item_id: str, library_id: Optional[str] = None) -> Optional[str]:
        """
        Export a single item as BibTeX.
        
        Args:
            item_id: Zotero item ID
            library_id: Optional library ID for group libraries
            
        Returns:
            BibTeX string or None if export failed
        """
        try:
            if library_id:
                url = f"{self.base_url}/api/groups/{library_id}/items/{item_id}?format=bibtex"
            else:
                url = f"{self.base_url}/api/users/0/items/{item_id}?format=bibtex"
            
            response = requests.get(url)
            if response.status_code == 200:
                return response.text.strip()
            else:
                return None
                
        except Exception as e:
            return None
    
    def get_collection_items(self, collection_id: str, library_id: Optional[str] = None, limit: int = 100) -> List[Dict[str, Any]]:
        """
        Get all items from a specific collection
        
        Args:
            collection_id: Zotero collection ID
            library_id: Library/group ID (if None, uses personal library)
            limit: Maximum number of items to return (default: 100)
            
        Returns:
            List of item dictionaries from the collection
        """
        params = f"?limit={limit}"
        
        if library_id:
            response = self._make_request(f"/api/groups/{library_id}/collections/{collection_id}/items{params}")
        else:
            response = self._make_request(f"/api/users/0/collections/{collection_id}/items{params}")
        
        # Zotero API returns data directly as a list
        if response and isinstance(response, list):
            return response
        return []
    
    def get_collection_info(self, collection_id: str, library_id: Optional[str] = None) -> Optional[Dict[str, Any]]:
        """
        Get information about a specific collection
        
        Args:
            collection_id: Zotero collection ID
            library_id: Library/group ID (if None, uses personal library)
            
        Returns:
            Collection information dictionary, or None if not found
        """
        if library_id:
            response = self._make_request(f"/api/groups/{library_id}/collections/{collection_id}")
        else:
            response = self._make_request(f"/api/users/0/collections/{collection_id}")
        
        if response and isinstance(response, dict):
            return response
        return None
    
    def get_all_collection_annotations(self, collection_id: str, library_id: Optional[str] = None) -> Dict[str, Any]:
        """
        Get all annotations from all items in a collection
        
        Args:
            collection_id: Zotero collection ID
            library_id: Library/group ID (if None, uses personal library)
            
        Returns:
            Dictionary containing collection info and all annotations organized by item
        """
        # Get collection information
        collection_info = self.get_collection_info(collection_id, library_id)
        if not collection_info:
            return {"error": f"Collection {collection_id} not found"}
        
        # Get all items in the collection
        collection_items = self.get_collection_items(collection_id, library_id, limit=1000)
        
        result = {
            "collection_id": collection_id,
            "collection_name": collection_info.get('data', {}).get('name', 'Unknown'),
            "collection_parent": collection_info.get('data', {}).get('parentCollection', None),
            "library_id": library_id,
            "items_count": len(collection_items),
            "items": []
        }
        
        # Process each item in the collection
        for item in collection_items:
            item_id = item['key']
            item_data = item.get('data', {})
            item_type = item_data.get('itemType', 'unknown')
            
            # Skip attachments and annotations - we only want top-level items
            if item_type in ['attachment', 'note', 'annotation']:
                continue
            
            # Get annotations for this item
            item_annotations = self.get_all_annotations_for_item(item_id, library_id)
            
            # Only include items that have annotations
            if "error" not in item_annotations and item_annotations.get('attachments'):
                total_annotations = sum(att['annotations_count'] for att in item_annotations['attachments'])
                if total_annotations > 0:
                    result["items"].append(item_annotations)
        
        return result
    
    def format_collection_annotations_as_org(self, collection_data: Dict[str, Any]) -> str:
        """
        Format collection annotation data as org-mode text
        
        Args:
            collection_data: Result from get_all_collection_annotations
            
        Returns:
            Formatted org-mode string
        """
        if "error" in collection_data:
            return f"# Error: {collection_data['error']}\n"
        
        org_content = []
        
        # Main collection header
        collection_name = self.normalize_text_encoding(collection_data.get('collection_name', 'Unknown'))
        collection_id = collection_data.get('collection_id', 'Unknown')
        library_id = collection_data.get('library_id', 'Personal Library')
        items_count = collection_data.get('items_count', 0)
        items_with_annotations = len(collection_data.get('items', []))
        
        org_content.append(f"* Collection: {collection_name}")
        org_content.append(f"  :PROPERTIES:")
        org_content.append(f"  :COLLECTION_ID: {collection_id}")
        if library_id:
            org_content.append(f"  :LIBRARY_ID: {library_id}")
        org_content.append(f"  :TOTAL_ITEMS: {items_count}")
        org_content.append(f"  :ITEMS_WITH_ANNOTATIONS: {items_with_annotations}")
        org_content.append(f"  :END:")
        org_content.append("")
        
        if not collection_data.get('items'):
            org_content.append("No items with annotations found in this collection.")
            org_content.append("")
            return "\n".join(org_content)
        
        # Process each item with annotations
        for item_data in collection_data['items']:
            # Get citation key for org-cite format
            citation_key = self.get_citation_key_for_item(item_data['item_id'], library_id)
            
            # Format item annotations (use existing function but at sub-level)
            item_org = self.format_as_org_mode(item_data, citation_key)
            
            # Adjust heading levels (add one * to each heading)
            adjusted_lines = []
            for line in item_org.split('\n'):
                if line.startswith('*'):
                    adjusted_lines.append('*' + line)
                else:
                    adjusted_lines.append(line)
            
            org_content.extend(adjusted_lines)
            org_content.append("")
        
        return "\n".join(org_content)
    
    def format_collection_annotations_as_markdown(self, collection_data: Dict[str, Any]) -> str:
        """
        Format collection annotation data as markdown text
        
        Args:
            collection_data: Result from get_all_collection_annotations
            
        Returns:
            Formatted markdown string
        """
        if "error" in collection_data:
            return f"# Error: {collection_data['error']}\n"
        
        md_content = []
        
        # Main collection header
        collection_name = self.normalize_text_encoding(collection_data.get('collection_name', 'Unknown'))
        collection_id = collection_data.get('collection_id', 'Unknown')
        library_id = collection_data.get('library_id', 'Personal Library')
        items_count = collection_data.get('items_count', 0)
        items_with_annotations = len(collection_data.get('items', []))
        
        md_content.append(f"# Collection: {collection_name}")
        md_content.append("")
        md_content.append(f"**Collection ID:** {collection_id}")
        if library_id:
            md_content.append(f"**Library ID:** {library_id}")
        md_content.append(f"**Total Items:** {items_count}")
        md_content.append(f"**Items with Annotations:** {items_with_annotations}")
        md_content.append("")
        
        if not collection_data.get('items'):
            md_content.append("No items with annotations found in this collection.")
            md_content.append("")
            return "\n".join(md_content)
        
        # Process each item with annotations
        for item_data in collection_data['items']:
            # Get citation key for citations
            citation_key = self.get_citation_key_for_item(item_data['item_id'], library_id)
            
            # Format item annotations (use existing function but at sub-level)
            item_md = self.format_as_markdown(item_data, citation_key)
            
            # Adjust heading levels (add one # to each heading)
            adjusted_lines = []
            for line in item_md.split('\n'):
                if line.startswith('#'):
                    adjusted_lines.append('#' + line)
                else:
                    adjusted_lines.append(line)
            
            md_content.extend(adjusted_lines)
            md_content.append("")
        
        return "\n".join(md_content)


def main():
    """Main function to demonstrate usage"""
    if len(sys.argv) < 2 or len(sys.argv) > 3:
        print("Usage: python get-annots.py <item_id> [--org|--markdown]")
        print("Example: python get-annots.py ABCD1234")
        print("Example: python get-annots.py ABCD1234 --org")
        print("Example: python get-annots.py ABCD1234 --markdown")
        sys.exit(1)
    
    item_id = sys.argv[1]
    format_type = sys.argv[2] if len(sys.argv) == 3 else None
    org_mode = format_type == '--org'
    markdown_mode = format_type == '--markdown'
    
    # Initialize API client
    api = ZoteroLocalAPI()
    
    # Get all annotations for the item
    print(f"Retrieving annotations for item: {item_id}")
    result = api.get_all_annotations_for_item(item_id)
    
    if "error" in result:
        print(f"Error: {result['error']}")
        sys.exit(1)
    
    # Print summary
    print(f"\nItem: {result['item_title']} ({result['item_type']})")
    print(f"PDF Attachments: {len(result['attachments'])}")
    
    total_annotations = sum(att['annotations_count'] for att in result['attachments'])
    print(f"Total Annotations: {total_annotations}")
    
    # Print detailed results
    for i, attachment in enumerate(result['attachments'], 1):
        print(f"\n--- Attachment {i}: {attachment['attachment_title']} ---")
        print(f"File: {attachment['filename']}")
        print(f"Annotations: {attachment['annotations_count']}")
        
        for j, annotation in enumerate(attachment['annotations'], 1):
            ann_data = annotation.get('data', {})
            ann_type = ann_data.get('annotationType', 'unknown')
            text = ann_data.get('annotationText', '')
            comment = ann_data.get('annotationComment', '')
            
            print(f"  {j}. Type: {ann_type}")
            if text:
                print(f"     Text: {text[:100]}{'...' if len(text) > 100 else ''}")
            if comment:
                print(f"     Comment: {comment[:100]}{'...' if len(comment) > 100 else ''}")
    
    # Handle formatted output
    if org_mode or markdown_mode:
        # Get citation key for citations
        print("Getting citation key for citations...")
        citation_key = api.get_citation_key_for_item(item_id)
        if citation_key:
            print(f"Citation key: {citation_key}")
        else:
            print("Warning: Could not get citation key, links will be omitted")
        
        if org_mode:
            content = api.format_as_org_mode(result, citation_key)
            file_ext = "org"
            format_name = "ORG-MODE"
        else:  # markdown_mode
            content = api.format_as_markdown(result, citation_key)
            file_ext = "md"
            format_name = "MARKDOWN"
        
        # Save to file
        output_file = f"annotations_{item_id}.{file_ext}"
        with open(output_file, 'w', encoding='utf-8') as f:
            f.write(content)
        
        print(f"\n{format_name} output saved to: {output_file}")
        
        # Also print to console
        print("\n" + "="*50)
        print(f"{format_name} OUTPUT:")
        print("="*50)
        print(content)
    else:
        # Save to JSON file
        output_file = f"annotations_{item_id}.json"
        with open(output_file, 'w', encoding='utf-8') as f:
            json.dump(result, f, ensure_ascii=False, indent=2)
        
        print(f"\nFull results saved to: {output_file}")


if __name__ == "__main__":
    main()


# Example usage as a module:
# 
# from get_annots import ZoteroLocalAPI
# 
# api = ZoteroLocalAPI()
# 
# # Get all libraries
# libraries = api.get_libraries()
# print("Available libraries:", json.dumps(libraries, indent=2))
# 
# # Get specific library info
# if libraries:
#     lib_info = api.get_library_info(libraries[0]['id'])
#     print("Library info:", json.dumps(lib_info, indent=2))
# 
# # Get annotations for an item
# annotations = api.get_all_annotations_for_item("YOUR_ITEM_ID")
# print(json.dumps(annotations, indent=2))
