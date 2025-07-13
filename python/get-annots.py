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
    
    def format_as_org_mode(self, annotations_data: Dict[str, Any]) -> str:
        """
        Format annotation data as org-mode text
        
        Args:
            annotations_data: Result from get_all_annotations_for_item
            
        Returns:
            Formatted org-mode string
        """
        if "error" in annotations_data:
            return f"# Error: {annotations_data['error']}\n"
        
        org_content = []
        
        # Main header with item info
        item_title = annotations_data.get('item_title', 'Unknown')
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
            attachment_title = attachment.get('attachment_title', 'Unknown PDF')
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
            
            # Process each annotation
            for i, annotation in enumerate(annotations, 1):
                ann_data = annotation.get('data', {})
                ann_type = ann_data.get('annotationType', 'unknown')
                text = ann_data.get('annotationText', '')
                comment = ann_data.get('annotationComment', '')
                
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
                
                # Annotation header
                page_info = f" (p. {page_label})" if page_label else f" (p. {page_index + 1})" if page_index else ""
                org_content.append(f"*** {ann_type.capitalize()}{page_info}")
                
                # Annotation text
                if text:
                    org_content.append(f"#+BEGIN_QUOTE")
                    org_content.append(text)
                    org_content.append(f"#+END_QUOTE")
                    org_content.append("")
                
                # Annotation comment
                if comment:
                    org_content.append(f"*Comment:* {comment}")
                    org_content.append("")
                
                # Zotero link
                org_content.append(f"[[{zotero_link}][Open in Zotero]]")
                org_content.append("")
        
        return "\n".join(org_content)
    
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
                url = f"{self.api_base}/groups/{library_id}/items/{item_id}?format=bibtex"
            else:
                url = f"{self.api_base}/users/0/items/{item_id}?format=bibtex"
            
            response = requests.get(url)
            if response.status_code == 200:
                return response.text.strip()
            else:
                return None
                
        except Exception as e:
            return None


def main():
    """Main function to demonstrate usage"""
    if len(sys.argv) < 2 or len(sys.argv) > 3:
        print("Usage: python get-annots.py <item_id> [--org]")
        print("Example: python get-annots.py ABCD1234")
        print("Example: python get-annots.py ABCD1234 --org")
        sys.exit(1)
    
    item_id = sys.argv[1]
    org_mode = len(sys.argv) == 3 and sys.argv[2] == '--org'
    
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
    
    # Handle org-mode output
    if org_mode:
        org_content = api.format_as_org_mode(result)
        
        # Save to org file
        org_file = f"annotations_{item_id}.org"
        with open(org_file, 'w', encoding='utf-8') as f:
            f.write(org_content)
        
        print(f"\nOrg-mode output saved to: {org_file}")
        
        # Also print to console
        print("\n" + "="*50)
        print("ORG-MODE OUTPUT:")
        print("="*50)
        print(org_content)
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
