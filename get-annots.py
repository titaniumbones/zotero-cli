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
    
    def get_item(self, item_id: str) -> Optional[Dict[Any, Any]]:
        """
        Get a single item by ID
        
        Args:
            item_id: Zotero item ID
            
        Returns:
            Item data as dictionary, or None if not found
        """
        return self._make_request(f"/items/{item_id}")
    
    def get_item_children(self, item_id: str) -> List[Dict[Any, Any]]:
        """
        Get all children of an item (attachments, notes, etc.)
        
        Args:
            item_id: Zotero item ID
            
        Returns:
            List of child items
        """
        response = self._make_request(f"/items/{item_id}/children")
        return response if response is not None else []
    
    def get_pdf_attachments(self, item_id: str) -> List[Dict[Any, Any]]:
        """
        Get all PDF attachments for a given item
        
        Args:
            item_id: Zotero item ID
            
        Returns:
            List of PDF attachment items
        """
        children = self.get_item_children(item_id)
        pdf_attachments = []
        
        for child in children:
            if (child.get('data', {}).get('itemType') == 'attachment' and 
                child.get('data', {}).get('contentType') == 'application/pdf'):
                pdf_attachments.append(child)
        
        return pdf_attachments
    
    def get_attachment_annotations(self, attachment_id: str) -> List[Dict[Any, Any]]:
        """
        Get all annotations for a PDF attachment
        
        Args:
            attachment_id: Zotero attachment item ID
            
        Returns:
            List of annotation items
        """
        response = self._make_request(f"/items/{attachment_id}/children")
        if response is None:
            return []
        
        # Filter for annotation items
        annotations = []
        for item in response:
            if item.get('data', {}).get('itemType') == 'annotation':
                annotations.append(item)
        
        return annotations
    
    def get_all_annotations_for_item(self, item_id: str) -> Dict[str, Any]:
        """
        Get all annotations from all PDF attachments for a given item
        
        Args:
            item_id: Zotero item ID
            
        Returns:
            Dictionary containing item info and all annotations organized by attachment
        """
        # Get the main item
        item = self.get_item(item_id)
        if not item:
            return {"error": f"Item {item_id} not found"}
        
        # Get PDF attachments
        pdf_attachments = self.get_pdf_attachments(item_id)
        
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
            annotations = self.get_attachment_annotations(attachment_id)
            
            attachment_data = {
                "attachment_id": attachment_id,
                "attachment_title": attachment_title,
                "filename": attachment.get('data', {}).get('filename', 'Unknown'),
                "annotations_count": len(annotations),
                "annotations": annotations
            }
            
            result["attachments"].append(attachment_data)
        
        return result


def main():
    """Main function to demonstrate usage"""
    if len(sys.argv) != 2:
        print("Usage: python zotero_annotations.py <item_id>")
        print("Example: python zotero_annotations.py ABCD1234")
        sys.exit(1)
    
    item_id = sys.argv[1]
    
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
    
    # Save to JSON file
    output_file = f"annotations_{item_id}.json"
    with open(output_file, 'w', encoding='utf-8') as f:
        json.dump(result, f, ensure_ascii=False, indent=2)
    
    print(f"\nFull results saved to: {output_file}")


if __name__ == "__main__":
    main()


# Example usage as a module:
# 
# from zotero_annotations import ZoteroLocalAPI
# 
# api = ZoteroLocalAPI()
# annotations = api.get_all_annotations_for_item("YOUR_ITEM_ID")
# print(json.dumps(annotations, indent=2))
