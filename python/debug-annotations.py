#!/usr/bin/env python3
"""
Debug script to trace annotation retrieval step by step
"""

import json
import sys
import os
import importlib.util

# Import the module with hyphens in the filename
spec = importlib.util.spec_from_file_location("get_annots", "./get-annots.py")
get_annots = importlib.util.module_from_spec(spec)
spec.loader.exec_module(get_annots)

ZoteroLocalAPI = get_annots.ZoteroLocalAPI

def debug_item_annotations(item_id):
    """Debug annotation retrieval step by step"""
    api = ZoteroLocalAPI()
    
    print(f"=== Debugging annotations for item: {item_id} ===")
    
    # Step 1: Get the main item
    print("\n1. Getting main item...")
    item = api.get_item(item_id)
    if not item:
        print(f"❌ Item {item_id} not found")
        return
    
    print(f"✅ Found item: {item.get('data', {}).get('title', 'Unknown')}")
    print(f"   Type: {item.get('data', {}).get('itemType', 'Unknown')}")
    
    # Step 2: Get children of the main item
    print("\n2. Getting children of main item...")
    children = api.get_item_children(item_id)
    print(f"   Found {len(children)} children")
    
    for i, child in enumerate(children):
        data = child.get('data', {})
        print(f"   Child {i+1}: {data.get('itemType', 'Unknown')} - {data.get('title', 'Unknown')}")
        if data.get('itemType') == 'attachment':
            print(f"     Content-Type: {data.get('contentType', 'Unknown')}")
            print(f"     Filename: {data.get('filename', 'Unknown')}")
    
    # Step 3: Filter for PDF attachments
    print("\n3. Filtering for PDF attachments...")
    pdf_attachments = api.get_pdf_attachments(item_id)
    print(f"   Found {len(pdf_attachments)} PDF attachments")
    
    for i, attachment in enumerate(pdf_attachments):
        data = attachment.get('data', {})
        print(f"   PDF {i+1}: {data.get('filename', 'Unknown')}")
        print(f"     Key: {attachment.get('key', 'Unknown')}")
    
    # Step 4: For each PDF attachment, get its annotations
    print("\n4. Getting annotations for each PDF attachment...")
    total_annotations = 0
    
    for i, attachment in enumerate(pdf_attachments):
        attachment_id = attachment['key']
        attachment_title = attachment.get('data', {}).get('filename', 'Unknown')
        
        print(f"\n   PDF {i+1}: {attachment_title} (ID: {attachment_id})")
        
        # Get children of this attachment (these should be annotations)
        attachment_children = api.get_item_children(attachment_id)
        print(f"     Found {len(attachment_children)} children of attachment")
        
        for j, child in enumerate(attachment_children):
            data = child.get('data', {})
            print(f"     Child {j+1}: {data.get('itemType', 'Unknown')}")
            if data.get('itemType') == 'annotation':
                print(f"       Annotation Type: {data.get('annotationType', 'Unknown')}")
                text = data.get('annotationText', '')
                if text:
                    print(f"       Text: {text[:50]}...")
        
        # Use the specific annotation function
        annotations = api.get_attachment_annotations(attachment_id)
        print(f"     ✅ Found {len(annotations)} annotations using get_attachment_annotations()")
        total_annotations += len(annotations)
    
    print(f"\n=== SUMMARY ===")
    print(f"Item: {item.get('data', {}).get('title', 'Unknown')}")
    print(f"Total children: {len(children)}")
    print(f"PDF attachments: {len(pdf_attachments)}")
    print(f"Total annotations: {total_annotations}")

def main():
    if len(sys.argv) != 2:
        print("Usage: python debug-annotations.py <item_id>")
        print("Example: python debug-annotations.py F7FGREW7")
        sys.exit(1)
    
    item_id = sys.argv[1]
    debug_item_annotations(item_id)

if __name__ == "__main__":
    main()