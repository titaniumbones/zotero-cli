#!/usr/bin/env python3
"""
Find items in your library that have PDF attachments
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

def find_items_with_pdfs():
    """Find items in library that have PDF attachments"""
    api = ZoteroLocalAPI()
    
    print("=== Finding items with PDF attachments ===")
    
    # Get top-level items (excluding attachments and notes)
    print("Getting top-level items...")
    items = api.get_top_level_items(limit=50)
    print(f"Found {len(items)} top-level items")
    
    items_with_pdfs = []
    
    for i, item in enumerate(items):
        data = item.get('data', {})
        item_id = item.get('key')
        title = data.get('title', 'Unknown')
        item_type = data.get('itemType', 'Unknown')
        
        print(f"\n{i+1}. {title[:60]}...")
        print(f"   Type: {item_type}, ID: {item_id}")
        
        # Get children of this item
        children = api.get_item_children(item_id)
        print(f"   Children: {len(children)}")
        
        # Count PDF attachments
        pdf_count = 0
        for child in children:
            child_data = child.get('data', {})
            if (child_data.get('itemType') == 'attachment' and 
                child_data.get('contentType') == 'application/pdf'):
                pdf_count += 1
        
        if pdf_count > 0:
            print(f"   ✅ Has {pdf_count} PDF attachment(s)")
            items_with_pdfs.append({
                'id': item_id,
                'title': title,
                'type': item_type,
                'pdf_count': pdf_count
            })
        else:
            print(f"   ❌ No PDF attachments")
    
    print(f"\n=== SUMMARY ===")
    print(f"Items with PDF attachments: {len(items_with_pdfs)}")
    
    if items_with_pdfs:
        print("\nItems you can test for annotations:")
        for item in items_with_pdfs[:10]:  # Show first 10
            print(f"  python get-annots.py {item['id']}  # {item['title'][:50]}...")
    else:
        print("No items found with PDF attachments in the first 50 items.")
        print("You might need to:")
        print("1. Add some PDFs to your Zotero library")
        print("2. Increase the limit in this script to search more items")

def main():
    find_items_with_pdfs()

if __name__ == "__main__":
    main()