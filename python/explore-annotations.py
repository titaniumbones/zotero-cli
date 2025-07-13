#!/usr/bin/env python3
"""
Explore annotation items in your library
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

def explore_annotations():
    """Explore annotation items in the library"""
    api = ZoteroLocalAPI()
    
    print("=== Exploring annotation items ===")
    
    # Get items of type annotation
    annotation_items = api.get_items(limit=100, item_type="annotation")
    print(f"Found {len(annotation_items)} annotation items")
    
    if not annotation_items:
        print("No annotation items found")
        return
    
    for i, annotation in enumerate(annotation_items[:10]):  # Show first 10
        data = annotation.get('data', {})
        
        print(f"\n{i+1}. Annotation ID: {annotation.get('key')}")
        print(f"   Type: {data.get('annotationType', 'Unknown')}")
        print(f"   Text: {data.get('annotationText', 'No text')[:100]}...")
        print(f"   Comment: {data.get('annotationComment', 'No comment')[:100]}...")
        print(f"   Parent Item: {data.get('parentItem', 'No parent')}")
        
        # Try to get the parent item
        parent_id = data.get('parentItem')
        if parent_id:
            try:
                parent = api.get_item(parent_id)
                if parent:
                    parent_data = parent.get('data', {})
                    print(f"   Parent Title: {parent_data.get('title', 'Unknown')}")
                    print(f"   Parent Type: {parent_data.get('itemType', 'Unknown')}")
            except Exception as e:
                print(f"   Error getting parent: {e}")
    
    # Also look for items with annotations as children (the correct structure)
    print(f"\n=== Looking for items with annotation children ===")
    
    # Get some items and check their children
    items = api.get_items(limit=50)
    items_with_annotations = []
    
    for item in items:
        if item.get('data', {}).get('itemType') in ['attachment', 'journalArticle', 'book']:
            children = api.get_item_children(item.get('key'))
            annotation_children = [c for c in children if c.get('data', {}).get('itemType') == 'annotation']
            
            if annotation_children:
                items_with_annotations.append({
                    'id': item.get('key'),
                    'title': item.get('data', {}).get('title', 'Unknown'),
                    'type': item.get('data', {}).get('itemType'),
                    'annotation_count': len(annotation_children)
                })
    
    if items_with_annotations:
        print(f"Found {len(items_with_annotations)} items with annotation children:")
        for item in items_with_annotations:
            print(f"  {item['id']}: {item['title'][:50]}... ({item['annotation_count']} annotations)")
    else:
        print("No items found with annotation children in the first 50 items")

def main():
    explore_annotations()

if __name__ == "__main__":
    main()