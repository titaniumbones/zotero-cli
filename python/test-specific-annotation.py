#!/usr/bin/env python3
"""
Test specific annotation retrieval
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

def test_specific_annotation():
    api = ZoteroLocalAPI()
    
    # Test the specific annotation we know exists
    print("=== Testing specific annotation ===")
    
    # Get the annotation item directly
    annotation_id = "HMD93DAE"
    print(f"Getting annotation {annotation_id} directly...")
    annotation = api.get_item(annotation_id)
    
    if annotation:
        data = annotation.get('data', {})
        print(f"✅ Found annotation: {data.get('annotationType', 'Unknown')}")
        print(f"   Text: {data.get('annotationText', 'No text')[:100]}...")
        print(f"   Parent: {data.get('parentItem', 'No parent')}")
    else:
        print("❌ Annotation not found")
        return
    
    # Test the attachment annotation function
    parent_id = annotation.get('data', {}).get('parentItem')
    if parent_id:
        print(f"\nTesting get_attachment_annotations for {parent_id}...")
        annotations = api.get_attachment_annotations(parent_id)
        print(f"Found {len(annotations)} annotations for attachment {parent_id}")
        
        for i, ann in enumerate(annotations):
            ann_data = ann.get('data', {})
            print(f"  {i+1}. {ann_data.get('annotationType', 'Unknown')}: {ann_data.get('annotationText', 'No text')[:50]}...")
    
    # Test finding the parent item of the attachment
    if parent_id:
        print(f"\nFinding parent item of attachment {parent_id}...")
        # Get all items and find one that has this attachment as a child
        items = api.get_items(limit=100)
        for item in items:
            if item.get('data', {}).get('itemType') not in ['attachment', 'annotation']:
                children = api.get_item_children(item.get('key'))
                for child in children:
                    if child.get('key') == parent_id:
                        print(f"✅ Found parent item: {item.get('key')} - {item.get('data', {}).get('title', 'Unknown')}")
                        
                        # Test the full annotation retrieval
                        print(f"\nTesting full annotation retrieval for parent item {item.get('key')}...")
                        result = api.get_all_annotations_for_item(item.get('key'))
                        print(f"Total attachments: {len(result.get('attachments', []))}")
                        total_annotations = sum(att.get('annotations_count', 0) for att in result.get('attachments', []))
                        print(f"Total annotations: {total_annotations}")
                        return
        print("❌ Parent item not found")

def main():
    test_specific_annotation()

if __name__ == "__main__":
    main()