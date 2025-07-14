#!/usr/bin/env python3
"""
Zotero Collection Annotations Retriever

This script retrieves all annotations from all items in a specified Zotero collection
using the Zotero local API.

Author: Matt Price, 2025
Note: Claude code was used to help write this code.
"""

import json
import sys
import importlib.util
from typing import Optional

# Import ZoteroLocalAPI from get-annots.py
spec = importlib.util.spec_from_file_location("get_annots", "get-annots.py")
get_annots = importlib.util.module_from_spec(spec)
spec.loader.exec_module(get_annots)
ZoteroLocalAPI = get_annots.ZoteroLocalAPI


def main():
    """Main function for collection annotation extraction"""
    if len(sys.argv) < 2:
        print("Usage: python get-collection-annots.py <collection_id> [--library-id <id>] [--output <file>] [--org]")
        print("Examples:")
        print("  python get-collection-annots.py ABC123DEF")
        print("  python get-collection-annots.py ABC123DEF --org")
        print("  python get-collection-annots.py ABC123DEF --library-id 12345 --org")
        print("  python get-collection-annots.py ABC123DEF --output my_annotations.org --org")
        sys.exit(1)
    
    collection_id = sys.argv[1]
    library_id: Optional[str] = None
    output_file: Optional[str] = None
    org_mode = False
    
    # Parse command line arguments
    i = 2
    while i < len(sys.argv):
        arg = sys.argv[i]
        if arg == "--library-id" and i + 1 < len(sys.argv):
            library_id = sys.argv[i + 1]
            i += 2
        elif arg == "--output" and i + 1 < len(sys.argv):
            output_file = sys.argv[i + 1]
            i += 2
        elif arg == "--org":
            org_mode = True
            i += 1
        else:
            print(f"Unknown argument: {arg}")
            sys.exit(1)
    
    # Initialize API client
    api = ZoteroLocalAPI()
    
    # Get collection information first
    print(f"Retrieving collection information for: {collection_id}")
    collection_info = api.get_collection_info(collection_id, library_id)
    
    if not collection_info:
        print(f"Error: Collection {collection_id} not found")
        if library_id:
            print(f"  (searched in library: {library_id})")
        else:
            print("  (searched in personal library)")
        sys.exit(1)
    
    collection_name = collection_info.get('data', {}).get('name', 'Unknown')
    print(f"Collection: {collection_name}")
    
    # Get all annotations from the collection
    print("Retrieving all annotations from collection...")
    result = api.get_all_collection_annotations(collection_id, library_id)
    
    if "error" in result:
        print(f"Error: {result['error']}")
        sys.exit(1)
    
    # Print summary
    print(f"\nCollection: {result['collection_name']}")
    print(f"Total items in collection: {result['items_count']}")
    print(f"Items with annotations: {len(result['items'])}")
    
    total_annotations = 0
    for item in result['items']:
        total_annotations += sum(att['annotations_count'] for att in item['attachments'])
    print(f"Total annotations: {total_annotations}")
    
    if total_annotations == 0:
        print("\nNo annotations found in this collection.")
        return
    
    # Print detailed summary by item
    print(f"\nBreakdown by item:")
    for i, item in enumerate(result['items'], 1):
        item_annotations = sum(att['annotations_count'] for att in item['attachments'])
        print(f"  {i}. {item['item_title']} ({item['item_type']}) - {item_annotations} annotations")
    
    # Handle output
    if org_mode:
        org_content = api.format_collection_annotations_as_org(result)
        
        # Determine output filename
        if not output_file:
            safe_name = collection_name.replace(' ', '_').replace('/', '_')
            output_file = f"collection_{safe_name}_{collection_id}.org"
        
        # Save to org file
        with open(output_file, 'w', encoding='utf-8') as f:
            f.write(org_content)
        
        print(f"\nOrg-mode output saved to: {output_file}")
        
        # Also print to console (first 50 lines)
        lines = org_content.split('\n')
        print(f"\n" + "="*60)
        print("ORG-MODE OUTPUT (first 50 lines):")
        print("="*60)
        for line in lines[:50]:
            print(line)
        if len(lines) > 50:
            print(f"... ({len(lines) - 50} more lines)")
    else:
        # Determine output filename
        if not output_file:
            safe_name = collection_name.replace(' ', '_').replace('/', '_')
            output_file = f"collection_{safe_name}_{collection_id}.json"
        
        # Save to JSON file
        with open(output_file, 'w', encoding='utf-8') as f:
            json.dump(result, f, ensure_ascii=False, indent=2)
        
        print(f"\nJSON output saved to: {output_file}")


if __name__ == "__main__":
    main()