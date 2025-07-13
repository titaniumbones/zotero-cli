#!/usr/bin/env python3
"""
Zotero Library Explorer

This script demonstrates the library retrieval functionality of the Zotero local API.
It lists all available libraries (groups) and shows detailed information for each.
"""

import json
import sys
import os
import importlib.util

# Import the module with hyphens in the filename
spec = importlib.util.spec_from_file_location("get_annots", "./get-annots.py")
get_annots = importlib.util.module_from_spec(spec)
spec.loader.exec_module(get_annots)

# Now we can use the ZoteroLocalAPI class
ZoteroLocalAPI = get_annots.ZoteroLocalAPI


def main():
    """Main function to demonstrate library functionality"""
    print("Zotero Library Explorer")
    print("=" * 50)
    
    # Initialize API client
    api = ZoteroLocalAPI()
    
    # Test connection by trying to get libraries
    print("Connecting to Zotero local API...")
    try:
        libraries = api.get_libraries()
        print(f"Libraries API call completed. Found {len(libraries)} libraries.")
    except Exception as e:
        print(f"Error connecting to Zotero: {e}")
        print("Make sure Zotero is running and the local API is enabled.")
        sys.exit(1)
    
    # Also try getting collections from personal library
    print("Trying to get collections from personal library...")
    try:
        collections = api.get_collections()
        print(f"Collections API call completed. Found {len(collections)} collections.")
    except Exception as e:
        print(f"Error getting collections: {e}")
        collections = []
    
    # Try getting items from personal library
    print("Trying to get items from personal library...")
    try:
        items = api.get_top_level_items(limit=10)
        print(f"Items API call completed. Found {len(items)} top-level items.")
    except Exception as e:
        print(f"Error getting items: {e}")
        items = []
    
    # Get available item types
    print("Getting available item types...")
    try:
        item_types = api.get_item_types()
        print(f"Item types API call completed. Found {len(item_types)} item types.")
    except Exception as e:
        print(f"Error getting item types: {e}")
        item_types = []
    
    # Display results
    print()
    print("RESULTS:")
    print("=" * 50)
    
    if not libraries:
        print("No group libraries found.")
        print("This could mean:")
        print("- You don't have any group libraries")
        print("- The API endpoint returned empty data")
        print("- There was an issue with the API call")
    else:
        print(f"Found {len(libraries)} group library/libraries")
    
    if collections:
        print(f"Found {len(collections)} collections in personal library")
        print("\nPersonal Library Collections:")
        for i, collection in enumerate(collections[:5], 1):  # Show first 5
            print(f"{i}. {collection.get('data', {}).get('name', 'Unknown')}")
            print(f"   Key: {collection.get('key', 'Unknown')}")
            if collection.get('data', {}).get('parentCollection'):
                print(f"   Parent: {collection.get('data', {}).get('parentCollection')}")
            print()
        
        if len(collections) > 5:
            print(f"... and {len(collections) - 5} more collections")
    else:
        print("No collections found in personal library")
    
    if items:
        print(f"\nFound {len(items)} items in personal library")
        print("\nPersonal Library Items:")
        for i, item in enumerate(items[:5], 1):  # Show first 5
            data = item.get('data', {})
            print(f"{i}. {data.get('title', 'Unknown Title')}")
            print(f"   Type: {data.get('itemType', 'Unknown')}")
            print(f"   Key: {item.get('key', 'Unknown')}")
            if data.get('creators'):
                creators = [c.get('lastName', 'Unknown') for c in data.get('creators', [])]
                print(f"   Authors: {', '.join(creators[:3])}")
            print()
        
        if len(items) > 5:
            print(f"... and {len(items) - 5} more items")
    else:
        print("No items found in personal library")
    
    if item_types:
        print(f"\nAvailable item types ({len(item_types)} total):")
        # Show first 10 item types
        for i, item_type in enumerate(item_types[:10], 1):
            print(f"{i}. {item_type.get('itemType', 'Unknown')} - {item_type.get('localized', 'Unknown')}")
        if len(item_types) > 10:
            print(f"... and {len(item_types) - 10} more item types")
    else:
        print("No item types found")
    
    if not libraries:
        return
    
    print(f"Found {len(libraries)} library/libraries:")
    print()
    
    # Display summary of all libraries
    for i, library in enumerate(libraries, 1):
        print(f"{i}. Library ID: {library.get('id', 'Unknown')}")
        print(f"   Name: {library.get('name', 'Unknown')}")
        print(f"   Type: {library.get('type', 'Unknown')}")
        if 'description' in library:
            description = library['description'][:100] + "..." if len(library['description']) > 100 else library['description']
            print(f"   Description: {description}")
        print()
    
    # Get detailed information for each library
    print("Detailed Library Information:")
    print("=" * 50)
    
    for i, library in enumerate(libraries, 1):
        library_id = library.get('id')
        if not library_id:
            print(f"Library {i}: No ID available, skipping detailed info")
            continue
        
        print(f"\n--- Library {i}: {library.get('name', 'Unknown')} ---")
        
        # Get detailed library info
        detailed_info = api.get_library_info(str(library_id))
        
        if detailed_info:
            print("Detailed Information:")
            # Display key fields in a readable format
            for key, value in detailed_info.items():
                if key in ['id', 'name', 'type', 'description', 'owner', 'members', 'created', 'modified']:
                    print(f"  {key.capitalize()}: {value}")
            
            # Show raw JSON if you want to see all fields
            print(f"\nRaw JSON data:")
            print(json.dumps(detailed_info, indent=2))
        else:
            print("  Could not retrieve detailed information")
    
    # Save results to file
    output_file = "libraries_info.json"
    output_data = {
        "libraries_summary": libraries,
        "detailed_info": {}
    }
    
    for library in libraries:
        library_id = library.get('id')
        if library_id:
            detailed_info = api.get_library_info(str(library_id))
            if detailed_info:
                output_data["detailed_info"][str(library_id)] = detailed_info
    
    with open(output_file, 'w', encoding='utf-8') as f:
        json.dump(output_data, f, ensure_ascii=False, indent=2)
    
    print(f"\nFull results saved to: {output_file}")


if __name__ == "__main__":
    main()