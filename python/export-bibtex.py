#!/usr/bin/env python3
"""
Zotero BibTeX Export Tool

This script demonstrates how to export BibTeX data from Zotero using the local API.
It shows how Zotero item IDs are represented in BibTeX entries.
"""

import requests
import json
import sys
from typing import Optional, List, Dict, Any


class ZoteroBibTeXExporter:
    """Tool for exporting BibTeX data from Zotero local API."""
    
    def __init__(self, base_url: str = "http://localhost:23119"):
        """
        Initialize the BibTeX exporter.
        
        Args:
            base_url: Base URL for Zotero local API
        """
        self.base_url = base_url.rstrip('/')
        self.api_base = f"{self.base_url}/api"
    
    def export_item_bibtex(self, item_key: str, library_id: Optional[str] = None) -> Optional[str]:
        """
        Export a single item as BibTeX.
        
        Args:
            item_key: Zotero item key/ID
            library_id: Optional library ID for group libraries
            
        Returns:
            BibTeX string or None if export failed
        """
        try:
            if library_id:
                url = f"{self.api_base}/groups/{library_id}/items/{item_key}?format=bibtex"
            else:
                url = f"{self.api_base}/users/0/items/{item_key}?format=bibtex"
            
            response = requests.get(url)
            if response.status_code == 200:
                return response.text.strip()
            else:
                print(f"Error exporting item {item_key}: HTTP {response.status_code}")
                return None
                
        except Exception as e:
            print(f"Error exporting item {item_key}: {e}")
            return None
    
    def export_multiple_items_bibtex(self, item_keys: List[str], 
                                   library_id: Optional[str] = None) -> Optional[str]:
        """
        Export multiple items as BibTeX using item key filter.
        
        Args:
            item_keys: List of Zotero item keys/IDs
            library_id: Optional library ID for group libraries
            
        Returns:
            BibTeX string or None if export failed
        """
        try:
            # Join item keys with comma for filtering
            itemkey_filter = ",".join(item_keys)
            
            if library_id:
                url = f"{self.api_base}/groups/{library_id}/items?format=bibtex&itemKey={itemkey_filter}"
            else:
                url = f"{self.api_base}/users/0/items?format=bibtex&itemKey={itemkey_filter}"
            
            response = requests.get(url)
            if response.status_code == 200:
                return response.text.strip()
            else:
                print(f"Error exporting items: HTTP {response.status_code}")
                return None
                
        except Exception as e:
            print(f"Error exporting items: {e}")
            return None
    
    def export_all_items_bibtex(self, library_id: Optional[str] = None, 
                              limit: Optional[int] = None) -> Optional[str]:
        """
        Export all items in a library as BibTeX.
        
        Args:
            library_id: Optional library ID for group libraries
            limit: Optional limit on number of items to export
            
        Returns:
            BibTeX string or None if export failed
        """
        try:
            if library_id:
                url = f"{self.api_base}/groups/{library_id}/items?format=bibtex"
            else:
                url = f"{self.api_base}/users/0/items?format=bibtex"
            
            if limit:
                url += f"&limit={limit}"
            
            response = requests.get(url)
            if response.status_code == 200:
                return response.text.strip()
            else:
                print(f"Error exporting all items: HTTP {response.status_code}")
                return None
                
        except Exception as e:
            print(f"Error exporting all items: {e}")
            return None
    
    def get_items_list(self, library_id: Optional[str] = None, 
                      limit: int = 10) -> List[Dict[str, Any]]:
        """
        Get a list of items with basic information.
        
        Args:
            library_id: Optional library ID for group libraries
            limit: Maximum number of items to retrieve
            
        Returns:
            List of item dictionaries
        """
        try:
            if library_id:
                url = f"{self.api_base}/groups/{library_id}/items?limit={limit}"
            else:
                url = f"{self.api_base}/users/0/items?limit={limit}"
            
            response = requests.get(url)
            if response.status_code == 200:
                items = response.json()
                # Filter out annotations and attachments for cleaner display
                return [item for item in items 
                       if item.get('data', {}).get('itemType') not in ['annotation', 'attachment']]
            else:
                print(f"Error getting items: HTTP {response.status_code}")
                return []
                
        except Exception as e:
            print(f"Error getting items: {e}")
            return []
    
    def analyze_bibtex_structure(self, bibtex_content: str) -> Dict[str, Any]:
        """
        Analyze BibTeX content to extract key information about how Zotero IDs are stored.
        
        Args:
            bibtex_content: BibTeX content string
            
        Returns:
            Dictionary with analysis results
        """
        import re
        
        analysis = {
            'entries': [],
            'total_entries': 0,
            'entry_types': set(),
            'zotero_fields': set(),
            'citation_keys': []
        }
        
        # Find all BibTeX entries
        entry_pattern = r'@(\w+)\{([^,]+),\s*(.*?)\n\}'
        entries = re.findall(entry_pattern, bibtex_content, re.DOTALL)
        
        analysis['total_entries'] = len(entries)
        
        for entry_type, citation_key, content in entries:
            analysis['entry_types'].add(entry_type.lower())
            analysis['citation_keys'].append(citation_key)
            
            # Extract individual fields
            field_pattern = r'(\w+)\s*=\s*\{([^}]+)\}'
            fields = re.findall(field_pattern, content)
            
            entry_info = {
                'type': entry_type,
                'citation_key': citation_key,
                'fields': dict(fields)
            }
            
            # Look for Zotero-specific fields
            for field_name in fields:
                if 'zotero' in field_name[0].lower() or 'url' in field_name[0].lower():
                    analysis['zotero_fields'].add(field_name[0])
            
            analysis['entries'].append(entry_info)
        
        return analysis


def main():
    """Command-line interface for BibTeX export tool."""
    if len(sys.argv) < 2:
        print("Usage: python export-bibtex.py <command> [args...]")
        print()
        print("Commands:")
        print("  list                                 - List available items")
        print("  export-item <item_key>               - Export single item as BibTeX")
        print("  export-multiple <key1,key2,key3>    - Export multiple items as BibTeX")
        print("  export-all [limit]                   - Export all items as BibTeX")
        print("  analyze [limit]                      - Analyze BibTeX structure")
        print("  save-sample <filename> [limit]       - Save sample BibTeX to file")
        print()
        print("Examples:")
        print("  python export-bibtex.py list")
        print("  python export-bibtex.py export-item YW7IQA55")
        print("  python export-bibtex.py export-multiple YW7IQA55,Z2U38Z28")
        print("  python export-bibtex.py export-all 5")
        print("  python export-bibtex.py analyze 3")
        print("  python export-bibtex.py save-sample sample.bib 5")
        sys.exit(1)
    
    command = sys.argv[1]
    exporter = ZoteroBibTeXExporter()
    
    if command == "list":
        items = exporter.get_items_list(limit=20)
        if items:
            print(f"Found {len(items)} items:")
            print()
            for item in items:
                key = item.get('key', 'unknown')
                title = item.get('data', {}).get('title', 'No title')
                item_type = item.get('data', {}).get('itemType', 'unknown')
                print(f"  {key}: {title}")
                print(f"    Type: {item_type}")
                print()
        else:
            print("No items found or error occurred")
    
    elif command == "export-item":
        if len(sys.argv) != 3:
            print("Usage: python export-bibtex.py export-item <item_key>")
            sys.exit(1)
        
        item_key = sys.argv[2]
        bibtex = exporter.export_item_bibtex(item_key)
        if bibtex:
            print(f"BibTeX export for item {item_key}:")
            print("=" * 50)
            print(bibtex)
        else:
            print(f"Failed to export item {item_key}")
    
    elif command == "export-multiple":
        if len(sys.argv) != 3:
            print("Usage: python export-bibtex.py export-multiple <key1,key2,key3>")
            sys.exit(1)
        
        item_keys = [key.strip() for key in sys.argv[2].split(',')]
        bibtex = exporter.export_multiple_items_bibtex(item_keys)
        if bibtex:
            print(f"BibTeX export for items {', '.join(item_keys)}:")
            print("=" * 50)
            print(bibtex)
        else:
            print(f"Failed to export items {', '.join(item_keys)}")
    
    elif command == "export-all":
        limit = None
        if len(sys.argv) == 3:
            try:
                limit = int(sys.argv[2])
            except ValueError:
                print("Error: limit must be a number")
                sys.exit(1)
        
        bibtex = exporter.export_all_items_bibtex(limit=limit)
        if bibtex:
            print(f"BibTeX export for all items (limit: {limit or 'none'}):")
            print("=" * 50)
            print(bibtex)
        else:
            print("Failed to export all items")
    
    elif command == "analyze":
        limit = 5
        if len(sys.argv) == 3:
            try:
                limit = int(sys.argv[2])
            except ValueError:
                print("Error: limit must be a number")
                sys.exit(1)
        
        bibtex = exporter.export_all_items_bibtex(limit=limit)
        if bibtex:
            analysis = exporter.analyze_bibtex_structure(bibtex)
            
            print(f"BibTeX Structure Analysis (limit: {limit}):")
            print("=" * 50)
            print(f"Total entries: {analysis['total_entries']}")
            print(f"Entry types: {', '.join(analysis['entry_types'])}")
            print(f"Citation keys: {', '.join(analysis['citation_keys'])}")
            print()
            
            if analysis['entries']:
                print("Sample entry details:")
                entry = analysis['entries'][0]
                print(f"  Type: {entry['type']}")
                print(f"  Citation key: {entry['citation_key']}")
                print(f"  Fields: {', '.join(entry['fields'].keys())}")
                
                # Show URL field if present (often contains Zotero info)
                if 'url' in entry['fields']:
                    print(f"  URL: {entry['fields']['url']}")
            
            print()
            print("Full BibTeX content:")
            print("-" * 30)
            print(bibtex)
        else:
            print("Failed to export items for analysis")
    
    elif command == "save-sample":
        if len(sys.argv) < 3:
            print("Usage: python export-bibtex.py save-sample <filename> [limit]")
            sys.exit(1)
        
        filename = sys.argv[2]
        limit = 5
        if len(sys.argv) == 4:
            try:
                limit = int(sys.argv[3])
            except ValueError:
                print("Error: limit must be a number")
                sys.exit(1)
        
        bibtex = exporter.export_all_items_bibtex(limit=limit)
        if bibtex:
            try:
                with open(filename, 'w', encoding='utf-8') as f:
                    f.write(bibtex)
                print(f"BibTeX data saved to {filename}")
                print(f"Exported {len(bibtex.split('@')) - 1} entries")
            except IOError as e:
                print(f"Error saving file: {e}")
        else:
            print("Failed to export items")
    
    else:
        print(f"Unknown command: {command}")
        print("Use 'python export-bibtex.py' for usage information.")
        sys.exit(1)


if __name__ == "__main__":
    main()