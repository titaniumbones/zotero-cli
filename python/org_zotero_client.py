#!/usr/bin/env python3
"""
Org-mode Zotero Client for Python

This module provides functionality for parsing org-mode files and extracting
Zotero citations to fetch annotations. It mirrors the functionality of the
Emacs Lisp org-zotero-client.el implementation.
"""

import re
import sys
import importlib.util
import os.path
from typing import List, Tuple, Optional, Dict, Any

# Try to import bibtexparser, with fallback for basic parsing
try:
    import bibtexparser
    BIBTEX_PARSER_AVAILABLE = True
except ImportError:
    BIBTEX_PARSER_AVAILABLE = False
    print("Warning: bibtexparser not available. Install with: pip install bibtexparser")

# Import get-annots.py module using importlib 
spec = importlib.util.spec_from_file_location("get_annots", "get-annots.py")
get_annots = importlib.util.module_from_spec(spec)
spec.loader.exec_module(get_annots)
ZoteroLocalAPI = get_annots.ZoteroLocalAPI


class OrgZoteroClient:
    """Client for processing org-mode files with Zotero citations."""
    
    def __init__(self, base_url: str = "http://localhost:23119"):
        """
        Initialize the Org-Zotero client.
        
        Args:
            base_url: Base URL for Zotero local API
        """
        self.api = ZoteroLocalAPI(base_url)
        
        # Citation patterns (same as Emacs Lisp version)
        self.citation_patterns = [
            r'@([A-Za-z0-9_-]+)',                           # @itemkey
            r'\[cite:@([A-Za-z0-9_-]+)\]',                  # [cite:@itemkey]
            r'\[\[zotero://select/library/items/([A-Za-z0-9]+)\]',  # [[zotero://...]]
            r'zotero://select/library/items/([A-Za-z0-9]+)',  # raw zotero URLs
        ]
    
    def find_citations_in_text(self, text: str) -> List[Tuple[int, int, str]]:
        """
        Find all Zotero citations in text.
        
        Args:
            text: Text content to search
            
        Returns:
            List of (start, end, item_id) tuples
        """
        citations = []
        
        for pattern in self.citation_patterns:
            for match in re.finditer(pattern, text):
                start = match.start()
                end = match.end()
                item_id = match.group(1)
                citations.append((start, end, item_id))
        
        # Remove duplicates and sort by position
        seen = set()
        unique_citations = []
        for citation in citations:
            if citation[2] not in seen:
                unique_citations.append(citation)
                seen.add(citation[2])
        
        return sorted(unique_citations, key=lambda x: x[0])
    
    def find_citations_in_file(self, filepath: str) -> List[Tuple[int, int, str]]:
        """
        Find all Zotero citations in an org-mode file.
        
        Args:
            filepath: Path to org-mode file
            
        Returns:
            List of (start, end, item_id) tuples
        """
        try:
            with open(filepath, 'r', encoding='utf-8') as f:
                content = f.read()
            return self.find_citations_in_text(content)
        except IOError as e:
            print(f"Error reading file {filepath}: {e}")
            return []
    
    def get_file_library_id(self, filepath: str) -> Optional[str]:
        """
        Get ZOTERO_LIBRARY_ID property from an org-mode file.
        
        Args:
            filepath: Path to org-mode file
            
        Returns:
            Library ID string or None if not set (indicating personal library)
        """
        try:
            with open(filepath, 'r', encoding='utf-8') as f:
                for line in f:
                    # Look for #+ZOTERO_LIBRARY_ID: property
                    match = re.match(r'^\s*#\+ZOTERO_LIBRARY_ID:\s*(.+)\s*$', line)
                    if match:
                        return match.group(1).strip()
            return None
        except IOError as e:
            print(f"Error reading file {filepath}: {e}")
            return None
    
    def get_bibliography_file(self, filepath: str) -> Optional[str]:
        """
        Get bibliography file path from #+BIBLIOGRAPHY keyword in org file.
        
        Args:
            filepath: Path to org-mode file
            
        Returns:
            Bibliography file path or None if not found
        """
        try:
            with open(filepath, 'r', encoding='utf-8') as f:
                for line in f:
                    # Look for #+BIBLIOGRAPHY: keyword
                    match = re.match(r'^\s*#\+BIBLIOGRAPHY:\s*(.+)\s*$', line, re.IGNORECASE)
                    if match:
                        bib_path = match.group(1).strip()
                        # Handle relative paths relative to org file directory
                        if not os.path.isabs(bib_path):
                            org_dir = os.path.dirname(os.path.abspath(filepath))
                            bib_path = os.path.join(org_dir, bib_path)
                        return bib_path
            return None
        except IOError as e:
            print(f"Error reading file {filepath}: {e}")
            return None
    
    def get_org_keywords(self, filepath: str) -> Dict[str, str]:
        """
        Get org-mode keywords from file (bibliography, cite_export, etc.).
        
        Args:
            filepath: Path to org-mode file
            
        Returns:
            Dictionary of keyword names to values
        """
        keywords = {}
        try:
            with open(filepath, 'r', encoding='utf-8') as f:
                for line in f:
                    # Look for #+KEYWORD: value patterns
                    match = re.match(r'^\s*#\+([A-Z_]+):\s*(.+)\s*$', line, re.IGNORECASE)
                    if match:
                        keyword = match.group(1).upper()
                        value = match.group(2).strip()
                        keywords[keyword] = value
            return keywords
        except IOError as e:
            print(f"Error reading file {filepath}: {e}")
            return {}
    
    def build_citation_to_zotero_id_mapping(self, bib_file: str, library_id: Optional[str] = None) -> Dict[str, str]:
        """
        Build mapping from BibTeX citation keys to Zotero item IDs.
        
        Parses the Zotero-exported BibTeX file directly to extract citation keys and their
        corresponding Zotero item IDs from the 'key = {XXX}' fields.
        
        Args:
            bib_file: Path to BibTeX file (exported from Zotero)
            library_id: Optional library ID (not used, kept for API compatibility)
            
        Returns:
            Dictionary mapping citation keys to Zotero item IDs
        """
        if not BIBTEX_PARSER_AVAILABLE:
            print("Warning: bibtexparser not available. Citation key resolution disabled.")
            return {}
        
        try:
            # Parse BibTeX file 
            with open(bib_file, 'r', encoding='utf-8') as f:
                bib_database = bibtexparser.load(f)
            
            mapping = {}
            
            print(f"Parsing {len(bib_database.entries)} entries from Zotero BibTeX export...")
            
            for entry in bib_database.entries:
                citation_key = entry['ID']  # The @article{citation_key, ...} part
                
                # Look for the Zotero item ID in the 'key' field
                if 'key' in entry:
                    zotero_id = entry['key']
                    mapping[citation_key] = zotero_id
                    print(f"  Mapped: {citation_key} → {zotero_id}")
                else:
                    print(f"  Warning: No 'key' field found for {citation_key}")
            
            print(f"Successfully mapped {len(mapping)} citation keys to Zotero IDs")
            return mapping
            
        except Exception as e:
            print(f"Error building citation mapping: {e}")
            return {}
    
    def resolve_citation_key_to_zotero_id(self, citation_key: str, filepath: str, 
                                        library_id: Optional[str] = None) -> Optional[str]:
        """
        Resolve a BibTeX citation key to a Zotero item ID.
        
        Args:
            citation_key: BibTeX citation key (e.g., 'smithResearch2023')
            filepath: Path to org file (to find bibliography)
            library_id: Optional library ID
            
        Returns:
            Zotero item ID or None if not found
        """
        # Check if we have a cached mapping
        if not hasattr(self, '_citation_mapping') or not hasattr(self, '_mapping_source'):
            # Build mapping from bibliography file
            bib_file = self.get_bibliography_file(filepath)
            if not bib_file:
                print(f"No bibliography file found for {filepath}")
                return None
                
            if not os.path.exists(bib_file):
                print(f"Bibliography file not found: {bib_file}")
                return None
                
            print(f"Building citation key mapping from: {bib_file}")
            self._citation_mapping = self.build_citation_to_zotero_id_mapping(bib_file, library_id)
            self._mapping_source = bib_file
        
        return self._citation_mapping.get(citation_key)
    
    def set_file_library_id(self, filepath: str, library_id: Optional[str] = None) -> None:
        """
        Set ZOTERO_LIBRARY_ID property in an org-mode file.
        
        Args:
            filepath: Path to org-mode file
            library_id: Library ID to set, or None to remove the property
        """
        try:
            # Read the file
            with open(filepath, 'r', encoding='utf-8') as f:
                lines = f.readlines()
            
            # Find existing property
            property_line_idx = None
            for i, line in enumerate(lines):
                if re.match(r'^\s*#\+ZOTERO_LIBRARY_ID:', line):
                    property_line_idx = i
                    break
            
            if library_id:
                # Set or update the property
                new_line = f"#+ZOTERO_LIBRARY_ID: {library_id}\n"
                if property_line_idx is not None:
                    # Update existing property
                    lines[property_line_idx] = new_line
                else:
                    # Add new property at the top, after any existing properties
                    insert_idx = 0
                    for i, line in enumerate(lines):
                        if line.strip() and not line.startswith('#+'):
                            insert_idx = i
                            break
                        elif line.startswith('#+'):
                            insert_idx = i + 1
                    lines.insert(insert_idx, new_line)
            else:
                # Remove the property
                if property_line_idx is not None:
                    del lines[property_line_idx]
            
            # Write the file back
            with open(filepath, 'w', encoding='utf-8') as f:
                f.writelines(lines)
            
            if library_id:
                print(f"Set ZOTERO_LIBRARY_ID to: {library_id}")
            else:
                print("Removed ZOTERO_LIBRARY_ID property (using personal library)")
                
        except IOError as e:
            print(f"Error modifying file {filepath}: {e}")
    
    def list_libraries_interactive(self) -> Optional[str]:
        """
        Present available libraries for interactive selection.
        
        Returns:
            Selected library ID or None for personal library
        """
        try:
            libraries = self.api.get_libraries()
            
            if not libraries:
                print("No group libraries found. Using personal library.")
                return None
            
            print("Available libraries:")
            print("0. Personal Library")
            
            for i, library in enumerate(libraries, 1):
                data = library.get('data', {})
                name = data.get('name', 'Unknown')
                lib_id = library.get('id', 'Unknown')
                print(f"{i}. {name} (ID: {lib_id})")
            
            while True:
                try:
                    choice = input("\nSelect library (number): ").strip()
                    if choice == '0':
                        return None
                    
                    idx = int(choice) - 1
                    if 0 <= idx < len(libraries):
                        selected = libraries[idx]
                        return str(selected.get('id'))
                    else:
                        print("Invalid selection. Please try again.")
                except (ValueError, KeyboardInterrupt):
                    print("Invalid input. Please enter a number.")
                    
        except Exception as e:
            print(f"Error retrieving libraries: {e}")
            return None
    
    def fetch_annotations_for_item(self, item_id: str, library_id: Optional[str] = None) -> str:
        """
        Fetch annotations for a Zotero item and return as org-mode text.
        
        Args:
            item_id: Zotero item ID
            library_id: Optional library ID for group libraries
            
        Returns:
            Formatted org-mode text with annotations
        """
        try:
            annotations_data = self.api.get_all_annotations_for_item(item_id, library_id)
            
            if "error" in annotations_data:
                return f"# Error fetching annotations for {item_id}: {annotations_data['error']}\n"
            
            return self.api.format_as_org_mode(annotations_data)
        except Exception as e:
            return f"# Error fetching annotations for {item_id}: {str(e)}\n"
    
    def get_item_title(self, item_id: str, library_id: Optional[str] = None) -> str:
        """
        Get the title of a Zotero item.
        
        Args:
            item_id: Zotero item ID
            library_id: Optional library ID for group libraries
            
        Returns:
            Item title or "Unknown Title" if not found
        """
        try:
            item = self.api.get_item(item_id, library_id)
            if item and 'data' in item:
                return item['data'].get('title', 'Unknown Title')
            return 'Unknown Title'
        except Exception:
            return 'Unknown Title'
    
    def process_org_file(self, input_file: str, output_file: str = None, 
                        auto_create_headings: bool = True) -> None:
        """
        Process an org-mode file and insert annotations for all citations.
        
        Args:
            input_file: Path to input org-mode file
            output_file: Path to output file (if None, overwrites input)
            auto_create_headings: Whether to create headings for annotations
        """
        try:
            # Read the input file
            with open(input_file, 'r', encoding='utf-8') as f:
                content = f.read()
            
            # Get the file's library ID
            file_library_id = self.get_file_library_id(input_file)
            if file_library_id:
                print(f"Using library ID from file: {file_library_id}")
            else:
                print("Using personal library (no ZOTERO_LIBRARY_ID found)")
            
            # Find all citations
            citations = self.find_citations_in_text(content)
            
            if not citations:
                print(f"No citations found in {input_file}")
                return
            
            print(f"Found {len(citations)} citations in {input_file}")
            
            # Process citations in reverse order to preserve positions
            modified_content = content
            offset = 0
            
            for start, end, item_id in reversed(citations):
                print(f"Processing citation: {item_id}")
                
                # Get item title and annotations using file's library ID
                title = self.get_item_title(item_id, file_library_id)
                annotations = self.fetch_annotations_for_item(item_id, file_library_id)
                
                # Create insertion text
                insertion = f"\n\n"
                if auto_create_headings:
                    insertion += f"** Annotations: {title}\n"
                insertion += annotations
                
                # Insert after the citation
                insert_pos = end + offset
                modified_content = (modified_content[:insert_pos] + 
                                  insertion + 
                                  modified_content[insert_pos:])
                offset += len(insertion)
            
            # Write the result
            output_path = output_file if output_file else input_file
            with open(output_path, 'w', encoding='utf-8') as f:
                f.write(modified_content)
            
            print(f"Processed {len(citations)} citations and saved to {output_path}")
            
        except IOError as e:
            print(f"Error processing file: {e}")
        except Exception as e:
            print(f"Unexpected error: {e}")
    
    def list_citations_in_file(self, filepath: str) -> None:
        """
        List all citations found in a file.
        
        Args:
            filepath: Path to org-mode file
        """
        citations = self.find_citations_in_file(filepath)
        
        if not citations:
            print(f"No citations found in {filepath}")
            return
        
        print(f"Found {len(citations)} citations in {filepath}:")
        print()
        
        for i, (start, end, item_id) in enumerate(citations, 1):
            title = self.get_item_title(item_id)
            print(f"{i}. Item ID: {item_id}")
            print(f"   Title: {title}")
            print(f"   Position: {start}-{end}")
            print()
    
    def extract_all_annotations_to_notes(self, input_file: str, notes_file: str = None, 
                                       include_item_metadata: bool = True) -> str:
        """
        Extract all annotations from citations in a file to a comprehensive notes file.
        
        This function identifies all citations, searches for PDF attachments, extracts
        annotations, and aggregates them into a single comprehensive notes document.
        
        Args:
            input_file: Path to input org-mode file containing citations
            notes_file: Path to output notes file (if None, returns content as string)
            include_item_metadata: Whether to include item titles and metadata
            
        Returns:
            String containing all annotations, or empty string if written to file
        """
        try:
            # Get the file's library ID
            file_library_id = self.get_file_library_id(input_file)
            if file_library_id:
                print(f"Using library ID from file: {file_library_id}")
            else:
                print("Using personal library (no ZOTERO_LIBRARY_ID found)")
            
            # Find all citations
            citations = self.find_citations_in_file(input_file)
            
            if not citations:
                print(f"No citations found in {input_file}")
                return ""
            
            print(f"Found {len(citations)} citations in {input_file}")
            print("Extracting annotations from PDF attachments...")
            
            # Collect all annotations
            all_annotations = []
            successful_items = 0
            failed_items = []
            
            for i, (start, end, citation_key) in enumerate(citations, 1):
                print(f"Processing citation {i}/{len(citations)}: {citation_key}")
                
                try:
                    # Resolve BibTeX citation key to Zotero item ID
                    zotero_id = self.resolve_citation_key_to_zotero_id(citation_key, input_file, file_library_id)
                    if not zotero_id:
                        failed_items.append((citation_key, "Citation key not found in Zotero library"))
                        print(f"  ❌ Citation key '{citation_key}' not found in library")
                        continue
                    
                    print(f"  Resolved: {citation_key} → {zotero_id}")
                    
                    # Get item title and check for PDFs
                    title = self.get_item_title(zotero_id, file_library_id)
                    annotations_data = self.api.get_all_annotations_for_item(zotero_id, file_library_id)
                    
                    if "error" in annotations_data:
                        failed_items.append((citation_key, annotations_data["error"]))
                        print(f"  ❌ Error: {annotations_data['error']}")
                        continue
                    
                    # Check if item has PDF attachments
                    pdf_count = len(annotations_data.get('attachments', []))
                    total_annotations = sum(att.get('annotations_count', 0) 
                                          for att in annotations_data.get('attachments', []))
                    
                    if pdf_count == 0:
                        print(f"  ⚠️  No PDF attachments found")
                        failed_items.append((citation_key, "No PDF attachments"))
                        continue
                    
                    if total_annotations == 0:
                        print(f"  ⚠️  No annotations found in {pdf_count} PDF(s)")
                        failed_items.append((citation_key, "No annotations in PDFs"))
                        continue
                    
                    print(f"  ✅ Found {total_annotations} annotations in {pdf_count} PDF(s)")
                    
                    # Store the annotation data
                    item_data = {
                        'citation_key': citation_key,
                        'zotero_id': zotero_id,
                        'title': title,
                        'annotations_data': annotations_data,
                        'citation_position': (start, end)
                    }
                    all_annotations.append(item_data)
                    successful_items += 1
                    
                except Exception as e:
                    failed_items.append((citation_key, str(e)))
                    print(f"  ❌ Unexpected error: {e}")
            
            # Get org keywords from source file
            org_keywords = self.get_org_keywords(input_file)
            
            # Generate comprehensive notes content
            notes_content = self._format_comprehensive_notes(
                all_annotations, input_file, include_item_metadata, 
                successful_items, failed_items, file_library_id, org_keywords
            )
            
            # Output handling
            if notes_file:
                with open(notes_file, 'w', encoding='utf-8') as f:
                    f.write(notes_content)
                print(f"\n✅ Comprehensive annotations saved to: {notes_file}")
                print(f"📊 Summary: {successful_items} items processed, {len(failed_items)} failed")
                return ""
            else:
                return notes_content
                
        except Exception as e:
            error_msg = f"Error processing file: {e}"
            print(error_msg)
            return f"# Error\n\n{error_msg}\n"
    
    def _format_comprehensive_notes(self, all_annotations: List[Dict], input_file: str,
                                  include_metadata: bool, successful_count: int, 
                                  failed_items: List[Tuple[str, str]], 
                                  library_id: Optional[str], org_keywords: Dict[str, str]) -> str:
        """
        Format all collected annotations into a comprehensive notes document.
        
        Args:
            all_annotations: List of annotation data for each item
            input_file: Original input file name
            include_metadata: Whether to include item metadata
            successful_count: Number of successfully processed items
            failed_items: List of (item_id, error) tuples for failed items
            library_id: Library ID used for processing
            
        Returns:
            Formatted org-mode content string
        """
        from datetime import datetime
        
        content = []
        
        # Header with org keywords from source file
        content.append("#+TITLE: Comprehensive Annotations")
        content.append(f"#+DATE: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
        content.append(f"#+SOURCE: {input_file}")
        
        # Include important org keywords from source file
        if 'BIBLIOGRAPHY' in org_keywords:
            content.append(f"#+BIBLIOGRAPHY: {org_keywords['BIBLIOGRAPHY']}")
        if 'CITE_EXPORT' in org_keywords:
            content.append(f"#+CITE_EXPORT: {org_keywords['CITE_EXPORT']}")
        if library_id:
            content.append(f"#+ZOTERO_LIBRARY_ID: {library_id}")
        elif 'ZOTERO_LIBRARY_ID' in org_keywords:
            content.append(f"#+ZOTERO_LIBRARY_ID: {org_keywords['ZOTERO_LIBRARY_ID']}")
        content.append("")
        
        # Summary
        content.append("* Summary")
        content.append("")
        content.append(f"- **Source file**: {input_file}")
        content.append(f"- **Total citations found**: {successful_count + len(failed_items)}")
        content.append(f"- **Successfully processed**: {successful_count}")
        content.append(f"- **Failed to process**: {len(failed_items)}")
        if library_id:
            content.append(f"- **Library ID**: {library_id}")
        else:
            content.append("- **Library**: Personal library")
        content.append("")
        
        # Failed items section
        if failed_items:
            content.append("* Failed Items")
            content.append("")
            for item_id, error in failed_items:
                content.append(f"- **{item_id}**: {error}")
            content.append("")
        
        # Annotations section
        if all_annotations:
            content.append("* Annotations")
            content.append("")
            
            for item_data in all_annotations:
                citation_key = item_data['citation_key']
                zotero_id = item_data['zotero_id']
                title = item_data['title']
                annotations_data = item_data['annotations_data']
                
                # Item header
                content.append(f"** {title}")
                if include_metadata:
                    content.append("")
                    content.append(f"- **Citation Key**: {citation_key}")
                    content.append(f"- **Zotero ID**: {zotero_id}")
                    content.append(f"- **Item Type**: {annotations_data.get('item_type', 'Unknown')}")
                    content.append(f"- **PDF Attachments**: {len(annotations_data.get('attachments', []))}")
                    total_annots = sum(att.get('annotations_count', 0) 
                                     for att in annotations_data.get('attachments', []))
                    content.append(f"- **Total Annotations**: {total_annots}")
                    content.append("")
                
                # Format annotations using existing function
                formatted_annotations = self.api.format_as_org_mode(annotations_data)
                
                # Extract just the annotation content (skip the main header)
                lines = formatted_annotations.split('\n')
                # Skip lines until we find the first attachment or annotation
                start_idx = 0
                for i, line in enumerate(lines):
                    if line.startswith('***') or line.startswith('- **'):
                        start_idx = i
                        break
                
                if start_idx < len(lines):
                    annotation_content = '\n'.join(lines[start_idx:])
                    content.append(annotation_content)
                else:
                    content.append("No annotations found.")
                
                content.append("")
        
        return '\n'.join(content)


def main():
    """Command-line interface for org-zotero-client."""
    if len(sys.argv) < 2:
        print("Usage: python org_zotero_client.py <command> [args...]")
        print()
        print("Commands:")
        print("  list <file.org>                      - List citations in file")
        print("  process <file.org> [output.org]      - Process file and insert annotations")
        print("  extract <file.org> <notes.org>       - Extract all annotations to notes file")
        print("  fetch <item_id>                      - Fetch annotations for single item")
        print("  set-library <file.org>               - Set library ID for org file")
        print("  show-library <file.org>              - Show current library ID for org file")
        print()
        print("Examples:")
        print("  python org_zotero_client.py list notes.org")
        print("  python org_zotero_client.py process notes.org annotated_notes.org")
        print("  python org_zotero_client.py extract research.org comprehensive_notes.org")
        print("  python org_zotero_client.py fetch YW7IQA55")
        print("  python org_zotero_client.py set-library notes.org")
        print("  python org_zotero_client.py show-library notes.org")
        sys.exit(1)
    
    command = sys.argv[1]
    client = OrgZoteroClient()
    
    if command == "list":
        if len(sys.argv) != 3:
            print("Usage: python org_zotero_client.py list <file.org>")
            sys.exit(1)
        client.list_citations_in_file(sys.argv[2])
    
    elif command == "process":
        if len(sys.argv) < 3 or len(sys.argv) > 4:
            print("Usage: python org_zotero_client.py process <input.org> [output.org]")
            sys.exit(1)
        
        input_file = sys.argv[2]
        output_file = sys.argv[3] if len(sys.argv) == 4 else None
        client.process_org_file(input_file, output_file)
    
    elif command == "extract":
        if len(sys.argv) != 4:
            print("Usage: python org_zotero_client.py extract <input.org> <notes.org>")
            sys.exit(1)
        
        input_file = sys.argv[2]
        notes_file = sys.argv[3]
        client.extract_all_annotations_to_notes(input_file, notes_file)
    
    elif command == "fetch":
        if len(sys.argv) != 3:
            print("Usage: python org_zotero_client.py fetch <item_id>")
            sys.exit(1)
        
        item_id = sys.argv[2]
        title = client.get_item_title(item_id)
        annotations = client.fetch_annotations_for_item(item_id)
        
        print(f"Title: {title}")
        print("=" * 50)
        print(annotations)
    
    elif command == "set-library":
        if len(sys.argv) != 3:
            print("Usage: python org_zotero_client.py set-library <file.org>")
            sys.exit(1)
        
        filepath = sys.argv[2]
        library_id = client.list_libraries_interactive()
        client.set_file_library_id(filepath, library_id)
    
    elif command == "show-library":
        if len(sys.argv) != 3:
            print("Usage: python org_zotero_client.py show-library <file.org>")
            sys.exit(1)
        
        filepath = sys.argv[2]
        library_id = client.get_file_library_id(filepath)
        if library_id:
            print(f"Current library ID: {library_id}")
        else:
            print("No library ID set (using personal library)")
    
    else:
        print(f"Unknown command: {command}")
        print("Use 'python org_zotero_client.py' for usage information.")
        sys.exit(1)


if __name__ == "__main__":
    main()