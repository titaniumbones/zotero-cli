#!/usr/bin/env python3
"""
Zotero Attachment Export Tool

Export PDF and EPUB attachments from Zotero libraries or collections
to a local folder, with optional conversion to markdown using markitdown.

Usage:
    python export-attachments.py library [--library-id LIBRARY_ID] [--target TARGET_FOLDER] [--types pdf,epub] [--no-convert]
    python export-attachments.py collection COLLECTION_ID [--library-id LIBRARY_ID] [--target TARGET_FOLDER] [--types pdf,epub] [--no-convert]

Examples:
    # Export all PDF/EPUB files from personal library
    python export-attachments.py library

    # Export from specific group library
    python export-attachments.py library --library-id 12345

    # Export from collection in personal library
    python export-attachments.py collection ABC123DEF

    # Export from collection in group library
    python export-attachments.py collection ABC123DEF --library-id 12345

    # Export only PDFs to custom folder without markdown conversion
    python export-attachments.py library --types pdf --target my_exports --no-convert

Dependencies:
    pip install markitdown pyyaml requests
"""

import sys
import argparse
import json
from typing import List, Optional

# Import the ZoteroLocalAPI class
import sys
import os

# Add current directory to Python path
sys.path.append(os.path.dirname(__file__))

# Import with module name that matches filename
import importlib.util
spec = importlib.util.spec_from_file_location("get_annots", os.path.join(os.path.dirname(__file__), "get-annots.py"))
get_annots_module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(get_annots_module)

ZoteroLocalAPI = get_annots_module.ZoteroLocalAPI


def main():
    parser = argparse.ArgumentParser(
        description="Export PDF and EPUB attachments from Zotero libraries or collections",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
    # Export all attachments from personal library
    python export-attachments.py library

    # Export from specific group library
    python export-attachments.py library --library-id 12345

    # Export from collection in personal library
    python export-attachments.py collection ABC123DEF

    # Export from collection in group library with custom target
    python export-attachments.py collection ABC123DEF --library-id 12345 --target research_papers

    # Export only PDFs without markdown conversion
    python export-attachments.py library --types pdf --no-convert
        """
    )
    
    # Subcommands
    subparsers = parser.add_subparsers(dest='command', help='Export mode')
    
    # Library export command
    library_parser = subparsers.add_parser('library', help='Export attachments from entire library')
    library_parser.add_argument('--library-id', type=str, help='Group library ID (omit for personal library)')
    library_parser.add_argument('--target', type=str, default='zotero_library_export', 
                              help='Target folder for export (default: zotero_library_export)')
    library_parser.add_argument('--types', type=str, default='pdf,epub',
                              help='File types to export, comma-separated (default: pdf,epub)')
    library_parser.add_argument('--no-convert', action='store_true',
                              help='Skip markdown conversion, only download original files')
    
    # Collection export command
    collection_parser = subparsers.add_parser('collection', help='Export attachments from specific collection')
    collection_parser.add_argument('collection_id', type=str, help='Collection ID')
    collection_parser.add_argument('--library-id', type=str, help='Group library ID (omit for personal library)')
    collection_parser.add_argument('--target', type=str, default='zotero_collection_export',
                                 help='Target folder for export (default: zotero_collection_export)')
    collection_parser.add_argument('--types', type=str, default='pdf,epub',
                                 help='File types to export, comma-separated (default: pdf,epub)')
    collection_parser.add_argument('--no-convert', action='store_true',
                                 help='Skip markdown conversion, only download original files')
    
    # Additional options
    parser.add_argument('--verbose', '-v', action='store_true', help='Enable verbose output')
    parser.add_argument('--dry-run', action='store_true', help='Show what would be exported without downloading')
    parser.add_argument('--output-summary', type=str, help='Save export summary to JSON file')
    
    args = parser.parse_args()
    
    if not args.command:
        parser.print_help()
        sys.exit(1)
    
    # Parse file types
    file_types = [ft.strip().lower() for ft in args.types.split(',')]
    valid_types = ['pdf', 'epub']
    file_types = [ft for ft in file_types if ft in valid_types]
    
    if not file_types:
        print("Error: No valid file types specified. Use --types pdf,epub")
        sys.exit(1)
    
    print(f"🚀 Starting Zotero attachment export...")
    print(f"   File types: {', '.join(file_types)}")
    print(f"   Target folder: {args.target}")
    print(f"   Convert to markdown: {'No' if args.no_convert else 'Yes'}")
    
    if args.library_id:
        print(f"   Library ID: {args.library_id}")
    else:
        print("   Library: Personal library")
    
    if args.dry_run:
        print("   🧪 DRY RUN MODE - No files will be downloaded")
    
    print()
    
    # Initialize API client
    try:
        api = ZoteroLocalAPI()
    except Exception as e:
        print(f"❌ Failed to initialize Zotero API client: {e}")
        print("   Make sure Zotero is running and the local API is enabled.")
        sys.exit(1)
    
    # Execute export based on command
    try:
        if args.command == 'library':
            if args.dry_run:
                print("🧪 DRY RUN: Would export library attachments")
                # In a real implementation, you could add a dry-run mode to the API methods
                print("   Use --verbose to see what items would be processed")
                summary = {'dry_run': True, 'message': 'Dry run completed'}
            else:
                summary = api.export_library_attachments(
                    library_id=args.library_id,
                    target_folder=args.target,
                    file_types=file_types,
                    convert_to_markdown=not args.no_convert
                )
        
        elif args.command == 'collection':
            if args.dry_run:
                print(f"🧪 DRY RUN: Would export collection {args.collection_id}")
                print("   Use --verbose to see what items would be processed")
                summary = {'dry_run': True, 'message': 'Dry run completed'}
            else:
                summary = api.export_collection_attachments(
                    collection_id=args.collection_id,
                    library_id=args.library_id,
                    target_folder=args.target,
                    file_types=file_types,
                    convert_to_markdown=not args.no_convert
                )
        
        # Handle any errors in the summary
        if 'error' in summary:
            print(f"❌ Export failed: {summary['error']}")
            sys.exit(1)
        
        # Print results
        if not args.dry_run:
            print(f"\n📊 Export Summary:")
            print(f"   Files exported: {summary.get('total_files_exported', 0)}")
            print(f"   Failed downloads: {summary.get('failed_downloads', 0)}")
            print(f"   Target folder: {summary.get('target_folder', 'Unknown')}")
            
            if summary.get('collection_name'):
                print(f"   Collection: {summary['collection_name']}")
        
        # Save summary to file if requested
        if args.output_summary:
            with open(args.output_summary, 'w') as f:
                json.dump(summary, f, indent=2, default=str)
            print(f"   Summary saved to: {args.output_summary}")
        
        # Show verbose output if requested
        if args.verbose and 'exported_files' in summary:
            print(f"\n📋 Exported Files:")
            for file_info in summary['exported_files']:
                print(f"   - {file_info['citation_key']}.{file_info['file_type']} ({file_info['original_filename']})")
        
        if args.verbose and 'failed_downloads_list' in summary:
            print(f"\n⚠️  Failed Downloads:")
            for file_info in summary['failed_downloads_list']:
                print(f"   - {file_info['citation_key']}: {file_info['filename']}")
    
    except KeyboardInterrupt:
        print(f"\n🛑 Export cancelled by user")
        sys.exit(1)
    except Exception as e:
        print(f"❌ Export failed with error: {e}")
        if args.verbose:
            import traceback
            traceback.print_exc()
        sys.exit(1)


if __name__ == "__main__":
    main()