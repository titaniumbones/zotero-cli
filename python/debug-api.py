#!/usr/bin/env python3
"""
Debug script to test Zotero API endpoints directly
"""

import requests
import json
import sys

def test_endpoint(url, description):
    """Test a specific API endpoint and show detailed response"""
    print(f"\n=== Testing: {description} ===")
    print(f"URL: {url}")
    
    try:
        response = requests.get(url)
        print(f"Status Code: {response.status_code}")
        print(f"Headers: {dict(response.headers)}")
        
        if response.status_code == 200:
            try:
                data = response.json()
                print(f"Response type: {type(data)}")
                if isinstance(data, dict):
                    print(f"Keys: {list(data.keys())}")
                    if 'data' in data:
                        print(f"Data length: {len(data['data']) if hasattr(data['data'], '__len__') else 'N/A'}")
                    print(f"Full response: {json.dumps(data, indent=2)[:500]}...")
                elif isinstance(data, list):
                    print(f"List length: {len(data)}")
                    if data:
                        print(f"First item: {json.dumps(data[0], indent=2)[:200]}...")
                else:
                    print(f"Raw response: {str(data)[:200]}...")
            except json.JSONDecodeError:
                print(f"Raw text response: {response.text[:200]}...")
        else:
            print(f"Error response: {response.text}")
            
    except Exception as e:
        print(f"Request failed: {e}")

def main():
    base_url = "http://localhost:23119/api"
    
    # Test basic connectivity
    test_endpoint(f"{base_url}/", "Root endpoint")
    
    # Test schema and item types
    test_endpoint(f"{base_url}/itemTypes", "Item types")
    
    # Test user endpoints with different user IDs
    test_endpoint(f"{base_url}/users/0/items", "User 0 items")
    test_endpoint(f"{base_url}/users/1/items", "User 1 items")
    
    # Test collections
    test_endpoint(f"{base_url}/users/0/collections", "User 0 collections")
    
    # Test groups
    test_endpoint(f"{base_url}/users/0/groups", "User 0 groups")
    
    # Test with query parameters
    test_endpoint(f"{base_url}/users/0/items?limit=5", "User 0 items with limit")
    test_endpoint(f"{base_url}/users/0/items?limit=5&format=json", "User 0 items with format")
    
    print("\n=== Testing complete ===")

if __name__ == "__main__":
    main()