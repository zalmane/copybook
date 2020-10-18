# Copybook
Copybook is a python module for parsing VSAM copybooks. 

# Features
- Supports all numeric PIC variations including:
  - integers
  - floats described with dot notation
  - floats described with V notation
  - leading SIGNED
  - trailing SIGNED
- Character PIC notations
- Groups
- OCCURS clauses
- REDEFINES clauses

In addition, it provides indexes of column locations to help with parsing fixed width files based on positions within each line

# Installation

```
pip install copybook
```

# Usage
Copybook provides two methods for parsing copybooks: `parse_file` and `parse_string`. The result is a `FieldGroup` object that represents the root of the copybook tree.

Examples:

```
import copybook
text = """
       01  WORK-BOOK.                                  
        10  TAX-RATE        PIC S9(13)V9(2)                
                    SIGN LEADING SEPARATE.                                     
"""
root = copybook.parse_string(text)
```

# Development

PRs are always welcome!

# Support
If you encounter an unsupported copybook feature, please paste the copybook example along with whatever logs or error message you have received.

# Gratitude
Copybook uses the awesome PyParsing library for tokenization

# License
MIT


