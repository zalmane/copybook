[![Downloads](https://pepy.tech/badge/copybook)](https://pepy.tech/project/copybook)

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

## Using Copybook module to parse a fixed width line

The `FieldGroup` object provides a `flatten` method that return a flat list of `Field` objects.

Example:

```
import copybook
text = """
       01  WORK-BOOK.
        10  TAX-RATE        PIC S9(13)V9(2)
                    SIGN LEADING SEPARATE.
        10  AMOUNT        PIC S9(4)V9(2).
"""
# copybook also provides a parse_file method that receives a text filename
root = copybook.parse_string(text)

# flatten returns a list of Fields and FieldGroups instead of traversing the tree
list_of_fields = root.flatten()

# dummy sample input
line = "          -13452987654"

# loop over the fields and parse the relevant position in the line
for field in list_of_fields:

  # FieldGroups are Copybook groups and contain Field objects as children
  if type(field)==copybook.Field:

    # each Field has a start_pos and a get_total_length method
    # to identify the position within the raw line input
    str_field = line[field.start_pos:field.start_pos+field.get_total_length()]

    # Field provides a parse method that returns a str, int, or float based on the PIC
    print(f"{field.name}: {field.parse(str_field)}")
```

# Development

PRs are always welcome!

# Support

If you encounter an unsupported copybook feature, please paste the copybook example along with whatever logs or error message you have received and open an issue.

# Gratitude

Copybook uses the awesome PyParsing library for tokenization

# License

MIT
