import copy
from .parser import stmt
from .field_group import FieldGroup
def parse_file(filename:str) -> FieldGroup:
    result:FieldGroup = stmt.parseFile(filename,parseAll=True)
    # rearrange in a tree structure
    return _list_to_tree(result)

def parse_string(contents:str) -> FieldGroup:
    result:FieldGroup = stmt.parseString(contents,parseAll=True)
    # rearrange in a tree structure
    return _list_to_tree(result)

# utility function to rearrange the fields in a tree structure based on the field IDs
def _relocate_field(root,field_list):
    root.children = []
    field_list.pop(0)
    while len(field_list)>0:
        field = field_list[0]
        # print(f"{field} under root: {root}")
        if field.level > root.level:
            root.children.append(_relocate_field(copy.copy(field),field_list))
        else:
            break
    return root

def _list_to_tree(field_list):
    return _relocate_field(copy.copy(field_list[0]),field_list.copy())

def _pprint_tree(root:FieldGroup,level=0):
    indent = "".join(['  ']*level)
    print(f"{indent}{root.title}")
    if type(root)==FieldGroup:
        for child in root.children:
            _pprint_tree(child,level+1)

