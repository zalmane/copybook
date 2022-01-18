# class representing a field group
import copy
from collections import namedtuple
from .abstract_field import AbstractField
class FieldGroup(AbstractField):
    def __init__(self,s,lok,toks):
        super().__init__(level=int(toks["level"]),name=toks["name"])
        self.redefine_target = toks.get("redefine_target")
        self.repeat = int(toks.get("occurs","1"))
        self.children = []

    def calculate_positions(self,start_pos=0):
        # take care of any reoccurs lurking out there
        self._normalize_reoccurs()
        self.start_pos = start_pos
        # loop through the children and update the positions
        for child in self.children:
            # handle redefines
            if child.redefine_target is not None:
                # redefine means the position is that of a group we already encountered. find it
                target = self.get_child_by_name(child.redefine_target)
                if type(child)==FieldGroup:
                    child.calculate_positions(target.start_pos)
                else:
                    child.start_pos = target.start_pos
                # skip to the next without advancing the start_pos. do not pass go, do not collect $200
                continue

            if type(child)==FieldGroup:
                child.calculate_positions(start_pos)
                # handle redefines and occurs
            child.start_pos = start_pos
            start_pos += child.get_total_length()

    def _rename_with_suffix(self,suffix:str):
        # in cases of reoccurs we want to normalize the field names so they are unique per record. useful for parsing
        super()._rename_with_suffix(suffix)
        for child in self.children:
            child._rename_with_suffix(suffix)

    def _normalize_reoccurs(self):
        # this method duplicates the reoccurs and renames the fields sequentially to avoid duplicate names
        childcount:int = len(self.children)
        for i in range(childcount,0,-1):
            # we reverse loop since we are changing the list within the loop
            child = self.children[i-1]
            if child.repeat>1:
                # deep copy
                for clone_number in range(child.repeat,1,-1):
                    newchild = copy.deepcopy(child)
                    newchild._rename_with_suffix("_"+str(clone_number))
                    # push into the list
                    self.children.insert(i,newchild)


    def flatten(self):
        # flatten the tree into a flat list of Fields
        return_list = [self]
        for child in self.children:
            if type(child)==FieldGroup:
                return_list.extend(child.flatten())
            else:
                return_list.append(child)
        return return_list

    def get_child_by_name(self,name:str):
        return next(filter(lambda x: x.name==name,self.children),None)

    def get_total_length(self) -> int:
        total_length = 0
        for child in self.children:
            # if this is a 'redefine' it does not occupy any space
            if child.redefine_target is None:
                total_length += child.get_total_length()

        return total_length

    def __repr__(self):
        return_value = f"""
name: {self.name}
- type: group
- redefines: {self.redefine_target}
"""
        return return_value

