# class representing a field or field group
class AbstractField:
    def __init__(self,level:int,name:str):
        self.level = level
        self.name = name
        self.start_pos:int = 0
        self.length:int = 0

    def __repr__(self):
        return  f"name: {self.name}"

    def _rename_with_suffix(self,suffix:str):
        # in cases of reoccurs we want to normalize the field names so they are unique per record. useful for parsing
        self.name += suffix

    def get_total_length(self) -> int:
        return 0
