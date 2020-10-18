# class representing a field group
class FieldGroup:
    def __init__(self,s,lok,toks):
        self.level = toks["level"]
        self.title = toks["title"]
        self.redefine_target = toks.get("redefine_target")
        self.repeat = toks.get("occurs",1)
        self.children = []

    def __repr__(self):
        return_value = f"""
title: {self.title}
- type: group
- redefines: {self.redefine_target}
"""
        return return_value

