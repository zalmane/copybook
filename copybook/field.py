class Field:
    def __init__(self,s,lok,toks):
        self.toks = toks
        self.level = toks["level"]
        self.title = toks["title"]
        self.list_of_values = {}
        self.length = None
        self.precision = None
        self.signed = False
        self.compressed = False
        self.signed_trailing = False

        if toks.get("type_numeric") is not None:
            type_descriptor = toks.get("type_numeric")
            self.length = int(type_descriptor.get("length","0"))
            if type_descriptor.get("precision"):
                self.datatype = "decimal"
                self.precision = int(type_descriptor.get("precision"))
            else:
                self.datatype = "int"
            
            # signed
            if toks.get("signed"):
                self.signed = True
                if toks.get("trailing"):
                    self.signed_trailing = True

            # compressed
            if toks.get("comp3"):
                self.compressed = True

            # validate
            if self.length is None and self.precision is None:
                raise ParseException(f"can't parse PIC for {toks}")
        elif toks.get("type_string"):
            # a string PIC
            type_descriptor = toks.get("type_string")
            self.length = int(type_descriptor.get("length","0"))

            self.datatype = "str"

            # check if we have a list of possible values. convert that to a nice constants array
            if toks.get("values"):
                for val in toks.get("values"):
                    self.list_of_values[val.get("title")] = val.get("value")
        else:
            # no type descriptor
            raise ParseException(f"unknown data type: {toks}")

    def __repr__(self):
        return_value = f"title: {self.title}\n- type: {self.datatype}\n- length: {self.length}\n- precision {self.precision}"
        if self.list_of_values != {}:
            for k,v in self.list_of_values.items():
                return_value += f"\n  - {k}:{v}"
        return return_value

