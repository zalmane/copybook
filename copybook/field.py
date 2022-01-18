from .abstract_field import AbstractField
from pyparsing import ParseException
class Field(AbstractField):
    def __init__(self,s,lok,toks):
        super().__init__(level=int(toks["level"]),name=toks["name"])
        self.toks = toks
        self.list_of_values = {}
        self.redefine_target = toks.get("redefine_target")
        self.repeat = int(toks.get("occurs","1"))
        self.length = 0
        self.precision = 0
        self.signed = False
        self.compressed = False
        self.signed_trailing = False
        self.explicit_decimal = False

        if toks.get("type_numeric") is not None:
            type_descriptor = toks.get("type_numeric")
            self.length = int(type_descriptor.get("length","0"))
            # handle precision
            self.precision = int(type_descriptor.get("precision","0"))
            if self.precision>0:
                self.datatype = "decimal"
                if type_descriptor.get("explicit_decimal"):
                    self.explicit_decimal = True
            else:
                self.datatype = "int"
            
            # signed
            if type_descriptor.get("signed"):
                self.signed = True
                if toks.get("trailing"):
                    self.signed_trailing = True

            # compressed
            if type_descriptor.get("comp3"):
                self.compressed = True

            # validate
            if self.length==0 and self.precision==0:
                raise ParseException(f"can't parse PIC for {toks}")
        elif toks.get("type_string"):
            # a string PIC
            type_descriptor = toks.get("type_string")
            self.length = int(type_descriptor.get("length","0"))

            self.datatype = "str"

            # check if we have a list of possible values. convert that to a nice constants array
            if toks.get("values"):
                for val in toks.get("values"):
                    self.list_of_values[val.get("name")] = val.get("value")
        else:
            # no type descriptor
            raise ParseException(f"unknown data type: {toks}")
    def get_total_length(self):
        return (1 if self.signed else 0) +\
            (1 if self.explicit_decimal else 0) +\
            self.length +\
            self.precision 

    def parse(self,input:str):
        if self.datatype=="str":
            return input
        elif self.datatype=="int":
            return int(input)
        elif self.datatype=="decimal":
            # put a decimal notation and parse as float
            return float(f"{input[0:self.length]}.{input[-self.precision:]}")

    def __repr__(self):
        return_value = f"name: {self.name}\n- type: {self.datatype}\n- length: {self.length}\n- precision {self.precision}"
        if self.list_of_values != {}:
            for k,v in self.list_of_values.items():
                return_value += f"\n  - {k}:{v}"
        return return_value
