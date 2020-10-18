import pytest

import copybook
from copybook import Field, FieldGroup
#
# Tests
#
def test_total_length():
    field:Field = Field("","",{
        "type_numeric": {
            "signed":True,
            "length":"2",
            "precision":"2",
        },
        "level":"01",
        "name":"test"
    })
    assert field.get_total_length()==5

def test_total_length_explicit_decimal():
    field:Field = Field("","",{
        "type_numeric": {
            "signed":True,
            "length":"2",
            "precision":"2",
            "explicit_decimal":True
        },
        "level":"01",
        "name":"test"
    })
    assert field.get_total_length()==6

def test_total_length_unsigned_implied_decimal():
    field:Field = Field("","",{
        "type_numeric": {
            "length":"2",
            "precision":"2",
        },
        "level":"01",
        "name":"test"
    })
    assert field.get_total_length()==4


def test_total_length_string():
    field:Field = Field("","",{
        "type_string": {
            "length":"2",
        },
        "level":"01",
        "name":"test"
    })
    assert field.get_total_length()==2

