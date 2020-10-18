import pytest

import copybook
from copybook import Field, FieldGroup

def test_total_length_empty_field_group():
    fg:FieldGroup = FieldGroup("","",{
        "level":"01",
        "name":"test"
    })
    assert fg.get_total_length()==0

def test_total_length_field_group():
    fg:FieldGroup = FieldGroup("","",{
        "level":"01",
        "name":"test"
    })
    fg.children.append(Field("","",{
        "type_string": {
            "length":"2",
        },
        "level":"05",
        "name":"test"
    }))
    fg.children.append(Field("","",{
        "type_numeric": {
            "length":"2",
            "precision":"2",
        },
        "level":"05",
        "name":"test"
    }))
    assert fg.get_total_length()==6

def test_get_field_by_name():
    fg:FieldGroup = FieldGroup("","",{
        "level":"01",
        "name":"test"
    })
    fg.children.append(Field("","",{
        "type_string": {
            "length":"2",
        },
        "level":"05",
        "name":"test"
    }))
    fg.children.append(Field("","",{
        "type_numeric": {
            "length":"2",
            "precision":"2",
        },
        "level":"05",
        "name":"test2"
    }))
    assert fg.get_child_by_name("test2").name=="test2"
    assert fg.get_child_by_name("test").name=="test"

def test_redefine_position():
    fg:FieldGroup = FieldGroup("","",{
        "level":"01",
        "name":"test"
    })
    fg.children.append(Field("","",{
        "type_string": {
            "length":"2",
        },
        "level":"05",
        "name":"field1"
    }))
    fg.children.append(FieldGroup("","",{
        "level":"05",
        "name":"group1"
    }))
    fg.children.append(Field("","",{
        "type_string": {
            "length":"2",
        },
        "level":"05",
        "name":"field2"
    }))
    fg.children.append(FieldGroup("","",{
        "level":"05",
        "name":"group2",
        "redefine_target":"group1"
    }))
    fg.children.append(Field("","",{
        "type_string": {
            "length":"2",
        },
        "level":"05",
        "name":"field3"
    }))
    # the positions here should be:
    # field1: 0
    # group1: 2
    # field2: 2
    # group2: 2
    # field3: 2
    fg.calculate_positions()
    assert fg.get_child_by_name("field1").start_pos==0
    assert fg.get_child_by_name("field2").start_pos==2
    assert fg.get_child_by_name("field3").start_pos==4
    assert fg.get_child_by_name("group1").start_pos==2
    assert fg.get_child_by_name("group2").start_pos==2

def test_create_group():
    fg:FieldGroup = FieldGroup("","",{
        "level":"05",
        "name":"group2",
        "redefine_target":"group1"
    })
    assert fg.redefine_target == "group1"

def test_normalize_reoccurs():
    fg:FieldGroup = FieldGroup("","",{
        "level":"01",
        "name":"test"
    })
    inner_fg:FieldGroup = FieldGroup("","",{
        "level":"05",
        "name":"test_inner",
        "occurs":"2"
    })
    inner_fg.children.append(Field("","",{
        "type_string": {
            "length":"2",
        },
        "level":"05",
        "name":"inner_field1"
    }))
    inner_fg.children.append(Field("","",{
        "type_string": {
            "length":"2",
        },
        "level":"05",
        "name":"inner_field2"
    }))
    fg.children.append(inner_fg)
    fg._normalize_reoccurs()
    assert len(fg.children)==2
    assert [child.name for child in fg.children]==['test_inner','test_inner_2']
    # cloned children have also changed names
    assert [child.name for child in fg.children[1].children]==['inner_field1_2','inner_field2_2']
    # original children have stayed as they were
    assert [child.name for child in fg.children[0].children]==['inner_field1','inner_field2']
    
def test_reoccurs_positions():
    fg:FieldGroup = FieldGroup("","",{
        "level":"01",
        "name":"test"
    })
    inner_fg:FieldGroup = FieldGroup("","",{
        "level":"05",
        "name":"test_inner",
        "occurs":"2"
    })
    inner_fg.children.append(Field("","",{
        "type_string": {
            "length":"2",
        },
        "level":"05",
        "name":"inner_field1"
    }))
    inner_fg.children.append(Field("","",{
        "type_string": {
            "length":"2",
        },
        "level":"05",
        "name":"inner_field2"
    }))
    fg.children.append(inner_fg)
    fg.calculate_positions()
    assert len(fg.children)==2
    assert [child.start_pos for child in fg.children]==[0,4]
    # original children positions
    assert [child.start_pos for child in fg.children[0].children]==[0,2]
    # cloned children positions
    assert [child.start_pos for child in fg.children[1].children]==[4,6]
        
def test_flat_list():
    fg:FieldGroup = FieldGroup("","",{
        "level":"01",
        "name":"test"
    })
    inner_fg:FieldGroup = FieldGroup("","",{
        "level":"05",
        "name":"test_inner",
    })
    inner_fg.children.append(Field("","",{
        "type_string": {
            "length":"2",
        },
        "level":"05",
        "name":"inner_field1"
    }))
    inner_fg.children.append(Field("","",{
        "type_string": {
            "length":"2",
        },
        "level":"05",
        "name":"inner_field2"
    }))
    inner_fg2:FieldGroup = FieldGroup("","",{
        "level":"05",
        "name":"test_inner2",
    })
    inner_fg2.children.append(Field("","",{
        "type_string": {
            "length":"2",
        },
        "level":"05",
        "name":"inner_field2_1"
    }))
    inner_fg2.children.append(Field("","",{
        "type_string": {
            "length":"2",
        },
        "level":"05",
        "name":"inner_field2_2"
    }))
    fg.children.append(inner_fg)
    fg.children.append(inner_fg2)
    fg.calculate_positions()

    assert {child.name for child in fg.to_flat_list()}=={"test","test_inner","test_inner2","inner_field1","inner_field2","inner_field2_1","inner_field2_2"}
    assert {child.name:child.start_pos for child in fg.to_flat_list()}=={
        "test":0,
        "test_inner":0,
        "test_inner2":4,
        "inner_field1":0,
        "inner_field2":2,
        "inner_field2_1":4,
        "inner_field2_2":6
    }
