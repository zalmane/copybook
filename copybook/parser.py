from pyparsing import *
from .field_group import FieldGroup
from .field import Field

# main pyparsing grammar definitions for the copybook format
stmt = Forward()
LPAR, RPAR, COMMA = map(Suppress, "(),")

char_pic = Group(
    Suppress("X")
    + LPAR
    + Word(nums)("length")
    + RPAR
)("type_string")

numeric_pic = Group(
    Optional(
        Suppress("S")("signed")
        | ("-"+FollowedBy("9"))("signed")
    )
    + Optional(
        # bracket notation, e.g. 9(13)
        (
            Suppress("9")
            + LPAR
            + Word(nums)("length")
            + RPAR
        )
        # picture notation, e.g. 9999
        | OneOrMore("9").setParseAction(lambda tokens: len(tokens))("length")
    )
    + Optional(
        (
            ("V"+FollowedBy("9")("implied_decimal"))
            | ("."+FollowedBy("9")("explicit_decimal"))
        )
        # bracket notation, e.g. 9(13)
        + (
            (
            Suppress("9")
            + LPAR
            + Word(nums)("precision")
            + RPAR
            )
            # picture notation, e.g. 9999
            | OneOrMore("9").setParseAction(lambda tokens: len(tokens))("precision")
        )
    )
    + Optional(
        Literal("SIGN")("signed")
        + (
            Literal("TRAILING")("trailing") | Literal("LEADING")("leading")
        )
        + Literal("SEPARATE")
    )
    + Optional(
        Suppress("COMP-3")("comp3")
    )
)

pic_expr = (
    "PIC" +
    (char_pic | numeric_pic("type_numeric"))
)
field_title = Word(alphanums+"-_")

field_group = (
    Word(nums)("level") 
    + field_title("name") 
    + Optional(
        Suppress("REDEFINES")
        + Word(alphanums+"-")("redefine_target")
    )
    + Optional(
        "OCCURS"
        + Word(nums)("occurs")
        + Optional("TIMES")
    )
    + "."
)

# the 88 field type is an indicator of possible list of values
QuotedString("'")("value")

field = (
    Word(nums)("level") 
    + field_title("name") 
    + Optional(
        Suppress("REDEFINES")
        + Word(alphanums+"-")("redefine_target")
    )
    + Optional(
        "OCCURS"
        + Word(nums)("occurs")
        + Optional("TIMES")
    )
    + pic_expr
    + "."
    + ZeroOrMore(
        Group(Suppress("88")
        + Word(alphanums+"-")("name") 
        + Suppress("VALUE")
        + SkipTo(".")("value")
        + "."
    )("constant"))("values")
)

non_field_row = Group(
    Word(alphas) +
    ... + 
    "."
)
EOF_CHR = "\x1a"
stmt << (
    Suppress(
        SkipTo( Combine(LineStart() +" "*6 + Char(" *") + ZeroOrMore(White()) + Keyword("01") ).leaveWhitespace())
    )
    + OneOrMore(
        (
            field_group.setParseAction(FieldGroup)
            | field.setParseAction(Field)
        )
    )
    + Suppress(ZeroOrMore(
        non_field_row
    ))
    + ZeroOrMore(EOF_CHR)

)
