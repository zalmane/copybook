import pytest

# Reader imports
import copybook

#
# Tests
#
tests = {
"extra header text":"""
       10 IDENTIFICATION DIVISION.
       PROGRAM-ID. 8-REPORT.
	   AUTHOR. DDT. MODIFIED BY OREN.
	   DATE WRITTEN. 10/13/2010.
	   DATE COMPILED. 10/13/2010.
       01  WORK-BOOK.                                                             
       10  TAX-RATE              PIC 9(3).
""",
"no header text":"""
       01  RECORD.                                                             
        05  TAX-RATE              PIC 9(3).
""",
"numeric pic no precision":"""
       01  WORK-BOOK.                                                             
       10  TAX-RATE              PIC 9(3).
""",
"numeric pic precision only":"""
       01  WORK-BOOK.                                                             
       10  TAX-RATE              PIC V9(3).
""",
"numeric pic length and precision":"""
       01  WORK-BOOK.                                                             
        10  TAX-RATE              PIC 9(13)V9(3).
""",
"numeric pic length and precision with dot":"""
       01  WORK-BOOK.                                                             
        10  TAX-RATE              PIC 9(13).9(3).
""",
"numeric pic signed S":"""
       01  WORK-BOOK.                                                             
        10  TAX-RATE              PIC S9(13)V9(3).
""",
"numeric pic signed minus":"""
       01  WORK-BOOK.                                                             
        10  TAX-RATE              PIC -9(13)V9(3).
""",
"numeric pic with 9s":"""
       01  WORK-BOOK.                                                             
        10  TAX-RATE              PIC 9999.
""",
"numeric pic with 9s and precision":"""
       01  WORK-BOOK.                                                             
        10  TAX-RATE              PIC 9(2)V99.
""",
"numeric pic with 9s and precision and sign":"""
       01  WORK-BOOK.                                                             
        10  TAX-RATE              PIC -9999.99.
""",
"numeric pic with 9s and precision and separate sign":"""
       01  WORK-BOOK.                                                             
        10  TAX-RATE        PIC S9(13)V9(2)                
                    SIGN TRAILING SEPARATE.                                     
""",
"numeric pic with 9s and precision and separate leading sign":"""
       01  WORK-BOOK.                                                             
        10  TAX-RATE        PIC S9(13)V9(2)                
                    SIGN LEADING SEPARATE.                                     
""",
"string pic with leading 0s":"""
       01  WORK-BOOK.                                                             
        10  TAX-RATE        PIC X(005).
""",

}

from pyparsing import ParseException
def test_parse_string():
    failed =0
    for test_name,test in tests.items():
        print(test_name)
        try:
            root = copybook.parse_string(test)
        except ParseException as pe:
            failed += 1
            print(f"test failed: {test_name}")
            print(pe)
    assert failed==0
