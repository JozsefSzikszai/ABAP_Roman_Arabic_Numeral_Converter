CLASS lct_numeral_converter DEFINITION DEFERRED.
CLASS zcl_numeral_converter DEFINITION LOCAL FRIENDS lct_numeral_converter.

CLASS lct_numeral_converter DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
.
*?﻿<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
*?<asx:values>
*?<TESTCLASS_OPTIONS>
*?<TEST_CLASS>lct_Numeral_Converter
*?</TEST_CLASS>
*?<TEST_MEMBER>f_Cut
*?</TEST_MEMBER>
*?<OBJECT_UNDER_TEST>ZCL_NUMERAL_CONVERTER
*?</OBJECT_UNDER_TEST>
*?<OBJECT_IS_LOCAL/>
*?<GENERATE_FIXTURE>X
*?</GENERATE_FIXTURE>
*?<GENERATE_CLASS_FIXTURE>X
*?</GENERATE_CLASS_FIXTURE>
*?<GENERATE_INVOCATION>X
*?</GENERATE_INVOCATION>
*?<GENERATE_ASSERT_EQUAL>X
*?</GENERATE_ASSERT_EQUAL>
*?</TESTCLASS_OPTIONS>
*?</asx:values>
*?</asx:abap>
  PRIVATE SECTION.

    TYPES: BEGIN OF ty_numbers,
             arabic TYPE i,
             roman  TYPE string,
           END OF ty_numbers.

    TYPES tt_numbers TYPE STANDARD TABLE OF ty_numbers WITH DEFAULT KEY.

    DATA:
      f_cut TYPE REF TO zif_numeral_converter.  "class under test

*    CLASS-METHODS: class_setup.
*    CLASS-METHODS: class_teardown.
    METHODS: setup.
*    METHODS: teardown.
    METHODS: from_arabic_to_roman FOR TESTING.
    METHODS: from_roman_to_arabic FOR TESTING.
    METHODS: is_roman_valid_simple_val FOR TESTING.
    METHODS: is_roman_valid_simple_nonval FOR TESTING.
ENDCLASS.       "lct_Numeral_Converter

CLASS lct_numeral_converter IMPLEMENTATION.

*  METHOD class_setup.
*  ENDMETHOD.
*
*  METHOD class_teardown.
*  ENDMETHOD.
*
  METHOD setup.

    f_cut = zcl_numeral_converter=>get_instance( ).

  ENDMETHOD.
*
*  METHOD teardown.
*  ENDMETHOD.

  METHOD from_arabic_to_roman.

    DATA(lt_numbers) = VALUE tt_numbers( ( arabic = 3 roman = |III| )
                                         ( arabic = 17 roman = |XVII| )
                                         ( arabic = 29 roman = |XXIX| )
                                         ( arabic = 48 roman = |XLVIII| )
                                         ( arabic = 71 roman = |LXXI| )
                                         ( arabic = 116 roman = |CXVI| )
                                         ( arabic = 333 roman = |CCCXXXIII| )
                                         ( arabic = 449 roman = |CDXLIX| )
                                         ( arabic = 772 roman = |DCCLXXII| )
                                         ( arabic = 999 roman = |CMXCIX| )
                                         ( arabic = 1005 roman = |MV| )
                                         ( arabic = 1995 roman = |MCMXCV| )
                                         ( arabic = 2025 roman = |MMXXV| )
                                         ( arabic = 3449 roman = |MMMCDXLIX| ) ).

    LOOP AT lt_numbers
         ASSIGNING FIELD-SYMBOL(<number>).
      cl_abap_unit_assert=>assert_equals(
        act   = f_cut->from_arabic_to_roman( <number>-arabic )
        exp   = <number>-roman
      " msg   = 'Testing value rv_Roman'
*       level =
      ).
    ENDLOOP.

  ENDMETHOD.

  METHOD from_roman_to_arabic.

    DATA(lt_numbers) = VALUE tt_numbers( ( arabic = 5 roman = |V| )
                                         ( arabic = 7 roman = |VII| )
                                         ( arabic = 17 roman = |XVII| )
                                         ( arabic = 44 roman = |XLIV| )
                                         ( arabic = 159 roman = |CLIX| )
                                         ( arabic = 425 roman = |CDXXV| )
                                         ( arabic = 601 roman = |DCI| )
                                         ( arabic = 832 roman = |DCCCXXXII| )
                                         ( arabic = 999 roman = |CMXCIX| )
                                         ( arabic = 1017 roman = |MXVII| )
                                         ( arabic = 2025 roman = |MMXXV| )
                                         ( arabic = 3671 roman = |MMMDCLXXI| ) ).

    LOOP AT lt_numbers
         ASSIGNING FIELD-SYMBOL(<number>).
      cl_abap_unit_assert=>assert_equals(
        act   = f_cut->from_roman_to_arabic( <number>-roman )
        exp   = <number>-arabic
      " msg   = 'Testing value rv_Roman'
*       level =
      ).
    ENDLOOP.

  ENDMETHOD.

  METHOD is_roman_valid_simple_val.

    cl_abap_unit_assert=>assert_equals(
      act   = zcl_numeral_converter=>get_instance( )->is_roman_valid( |I| )
      exp   = abap_true
    " msg   = 'Testing value rv_Valid'
*     level =
    ).

  ENDMETHOD.

  METHOD is_roman_valid_simple_nonval.

    cl_abap_unit_assert=>assert_equals(
      act   = zcl_numeral_converter=>get_instance( )->is_roman_valid( |K| )
      exp   = abap_false
    " msg   = 'Testing value rv_Valid'
*     level =
    ).

  ENDMETHOD.

ENDCLASS.
