CLASS zcl_numeral_converter DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_numeral_converter .

    ALIASES from_arabic_to_roman
      FOR zif_numeral_converter~from_arabic_to_roman .
    ALIASES from_roman_to_arabic
      FOR zif_numeral_converter~from_roman_to_arabic .
    ALIASES is_roman_valid
      FOR zif_numeral_converter~is_roman_valid .

    CLASS-METHODS get_instance
      RETURNING
        VALUE(ro_numeral_converter) TYPE REF TO zif_numeral_converter
      RAISING
        cx_sy_move_cast_error .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_roman_double,
        digit TYPE char2,
        value TYPE i,
      END OF ty_roman_double .
    TYPES:
      tt_roman_double TYPE HASHED TABLE OF ty_roman_double WITH UNIQUE KEY digit .
    TYPES:
      BEGIN OF ty_roman_single,
        digit TYPE char1,
        value TYPE i,
      END OF ty_roman_single .
    TYPES:
      tt_roman_single TYPE HASHED TABLE OF ty_roman_single WITH UNIQUE KEY digit .
    TYPES:
      BEGIN OF ty_arabic,
        value TYPE i,
        roman TYPE string,
      END OF ty_arabic .
    TYPES:
      tt_arabic TYPE STANDARD TABLE OF ty_arabic WITH DEFAULT KEY .

    CLASS-DATA mo_numeral_converter TYPE REF TO zif_numeral_converter .
    DATA mt_arabic TYPE tt_arabic .
    DATA mt_roman_double TYPE tt_roman_double .
    DATA mt_roman_single TYPE tt_roman_single .

    METHODS constructor .
    METHODS initialize_arabic .
    METHODS initialize_roman .
    METHODS initialize_roman_double .
    METHODS initialize_roman_single .
ENDCLASS.



CLASS zcl_numeral_converter IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_NUMERAL_CONVERTER->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD constructor.

    initialize_arabic( ).
    initialize_roman( ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_NUMERAL_CONVERTER=>GET_INSTANCE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RO_NUMERAL_CONVERTER           TYPE REF TO ZIF_NUMERAL_CONVERTER
* | [!CX!] CX_SY_MOVE_CAST_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_instance.

    IF mo_numeral_converter IS INITIAL.
      TRY.
          mo_numeral_converter = CAST zif_numeral_converter( NEW zcl_numeral_converter( ) ).
        CATCH cx_sy_move_cast_error.
          RAISE EXCEPTION TYPE cx_sy_move_cast_error.
      ENDTRY.
    ENDIF.
    ro_numeral_converter = mo_numeral_converter.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_NUMERAL_CONVERTER->INITIALIZE_ARABIC
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD initialize_arabic.

    mt_arabic = VALUE #( ( value = 1000 roman = |M| )
                         ( value = 900  roman = |CM| )
                         ( value = 500  roman = |D| )
                         ( value = 400  roman = |CD| )
                         ( value = 100  roman = |C| )
                         ( value = 90   roman = |XC| )
                         ( value = 50   roman = |L| )
                         ( value = 40   roman = |XL| )
                         ( value = 10   roman = |X| )
                         ( value = 9    roman = |IX| )
                         ( value = 5    roman = |V| )
                         ( value = 4    roman = |IV| )
                         ( value = 1    roman = |I| ) ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_NUMERAL_CONVERTER->INITIALIZE_ROMAN
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD initialize_roman.

    initialize_roman_single( ).
    initialize_roman_double( ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_NUMERAL_CONVERTER->INITIALIZE_ROMAN_DOUBLE
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD initialize_roman_double.

    mt_roman_double = VALUE #( ( digit = |IV| value = 4 )
                               ( digit = |IX| value = 9 )
                               ( digit = |XL| value = 40 )
                               ( digit = |XC| value = 90 )
                               ( digit = |CD| value = 400 )
                               ( digit = |CM| value = 900 ) ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_NUMERAL_CONVERTER->INITIALIZE_ROMAN_SINGLE
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD initialize_roman_single.

    mt_roman_single = VALUE #( ( digit = |I| value = 1 )
                               ( digit = |V| value = 5 )
                               ( digit = |X| value = 10 )
                               ( digit = |L| value = 50 )
                               ( digit = |C| value = 100 )
                               ( digit = |D| value = 500 )
                               ( digit = |M| value = 1000 ) ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_NUMERAL_CONVERTER->ZIF_NUMERAL_CONVERTER~FROM_ARABIC_TO_ROMAN
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ARABIC                      TYPE        I
* | [<-()] RV_ROMAN                       TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_numeral_converter~from_arabic_to_roman.

    DATA(arabic) = iv_arabic.

    WHILE arabic > 0.
      LOOP AT mt_arabic
           ASSIGNING FIELD-SYMBOL(<arabic>).
        IF <arabic>-value <= arabic.
          rv_roman = rv_roman && <arabic>-roman.
          SUBTRACT <arabic>-value FROM arabic.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDWHILE.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_NUMERAL_CONVERTER->ZIF_NUMERAL_CONVERTER~FROM_ROMAN_TO_ARABIC
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ROMAN                       TYPE        STRING
* | [<-()] RV_ARABIC                      TYPE        I
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_numeral_converter~from_roman_to_arabic.

    DATA(position) = 0.

    WHILE position < strlen( iv_roman ).

      IF position < strlen( iv_roman ) - 1.
        " If not in the last position, check if we have a valid 2 letters
        DATA(lv_value) = VALUE #( mt_roman_double[ digit = substring( val = iv_roman
                                                                      off = position
                                                                      len = 2 ) ]-value OPTIONAL ).
        IF lv_value IS NOT INITIAL.
          ADD 1 TO position.
        ENDIF.
      ENDIF.

      IF lv_value IS INITIAL.
        " There is no valid 2 letters, so check single letter
        lv_value = VALUE #( mt_roman_single[ digit = substring( val = iv_roman
                                                                off = position
                                                                len = 1 ) ]-value OPTIONAL ).
      ENDIF.

      IF lv_value IS NOT INITIAL.
        ADD lv_value TO rv_arabic.
      ELSE.
        " This cannot happen, if the roman numeral is valid
      ENDIF.

      CLEAR lv_value.
      ADD 1 TO position.

    ENDWHILE.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_NUMERAL_CONVERTER->ZIF_NUMERAL_CONVERTER~IS_ROMAN_VALID
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ROMAN                       TYPE        STRING
* | [<-()] RV_VALID                       TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_numeral_converter~is_roman_valid.

    rv_valid = COND #( WHEN iv_roman CO 'IVXLCDM' THEN abap_true
                       ELSE abap_false ).

  ENDMETHOD.
ENDCLASS.
