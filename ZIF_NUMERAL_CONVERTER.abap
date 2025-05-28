INTERFACE zif_numeral_converter
  PUBLIC .
  METHODS from_arabic_to_roman
    IMPORTING
      !iv_arabic      TYPE i
    RETURNING
      VALUE(rv_roman) TYPE string .
  METHODS from_roman_to_arabic
    IMPORTING
      !iv_roman        TYPE string
    RETURNING
      VALUE(rv_arabic) TYPE i .
  METHODS is_roman_valid
    IMPORTING
      !iv_roman       TYPE string
    RETURNING
      VALUE(rv_valid) TYPE abap_bool .
ENDINTERFACE.
