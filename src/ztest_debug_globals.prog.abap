*&---------------------------------------------------------------------*
*& Report  ZTEST_DEBUG_GLOBALS
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZTEST_DEBUG_GLOBALS.

DATA: var_i TYPE i VALUE 5,
      var_string TYPE string VALUE 'a sample string',
      var_character(10) VALUE 'CHARACT',
      " flat structure,
      BEGIN OF struct,
        key TYPE i,
        ch(10),
      END OF struct,
      " Table with a flat structered type
      table_struc_type TYPE STANDARD TABLE OF sflight,
      " Hashed table
      hashed_table TYPE HASHED TABLE OF sflight WITH UNIQUE KEY
        carrid connid fldate,
      " complex structure
      BEGIN OF com_struct,
        key TYPE i,
        table LIKE table_struc_type,
      END OF com_struct,
      " Table with a simple type
      table_simple_type TYPE STANDARD TABLE OF i,
      " Table with a complex structured type
      table_com_type LIKE STANDARD TABLE OF table_struc_type.

* Setup of global Variables
FORM gl_setup.

  struct-key = 2.
  struct-ch = 'chara'.
  table_struc_type = VALUE #(
  ( carrid = 'LH' connid = '300' )
  ( carrid = 'LH' connid = '350' ) ).
  INSERT LINES OF table_struc_type INTO TABLE hashed_table.
  APPEND: 1 TO table_simple_type,
    2 TO table_simple_type.

  com_struct-key = 3.
  com_struct-table = table_struc_type.

  APPEND: table_struc_type TO table_com_type,
    table_struc_type TO table_com_type.

  BREAK-POINT.

ENDFORM.

CLASS test_globals DEFINITION FOR TESTING DURATION SHORT
  RISK LEVEL HARMLESS INHERITING FROM cl_aunit_assert.

  PRIVATE SECTION.
  DATA: cut TYPE REF TO zdbgl_get_globals.

  METHODS setup
    RAISING cx_static_check.

  METHODS act_variables FOR TESTING
    RAISING cx_static_check.

ENDCLASS.

CLASS test_globals IMPLEMENTATION.

  METHOD setup.
    PERFORM gl_setup.
    cut = zdbgl_get_globals=>factory( EXPORTING program = sy-repid
      key_testcase = 'B' ).
  ENDMETHOD.

  METHOD act_variables.
    DATA: act_var_i TYPE i,
          act_string TYPE string,
          act_character LIKE var_character,
          act_struct LIKE struct,
          "act_com_struct LIKE com_struct,
          act_table_simple_type LIKE table_simple_type,
          act_table_struc_type LIKE table_struc_type,
          act_hashed_table LIKE hashed_table.

    cut->get_simple( EXPORTING name = 'VAR_STRING'
      IMPORTING value = act_string ).
    cut->get_simple( EXPORTING name = 'VAR_I'
      IMPORTING value = act_var_i ).
    cut->get_simple( EXPORTING name = 'VAR_CHARACTER'
      IMPORTING value = act_character ).
    cut->get_structur( EXPORTING name = 'STRUCT'
      IMPORTING value = act_struct ).
    " Complex structur is not supported
    "cut->get_structur( EXPORTING name = 'COM_STRUCT'
    "  IMPORTING value = act_com_struct ).
    cut->get_table( EXPORTING name = 'TABLE_SIMPLE_TYPE'
      IMPORTING value = act_table_simple_type ).
    cut->get_table( EXPORTING name = 'TABLE_STRUC_TYPE'
      IMPORTING value = act_table_struc_type ).
    cut->get_table( EXPORTING name = 'HASHED_TABLE'
      IMPORTING value = act_hashed_table ).

    assert_equals( exp = var_i act = act_var_i
      msg = 'Variable var_i of type integer' ).
    assert_equals( exp = var_i act = act_var_i
      msg = 'Variable var_string of type string' ).
    assert_equals( exp = var_character act = act_character
      msg = 'Variable var_character of type character' ).
    assert_equals( exp = struct act = act_struct
      msg = 'Variable struct a flat structur' ).
    "assert_equals( exp = com_struct act = act_com_struct
    "  msg = 'Variable com_struct a complex structur' ).
    assert_equals( exp = table_simple_type act = act_table_simple_type
      msg = 'Variable table_simple_type table with simple type' ).
    assert_equals( exp = table_struc_type act = act_table_struc_type
      msg = 'Variable table_struc_type table with structured type' ).
    assert_equals( exp = hashed_table act = act_hashed_table
      msg = 'Variable hashed_table of type hashed_table' ).

  ENDMETHOD.

ENDCLASS.
