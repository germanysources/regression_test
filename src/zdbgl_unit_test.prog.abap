*&---------------------------------------------------------------------*
*& Report  ZTEST_DEBUG_GLOBALS
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZDBGL_UNIT_TEST.

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
      table_com_type LIKE STANDARD TABLE OF table_struc_type,
      " Object-Reference
      object_ref TYPE REF TO zdbgl_store_globals,
      data_ref TYPE REF TO data.

* Setup of global Variables
FORM gl_setup.
  DATA: l_var_i TYPE i VALUE 70,
        BEGIN OF l_struct,
          key TYPE i,
          ch(10),
        END OF l_struct.

  struct-key = 2.
  struct-ch = 'chara'.
  l_struct-key = 3.
  l_struct-ch = 'local'.
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

  " Steps at this breakpoint:
  " load script "ZDBGL_SCRIPT_STORE_GLOBALS", execute with test-case "B"
  " load script "ZDBGL_SCRIPT_STORE_LOCALS", execute with test-case "B"
  BREAK-POINT.

ENDFORM.

CLASS test_globals DEFINITION FOR TESTING DURATION SHORT
  RISK LEVEL HARMLESS INHERITING FROM cl_aunit_assert.

  PRIVATE SECTION.
  CLASS-DATA: cut_globals TYPE REF TO zdbgl_get_globals,
    cut_locals TYPE REF TO zdbgl_get_locals.

  CLASS-METHODS class_setup
    RAISING cx_static_check.

  METHODS act_global_variables FOR TESTING
    RAISING cx_static_check.

  METHODS act_local_variables FOR TESTING
    RAISING cx_static_check.

ENDCLASS.

CLASS test_globals IMPLEMENTATION.

  METHOD class_setup.
    DATA source_position TYPE zdbgl_get_locals=>_source_position.

    source_position-abap_program = sy-repid.
    source_position-line = 64.

    PERFORM gl_setup.

    cut_globals = zdbgl_get_globals=>factory( EXPORTING program = sy-repid
      key_testcase = 'B' ).
    cut_locals = zdbgl_get_locals=>factory( EXPORTING source_position = source_position
      key_testcase = 'B' ).

  ENDMETHOD.

  METHOD act_global_variables.
    DATA: act_var_i TYPE i,
          act_string TYPE string,
          act_character LIKE var_character,
          act_struct LIKE struct,
          "act_com_struct LIKE com_struct,
          act_table_simple_type LIKE table_simple_type,
          act_table_struc_type LIKE table_struc_type,
          act_hashed_table LIKE hashed_table.

    cut_globals->get_simple( EXPORTING name = 'VAR_STRING'
      IMPORTING value = act_string ).
    cut_globals->get_simple( EXPORTING name = 'VAR_I'
      IMPORTING value = act_var_i ).
    cut_globals->get_simple( EXPORTING name = 'VAR_CHARACTER'
      IMPORTING value = act_character ).
    cut_globals->get_structur( EXPORTING name = 'STRUCT'
      IMPORTING value = act_struct ).
    " Complex structur is not supported
    "cut_globals->get_structur( EXPORTING name = 'COM_STRUCT'
    "  IMPORTING value = act_com_struct ).
    cut_globals->get_table( EXPORTING name = 'TABLE_SIMPLE_TYPE'
      IMPORTING value = act_table_simple_type ).
    cut_globals->get_table( EXPORTING name = 'TABLE_STRUC_TYPE'
      IMPORTING value = act_table_struc_type ).
    cut_globals->get_table( EXPORTING name = 'HASHED_TABLE'
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

  METHOD act_local_variables.
    DATA: act_var_i TYPE i,
          act_struct LIKE struct,
          exp_var_i TYPE i VALUE 70,
          exp_struct LIKE struct.

    exp_struct-key = 3.
    exp_struct-ch = 'local'.

    cut_globals->get_simple( EXPORTING name = 'L_VAR_I'
      IMPORTING value = act_var_i ).
    cut_globals->get_structur( EXPORTING name = 'L_STRUCT'
      IMPORTING value = act_struct ).

    assert_equals( exp = exp_var_i act = act_var_i
      msg = 'Variable l_var_i of type integer' ).
    assert_equals( exp = exp_struct act = act_struct
      msg = 'Variable l_struct a flat structur' ).

  ENDMETHOD.

ENDCLASS.
