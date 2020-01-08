*&---------------------------------------------------------------------*
*& Report  ZTEST_DEBUG_GLOBALS
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zdbgl_unit_test.

DATA: var_i             TYPE i VALUE 5,
      var_string        TYPE string VALUE 'a sample string',
      var_character(10) VALUE 'CHARACT',
      fldate            TYPE d VALUE '20191115',
      " flat structure,
      BEGIN OF struct,
        key    TYPE i,
        ch(10),
      END OF struct,
      " Table with a flat structered type
      table_struc_type TYPE STANDARD TABLE OF sflight,
      " Hashed table
      hashed_table     TYPE HASHED TABLE OF sflight WITH UNIQUE KEY
        carrid connid fldate,
      " complex structure
      BEGIN OF com_struct,
        key   TYPE i,
        table LIKE table_struc_type,
      END OF com_struct,
      " Table with a simple type
      table_simple_type TYPE STANDARD TABLE OF i,
      " Table with a complex structured type
      table_com_type    LIKE STANDARD TABLE OF table_struc_type,
      " Object-Reference
      object_ref        TYPE REF TO zdbgl_store_globals,
      data_ref          TYPE REF TO data.

FORM setup_variables.
  DATA: l_var_i TYPE i VALUE 70,
        BEGIN OF l_struct,
          key    TYPE i,
          ch(10),
        END OF l_struct.
  FIELD-SYMBOLS: <flight> TYPE sflight.

  struct-key = 2.
  struct-ch = 'chara'.
  l_struct-key = 3.
  l_struct-ch = 'local'.

  APPEND INITIAL LINE TO table_struc_type ASSIGNING <flight>.
  <flight>-carrid = 'LH'. <flight>-connid = '300'.
  APPEND INITIAL LINE TO table_struc_type ASSIGNING <flight>.
  <flight>-carrid = 'LH'. <flight>-connid = '350'.

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
  " load script "ZDBGL_SCRIPT_STORE_IN_TDC", execute with
  "   test-data-container "ZDBGL_UNIT_TEST_CONTAINER", Version 1, Variant "SCRIPT_STORE_IN_TDC"
  BREAK-POINT.

ENDFORM.

CLASS test_debugger_scripts DEFINITION FOR TESTING DURATION SHORT
  RISK LEVEL HARMLESS INHERITING FROM cl_aunit_assert.

  PRIVATE SECTION.
    CLASS-DATA: cut_globals TYPE REF TO zdbgl_get_globals,
                cut_locals  TYPE REF TO zdbgl_get_locals.

    CLASS-METHODS class_setup
      RAISING cx_static_check.

    CLASS-METHODS class_teardown
      RAISING cx_static_check.

    CLASS-METHODS get_tdc_accessor
      IMPORTING
        write_access TYPE abap_bool
      RETURNING VALUE(result) TYPE REF TO cl_apl_ecatt_tdc_api.

    METHODS act_global_variables FOR TESTING
      RAISING cx_static_check.

    METHODS act_local_variables FOR TESTING
      RAISING cx_static_check.

    METHODS act_variables_stored_in_tdc FOR TESTING
      RAISING cx_static_check.

ENDCLASS.

CLASS test_debugger_scripts IMPLEMENTATION.

  METHOD class_setup.
    DATA source_position TYPE zdbgl_get_locals=>_source_position.

    source_position-abap_program = sy-repid.
    source_position-include = sy-repid.
    source_position-line = 64.

    PERFORM setup_variables.

    cut_globals = zdbgl_get_globals=>factory( EXPORTING program = sy-repid
      key_testcase = 'B' ).
    cut_locals = zdbgl_get_locals=>factory( EXPORTING source_position = source_position
      key_testcase = 'B' ).

  ENDMETHOD.

  METHOD class_teardown.

    get_tdc_accessor( abap_true )->delete_variant( 'SCRIPT_STORE_IN_TDC' ).
    COMMIT WORK.

  ENDMETHOD.

  METHOD get_tdc_accessor.

    result = cl_apl_ecatt_tdc_api=>get_instance(
      i_testdatacontainer = 'ZDBGL_UNIT_TEST_CONTAINER'
      i_testdatacontainer_version = 1 i_write_access = write_access ).

  ENDMETHOD.

  METHOD act_global_variables.
    DATA: act_var_i             TYPE i,
          act_string            TYPE string,
          act_character         LIKE var_character,
          act_struct            LIKE struct,
          act_fldate            TYPE d,
          act_table_simple_type LIKE table_simple_type,
          act_table_struc_type  LIKE table_struc_type,
          act_hashed_table      LIKE hashed_table.

    cut_globals->get_simple( EXPORTING name = 'VAR_STRING'
      IMPORTING value = act_string ).
    cut_globals->get_simple( EXPORTING name = 'VAR_I'
      IMPORTING value = act_var_i ).
    cut_globals->get_simple( EXPORTING name = 'VAR_CHARACTER'
      IMPORTING value = act_character ).
    cut_globals->get_simple( EXPORTING name = 'FLDATE'
      IMPORTING value = act_fldate ).
    cut_globals->get_structur( EXPORTING name = 'STRUCT'
      IMPORTING value = act_struct ).
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
    assert_equals( exp = fldate act = act_fldate
      msg = 'Variable fldate with same name as component internal table' ).
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
    DATA: act_var_i  TYPE i,
          act_struct LIKE struct,
          exp_var_i  TYPE i VALUE 70,
          exp_struct LIKE struct.

    exp_struct-key = 3.
    exp_struct-ch = 'local'.

    cut_locals->get_simple( EXPORTING name = 'L_VAR_I'
      IMPORTING value = act_var_i ).
    cut_locals->get_structur( EXPORTING name = 'L_STRUCT'
      IMPORTING value = act_struct ).

    assert_equals( exp = exp_var_i act = act_var_i
      msg = 'Variable l_var_i of type integer' ).
    assert_equals( exp = exp_struct act = act_struct
      msg = 'Variable l_struct a flat structur' ).

  ENDMETHOD.

  METHOD act_variables_stored_in_tdc.
    DATA: act_var_character LIKE var_character,
          act_var_string TYPE string,
          act_table_struc_type LIKE table_struc_type,
          act_table_simple_type LIKE table_simple_type,
          tdc_accessor TYPE REF TO cl_apl_ecatt_tdc_api.

    tdc_accessor = get_tdc_accessor( abap_false ).

    tdc_accessor->get_value( EXPORTING i_param_name = 'VAR_CHARACTER'
      i_variant_name = 'SCRIPT_STORE_IN_TDC'
      CHANGING e_param_value = act_var_character ).
    tdc_accessor->get_value( EXPORTING i_param_name = 'VAR_STRING'
      i_variant_name = 'SCRIPT_STORE_IN_TDC'
      CHANGING e_param_value = act_var_string ).
    tdc_accessor->get_value( EXPORTING i_param_name = 'TABLE_STRUC_TYPE'
      i_variant_name = 'SCRIPT_STORE_IN_TDC'
      CHANGING e_param_value = act_table_struc_type ).
    tdc_accessor->get_value( EXPORTING i_param_name = 'TABLE_SIMPLE_TYPE'
      i_variant_name = 'SCRIPT_STORE_IN_TDC'
      CHANGING e_param_value = act_table_simple_type ).

    assert_equals( exp = var_character act = act_var_character
      msg = 'Variable var_charachter of type char' ).
    assert_equals( exp = var_string act = act_var_string
      msg = 'Variable var_string of type string' ).
    assert_equals( exp = table_struc_type act = act_table_struc_type
      msg = 'Internal table table_struc_type with line-type sflight' ).
    assert_equals( exp = table_simple_type act = act_table_simple_type
      msg = 'Internal table table_simple_type with line-type integer' ).

  ENDMETHOD.

ENDCLASS.
