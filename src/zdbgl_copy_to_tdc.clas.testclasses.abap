DEFINE assert_equals.

  tdc_accessor->get_value( EXPORTING i_param_name = &4
     i_variant_name = 'COPY_TO_TDC'
     CHANGING e_param_value = &2 ).
  cl_abap_unit_assert=>assert_equals( exp = &1 act = &2
    msg = &3 ).

END-OF-DEFINITION.

DEFINE create_tdc_accessor.

  DATA(tdc_accessor) = cl_apl_ecatt_tdc_api=>get_instance( i_testdatacontainer = 'ZDBGL_UNIT_TEST_CONTAINER'
      i_write_access = abap_true i_testdatacontainer_version = 1 ).

END-OF-DEFINITION.

CLASS test_copy DEFINITION FOR TESTING
  DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA: cut          TYPE REF TO zdbgl_copy_to_tdc.

    METHODS setup
      RAISING cx_static_check.

    METHODS copy_all_parameters FOR TESTING
      RAISING cx_static_check.

    METHODS assert_tdc_variables
      RAISING cx_ecatt_tdc_access.

ENDCLASS.

CLASS test_copy IMPLEMENTATION.

  METHOD setup.

    create_tdc_accessor.

    TRY.
        tdc_accessor->delete_variant( 'COPY_TO_TDC' ).
        tdc_accessor->commit_changes( ).
        COMMIT WORK AND WAIT.
        ##NO_HANDLER
      CATCH cx_ecatt_tdc_access.
    ENDTRY.

  ENDMETHOD.

  METHOD copy_all_parameters.
    DATA recorded_values TYPE string.

    " given
    recorded_values = '{"VAR_I":"BQAAAA==","VAR_STRING":"YSBzYW1wbGUgc3RyaW5n","VAR_CHARACTER":"Q0hBUkFDVCAgIA==","STRUCT":"AgAAAGNoYXJhICAgICAAAA==",' &&
    '"TABLE_STRUC_TYPE":["ICAgTEggMDMwMDAwMDAwMDAwAAAAAAAAAAwgICAgICAgICAgICAgICAgICAAAAAAAAAAAAAAAAAAAAAADCAgIAAAAAAAAAAAAAAAAAAAAAA=","ICAgTEggMDM1MDAwMDAwMDAwAAAAAAAAAAwgICAgICAgICAgICAgICAgICAAAAAAAAAAAAAAAAAAAAAADCAgIAAAAAAAAAAAAAAAAAAAAAA="],' &&
    '"HASHED_TABLE":["ICAgTEggMDMwMDAwMDAwMDAwAAAAAAAAAAwgICAgICAgICAgICAgICAgICAAAAAAAAAAAAAAAAAAAAAADCAgIAAAAAAAAAAAAAAAAAAAAAA=","ICAgTEggMDM1MDAwMDAwMDAwAAAAAAAAAAwgICAgICAgICAgICAgICAgICAAAAAAAAAAAAAAAAAAAAAADCAgIAAAAAAAAAAAAAAAAAAAAAA="],' &&
    '"COM_STRUCT":"AwAAAAAAAADSAAAALQAAAA==","TABLE_SIMPLE_TYPE":["AQAAAA==","AgAAAA=="],"TABLE_COM_TYPE":[["ICAgTEggMDMwMDAwMDAwMDAwAAAAAAAAAAwgICAgICAgICAgICAgICAgICAAAAAAAAAAAAAAAAAAAAAADCAgIAAAAAAAAAAAAAAAAAAAAAA=",' &&
    '"ICAgTEggMDM1MDAwMDAwMDAwAAAAAAAAAAwgICAgICAgICAgICAgICAgICAAAAAAAAAAAAAAAAAAAAAADCAgIAAAAAAAAAAAAAAAAAAAAAA="],["ICAgTEggMDMwMDAwMDAwMDAwAAAAAAAAAAwgICAgICAgICAgICAgICAgICAAAAAAAAAAAAAAAAAAAAAADCAgIAAAAAAAAAAAAAAAAAAAAAA=",' &&
    '"ICAgTEggMDM1MDAwMDAwMDAwAAAAAAAAAAwgICAgICAgICAgICAgICAgICAAAAAAAAAAAAAAAAAAAAAADCAgIAAAAAAAAAAAAAAAAAAAAAA="]]}'.

    DATA(recorder) = NEW zdbgl_getter( values = recorded_values
      program = 'ZDBGL_UNIT_TEST' ).

    " when
    DATA(cut) = NEW zdbgl_copy_to_tdc( tdc = 'ZDBGL_UNIT_TEST_CONTAINER'
      tdc_version = 1 ).

    cut->copy_all_parameter( variant = 'COPY_TO_TDC'
      recorded_variables = recorder ).
    cut->save( execute_commit = abap_false ).
    COMMIT WORK AND WAIT.

    " then
    assert_tdc_variables( ).

  ENDMETHOD.

  METHOD assert_tdc_variables.
    DATA: saved_string                 TYPE string,
          saved_character_seq          TYPE char10,
          saved_table_with_struc_type  TYPE STANDARD TABLE OF sflight,
          saved_table_with_simple_type TYPE STANDARD TABLE OF i,
          exp_string                   TYPE string VALUE 'a sample string',
          exp_character_seq            TYPE char10 VALUE 'CHARACT',
          exp_table_with_struc_type    TYPE STANDARD TABLE OF sflight,
          exp_table_with_simple_type   TYPE STANDARD TABLE OF i.

    create_tdc_accessor.

    exp_table_with_struc_type = VALUE #(
    ( carrid = 'LH' connid = '300' )
    ( carrid = 'LH' connid = '350' ) ).
    APPEND: 1 TO exp_table_with_simple_type,
      2 TO exp_table_with_simple_type.

    assert_equals exp_string saved_string 'Variable of type string' 'VAR_STRING'.
    assert_equals exp_character_seq saved_character_seq 'Variable of type character sequence' 'VAR_CHARACTER'.
    assert_equals exp_table_with_struc_type saved_table_with_struc_type
      'Internal type with structured type' 'TABLE_STRUC_TYPE'.
    assert_equals exp_table_with_simple_type saved_table_with_simple_type
      'Internal type with simple type' 'TABLE_SIMPLE_TYPE'.

  ENDMETHOD.

ENDCLASS.
