CLASS test_signature_record DEFINITION FOR TESTING
  DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS record_sample_function_mod FOR TESTING
      RAISING cx_static_check.

ENDCLASS.

CLASS test_signature_record IMPLEMENTATION.

  METHOD record_sample_function_mod.
    DATA: exp_declaration TYPE string,
          act_declaration TYPE string,
          exp_values TYPE string,
          act_values TYPE string.

    " given
    DATA(recorder) = NEW zdbgl_getter( values =
      `{"TABLE":[{"MANDT":"ICAg","CARRID":"TEgg","CONNID":"NTAwMA==",` &&
      `"FLDATE":"MjAyMDAyMDY=","PRICE":"AAAAAAAAAAw=","CURRENCY":"ICAgICA=",` &&
      `"PLANETYPE":"ICAgICAgICAgIA==","SEATSMAX":"AAAAAA==","SEATSOCC":"AAAAAA==",` &&
      `"PAYMENTSUM":"AAAAAAAAAAAM","SEATSMAX_B":"AAAAAA==",` &&
      `"SEATSOCC_B":"AAAAAA==","SEATSMAX_F":"AAAAAA==","SEATSOCC_F":"AAAAAA=="}],` &&
      `"E_MESSAGE":"","C_PARAMETER":{"MANDT":"ICAg","CARRID":"TEcg","CONNID":"MDUwMA==",` &&
      `"FLDATE":"MjAyMDAyMDU=","PRICE":"AAAAAAAAAAw=","CURRENCY":"ICAgICA=","PLANETYPE":"ICAgICAgICAgIA==",` &&
      `"SEATSMAX":"AAAAAA==","SEATSOCC":"AAAAAA==","PAYMENTSUM":"AAAAAAAAAAAM",` &&
      `"SEATSMAX_B":"AAAAAA==","SEATSOCC_B":"AAAAAA==","SEATSMAX_F":"AAAAAA==",` &&
      `"SEATSOCC_F":"AAAAAA=="},"I_ABAP_BUILT_IN":"AgAAAA==","I_DICTIONARY_TYPE":` &&
      `{"MANDT":"ICAg","CARRID":"TEgg","CARRNAME":"TFVGVEhBTlNBICAgICAgICAgICA=",` &&
      `"CURRCODE":"ICAgICA=","URL":"ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg` &&
      `ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg` &&
      `ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg` &&
      `ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg"}}`
      program = 'ZDBGL_DEMO_MODULE' ).

    " when
    DATA(cut) = NEW zdbgl_signature_record( )->record_function_mod_signature(
      recorded_locals = recorder
      source_position = VALUE #( eventtype = 'FUNCTION' eventname = 'ZDBGL_DEMO_MODULE' ) ).
    cut->get_parameter_values( IMPORTING values_as_json = DATA(act_binary_values)
      declaration_as_json = DATA(act_binary_declaration) ).

    " then
    exp_declaration = `{"C_PARAMETER":{"IS_OPTIONAL":false,"IS_IMPORT":false,"IS_EXPORT":false,"IS_CHANGING":true,"IS_TABLE":false,"DICTIONARY_TYPE":"SFLIGHT","KIND":"S"},` &&
     `"E_MESSAGE":{"IS_OPTIONAL":false,"IS_IMPORT":false,"IS_EXPORT":true,"IS_CHANGING":false,"IS_TABLE":false,"DICTIONARY_TYPE":"STRING","KIND":"E"},` &&
     `"I_ABAP_BUILT_IN":{"IS_OPTIONAL":false,"IS_IMPORT":true,"IS_EXPORT":false,"IS_CHANGING":false,"IS_TABLE":false,"DICTIONARY_TYPE":"INT4","KIND":"E"},` &&
     `"I_DICTIONARY_TYPE":{"IS_OPTIONAL":false,"IS_IMPORT":true,"IS_EXPORT":false,"IS_CHANGING":false,"IS_TABLE":false,"DICTIONARY_TYPE":"SCARR","KIND":"S"},` &&
     `"TABLE":{"IS_OPTIONAL":false,"IS_IMPORT":false,"IS_EXPORT":false,"IS_CHANGING":false,"IS_TABLE":true,"DICTIONARY_TYPE":"SFLIGHT","KIND":"T"}}`.
    exp_values = `{"C_PARAMETER":{"MANDT":"","CARRID":"LG","CONNID":"0500","FLDATE":"2020-02-05","PRICE":0.0,"CURRENCY":"","PLANETYPE":"","SEATSMAX":0,"SEATSOCC":0,"PAYMENTSUM":0.0,"SEATSMAX_B":0,"SEATSOCC_B":0,"SEATSMAX_F":0,"SEATSOCC_F":0},` &&
      `"E_MESSAGE":"","I_ABAP_BUILT_IN":2,"I_DICTIONARY_TYPE":{"MANDT":"","CARRID":"LH","CARRNAME":"LUFTHANSA","CURRCODE":"","URL":""},` &&
      `"TABLE":[{"MANDT":"","CARRID":"LH","CONNID":"5000","FLDATE":"2020-02-06","PRICE":0.0,"CURRENCY":"","PLANETYPE":"","SEATSMAX":0,"SEATSOCC":0,"PAYMENTSUM":0.0,"SEATSMAX_B":0,"SEATSOCC_B":0,"SEATSMAX_F":0,"SEATSOCC_F":0}]}`.

    DATA(converter) = cl_abap_conv_in_ce=>create( encoding = 'UTF-8' ).
    converter->convert( EXPORTING input = act_binary_values->get_output( )
      IMPORTING data = act_values ).
    converter->convert( EXPORTING input = act_binary_declaration->get_output( )
      IMPORTING data = act_declaration ).
    cl_abap_unit_assert=>assert_equals( exp = exp_values act = act_values ).
    cl_abap_unit_assert=>assert_equals( exp = exp_declaration act = act_declaration ).

  ENDMETHOD.

ENDCLASS.
