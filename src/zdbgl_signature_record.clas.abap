class ZDBGL_SIGNATURE_RECORD definition
  public
  final
  create public .

public section.

  methods RECORD_FUNCTION_MOD_SIGNATURE
    importing
      !RECORDED_LOCALS type ref to ZDBGL_GETTER
      !SOURCE_POSITION type TPDA_CURR_SOURCE_POS
    returning
      value(INSTANCE) type ref to ZDBGL_SIGNATURE_RECORD
    raising
      ZCX_DBGL_TESTCASE
      ZCX_DBGL_TYPE_NOT_SUPPORTED .
  methods GET_PARAMETER_VALUES
    exporting
      values_as_json type ref to cl_sxml_string_writer
      declaration_as_json type ref to cl_sxml_string_writer
      signature type string.
protected section.
private section.

  types:
    begin of parameter_type,
    is_optional TYPE xsdboolean,
    is_import TYPE xsdboolean,
    is_export TYPE xsdboolean,
    is_changing TYPE xsdboolean,
    is_table TYPE xsdboolean,
    dictionary_type TYPE likefield,
    kind TYPE abap_typecategory,
  END OF parameter_type.
  types:
    BEGIN OF _parameter_declaration,
    name TYPE string,
    type TYPE parameter_type,
    value_ref TYPE REF TO data,
  END OF _parameter_declaration .

  data PARAMETER_VALUES type ABAP_TRANS_SRCBIND_TAB .
  DATA parameter_declaration TYPE ABAP_TRANS_SRCBIND_TAB .
  DATA signature TYPE STANDARD TABLE OF _parameter_declaration.
  DATA function_module TYPE rs38l_fnam.
  DATA end_reached TYPE abap_bool.

  methods GET_FUNCTION_MOD_SIGNATURE
    importing
      !NAME type RS38L_FNAM.
ENDCLASS.



CLASS ZDBGL_SIGNATURE_RECORD IMPLEMENTATION.


  method GET_FUNCTION_MOD_SIGNATURE.
    DATA: parameter_declaration TYPE STANDARD TABLE OF rfc_fint_p,
          parameter TYPE _parameter_declaration.

    CALL FUNCTION 'RFC_GET_FUNCTION_INTERFACE_P'
      EXPORTING
        funcname = name
      TABLES
        params_p = parameter_declaration.

    LOOP AT parameter_declaration REFERENCE INTO DATA(_parameter).

      CLEAR parameter.
      parameter-name = _parameter->*-parameter.
      parameter-type-is_optional = _parameter->*-optional.
      CASE _parameter->*-paramclass.
        WHEN 'I'.
          parameter-type-is_import = abap_true.
        WHEN 'E'.
          parameter-type-is_export = abap_true.
        WHEN 'C'.
          parameter-type-is_changing = abap_true.
        WHEN 'T'.
          parameter-type-is_table = abap_true.
        WHEN OTHERS.
          " Don't consider exceptions
          CONTINUE.
      ENDCASE.
      IF _parameter->*-fieldname IS INITIAL.
        parameter-type-dictionary_type = _parameter->*-tabname.
      ELSE.
        parameter-type-dictionary_type = |{ _parameter->*-tabname }-{ _parameter->*-fieldname }|.
      ENDIF.

      DATA(data_descr) = cl_abap_typedescr=>describe_by_name( parameter-type-dictionary_type ).
      IF data_descr->kind <> data_descr->kind_table AND parameter-type-is_table = abap_true.
        " Tables can be referenced with a flat structure
        CREATE DATA parameter-value_ref TYPE STANDARD TABLE OF (parameter-type-dictionary_type).
        parameter-type-kind = data_descr->kind_table.
      ELSE.
        parameter-type-kind = data_descr->kind.
        CREATE DATA parameter-value_ref TYPE (parameter-type-dictionary_type).
      ENDIF.

      INSERT parameter INTO TABLE signature.

    ENDLOOP.

  endmethod.


  method GET_PARAMETER_VALUES.
    DATA: values_decoded TYPE string,
          declaration_decoded TYPE string,
          decoder TYPE REF TO cl_abap_conv_in_ce,
          signature_decoded TYPE string,
          header TYPE string.

    decoder = cl_abap_conv_in_ce=>create( encoding = 'UTF-8' ).

    values_as_json = cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ).
    declaration_as_json = cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ).

    CALL TRANSFORMATION id
      SOURCE (parameter_values)
      RESULT XML values_as_json.

    CALL TRANSFORMATION id
      SOURCE (parameter_declaration)
      RESULT XML declaration_as_json.

    decoder->convert( EXPORTING input = values_as_json->get_output( )
      IMPORTING data = values_decoded ).
    decoder->convert( EXPORTING input = declaration_as_json->get_output( )
      IMPORTING data = declaration_decoded ).

    IF end_reached = abap_true.
      header = |"function_module":"{ function_module }","end_reached":true|.
    ELSE.
      header = |"function_module":"{ function_module }"|.
    ENDIF.
    signature = |\{{ header },"declaration":{ declaration_decoded },"values":{ values_decoded }\}|.

  endmethod.


  METHOD record_function_mod_signature.
    DATA: parameter_value TYPE ABAP_TRANS_SRCBIND.

    IF source_position-eventtype <> 'FUNCTION'.
      RAISE EXCEPTION TYPE zcx_dbgl_testcase
        EXPORTING
          textid = zcx_dbgl_testcase=>no_function_module.
    ENDIF.

    function_module = source_position-eventname.
    end_reached = source_position-flag_eoev.
    get_function_mod_signature( EXPORTING name = function_module ).
    LOOP AT signature ASSIGNING FIELD-SYMBOL(<parameter>).

      ASSIGN <parameter>-value_ref->* TO FIELD-SYMBOL(<value>).
      CASE <parameter>-type-kind.
        WHEN cl_abap_datadescr=>kind_elem.
          recorded_locals->get_simple( EXPORTING name = <parameter>-name
            IMPORTING value = <value> ).
        WHEN cl_abap_datadescr=>kind_struct.
          recorded_locals->get_structur( EXPORTING name = <parameter>-name
            IMPORTING value = <value> ).
        WHEN cl_abap_datadescr=>kind_table.
          recorded_locals->get_table( EXPORTING name = <parameter>-name
            IMPORTING value = <value> ).
        WHEN OTHERS.
          CONTINUE.
      ENDCASE.

      parameter_value-name = <parameter>-name.
      GET REFERENCE OF <parameter>-type INTO parameter_value-value.
      INSERT parameter_value INTO TABLE parameter_declaration.
      INSERT VALUE #( name = <parameter>-name value = <parameter>-value_ref )
        INTO TABLE parameter_values.

    ENDLOOP.

    instance = me.

  ENDMETHOD.
ENDCLASS.
