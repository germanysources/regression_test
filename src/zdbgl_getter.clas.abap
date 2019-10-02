class ZDBGL_GETTER definition
  public
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !VALUES type STRING
      !PROGRAM type PROGRAM .
  methods GET_SIMPLE
    importing
      !NAME type STRING
    exporting
      !VALUE type SIMPLE
    raising
      ZCX_DBGL_TESTCASE .
  methods GET_STRUCTUR
    importing
      !NAME type STRING
    exporting
      !VALUE type ANY
    raising
      ZCX_DBGL_TESTCASE .
  methods GET_TABLE
    importing
      !NAME type STRING
    exporting
      !VALUE type ANY TABLE
    raising
      ZCX_DBGL_TESTCASE .
protected section.

  data PROGRAM type PROGRAM .
private section.

  types:
    xstring_tab TYPE STANDARD TABLE OF xstring .

  data JSON_XTEXT type XSTRING .
  data ABAP_CONVERSION type ref to CL_ABAP_CONV_IN_CE .

  methods GET_HEX_VALUE_TABLE
    importing
      !NAME type STRING
    exporting
      !VALUES type XSTRING_TAB
    raising
      ZCX_DBGL_TESTCASE .
  methods GET_HEX_VALUE
    importing
      !NAME type STRING
    returning
      value(VALUE) type STRING
    raising
      ZCX_DBGL_TESTCASE .
  methods IS_UTF16
    returning
      value(UNICODE) type SAP_BOOL .
  methods CONVERT_STRUCTUR
    importing
      !HEX_VALUE type XSTRING
    exporting
      !DATA type ANY .
ENDCLASS.



CLASS ZDBGL_GETTER IMPLEMENTATION.


  method CONSTRUCTOR.

    me->program = program.

    json_xtext = cl_abap_codepage=>convert_to( values ).
    abap_conversion = cl_abap_conv_in_ce=>create( ).

  endmethod.


  METHOD CONVERT_STRUCTUR.
    DATA: struct_descr TYPE REF TO cl_abap_view_offlen.

    IF is_utf16( ) = abap_true.
      struct_descr = cl_abap_view_offlen=>create_unicode16_view(
        data ).
    ELSE.
      struct_descr = cl_abap_view_offlen=>create_legacy_view(
        data ).
    ENDIF.

    abap_conversion->convert_struc( EXPORTING input = hex_value view = struct_descr
      IMPORTING data = data ).

  ENDMETHOD.


METHOD GET_HEX_VALUE.
  DATA: node       TYPE REF TO if_sxml_node,
        attributes TYPE if_sxml_attribute=>attributes,
        node_found TYPE sap_bool,
        reader TYPE REF TO if_sxml_reader.
  FIELD-SYMBOLS: <attribute> TYPE REF TO if_sxml_attribute.

  " create reader in every method, loop through nodes is statefull
  reader = cl_sxml_string_reader=>create( json_xtext ).
  DO.
    node = reader->read_next_node( ).
    IF node IS INITIAL.
      EXIT.
    ENDIF.

    IF node->type = if_sxml_node=>co_nt_element_open.
      attributes = CAST if_sxml_open_element( node )->get_attributes( ).
      LOOP AT attributes ASSIGNING <attribute>.
        IF <attribute>->qname-name = 'name'
          AND <attribute>->get_value( ) = name.
          node_found = abap_true.
        ENDIF.
      ENDLOOP.
    ELSEIF node->type = if_sxml_node=>co_nt_value
      AND node_found = abap_true.
      value = CAST if_sxml_value_node( node )->get_value( ).
      RETURN.
    ENDIF.

  ENDDO.

  " value for variable <name> not found
  RAISE EXCEPTION TYPE zcx_dbgl_testcase
    EXPORTING
      textid   = zcx_dbgl_testcase=>variable_not_found
      program  = program
      variable = name.

ENDMETHOD.


  METHOD GET_HEX_VALUE_TABLE.
" @todo linked list for complex table types
    DATA: node         TYPE REF TO if_sxml_node,
          attributes   TYPE if_sxml_attribute=>attributes,
          node_found   TYPE sap_bool,
          base64_value TYPE string,
          reader       TYPE REF TO if_sxml_reader.
    FIELD-SYMBOLS: <attribute> TYPE REF TO if_sxml_attribute.

    " create reader in every method, loop through nodes is statefull
    reader = cl_sxml_string_reader=>create( json_xtext ).
    DO.
      node = reader->read_next_node( ).
      IF node IS INITIAL.
        EXIT.
      ENDIF.

      IF node->type = if_sxml_node=>co_nt_element_open.
        attributes = CAST if_sxml_open_element( node )->get_attributes( ).
        LOOP AT attributes ASSIGNING <attribute>.
          IF <attribute>->qname-name = 'name'
            AND <attribute>->get_value( ) = name.
            node_found = abap_true.
          ELSEIF <attribute>->qname-name = 'name'.
            node_found = abap_false.
          ENDIF.
        ENDLOOP.
      ELSEIF node->type = if_sxml_node=>co_nt_value
        AND node_found = abap_true.
        base64_value = CAST if_sxml_value_node( node )->get_value( ).
        APPEND cl_http_utility=>decode_x_base64( base64_value ) TO values.
      ENDIF.

    ENDDO.

    IF values IS INITIAL.
      " value for variable <name> not found
      RAISE EXCEPTION TYPE zcx_dbgl_testcase
        EXPORTING
          textid   = zcx_dbgl_testcase=>variable_not_found
          program  = program
          variable = name.
    ENDIF.

  ENDMETHOD.


  method GET_SIMPLE.
    DATA: hex_value TYPE xstring.

    hex_value = cl_http_utility=>decode_x_base64( get_hex_value( name ) ).

    abap_conversion->convert( EXPORTING input = hex_value
      IMPORTING data = value ).

  endmethod.


  method GET_STRUCTUR.
    DATA: hex_value TYPE xstring.

    hex_value = cl_http_utility=>decode_x_base64( get_hex_value( name ) ).

    convert_structur( EXPORTING hex_value = hex_value
      IMPORTING data = value ).

  endmethod.


  method GET_TABLE.
    DATA: hex_values TYPE xstring_tab,
          tabletype TYPE REF TO cl_abap_tabledescr,
          linetype TYPE REF TO cl_abap_typedescr,
          line TYPE REF TO data.
    FIELD-SYMBOLS: <hval> TYPE xstring,
                   <line> TYPE any.

    CLEAR value.

    " get line type
    tabletype = cast cl_abap_tabledescr(
      cl_abap_typedescr=>describe_by_data( value ) ).
    linetype = tabletype->get_table_line_type( ).

    get_hex_value_table( EXPORTING name = name
      IMPORTING values = hex_values ).

    LOOP AT hex_values ASSIGNING <hval>.
      " create data reference and set value
      CREATE DATA line LIKE LINE OF value.
      ASSIGN line->* TO <line>.
      CASE linetype->kind.
        WHEN linetype->kind_elem.
          " a simple type
          abap_conversion->convert( EXPORTING input = <hval>
            IMPORTING data = <line> ).
          INSERT <line> INTO TABLE value.
        WHEN linetype->kind_struct.
          " a structur
          convert_structur( EXPORTING hex_value = <hval>
            IMPORTING data = <line> ).
          INSERT <line> INTO TABLE value.
        WHEN OTHERS.
          " not supported
          RAISE EXCEPTION TYPE zcx_dbgl_testcase
            EXPORTING
              textid = zcx_dbgl_testcase=>table_line_type_not_supported
              linetype = linetype->kind.
      ENDCASE.
    ENDLOOP.

  endmethod.


  method IS_UTF16.
    DATA: codepage TYPE cpcodepage.

    CALL FUNCTION 'SCP_CODEPAGE_FOR_LANGUAGE'
      EXPORTING
        language = sy-langu
      IMPORTING
        codepage = codepage.

    IF codepage(1) = '1'.
      unicode = abap_false.
    ELSE.
      unicode = abap_true.
    ENDIF.

  endmethod.
ENDCLASS.
