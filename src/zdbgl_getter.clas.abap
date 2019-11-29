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
      ZCX_DBGL_TESTCASE
      ZCX_DBGL_TYPE_NOT_SUPPORTED.
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

  methods GET_HEX_VALUE
    importing
      !NAME type STRING
    returning
      value(VALUE) type STRING
    raising
      ZCX_DBGL_TESTCASE .
  methods _GET_SIMPLE
    importing
      !BASE64_VALUE type STRING
    exporting
      !VALUE type SIMPLE .
  methods LINE_HAS_SIMPLE_TYPE
    IMPORTING
      line TYPE any
    RETURNING VALUE(is_simple) TYPE sap_bool.
ENDCLASS.



CLASS ZDBGL_GETTER IMPLEMENTATION.


  method CONSTRUCTOR.

    me->program = program.

    json_xtext = cl_abap_codepage=>convert_to( values ).
    abap_conversion = cl_abap_conv_in_ce=>create( ).

  endmethod.


METHOD GET_HEX_VALUE.
  DATA: node       TYPE REF TO if_sxml_node,
        node_open_element TYPE REF TO if_sxml_open_element,
        node_found TYPE sap_bool,
        reader TYPE REF TO if_sxml_reader,
        attribute TYPE REF TO if_sxml_attribute,
        object_level TYPE i.

  " create reader in every call, loop through nodes is statefull
  reader = cl_sxml_string_reader=>create( json_xtext ).
  DO.
    node = reader->read_next_node( ).
    IF node IS INITIAL.
      EXIT.
    ENDIF.

    IF node->type = if_sxml_node=>co_nt_element_open.

      node_open_element = CAST if_sxml_open_element( node ).
      LOOP AT node_open_element->get_attributes( ) INTO attribute.
        IF attribute->qname-name = 'name' AND attribute->get_value( ) = name
          AND node_open_element->if_sxml_named~qname-name = 'str'
          AND object_level = 1.
          node_found = abap_true.
        ENDIF.
      ENDLOOP.

      IF node_open_element->if_sxml_named~qname-name = 'object'.
        ADD 1 TO object_level.
      ENDIF.

    ELSEIF node->type = if_sxml_node=>co_nt_value
      AND node_found = abap_true.

      value = CAST if_sxml_value_node( node )->get_value( ).
      RETURN.

    ELSEIF node->type = if_sxml_node=>co_nt_element_close.

      IF CAST if_sxml_close_element( node )->if_sxml_named~qname-name = 'object'.
        object_level = object_level - 1.
      ENDIF.

    ENDIF.

  ENDDO.

  RAISE EXCEPTION TYPE zcx_dbgl_testcase
    EXPORTING
      textid   = zcx_dbgl_testcase=>variable_not_found
      program  = program
      variable = name.

ENDMETHOD.


  method GET_SIMPLE.

    _get_simple( EXPORTING base64_value = get_hex_value( name )
      IMPORTING value = value ).

  endmethod.


METHOD get_structur.
  DATA: node                       TYPE REF TO if_sxml_node,
        node_open_element          TYPE REF TO if_sxml_open_element,
        node_for_structure_pending TYPE sap_bool,
        reader                     TYPE REF TO if_sxml_reader,
        attribute                  TYPE REF TO if_sxml_attribute,
        object_level               TYPE i.
  FIELD-SYMBOLS: <component> TYPE any.

  " create reader in every call, loop through nodes is statefull
  reader = cl_sxml_string_reader=>create( json_xtext ).
  DO.
    node = reader->read_next_node( ).
    IF node IS INITIAL.
      EXIT.
    ENDIF.

    IF node->type = if_sxml_node=>co_nt_element_open.

      node_open_element = CAST if_sxml_open_element( node ).

      IF ( node_open_element->if_sxml_named~qname-name = 'array'
        OR node_open_element->if_sxml_named~qname-name = 'object' )
        AND node_for_structure_pending = abap_true.

        RAISE EXCEPTION TYPE zcx_dbgl_type_not_supported
          EXPORTING
            type = |deep structure with name { name }|.

      ENDIF.

      LOOP AT node_open_element->get_attributes( ) INTO attribute.

        IF attribute->qname-name = 'name'.
          IF node_open_element->if_sxml_named~qname-name = 'object'
          AND attribute->get_value( ) = name AND object_level = 1.
            node_for_structure_pending = abap_true.
          ELSEIF node_open_element->if_sxml_named~qname-name = 'str'
          AND node_for_structure_pending = abap_true.
            ASSIGN COMPONENT attribute->get_value( ) OF STRUCTURE value
              TO <component>.
          ENDIF.
        ENDIF.

      ENDLOOP.

      IF node_open_element->if_sxml_named~qname-name = 'object'.
        ADD 1 TO object_level.
      ENDIF.

    ELSEIF node->type = if_sxml_node=>co_nt_value
      AND node_for_structure_pending = abap_true.

      _get_simple( EXPORTING base64_value = CAST if_sxml_value_node( node )->get_value( )
        IMPORTING value = <component> ).

    ELSEIF node->type = if_sxml_node=>co_nt_element_close.

      IF CAST if_sxml_close_element( node )->if_sxml_named~qname-name = 'object'.
        IF node_for_structure_pending = abap_true.
          " object finished
          RETURN.
        ENDIF.
        object_level = object_level - 1.
      ENDIF.

    ENDIF.

  ENDDO.

  " value for variable <name> not found
  RAISE EXCEPTION TYPE zcx_dbgl_testcase
    EXPORTING
      textid   = zcx_dbgl_testcase=>variable_not_found
      program  = program
      variable = name.

ENDMETHOD.


METHOD get_table.
  DATA: node                   TYPE REF TO if_sxml_node,
        node_for_table_pending TYPE sap_bool,
        reader                 TYPE REF TO if_sxml_reader,
        line                   TYPE REF TO data,
        node_open_element      TYPE REF TO if_sxml_open_element,
        object_element_name    TYPE string,
        attribute              TYPE REF TO if_sxml_attribute,
        object_level           TYPE i.
  FIELD-SYMBOLS: <line>         TYPE any,
                 <line_element> TYPE any.

  CREATE DATA line LIKE LINE OF value.
  ASSIGN line->* TO <line>.

  " create reader in every call, loop through nodes is statefull
  reader = cl_sxml_string_reader=>create( json_xtext ).
  DO.
    node = reader->read_next_node( ).
    IF node IS INITIAL.
      EXIT.
    ENDIF.

    IF node->type = if_sxml_node=>co_nt_element_open.

      node_open_element = CAST if_sxml_open_element( node ).
      attribute_handler.
      prepare_table_line.

    ELSEIF node->type = if_sxml_node=>co_nt_value
      AND node_for_table_pending = abap_true.

      _get_simple( EXPORTING base64_value = CAST if_sxml_value_node( node )->get_value( )
        IMPORTING value = <line_element> ).

    ELSEIF node->type = if_sxml_node=>co_nt_element_close.

      close_element_handler.

    ENDIF.

  ENDDO.

  RAISE EXCEPTION TYPE zcx_dbgl_testcase
    EXPORTING
      textid   = zcx_dbgl_testcase=>variable_not_found
      program  = program
      variable = name.

ENDMETHOD.


  method LINE_HAS_SIMPLE_TYPE.

    IF cl_abap_typedescr=>describe_by_data( line )->kind =
      cl_abap_typedescr=>kind_elem.
      is_simple = abap_true.
    ENDIF.

  endmethod.


  method _GET_SIMPLE.
    DATA: hex_value TYPE xstring.

    hex_value = cl_http_utility=>decode_x_base64( base64_value ).

    abap_conversion->convert( EXPORTING input = hex_value
      IMPORTING data = value ).

  endmethod.
ENDCLASS.
