class ZDBGL_ABSTRACT_STORAGE definition
  public
  abstract
  create public .

public section.
protected section.

  constants:
    QUOTE(1) value '"' ##NO_TEXT.
  constants:
    COLON(1) value ':' ##NO_TEXT.
  constants:
    COMMA(1) value ',' ##NO_TEXT.
  data:
    JSON_FRAGMENTS type standard table of STRING.

  methods HANDLE_TAB
    importing
      !IS_OBJECT      type ABAP_BOOL
      !NAME           type STRING
      !DESCR          type ref to CL_TPDA_SCRIPT_DATA_DESCR
    returning
      value(FRAGMENT) type STRING
    raising
      CX_TPDA
      ZCX_DBGL_TYPE_NOT_SUPPORTED .
  methods HANDLE_OBJECT
    importing
      !NAME  type STRING
      !DESCR type ref to CL_TPDA_SCRIPT_DATA_DESCR
    raising
      CX_TPDA
      ZCX_DBGL_TYPE_NOT_SUPPORTED .
  methods HANDLE_OBJREF
    importing
      !NAME type STRING
    raising
      CX_TPDA
      ZCX_DBGL_TYPE_NOT_SUPPORTED .
  methods HANDLE_STRING
    importing
      !IS_OBJECT      type ABAP_BOOL
      !NAME           type STRING
      !DESCR          type ref to CL_TPDA_SCRIPT_DATA_DESCR
    returning
      value(FRAGMENT) type STRING
    raising
      CX_TPDA .
  methods HANDLE_SIMPLE
    importing
      !IS_OBJECT      type ABAP_BOOL
      !NAME           type STRING
      !DESCR          type ref to CL_TPDA_SCRIPT_DATA_DESCR
    returning
      value(FRAGMENT) type STRING
    raising
      CX_TPDA .
  methods HANDLE_STRUCT
    importing
      !IS_OBJECT      type ABAP_BOOL
      !NAME           type STRING
      !DESCR          type ref to CL_TPDA_SCRIPT_DATA_DESCR
    returning
      value(FRAGMENT) type STRING
    raising
      CX_TPDA
      ZCX_DBGL_TYPE_NOT_SUPPORTED .
  methods HANDLE_DATAREF
    importing
      !NAME  type STRING
      !DESCR type ref to CL_TPDA_SCRIPT_DATA_DESCR
    raising
      CX_TPDA
      ZCX_DBGL_TYPE_NOT_SUPPORTED .
  methods _HANDLE
    importing
      !NAME           type STRING
      !IS_OBJECT      type ABAP_BOOL
    returning
      value(FRAGMENT) type STRING
    raising
      CX_TPDA
      ZCX_DBGL_TYPE_NOT_SUPPORTED .
  methods CONCAT_JSON_FRAGMENTS_STRING
    returning
      value(JSON_AS_STRING) type STRING .
private section.
ENDCLASS.



CLASS ZDBGL_ABSTRACT_STORAGE IMPLEMENTATION.


  METHOD concat_json_fragments_string.

    json_as_string = '{'.
    LOOP AT json_fragments ASSIGNING FIELD-SYMBOL(<frag>).

      IF sy-tabix = 1.
        json_as_string = json_as_string && <frag>.
      ELSE.
        json_as_string = json_as_string && comma
          && <frag>.
      ENDIF.

    ENDLOOP.
    json_as_string = json_as_string && '}'.

  ENDMETHOD.


  method HANDLE_DATAREF.

    RAISE EXCEPTION TYPE zcx_dbgl_type_not_supported
      EXPORTING
        type = 'DATAREF'.

  endmethod.


  method HANDLE_OBJECT.

    RAISE EXCEPTION TYPE zcx_dbgl_type_not_supported
      EXPORTING
        type = 'OBJECT'.

  endmethod.


  method HANDLE_OBJREF.

    RAISE EXCEPTION TYPE zcx_dbgl_type_not_supported
      EXPORTING
        type = 'OBJECTREF'.

  endmethod.


  method HANDLE_SIMPLE.
    data_serialize_hex_format cl_tpda_script_elemdescr.
    serialize_hex_format.
  endmethod.


  method HANDLE_STRING.
    data_serialize_hex_format cl_tpda_script_stringdescr.
    serialize_hex_format.
  endmethod.


  method HANDLE_STRUCT.

    DATA(struct_descr) = CAST cl_tpda_script_structdescr( descr ).
    struct_descr->components( IMPORTING p_components_it = DATA(components) ).

    IF is_object = abap_true.
      fragment = quote && name && quote && colon.
    ENDIF.
    fragment = fragment && '{'.
    LOOP AT components REFERENCE INTO DATA(component).

      DATA(tabix) = sy-tabix.
      fragment = fragment && quote && component->*-compname && quote && colon &&
        _handle( name = |{ name }-{ component->*-compname }| is_object = abap_false ).
      IF tabix < lines( components ).
        fragment = fragment && comma.
      ENDIF.

    ENDLOOP.
    fragment = fragment && '}'.

  endmethod.


  method HANDLE_TAB.
    DATA: tab_fragments TYPE STANDARD TABLE OF string,
          table TYPE REF TO cl_tpda_script_tabledescr,
          name_line TYPE string,
          len TYPE i.
    FIELD-SYMBOLS: <frag> TYPE string.

    table ?= descr.
    DO table->linecnt( ) TIMES.
      name_line = |{ name }[{ sy-index }]|.
      " if _handle( name_line ) is an json object or an json table
      " it is enclosed in brackets
      APPEND _handle( name = name_line is_object = abap_false )
        TO tab_fragments.
    ENDDO.

    fragment = '['.
    LOOP AT tab_fragments ASSIGNING <frag>.
      fragment = fragment && <frag>.
      IF sy-tabix < lines( tab_fragments ).
        fragment = fragment && comma.
      ENDIF.
    ENDLOOP.
    fragment = fragment && ']'.

    IF is_object = abap_true.
      fragment = quote && name && quote && colon && fragment.
    ENDIF.

  endmethod.


  method _HANDLE.
    DATA: descr TYPE REF TO cl_tpda_script_data_descr,
          info  TYPE tpda_scr_quick_info.

    descr = cl_tpda_script_data_descr=>factory( name ).
    info = cl_tpda_script_data_descr=>get_quick_info( name ).

    CASE info-metatype.
      WHEN cl_tpda_script_data_descr=>mt_simple.
        fragment = handle_simple( name = name is_object = is_object
                       descr = descr ).
      WHEN cl_tpda_script_data_descr=>mt_struct.
        fragment = handle_struct( name  = name is_object = is_object
                       descr = descr ).
      WHEN cl_tpda_script_data_descr=>mt_string.
        fragment = handle_string( name  = name is_object = is_object
                       descr = descr ).
      WHEN cl_tpda_script_data_descr=>mt_tab.
        fragment = handle_tab( name  = name is_object = is_object
                    descr = descr ).
      WHEN cl_tpda_script_data_descr=>mt_datref.
        handle_dataref( name  = name
                        descr = descr ).
      WHEN cl_tpda_script_data_descr=>mt_object.
        handle_object( name  = name
                       descr = descr ).
      WHEN cl_tpda_script_data_descr=>mt_objref.
        handle_objref( name ).
      WHEN OTHERS.
        ASSERT FIELDS 'unknown type' CONDITION 1 = 0.
    ENDCASE.

  endmethod.
ENDCLASS.
