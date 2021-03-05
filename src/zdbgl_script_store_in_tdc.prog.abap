*<SCRIPT:PERSISTENT>
REPORT  rstpda_script_template.

*<SCRIPT:HEADER>
*<SCRIPTNAME>ZDBGL_SCRIPT_STORE_GLOBALS</SCRIPTNAME>
*<SCRIPT_CLASS>LCL_DEBUGGER_SCRIPT</SCRIPT_CLASS>
*<SCRIPT_COMMENT>Debugger Skript: Default Template</SCRIPT_COMMENT>
*<SINGLE_RUN>X</SINGLE_RUN>

*</SCRIPT:HEADER>

*<SCRIPT:PRESETTINGS>

*</SCRIPT:PRESETTINGS>

*<SCRIPT:SCRIPT_CLASS>
*---------------------------------------------------------------------*
*       CLASS lcl_debugger_script DEFINITION
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
CLASS lcl_debugger_script DEFINITION INHERITING FROM  cl_tpda_script_class_super  .

  PUBLIC SECTION.
    METHODS: prologue  REDEFINITION,
      init    REDEFINITION,
      script  REDEFINITION,
      end     REDEFINITION.

  PRIVATE SECTION.
    METHODS get_key_tdc_variant
      RETURNING VALUE(result) TYPE zdbgl_tdc_variant_key1.

    METHODS dequeue
      IMPORTING
        key_tdc_variant TYPE zdbgl_tdc_variant_key1.

ENDCLASS.                    "lcl_debugger_script DEFINITION
*---------------------------------------------------------------------*
*       CLASS lcl_debugger_script IMPLEMENTATION
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
CLASS lcl_debugger_script IMPLEMENTATION.
  METHOD prologue.
*** generate abap_source (source handler for ABAP)
    super->prologue( ).
  ENDMETHOD.                    "prolog

  METHOD init.
*** insert your initialization code here
  ENDMETHOD.                    "init

  METHOD script.
    DATA: key_tdc_variant TYPE zdbgl_tdc_variant_key1,
          globals_parser  TYPE REF TO zdbgl_store_globals,
          locals_parser   TYPE REF TO zdbgl_store_locals,
          source_info     TYPE REF TO data,
          exception       TYPE REF TO cx_static_check,
          appl_log        TYPE REF TO zif_logger,
          ##NEEDED
          mtext           TYPE string.
    FIELD-SYMBOLS: <prog> TYPE tpda_curr_source_pos.

    appl_log = zcl_logger_factory=>create_log( ).
    TRY.

        source_info = cl_tpda_ctrl_handler=>get_srcinfo( ).
        ASSIGN source_info->* TO <prog>.

        key_tdc_variant = get_key_tdc_variant( ).
        IF key_tdc_variant IS INITIAL.
          RETURN.
        ENDIF.
        zdbgl_utils=>ask_for_transport_request( CHANGING
          key_tdc_variant = key_tdc_variant-a ).

        MESSAGE s000(zdbgl) INTO mtext.
        appl_log->add( ).
        CREATE OBJECT globals_parser
          EXPORTING
            program = <prog>-program.
        CREATE OBJECT locals_parser.

        zdbgl_copy_to_tdc=>copy_from_debugger(
          tdc_variant_key = key_tdc_variant-a program = <prog>-program
          debugger_parser = globals_parser appl_log = appl_log ).
        zdbgl_copy_to_tdc=>copy_from_debugger(
          tdc_variant_key = key_tdc_variant-a program = <prog>-program
          debugger_parser = locals_parser appl_log = appl_log ).
        IF lines( appl_log->export_to_table( ) ) = 1.
          dequeue( key_tdc_variant ).
          MESSAGE s001(zdbgl) DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.
        COMMIT WORK.

        IF key_tdc_variant-display_app_log = abap_true.
          appl_log->popup( ).
        ENDIF.

      CATCH cx_static_check INTO exception.
        dequeue( key_tdc_variant ).
        MESSAGE exception TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

    me->break( ).

  ENDMETHOD.                    "script

  METHOD dequeue.

    DO 2 TIMES.
      CALL FUNCTION 'DEQUEUE_E_ECATT_TD'
        EXPORTING
          name   = key_tdc_variant-name
          _scope = 1.
    ENDDO.

  ENDMETHOD.

  METHOD end.
*** insert your code which shall be executed at the end of the scripting (before trace is saved)
*** here

  ENDMETHOD.                    "end

  METHOD get_key_tdc_variant.
    DATA: fields        TYPE STANDARD TABLE OF sval
          INITIAL SIZE 3,
          returncode(1).
    FIELD-SYMBOLS: <field> TYPE sval.

    APPEND INITIAL LINE TO fields ASSIGNING <field>.
    <field>-tabname = 'ZDBGL_TDC_VARIANT_KEY1'.
    <field>-fieldname = 'NAME'.
    <field>-fieldtext = text-nam.
    <field>-field_obl = abap_true.
    APPEND INITIAL LINE TO fields ASSIGNING <field>.
    <field>-tabname = 'ZDBGL_TDC_VARIANT_KEY1'.
    <field>-fieldname = 'VERSION'.
    <field>-fieldtext = text-ver.
    <field>-field_obl = abap_true.
    APPEND INITIAL LINE TO fields ASSIGNING <field>.
    <field>-tabname = 'ZDBGL_TDC_VARIANT_KEY1'.
    <field>-fieldname = 'VARIANT_NAME'.
    <field>-fieldtext = text-var.
    <field>-field_obl = abap_true.
    APPEND INITIAL LINE TO fields ASSIGNING <field>.
    <field>-tabname = 'ZDBGL_TDC_VARIANT_KEY1'.
    <field>-fieldname = 'DISPLAY_APP_LOG'.
    <field>-fieldtext = text-dis.

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        popup_title = text-tdc
      IMPORTING
        returncode  = returncode
      TABLES
        fields      = fields.
    IF returncode = 'A'.
      RETURN.
    ENDIF.

    READ TABLE fields ASSIGNING <field> INDEX 1.
    result-name = <field>-value.
    READ TABLE fields ASSIGNING <field> INDEX 2.
    result-version = <field>-value.
    READ TABLE fields ASSIGNING <field> INDEX 3.
    result-variant_name = <field>-value.
    READ TABLE fields ASSIGNING <field> INDEX 4.
    result-display_app_log = <field>-value.

  ENDMETHOD.

ENDCLASS.                    "lcl_debugger_script IMPLEMENTATION
*</SCRIPT:SCRIPT_CLASS>

*</SCRIPT:PERSISTENT>
