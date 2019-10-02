class ZDBGL_COPY_TO_TDC definition
  public
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !TDC type ETOBJ_NAME
      !TDC_VERSION type ETOBJ_VER
    raising
      ZCX_DBGL_COPY_ERROR .
  methods COPY_ALL_PARAMETER
    importing
      !VARIANT type ETVAR_ID
      !RECORDED_VARIABLES type ref to ZDBGL_GETTER
    raising
      ZCX_DBGL_COPY_ERROR .
  methods SAVE
    importing
      !TRANSPORT_ORDER type E070-TRKORR OPTIONAL
    raising
      ZCX_DBGL_COPY_ERROR .
protected section.
private section.

  CONSTANTS: table_pattern TYPE string VALUE 'STANDARD TABLE OF *'.
  data TDC type ref to CL_APL_ECATT_TDC_API .

  methods CREATE_PARAMETER_INSTANCE
    importing
      !NAME type ETP_NAME
    returning
      value(RESULT) type ref to DATA
    RAISING
      cx_ecatt_tdc_access.
  methods TYPEOF_PARAMETER
    importing
      !PARAM_VALUE type ref to DATA
    returning
      value(category) type ABAP_TYPECATEGORY
    RAISING
      cx_ecatt_tdc_access.
  METHODS create_variant_if_not_exists
    IMPORTING
      variant TYPE etvar_id
    RAISING
      cx_ecatt_tdc_access.
ENDCLASS.



CLASS ZDBGL_COPY_TO_TDC IMPLEMENTATION.


  METHOD constructor.

    TRY.
        me->tdc = cl_apl_ecatt_tdc_api=>get_instance( i_testdatacontainer = tdc
          i_write_access = abap_true i_testdatacontainer_version = tdc_version ).

      CATCH cx_ecatt_tdc_access INTO DATA(ecatt_failure).
        zcx_dbgl_copy_error=>wrap_failure( ecatt_failure ).
    ENDTRY.

  ENDMETHOD.


  method COPY_ALL_PARAMETER.
    DATA: param_value TYPE REF TO data.
    FIELD-SYMBOLS: <param_value> TYPE any.

    TRY.
        create_variant_if_not_exists( variant ).

        LOOP AT tdc->get_param_list( ) REFERENCE INTO DATA(parameter).

          param_value = create_parameter_instance( parameter->* ).
          ASSIGN param_value->* TO <param_value>.
          CASE typeof_parameter( param_value ).
            WHEN cl_abap_datadescr=>kind_elem.
              recorded_variables->get_simple(
                EXPORTING name = conv string( parameter->* )
                IMPORTING value = <param_value> ).
            WHEN cl_abap_datadescr=>kind_struct.
              recorded_variables->get_structur(
                EXPORTING name = conv string( parameter->* )
                IMPORTING value = <param_value> ).
            WHEN cl_abap_datadescr=>kind_table.
              recorded_variables->get_table(
                EXPORTING name = conv string( parameter->* )
                IMPORTING value = <param_value> ).
          ENDCASE.
          tdc->set_value_ref( EXPORTING i_param_name = parameter->*
            i_variant_name = variant i_param_ref = param_value ).

        ENDLOOP.

      CATCH cx_ecatt_tdc_access INTO DATA(ecatt_failure).
        zcx_dbgl_copy_error=>wrap_failure( ecatt_failure ).
      CATCH zcx_dbgl_testcase INTO DATA(recording_failure).
        zcx_dbgl_copy_error=>wrap_failure( recording_failure ).
    ENDTRY.

  endmethod.


  method CREATE_PARAMETER_INSTANCE.
    DATA: table_type(30).

    DATA(param_def) = tdc->get_param_definition( name ).

    IF param_def CP table_pattern.
      DATA(offset) = strlen( table_pattern ) - 1.
      table_type = param_def+offset.
      CONDENSE table_type NO-GAPS.
      CREATE DATA result TYPE STANDARD TABLE OF (table_type).
      RETURN.
    ENDIF.

    " structured and elementary types can be created directly
    CREATE DATA result TYPE (param_def).

  endmethod.


  method CREATE_VARIANT_IF_NOT_EXISTS.

    DATA(existing_variants) = tdc->get_variant_list( ).
    IF line_exists( existing_variants[ table_line = variant ] ).
      RETURN.
    ENDIF.

    tdc->create_variant( i_variant_name = variant ).

  endmethod.


  METHOD save.

    TRY.
        tdc->commit_changes( i_tr_order = transport_order
          i_release_lock = abap_true ).

      CATCH cx_ecatt_tdc_access INTO DATA(ecatt_failure).
        zcx_dbgl_copy_error=>wrap_failure( ecatt_failure ).
    ENDTRY.

  ENDMETHOD.


  method TYPEOF_PARAMETER.

    category = cl_abap_datadescr=>describe_by_data_ref( param_value )->kind.

  endmethod.
ENDCLASS.
