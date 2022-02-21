CLASS zdbgl_utils DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS ask_for_transport_request
      IMPORTING
                VALUE(package_name) TYPE devclass OPTIONAL
      CHANGING  key_tdc_variant     TYPE zdbgl_tdc_variant_key
      RAISING   cx_ecatt_tdc_access.

    CLASS-METHODS must_add_to_transport_request
      IMPORTING
                VALUE(package_name) TYPE devclass OPTIONAL
                key_tdc_variant     TYPE zdbgl_tdc_variant_key
      RETURNING VALUE(result)       TYPE sap_bool
      RAISING   cx_ecatt_tdc_access.

    CLASS-METHODS create_tadir_entry
      IMPORTING
                tdc_name            TYPE etobj_name
      RETURNING VALUE(package_name) TYPE devclass.

  PROTECTED SECTION.

  PRIVATE SECTION.

    CLASS-METHODS transport_request_is_mandatory
      IMPORTING
                package_name        TYPE devclass
      RETURNING VALUE(is_mandatory) TYPE abap_bool
      RAISING   cx_ecatt_tdc_access.

    CLASS-METHODS tdc_is_part_of_open_task
      IMPORTING
                key_tdc_variant TYPE zdbgl_tdc_variant_key
      RETURNING VALUE(result)   TYPE abap_bool.

    CLASS-METHODS read_package_name
      IMPORTING
                key_tdc_variant TYPE zdbgl_tdc_variant_key
      RETURNING VALUE(result)   TYPE devclass
      RAISING   cx_ecatt_tdc_access.

ENDCLASS.



CLASS ZDBGL_UTILS IMPLEMENTATION.


  METHOD ask_for_transport_request.

    IF package_name IS NOT SUPPLIED.
      package_name = read_package_name( key_tdc_variant ).
    ENDIF.

    IF transport_request_is_mandatory( package_name ) = abap_true
      AND tdc_is_part_of_open_task( key_tdc_variant ) = abap_false.

      CALL FUNCTION 'TR_POPUP_INPUT_REQUEST'
        IMPORTING
          ev_trkorr = key_tdc_variant-transport_request.

    ENDIF.

  ENDMETHOD.


  METHOD create_tadir_entry.
    DATA package TYPE tdevc.

    CALL FUNCTION 'TR_TADIR_POPUP_ENTRY_E071'
      EXPORTING
        wi_e071_pgmid    = 'R3TR'
        wi_e071_object   = cl_apl_ecatt_const=>obj_type_test_data
        wi_e071_obj_name = CONV e071-obj_name( tdc_name )
      IMPORTING
        es_tdevc         = package.
    package_name = package-devclass.

  ENDMETHOD.


  METHOD must_add_to_transport_request.

    IF package_name IS NOT SUPPLIED.
      package_name = read_package_name( key_tdc_variant ).
    ENDIF.

    result = xsdbool( transport_request_is_mandatory( package_name ) = abap_true AND
      tdc_is_part_of_open_task( key_tdc_variant ) = abap_false ).

  ENDMETHOD.


  METHOD read_package_name.

    SELECT SINGLE devclass FROM tadir INTO result
      WHERE pgmid = 'R3TR' AND object = cl_apl_ecatt_const=>obj_type_test_data
      AND obj_name = key_tdc_variant-name.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_ecatt_tdc_access
        EXPORTING
          textid        = cx_ecatt_tdc_access=>object_not_found
          last_obj_name = key_tdc_variant-name
          last_obj_type = CONV string( cl_apl_ecatt_const=>obj_type_test_data )
          last_obj_ver  = CONV string( key_tdc_variant-version ).
    ENDIF.

  ENDMETHOD.


  METHOD tdc_is_part_of_open_task.

    SELECT COUNT(*) FROM e071
      WHERE pgmid = 'R3TR' AND object = cl_apl_ecatt_const=>obj_type_test_data
      AND obj_name = key_tdc_variant-name AND lockflag = abap_true.
    IF sy-subrc = 0.
      result = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD transport_request_is_mandatory.

    cl_package_helper=>check_package_name(
     EXPORTING i_package_name = package_name
     IMPORTING e_package_type = DATA(package_type) ).
    IF package_type <> '$'.
      is_mandatory = abap_true.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
