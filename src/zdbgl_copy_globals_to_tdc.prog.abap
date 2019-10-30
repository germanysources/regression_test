*&---------------------------------------------------------------------*
*& Report  ZDBGL_COPY_GLOBALS_TO_TDC
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZDBGL_COPY_GLOBALS_TO_TDC.

PARAMETERS: program TYPE progname,
  key_tc TYPE zdbgl_key_testcases,
  tdc TYPE etobj_name,
  version TYPE etobj_ver,
  variant TYPE etvar_id.

START-OF-SELECTION.

  TRY.
      DATA(base64_storage) = zdbgl_get_globals=>factory(
        key_testcase = key_tc program = program
      ).
      DATA(tdc_manager) = NEW zdbgl_copy_to_tdc( tdc = tdc
        tdc_version = version ).
      tdc_manager->copy_all_parameter( variant = variant
        recorded_variables = base64_storage ).
      tdc_manager->save( ).
      MESSAGE text-sav TYPE 'S'.

    CATCH zcx_dbgl_testcase INTO DATA(failure).
      MESSAGE failure TYPE 'S' DISPLAY LIKE 'E'.
    CATCH zcx_dbgl_copy_error INTO DATA(copy_failure).
      MESSAGE copy_failure TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.
