CLASS test_snapshot_tdc DEFINITION FOR TESTING
  DURATION SHORT RISK LEVEL DANGEROUS FINAL.

  PRIVATE SECTION.
    CONSTANTS: tdc_name TYPE etobj_name VALUE 'ZDBGL_TEST_SNAPSHOT'.

    METHODS setup
      RAISING cx_static_check.

    METHODS delete_snapshot.

    METHODS snapshot
      RAISING cx_static_check.

    METHODS compare
      IMPORTING
        key_tdc_variant TYPE zdbgl_tdc_variant_key
      RAISING cx_static_check.

    METHODS compare_version_1 FOR TESTING
      RAISING cx_static_check.

    METHODS compare_latest_version FOR TESTING
      RAISING cx_static_check.

ENDCLASS.

CLASS test_snapshot_tdc IMPLEMENTATION.

  METHOD setup.

    delete_snapshot( ).
    snapshot( ).

  ENDMETHOD.

  METHOD delete_snapshot.

    TRY.
        cl_apl_ecatt_tdc_api=>delete_tdc( i_name = tdc_name i_version = 1 ).
        ##NO_HANDLER
      CATCH cx_ecatt_tdc_access.
    ENDTRY.

  ENDMETHOD.

  METHOD snapshot.
    DATA: carrier TYPE HASHED TABLE OF scarr WITH UNIQUE KEY carrid.

    carrier = VALUE #(
      ( carrid = 'SAP' carrname = 'SAP Flights' )
      ( carrid = 'IN' carrname = 'Internat Airways' )
    ).
    DATA(flight) = VALUE sflight( carrid = 'SAP' connid = 700 fldate = '20201231' ).

    SET PARAMETER ID 'ZDBGL_SNAP_RECORD' FIELD abap_true.

    DATA(snapshot_manager) = CAST zif_dbgl_snapshots( NEW zdbgl_snapshots_tdc(
      key_tdc_variant = VALUE #( name = tdc_name version = 1 variant_name = 'ECATTDEFAULT' ) ) ).

    snapshot_manager->compare_or_record( name = 'CARRIER' actual = carrier ).
    snapshot_manager->compare_or_record( name = 'FLIGHT' actual = flight ).

    snapshot_manager->commit_changes( ).

  ENDMETHOD.

  METHOD compare.
    DATA: carrier TYPE HASHED TABLE OF scarr WITH UNIQUE KEY carrid.

    carrier = VALUE #(
      ( carrid = 'SAP' carrname = 'SAP Flights' )
      ( carrid = 'IN' carrname = 'Internat Airways' )
    ).

    DATA(flight) = VALUE sflight( carrid = 'SAP' connid = 700 fldate = '20201231' ).

    SET PARAMETER ID 'ZDBGL_SNAP_RECORD' FIELD abap_false.

    DATA(snapshot_manager) = CAST zif_dbgl_snapshots( NEW zdbgl_snapshots_tdc(
      key_tdc_variant = key_tdc_variant ) ).

    snapshot_manager->compare_or_record( name = 'CARRIER' actual = carrier ).
    snapshot_manager->compare_or_record( name = 'FLIGHT' actual = flight ).

  ENDMETHOD.

  METHOD compare_version_1.

    compare( VALUE #( name = tdc_name version = 1 variant_name = 'ECATTDEFAULT' ) ).

  ENDMETHOD.

  METHOD compare_latest_version.

    compare( VALUE #( name = tdc_name variant_name = 'ECATTDEFAULT' use_latest_version = abap_true ) ).

  ENDMETHOD.

ENDCLASS.
