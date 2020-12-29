*&---------------------------------------------------------------------*
*& Report  ZDBGL_SNAPSHOTS
*& This sample reports shows how to create and use snapshots
*& in ABAP unit testing.
*&---------------------------------------------------------------------*
REPORT ZDBGL_SNAPSHOTS.

CLASS test_snapshot DEFINITION FOR TESTING
  DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS flight_test FOR TESTING
      RAISING zcx_dbgl_snapshot.

ENDCLASS.

CLASS test_snapshot IMPLEMENTATION.

  METHOD flight_test.

    DATA(snapshot_manager) = CAST zif_dbgl_snapshots(
      NEW zdbgl_snapshots_tdc( key_tdc_variant =
        VALUE #( name = 'ZDBGL_TEST_SNAPSHOT1' version = 1 variant_name = 'ECATTDEFAULT' )
      )
    ).

    DATA(carrier) = VALUE scarr_tab(
      ( carrid = 'SAP' carrname = 'SAP Flights' )
      ( carrid = 'IN' carrname = 'Internat. Airways' )
    ).
    DATA(flight) = VALUE sflight( carrid = 'SAP' connid = 700 fldate = '20201231' ).

    snapshot_manager->compare_or_record( name = 'CARRIER' actual = carrier ).
    snapshot_manager->compare_or_record( name = 'FLIGHT' actual = flight ).

    " can be omitted, when autosave = abap_true in constructor
    snapshot_manager->commit_changes( ).

  ENDMETHOD.

ENDCLASS.
