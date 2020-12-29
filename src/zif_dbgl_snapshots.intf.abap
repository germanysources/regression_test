INTERFACE zif_dbgl_snapshots
  PUBLIC .

  METHODS compare_or_record
    IMPORTING
      name TYPE clike
      actual TYPE any
    RETURNING VALUE(unequal) TYPE sap_bool
    RAISING
      zcx_dbgl_snapshot.

  METHODS commit_changes
    RAISING
      zcx_dbgl_snapshot.

ENDINTERFACE.
