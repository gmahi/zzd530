CLASS lhc_booking DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS create FOR MODIFY
      IMPORTING entities FOR CREATE booking.

    METHODS delete FOR MODIFY
      IMPORTING keys FOR DELETE booking.

    METHODS update FOR MODIFY
      IMPORTING entities FOR UPDATE booking.

    METHODS read FOR READ
      IMPORTING keys FOR READ booking RESULT result.

ENDCLASS.

CLASS lhc_booking IMPLEMENTATION.

  METHOD create.
  ENDMETHOD.

  METHOD delete.
  ENDMETHOD.

  METHOD update.
  ENDMETHOD.

  METHOD read.
  ENDMETHOD.

ENDCLASS.
