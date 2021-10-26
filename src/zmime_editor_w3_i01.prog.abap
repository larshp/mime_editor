*&---------------------------------------------------------------------*
*&  Include           ZMIME_EDITOR_W3_I01
*&---------------------------------------------------------------------*

MODULE user_command_2000 INPUT.

  CASE gv_ok_code.
    WHEN 'SAVE' OR 'SAVE2'.
      CLEAR gv_ok_code.
      lcl_editor=>save( ).
    WHEN 'BACK'.
      CLEAR gv_ok_code.
      PERFORM back.
  ENDCASE.

ENDMODULE.
