*&---------------------------------------------------------------------*
*&  Include           ZMIME_EDITOR_W3_O01
*&---------------------------------------------------------------------*

MODULE init_2000 OUTPUT.
  PERFORM init_2000.
ENDMODULE.

MODULE status_2000 OUTPUT.
  SET PF-STATUS 'STATUS_2000'.
  SET TITLEBAR 'TITLE_2000' WITH p_devc.
ENDMODULE.
