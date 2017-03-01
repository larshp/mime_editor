REPORT zmime_editor.

DATA: gv_ok_code   LIKE sy-ucomm,
      gv_init      TYPE abap_bool,
      go_container TYPE REF TO cl_gui_custom_container,
      go_editor    TYPE REF TO cl_gui_textedit,
      gs_smimloio  TYPE smimloio.

PARAMETERS: p_loio TYPE smimloio-loio_id OBLIGATORY.

START-OF-SELECTION.
  PERFORM run.

FORM run.

  SELECT SINGLE * FROM smimloio INTO gs_smimloio
    WHERE loio_id = p_loio.                             "#EC CI_GENBUFF
  IF sy-subrc <> 0.
    WRITE: / 'Not found'(001).
    RETURN.
  ENDIF.

  IF gs_smimloio-lo_class = wbmr_c_skwf_folder_class.
    WRITE: / 'Is folder'(002).
    RETURN.
  ENDIF.

  gv_init = abap_false.

  CALL SCREEN 2000.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_2000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_2000 OUTPUT.
  SET PF-STATUS 'STATUS_2000'.
  SET TITLEBAR 'TITLE_2000'.
ENDMODULE.

FORM get_content CHANGING cv_content TYPE string.

  DATA: li_api     TYPE REF TO if_mr_api,
        lv_content TYPE xstring,
        lv_url     TYPE string,
        lv_len     TYPE i,
        lo_obj     TYPE REF TO cl_abap_conv_in_ce.


  PERFORM get_url CHANGING lv_url.

  li_api = cl_mime_repository_api=>if_mr_api~get_api( ).
  li_api->get(
    EXPORTING
      i_url              = lv_url
    IMPORTING
      e_content          = lv_content
    EXCEPTIONS
      parameter_missing  = 1
      error_occured      = 2
      not_found          = 3
      permission_failure = 4
      OTHERS             = 5 ).
  IF sy-subrc <> 0.
    BREAK-POINT.
  ENDIF.

  TRY.
      lo_obj = cl_abap_conv_in_ce=>create(
          input    = lv_content
          encoding = 'UTF-8' ).
      lv_len = xstrlen( lv_content ).

      lo_obj->read( EXPORTING n    = lv_len
                    IMPORTING data = cv_content ).

    CATCH cx_parameter_invalid_range
          cx_sy_codepage_converter_init
          cx_sy_conversion_codepage
          cx_parameter_invalid_type.
      BREAK-POINT.
  ENDTRY.

ENDFORM.

FORM get_url CHANGING cv_url TYPE string.

  DATA: lv_url TYPE skwf_url,
        ls_io  TYPE skwf_io.


  ls_io-objtype = skwfc_obtype_loio.
  ls_io-class = gs_smimloio-lo_class.
  ls_io-objid = gs_smimloio-loio_id.

  CALL FUNCTION 'SKWF_NMSPC_IO_ADDRESS_GET'
    EXPORTING
      io  = ls_io
    IMPORTING
      url = lv_url.

  cv_url = lv_url.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_2000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_2000 INPUT.

  CASE gv_ok_code.
    WHEN 'SAVE'.
      CLEAR gv_ok_code.
      PERFORM save.
    WHEN 'BACK'.
      CLEAR gv_ok_code.
      PERFORM back.
  ENDCASE.

ENDMODULE.

FORM save.

  DATA: lv_content  TYPE xstring,
        lv_string   TYPE string,
        lv_lines    TYPE i,
        lv_filename TYPE skwf_filnm,
        li_api      TYPE REF TO if_mr_api,
        lv_url      TYPE string,
        ls_skwf_io  TYPE skwf_io,
        lt_strings  TYPE TABLE OF string,
        lo_obj      TYPE REF TO cl_abap_conv_out_ce.


  go_editor->get_textstream( IMPORTING text = lv_string ).
  cl_gui_cfw=>flush( ).

  TRY.
      lo_obj = cl_abap_conv_out_ce=>create( encoding = 'UTF-8' ).
      lo_obj->convert( EXPORTING data = lv_string
                       IMPORTING buffer = lv_content ).
    CATCH cx_parameter_invalid_range
          cx_sy_codepage_converter_init
          cx_sy_conversion_codepage
          cx_parameter_invalid_type.
      BREAK-POINT.
  ENDTRY.

  PERFORM get_url CHANGING lv_url.

  SPLIT lv_url AT '/' INTO TABLE lt_strings.
  lv_lines = lines( lt_strings ).
  ASSERT lv_lines > 0.
  READ TABLE lt_strings INDEX lv_lines INTO lv_filename.
  ASSERT sy-subrc = 0.

  ls_skwf_io-objid = p_loio.

  cl_wb_mime_repository=>determine_io_class(
    EXPORTING
      filename = lv_filename
    IMPORTING
      io_class = ls_skwf_io-class ).
  CONCATENATE ls_skwf_io-class '_L' INTO ls_skwf_io-class.

  li_api = cl_mime_repository_api=>if_mr_api~get_api( ).
  li_api->put(
    EXPORTING
      i_url                   = lv_url
      i_content               = lv_content
      i_new_loio              = ls_skwf_io
    EXCEPTIONS
      parameter_missing       = 1
      error_occured           = 2
      cancelled               = 3
      permission_failure      = 4
      data_inconsistency      = 5
      new_loio_already_exists = 6
      is_folder               = 7
      OTHERS                  = 8 ).
  IF sy-subrc <> 0.
    BREAK-POINT.
  ENDIF.

  MESSAGE 'Saved'(003) TYPE 'S'.

  go_editor->set_textmodified_status( 0 ).

ENDFORM.

FORM back.

  DATA: lv_status TYPE i.

  go_editor->get_textmodified_status( IMPORTING status = lv_status ).
  cl_gui_cfw=>flush( ).
  IF lv_status = 0.
    LEAVE TO SCREEN 0.
  ELSE.
    MESSAGE 'Not saved'(004) TYPE 'W'.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  INIT_2000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE init_2000 OUTPUT.

  PERFORM init_2000.

ENDMODULE.

FORM init_2000.

  DATA: lv_content  TYPE string.

  IF NOT go_container IS BOUND.
    CREATE OBJECT go_container
      EXPORTING
        container_name = 'CUSTOM_2000'.

    CREATE OBJECT go_editor
      EXPORTING
        parent = go_container.
    go_editor->set_font_fixed( ).
  ENDIF.

  IF gv_init = abap_false.
    PERFORM get_content CHANGING lv_content.
    go_editor->set_textstream( lv_content ).
    go_editor->set_focus( go_editor ).
    gv_init = abap_true.
  ENDIF.

ENDFORM.
