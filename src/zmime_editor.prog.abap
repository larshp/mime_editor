REPORT zmime_editor.

* See https://github.com/larshp/mime_editor

********************************************************************************
* The MIT License (MIT)
*
* Copyright (c) 2017 Lars Hvam
*
* Permission is hereby granted, free of charge, to any person obtaining a copy
* of this software and associated documentation files (the "Software"), to deal
* in the Software without restriction, including without limitation the rights
* to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
* copies of the Software, and to permit persons to whom the Software is
* furnished to do so, subject to the following conditions:
*
* The above copyright notice and this permission notice shall be included in all
* copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
* OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
* SOFTWARE.
********************************************************************************

DATA: gv_ok_code   LIKE sy-ucomm,
      gv_init      TYPE abap_bool,
      go_container TYPE REF TO cl_gui_custom_container,
      go_editor    TYPE REF TO cl_gui_textedit,
      gs_smimloio  TYPE smimloio.

PARAMETERS: p_loio TYPE smimloio-loio_id OBLIGATORY.

INCLUDE zmime_editor_f01.
INCLUDE zmime_editor_o01.
INCLUDE zmime_editor_i01.

START-OF-SELECTION.
  PERFORM run.
