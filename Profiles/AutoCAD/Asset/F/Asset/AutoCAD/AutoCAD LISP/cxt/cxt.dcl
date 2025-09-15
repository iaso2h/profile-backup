//---------------------------------------------------------------------------------------------------------
// MyAlert2
// Note: Added key "Text2" tile and changed widths to 30.6 for example.
//---------------------------------------------------------------------------------------------------------
CXT : dialog {
  key = "Title";
  label = "";//Title$ from lsp file
  spacer;
  : text {
    key = "Text1";
    label = "";//Message1$ from lsp file
    width = 30.6;
  }
  : text {
    key = "Text2";
    label = "";//Message2$ from lsp file
    width = 30.6;
  }
  spacer;
  : row {
    fixed_width = true;
    alignment = centered;
    : ok_button {
      label = "OK";
      width = 8.59;
      is_cancel = true;
    }
    : button {
      key = "Help";
      label = "Help";
      width = 8.59;
      fixed_width = true;
    }
  }
}//MyAlert2