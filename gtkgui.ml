open StdLabels
open Core
open Types
open Top
open Utility

type model = {
  env: Env.env;
  inputs : string list
}

let new_model dict = {
  inputs = ["a retinas"; "p can.."];
  env = { Env.dict = dict; op = Anagram }
}

let utf8 s = Glib.Convert.convert s "UTF-8" "ISO-8859-1"

let display env ws =
  let ws = Wordset.to_list ws in
  let wlist = match env.Env.op with
  | Anagram -> sort_by caps_in ws
  | Build -> sort_by String.length ws
  | _ -> ws
  in
  String.concat ~sep:"\n" wlist

let display_error e =
  "Error: " ^ e

let display_exception e =
  "Exception: " ^ (Exn.to_string e)
  

let eval env str =
  let open Env in
  try
    match Parser.parse str with
    | Ok expr -> display env (Eval.eval env expr)
    | Error m -> display_error m
  with
  | x -> display_exception x

let make_cell_view ~column ~title ~opts =
  let renderer = GTree.cell_renderer_text opts in
  let col = GTree.view_column ~title () in
  col#pack renderer;
  col#set_cell_data_func renderer
    (fun model row ->
       let str = model#get ~row ~column in
       renderer#set_properties [ `TEXT (utf8 str) ]);
  col

class input_widget ~model ~output ?packing ?show () =
  let scrolled_win =
    GBin.scrolled_window ?packing
      ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ()
  in
  let cols = new GTree.column_list in
  let val_col = cols#add Gobject.Data.string in
  let list_model = GTree.list_store cols in
  let make_view ~column = make_cell_view ~column ~title:""
      ~opts: [ `XALIGN 0.; `YPAD 1 ]
  in
  let val_col_view = make_view val_col in
  let view = GTree.view ~model:list_model ~packing:scrolled_win#add () in
  object(self)
    inherit GObj.widget_full scrolled_win#as_widget

    initializer
      List.iter ~f:(fun v ->
          let row = list_model#append () in
          list_model#set ~row ~column:val_col v;
        )
        !model.inputs;
      ignore @@ view#append_column val_col_view;
      ignore @@ view#connect#after#row_activated ~callback: begin
        fun path vcol ->
          let it : Gtk.tree_iter = list_model#get_iter path in
          let v = list_model#get ~row:it ~column:val_col in
          let out = eval !model.env v in
          output#buffer#set_text out
      end
  end

let () =
  let dict = Trie.load_from_text_file "csw15.lower" in
  let model = ref (new_model dict) in
  let _locale = GMain.init ~setlocale:true () in
  let w = GWindow.window () in
  ignore @@ w#connect#destroy ~callback:GMain.quit;
  let vbox = GPack.vbox ~packing:w#add () in
  let hbox = GPack.hbox ~packing:vbox#add () in
  let vb1 = GPack.vbox ~packing:hbox#add () in
  let vb2 = GPack.vbox ~packing:hbox#add () in
  let scroll = GBin.scrolled_window
      ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC
      ~packing:vb2#add () in
  let output = GText.view ~packing:scroll#add () in
  let _input = new input_widget ~packing:vb1#add ~model ~output () in

  (* set text *)
  output#buffer#set_text "multi-\nline\ntext";

  let quit = GButton.button ~label:"Quit" ~packing:vbox#pack () in
  ignore @@ quit#connect#clicked ~callback:GMain.quit;
  w#show ();
  GMain.main ()
