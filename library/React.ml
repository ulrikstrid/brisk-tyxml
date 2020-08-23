module RemoteAction = Brisk_reconciler.RemoteAction

(*let () = print_endline ("parent - " ^ ele) in
          let () = print_string "child - " in
          let () =
            List.iter
              Tyxml.Xml.(
                function
                | Empty -> print_endline "empty"
                | Comment s -> print_endline ("comment " ^ s)
                | EncodedPCDATA s -> print_endline ("EPCDATA " ^ s)
                | PCDATA s -> print_endline ("PCDATA " ^ s)
                | Entity s -> print_endline ("Entity " ^ s)
                | Leaf (name, _) -> print_endline ("Leaf " ^ name)
                | Node (name, _, _) -> print_endline ("Node " ^ name))
              (child :: children)
          in*)

let insertNode (type a) ~(parent : a Tyxml.Html.elt)
    ~(child : 'b Tyxml.Html.elt) ~position:_ : a Tyxml.Html.elt =
  match Obj.magic (Tyxml.Html.toelt parent) with
  | Tyxml_xml.Node (ele, a, children) ->
      let child = Tyxml.Html.toelt child in
      Tyxml_xml.node ~a ele (List.rev (child :: children)) |> Tyxml.Html.tot
  | _ -> parent

let deleteNode (type a) ~(parent : a Tyxml.Html.elt)
    ~(child : 'b Tyxml.Html.elt) ~position:_ : a Tyxml.Html.elt =
  ( match Obj.magic (Tyxml.Html.toelt parent) with
    | Tyxml_xml.Node (ele, a, children) ->
        let child = Tyxml.Html.toelt child in
        Tyxml_xml.node ~a ele (List.filter (fun c -> c <> child) children)
        |> Tyxml.Html.tot
    | _ -> parent
    : a Tyxml.Html.elt )

let moveNode (type a) ~(parent : a Tyxml.Html.elt) ~child:_ ~from:_ ~to_:_ :
    a Tyxml.Html.elt =
  (parent : a Tyxml.Html.elt)

let onStale = RemoteAction.create ()

let () =
  Brisk_reconciler.addStaleTreeHandler
    (fun () -> RemoteAction.send ~action:() onStale)
    ()

let div =
  let open Brisk_reconciler in
  let component = Expert.nativeComponent "div" in
  fun ~(children :
         Html_types.div_content_fun Tyxml.Html.elt Brisk_reconciler.element)
      ?(className : string option) () :
      Html_types.div Tyxml.Html.elt Brisk_reconciler.element ->
    component (fun hooks ->
        ( {
            make =
              (fun () ->
                let a =
                  Option.map
                    (fun className -> [ Tyxml.Html.a_class [ className ] ])
                    className
                in
                Tyxml.Html.div ?a []);
            configureInstance = (fun ~(isFirstRender : _) node -> node);
            children;
            insertNode;
            deleteNode;
            moveNode;
          },
          hooks ))

let a =
  let open Brisk_reconciler in
  let component = Expert.nativeComponent "a" in
  fun ~(children :
         Html_types.a_content_fun Tyxml.Html.elt Brisk_reconciler.element) ~href
      ?className () :
      Html_types.a_content Html_types.a Tyxml.Html.elt Brisk_reconciler.element ->
    component (fun hooks ->
        let _className =
          Option.map
            (fun className -> [ Tyxml.Html.a_class [ className ] ])
            className
        in
        ( {
            make =
              (fun () ->
                Tyxml.Html.a
                  ~a:
                    (List.filter_map
                       (fun a -> a)
                       [ Some (Tyxml.Html.a_href href) ])
                  []);
            configureInstance = (fun ~(isFirstRender : _) node -> node);
            children;
            insertNode;
            deleteNode;
            moveNode;
          },
          hooks ))

let span =
  let open Brisk_reconciler in
  let component = Expert.nativeComponent "span" in
  fun ~(children :
         Html_types.span_content_fun Tyxml.Html.elt Brisk_reconciler.element)
      ?className () : Html_types.span Tyxml.Html.elt Brisk_reconciler.element ->
    component (fun hooks ->
        ( {
            make =
              (fun () ->
                Tyxml.Html.span
                  ?a:
                    (Option.map
                       (fun className -> [ Tyxml.Html.a_class [ className ] ])
                       className)
                  []);
            configureInstance = (fun ~(isFirstRender : _) node -> node);
            children;
            insertNode;
            deleteNode;
            moveNode;
          },
          hooks ))

let p =
  let open Brisk_reconciler in
  let component = Expert.nativeComponent "p" in
  fun ~text ?(className : string option) () :
      Html_types.p Tyxml.Html.elt Brisk_reconciler.element ->
    component (fun hooks ->
        ( {
            make =
              (fun () ->
                Tyxml.Html.p
                  ?a:
                    (Option.map
                       (fun className -> [ Tyxml.Html.a_class [ className ] ])
                       className)
                  [ Tyxml.Html.txt text ]);
            configureInstance = (fun ~(isFirstRender : _) node -> node);
            children = Brisk_reconciler.empty;
            insertNode = (fun ~parent ~child ~position -> assert false);
            deleteNode = (fun ~parent ~child ~position -> assert false);
            moveNode = (fun ~parent ~child ~from ~to_ -> assert false);
          },
          hooks ))

let img =
  let open Brisk_reconciler in
  let component = Expert.nativeComponent "img" in
  fun ~src ~alt ?className () :
      Html_types.img Tyxml.Html.elt Brisk_reconciler.element ->
    component (fun hooks ->
        ( {
            make =
              Tyxml.Html.img ~src ~alt
                ?a:
                  (Option.map
                     (fun className -> [ Tyxml.Html.a_class [ className ] ])
                     className);
            configureInstance = (fun ~(isFirstRender : _) node -> node);
            children = Brisk_reconciler.empty;
            insertNode;
            deleteNode;
            moveNode;
          },
          hooks ))

let render _application =
  print_endline
    ("This is no-op on native" [@reason.raw_literal "This is no-op on native"])

let renderToString application =
  let rendered :
      ('a Tyxml.Html.elt, 'b Tyxml.Html.elt) Brisk_reconciler.RenderedElement.t
      ref =
    ref
      (Brisk_reconciler.RenderedElement.render
         { node = Tyxml.Html.body []; insertNode; deleteNode; moveNode }
         application)
  in
  let hostView =
    Brisk_reconciler.RenderedElement.executeHostViewUpdates !rendered
  in
  Format.asprintf "%a" (Tyxml.Html.pp ()) (Obj.magic hostView)

(*let () = print_string "hostView - " in
    let () =
      Tyxml.Xml.(
        match hostView with
        | Empty -> print_endline "empty"
        | Comment s -> print_endline ("comment " ^ s)
        | EncodedPCDATA s -> print_endline ("EPCDATA " ^ s)
        | PCDATA s -> print_endline ("PCDATA " ^ s)
        | Entity s -> print_endline ("Entity " ^ s)
        | Leaf (name, _) -> print_endline ("Leaf " ^ name)
        | Node (name, _, _) -> print_endline ("Node " ^ name))
    in
  let html : 'a Tyxml.Html.elt =
    Tyxml.Html.html
      (Tyxml.Html.head (Tyxml.Html.title (Tyxml.Html.txt "Brisk-Tyxlm")) [])
      hostView
  in*)
