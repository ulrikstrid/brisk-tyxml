module B = Brisk_reconciler

let constant name x =
  B.Expert.nativeComponent name (fun hooks ->
      ( {
          make = (fun () -> x);
          configureInstance = (fun ~(isFirstRender : _) node -> node);
          children = B.empty;
          insertNode = (fun ~parent ~child:_ ~position:_ -> parent);
          deleteNode = (fun ~parent ~child:_ ~position:_ -> parent);
          moveNode = (fun ~parent ~child:_ ~from:_ ~to_:_ -> parent);
        },
        hooks ))

module Component = struct
  module BriskElt :
    Xml_wrap.NODE with type 'a t = 'a B.element and type 'a child = 'a B.element =
  struct
    type 'a t = 'a B.element

    type 'a child = 'a B.element

    let inject x = x
  end

  module BriskChild :
    Xml_wrap.MANY
      with type 'a t = 'a B.element
       and type 'a list = 'a B.element List.t = struct
    type 'a t = 'a B.element

    let return x = constant "generatethis" x

    type 'a list = 'a B.element List.t

    let nil () = []

    let singleton x = [ x ]

    let cons = List.cons

    let append = List.append
  end

  module Make (Xml : Xml_sigs.Iterable) :
    Xml_sigs.T
      with module Elt = BriskElt
       and module Child = BriskChild
       and module Attr = Xml_wrap.NoWrap = struct
    let insertNode f ~parent ~child ~position:_ =
      match Xml.content parent with
      | Node (ele, a, children) ->
          Xml.node ~a ele (List.rev (f child :: children))
      | _ -> parent

    let deleteNode f ~parent ~child ~position:_ =
      match Xml.content parent with
      | Node (ele, a, children) ->
          Xml.node ~a ele (List.filter (fun c -> c <> f child) children)
      | _ -> parent

    let moveNode ~parent ~child:_ ~from:_ ~to_ = parent

    include Xml
    module Elt = BriskElt
    module Child = BriskChild
    module Attr = Xml_wrap.NoWrap

    type data = Xml.data

    type elt = data Elt.t

    type children = data Child.list

    let empty () = B.empty

    let elt f name children base =
      B.Expert.nativeComponent name (fun hooks ->
          ( {
              make = (fun () -> base);
              configureInstance = (fun ~(isFirstRender : _) node -> node);
              children;
              insertNode = insertNode f;
              deleteNode = deleteNode f;
              moveNode;
            },
            hooks ))

    let leaf ?a name = constant name @@ Xml.leaf ?a name

    let node ?a name (children : children) =
      elt (fun x -> x) name (B.listToElement children) (Xml.node ?a name [])

    let ( % ) f g x = f (g x)

    let comment = constant "comment" % Xml.comment

    let entity = constant "entity" % Xml.entity

    let cdata = constant "cdata" % Xml.cdata

    let cdata_script = constant "cdata" % Xml.cdata_script

    let cdata_style = constant "cdata" % Xml.cdata_style

    let pcdata content =
      elt (fun x -> Xml.pcdata x) "pcdata" content (Xml.pcdata "")

    let encodedpcdata content =
      elt (fun x -> Xml.encodedpcdata x) "pcdata" content (Xml.encodedpcdata "")
  end

  module Xml = Make (Tyxml.Xml)

  module Svg : Svg_sigs.Make(Xml).T = Svg_f.Make (Xml)

  module Html : Html_sigs.Make(Xml)(Svg).T = Html_f.Make (Xml) (Svg)
end

(*
let make id =
  let open Component in
  [%html "<div id=" id "><p>foooo</p></div>"]
  *)

(* val make : string -> [> Html_types.div ] Component.Html.data Brisk_reconciler.element *)
