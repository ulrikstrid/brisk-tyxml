module Xml = struct
  module W = Xml_wrap.NoWrap

  type 'a wrap = 'a

  type 'a list_wrap = 'a list

  type uri = string

  type separator = Space | Comma

  type aname = string

  type acontent =
    | AFloat of float
    | AInt of int
    | AStr of string
    | AStrL of separator * string list

  type attrib = aname * acontent

  let acontent (_, a) = a

  let aname (name, _) = name

  type ename = string

  type econtent =
    | Empty
    | Comment of string
    | EncodedPCDATA of string
    | PCDATA of string
    | Entity of string
    | Leaf of ename * attrib list
    | Node of ename * attrib list * econtent list

  let uri_of_string = Tyxml.Xml.uri_of_string

  let string_of_uri = Tyxml.Xml.string_of_uri

  type event_handler = Tyxml.Xml.event_handler

  type mouse_event_handler = Tyxml.Xml.mouse_event_handler

  type touch_event_handler = Tyxml.Xml.touch_event_handler

  type keyboard_event_handler = Tyxml.Xml.keyboard_event_handler

  let float_attrib name value = (name, AFloat value)

  let int_attrib name value = (name, AInt value)

  let string_attrib name value = (name, AStr value)

  let space_sep_attrib name values = (name, AStrL (Space, values))

  let comma_sep_attrib name values = (name, AStrL (Comma, values))

  let event_handler_attrib name value = (name, AStr value)

  let mouse_event_handler_attrib name value = (name, AStr value)

  let keyboard_event_handler_attrib name value = (name, AStr value)

  let touch_event_handler_attrib name value = (name, AStr value)

  let uri_attrib name value = (name, AStr value)

  let uris_attrib name values = (name, AStrL (Space, values))

  (** Element *)

  type elt = econtent

  let content elt = elt

  let empty () = Empty

  let comment c = Comment c

  let pcdata d = PCDATA d

  let encodedpcdata d = EncodedPCDATA d

  let entity e = Entity e

  let leaf ?(a = []) name = Leaf (name, a)

  let node ?(a = []) name children = Node (name, a, children)

  let cdata a = assert false

  let cdata_script a = assert false

  let cdata_style a = assert false
end

module Xml_Svg = struct
  include Xml
end

module Svg = Svg_f.Make (Xml_Svg)
module Html = Html_f.Make (Xml) (Svg)
module Xml_print = Xml_print.Make_typed_fmt (Xml) (Html)
