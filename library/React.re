module RemoteAction = Brisk_reconciler.RemoteAction;

type node =
  | Empty(list(node))
  | Div(layoutNode)
  | Span(layoutNode)
  | Image({
      className: option(string),
      src: string,
    })
  | Text({text: string})
and layoutNode = {
  className: option(string),
  children: list(node),
};

let insertNode = (~parent: node, ~child: node, ~position as _) => {
  switch (parent) {
  | Empty(children) => Empty(children @ [child])
  | Span(node) => Span({...node, children: node.children @ [child]})
  | Div(node) => Div({...node, children: node.children @ [child]})
  | parent => parent
  };
};

let deleteNode = (~parent: node, ~child: node, ~position as _) => {
  switch (parent) {
  | Empty(children) => Empty(children |> List.filter(c => c === child))
  | Div(node) =>
    Div({...node, children: node.children |> List.filter(c => c === child)})
  | Span(node) =>
    Span({...node, children: node.children |> List.filter(c => c === child)})
  | parent => parent
  };
};

let moveNode = (~parent, ~child as _, ~from as _, ~to_ as _) => {
  parent;
};

let onStale = RemoteAction.create();
Brisk_reconciler.addStaleTreeHandler(() =>
  RemoteAction.send(~action=(), onStale)
);

let%nativeComponent div = (~children, ~className=?, (), hooks) => (
  {
    make: () => {
      Div({className, children: []});
    },
    configureInstance: (~isFirstRender as _, node) => {
      node;
    },
    children,
    insertNode,
    deleteNode,
    moveNode,
  },
  hooks,
);

let%nativeComponent span =
                    (
                      ~className=?,
                      ~text="",
                      ~onClick as _: option(unit => unit)=?,
                      (),
                      hooks,
                    ) => (
  {
    make: () => {
      Span({className, children: [Text({text: text})]});
    },
    configureInstance: (~isFirstRender as _, node) => {
      node;
    },
    children: Brisk_reconciler.empty,
    insertNode,
    deleteNode,
    moveNode,
  },
  hooks,
);

let%nativeComponent img = (~className=?, ~src, (), hooks) => (
  {
    make: () => {
      Image({className, src});
    },
    configureInstance: (~isFirstRender as _, node) => {
      node;
    },
    children: Brisk_reconciler.empty,
    insertNode,
    deleteNode,
    moveNode,
  },
  hooks,
);

let render = _application => {
  // Logs.warn(m => m("This is no-op on native"));
  print_endline(
    "This is no-op on native",
  );
};

let renderToString = application => {
  let rendered =
    ref(
      Brisk_reconciler.RenderedElement.render(
        {node: Empty([]), insertNode, deleteNode, moveNode},
        application,
      ),
    );

  let hostView =
    Brisk_reconciler.RenderedElement.executeHostViewUpdates(rendered^);

  let or_else = default =>
    fun
    | None => default
    | Some(a) => a;

  let get_class_attrib = className => {
    Option.map(String.split_on_char(' '), className)
    |> or_else([])
    |> Tyxml.Html.a_class;
  };

  let rec toHtml = (node: node): list(Tyxml.Html.elt('typ)) => {
    Tyxml.(
      switch (node) {
      | Empty(children) => children |> List.map(toHtml) |> List.flatten
      | Div(node) => [
          Html.div(
            ~a=[get_class_attrib(node.className)],
            node.children |> List.map(toHtml) |> List.flatten,
          ),
        ]
      | Image(node) => [
          Html.img(
            ~src=node.src,
            ~alt="",
            ~a=[get_class_attrib(node.className)],
            (),
          ),
        ]
      | Span(node) => [
          Html.span(
            ~a=[get_class_attrib(node.className)],
            node.children |> List.map(toHtml) |> Obj.magic |> List.flatten,
          ),
        ]
      | Text(node) => [Html.txt(node.text)]
      }
    );
  };

  let html =
    Tyxml.Html.html(
      Tyxml.Html.head(Tyxml.Html.title(Tyxml.Html.txt("Brisk-Tyxlm")), []),
      Tyxml.Html.body(hostView |> toHtml),
    );

  Format.asprintf("%a", Tyxml.Html.pp(), html);
};
