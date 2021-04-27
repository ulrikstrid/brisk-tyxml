module RemoteAction = Brisk_reconciler.RemoteAction

let onStale = RemoteAction.create ()

let () =
  Brisk_reconciler.addStaleTreeHandler
    (fun () -> RemoteAction.send ~action:() onStale)
    ()

let render _application = print_endline "This is no-op on native"

let renderToString application =
  let rendered =
    ref
      (Brisk_reconciler.RenderedElement.render
         {
           node = BriskXml.Component.Xml.;
           insertNode =(fun ~parent:_ ~child ~position:_ -> BriskXml.Component.Html.body [child] |> BriskXml.Component.Html.toelt);
           deleteNode = (fun ~parent ~child:_ ~position:_ -> parent);
           moveNode = (fun ~parent ~child:_ ~from:_ ~to_:_ -> parent);
         }
         application)
  in
  let hostView =
    Brisk_reconciler.RenderedElement.executeHostViewUpdates !rendered
  in
  Console.log hostView
