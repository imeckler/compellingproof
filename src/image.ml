type t = Dom_html.imageElement Js.t

let load url =
  let (s, trigger) = Frp.Stream.create' () in
  let img = Dom_html.(createImg document) in
  img##onload <- Dom.handler (fun _ -> trigger img; Js._false);
  img##src    <- Js.string url;
  s
;;

