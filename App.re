open Webapi;
open Webapi.Dom;
open Js.Global;

[@mel.set]
external fillStyle: (Canvas.Canvas2d.t, String.t) => unit = "fillStyle";

let canvas = Document.getElementById("myCanvas", Dom.document) |> Option.get;
let ctx = Canvas.CanvasElement.getContext2d(canvas);

let x = ref((canvas |> Canvas.CanvasElement.width) / 2);
let y = ref((canvas |> Canvas.CanvasElement.height) - 30);
let dx = 2;
let dy = (-2);

let drawBall = () => {
  ctx |> Canvas.Canvas2d.beginPath;
  ctx
  |> Canvas.Canvas2d.arc(
       ~x=x^ |> float_of_int,
       ~y=y^ |> float_of_int,
       ~r=10.0,
       ~startAngle=0.0,
       ~endAngle=Js.Math._PI *. 2.0,
       ~anticw=false,
     );
  ctx->fillStyle("#0095DD");
  ctx |> Canvas.Canvas2d.fill;
  ctx |> Canvas.Canvas2d.closePath;
};
let draw = () => {
  ctx
  |> Canvas.Canvas2d.clearRect(
       ~x=0.0,
       ~y=0.0,
       ~w=canvas |> Canvas.CanvasElement.width |> float_of_int,
       ~h=canvas |> Canvas.CanvasElement.height |> float_of_int,
     );

  drawBall();

  x := x^ + dx;
  y := y^ + dy;
};
let startGame = () => {
  let _interval = setInterval(~f=draw, 10);
  ();
};

switch (Dom.Document.getElementById("runButton", Dom.document)) {
| Some(button) =>
  button
  |> Dom.Element.addClickEventListener(_ => {
       startGame();
       button |> Dom.Element.setAttribute("disabled", "true");
     })
| None => ()
};
