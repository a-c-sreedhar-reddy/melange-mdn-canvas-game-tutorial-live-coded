open Webapi;
open Webapi.Dom;
open Js.Global;

[@mel.set]
external fillStyle: (Canvas.Canvas2d.t, String.t) => unit = "fillStyle";

let canvas = Document.getElementById("myCanvas", Dom.document) |> Option.get;
let ctx = Canvas.CanvasElement.getContext2d(canvas);
let canvas_width = canvas |> Canvas.CanvasElement.width;
let canvas_height = canvas |> Canvas.CanvasElement.height;

let x = ref((canvas |> Canvas.CanvasElement.width) / 2);
let y = ref((canvas |> Canvas.CanvasElement.height) - 30);
let dx = ref(2);
let dy = ref(-2);

let ballRadius = 10;

let drawBall = () => {
  ctx |> Canvas.Canvas2d.beginPath;
  ctx
  |> Canvas.Canvas2d.arc(
       ~x=x^ |> float_of_int,
       ~y=y^ |> float_of_int,
       ~r=ballRadius |> float_of_int,
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

  x := x^ + dx^;
  y := y^ + dy^;

  if (x^ + dx^ > canvas_width - ballRadius || x^ + dx^ < ballRadius) {
    dx := - dx^;
  };

  if (y^ + dy^ > canvas_height - ballRadius || y^ + dy^ < ballRadius) {
    dy := - dy^;
  };
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
