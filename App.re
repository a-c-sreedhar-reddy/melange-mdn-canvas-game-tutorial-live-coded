open Webapi;
open Webapi.Dom;
open Js.Global;

[@mel.set]
external setFillStyleString: (Canvas.Canvas2d.t, String.t) => unit =
  "fillStyle";

let canvas = Document.getElementById("myCanvas", Dom.document) |> Option.get;
let ctx = Canvas.CanvasElement.getContext2d(canvas);
let canvas_width = canvas |> Canvas.CanvasElement.width;
let canvas_height = canvas |> Canvas.CanvasElement.height;

let x = ref((canvas |> Canvas.CanvasElement.width) / 2);
let y = ref((canvas |> Canvas.CanvasElement.height) - 30);
let dx = ref(2);
let dy = ref(-2);

let ballRadius = 10;

let paddleHeight = 10;
let paddleWidth = 75;
let paddleX = ref((canvas_width - paddleWidth) / 2);

let rightPressed = ref(false);
let leftPressed = ref(false);

let interval: option(intervalId) = None;
let interval: ref(option(intervalId)) = ref(interval);

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
  ctx->setFillStyleString("#0095DD");
  ctx |> Canvas.Canvas2d.fill;
  ctx |> Canvas.Canvas2d.closePath;
};

let drawPaddle = () => {
  open Canvas.Canvas2d;
  ctx |> beginPath;
  ctx
  |> rect(
       ~x=paddleX^ |> float_of_int,
       ~y=canvas_height - paddleHeight |> float_of_int,
       ~w=paddleWidth |> float_of_int,
       ~h=paddleHeight |> float_of_int,
     );
  setFillStyleString(ctx, "#0095DD");
  ctx |> fill;
  ctx |> closePath;
};

let keyDownHandler = e => {
  switch (KeyboardEvent.key(e)) {
  | "Right"
  | "ArrowRight" => rightPressed := true

  | "Left"
  | "ArrowLeft" => leftPressed := true
  | _ => ()
  };
};

let keyUpHandler = e => {
  switch (KeyboardEvent.key(e)) {
  | "Right"
  | "ArrowRight" => rightPressed := false

  | "Left"
  | "ArrowLeft" => leftPressed := false
  | _ => ()
  };
};

document |> Document.addKeyDownEventListener(keyDownHandler);
document |> Document.addKeyUpEventListener(keyUpHandler);
let draw = () => {
  ctx
  |> Canvas.Canvas2d.clearRect(
       ~x=0.0,
       ~y=0.0,
       ~w=canvas |> Canvas.CanvasElement.width |> float_of_int,
       ~h=canvas |> Canvas.CanvasElement.height |> float_of_int,
     );

  drawBall();
  drawPaddle();

  x := x^ + dx^;
  y := y^ + dy^;

  if (x^ + dx^ > canvas_width - ballRadius || x^ + dx^ < ballRadius) {
    dx := - dx^;
  };
  if (y^ + dy^ < ballRadius) {
    dy := - dy^;
  } else if (y^ + dy^ > canvas_height - ballRadius) {
    if (x^ > paddleX^ && x^ < paddleX^ + paddleWidth) {
      dy := - dy^;
    } else {
      window |> Window.alert("GAME OVER");

      document
      |> Dom.Document.unsafeAsHtmlDocument
      |> Dom.HtmlDocument.location
      |> Location.reload;
      switch (interval^) {
      | Some(intervalId) => clearInterval(intervalId)
      | None => ()
      };
    };
  };

  if (rightPressed^) {
    paddleX := Js.Math.min_int(paddleX^ + 7, canvas_width - paddleWidth);
  } else if (leftPressed^) {
    paddleX := Js.Math.max_int(paddleX^ - 7, 0);
  };
};

let startGame = () => {
  interval := Some(setInterval(~f=draw, 10));
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
