open Webapi;
open Webapi.Dom;

[@mel.set]
external fillStyle: (Canvas.Canvas2d.t, string) => unit = "fillStyle";

module Math = {
  [@mel.scope "Math"] external pi: float = "PI";
};

type brickStatus =
  | Hidden
  | Visible;

type brick = {
  mutable x: int,
  mutable y: int,
  mutable status: brickStatus,
};

let ( let* ) = Option.bind;

let program = () => {
  let* canvas = Document.getElementById("myCanvas", Dom.document);

  let ctx = Canvas.CanvasElement.getContext2d(canvas);
  let ballRadius = 10;
  let x = ref(Canvas.CanvasElement.width(canvas) / 2);
  let y = ref(Canvas.CanvasElement.height(canvas) - 30);

  let dx = ref(2);
  let dy = ref(-2);
  let paddleHeight = 10;
  let paddleWidth = 75;

  let paddleX = ref((Canvas.CanvasElement.width(canvas) - paddleWidth) / 2);
  let rightPressed = ref(false);
  let leftPressed = ref(false);
  let brickRowCount = 5;
  let brickColumnCount = 3;
  let brickWidth = 75;
  let brickHeight = 20;
  let brickPadding = 10;
  let brickOffsetTop = 30;
  let brickOffsetLeft = 30;
  let score = ref(0);
  let lives = ref(3);
  let bricks: array(array(brick)) =
    Array.init_matrix(brickColumnCount, brickRowCount, (_, _) =>
      {x: 0, y: 0, status: Visible}
    );

  let keyDownHandler = event => {
    switch (KeyboardEvent.code(event)) {
    | "ArrowRight" => rightPressed := true
    | "ArrowLeft" => leftPressed := true
    | _ => ()
    };
  };

  let keyUpHandler = event => {
    switch (KeyboardEvent.code(event)) {
    | "ArrowRight" => rightPressed := false
    | "ArrowLeft" => leftPressed := false
    | _ => ()
    };
  };

  let mouseMoveHandler = event => {
    let handleEvent = () => {
      let* canvasHtmlElement = canvas |> Dom.Element.asHtmlElement;
      let relativeX =
        MouseEvent.clientX(event)
        - (canvasHtmlElement |> HtmlElement.offsetLeft);
      if (relativeX > 0 && relativeX < Canvas.CanvasElement.width(canvas)) {
        paddleX := relativeX - paddleWidth / 2;
      };
      None;
    };
    let _ = handleEvent();
    ();
  };

  Document.addKeyDownEventListener(keyDownHandler, document);
  Document.addKeyUpEventListener(keyUpHandler, document);
  Document.addMouseMoveEventListener(mouseMoveHandler, document);

  let collisionDetection = () => {
    for (c in 0 to brickColumnCount - 1) {
      for (r in 0 to brickRowCount - 1) {
        let brick = bricks[c][r];
        switch (brick.status) {
        | Visible =>
          if (x^ > brick.x
              && x^ < brick.x
              + brickWidth
              && y^ > brick.y
              && y^ < brick.y
              + brickHeight) {
            dy := - dy^;
            brick.status = Hidden;
            score := score^ + 1;

            if (score^ === brickRowCount * brickColumnCount) {
              Window.alert("YOU WIN, CONGRATS!", window);
              Location.reload(location);
            };
          }
        | Hidden => ()
        };
      };
    };
  };

  let drawBall = () => {
    Canvas.Canvas2d.beginPath(ctx);
    Canvas.Canvas2d.arc(
      ~x=float_of_int(x^),
      ~y=float_of_int(y^),
      ~r=float_of_int(ballRadius),
      ~startAngle=0.0,
      ~endAngle=2.0 *. Math.pi,
      ~anticw=true,
      ctx,
    );

    fillStyle(ctx, "#0095DD");
    Canvas.Canvas2d.fill(ctx);
    Canvas.Canvas2d.closePath(ctx);
  };

  let drawPaddle = () => {
    Canvas.Canvas2d.beginPath(ctx);
    Canvas.Canvas2d.rect(
      ~x=paddleX^ |> float_of_int,
      ~y=float_of_int(Canvas.CanvasElement.height(canvas) - paddleHeight),
      ~w=paddleWidth |> float_of_int,
      ~h=paddleHeight |> float_of_int,
      ctx,
    );
    fillStyle(ctx, "#0095DD");
    Canvas.Canvas2d.fill(ctx);
    Canvas.Canvas2d.closePath(ctx);
  };

  let drawBricks = () => {
    for (c in 0 to brickColumnCount - 1) {
      for (r in 0 to brickRowCount - 1) {
        switch (bricks[c][r].status) {
        | Visible =>
          let brickX = r * (brickWidth + brickPadding) + brickOffsetLeft;
          let brickY = c * (brickHeight + brickPadding) + brickOffsetTop;

          bricks[c][r].x = brickX;
          bricks[c][r].y = brickY;
          Canvas.Canvas2d.beginPath(ctx);
          Canvas.Canvas2d.rect(
            ~x=brickX |> float_of_int,
            ~y=brickY |> float_of_int,
            ~w=brickWidth |> float_of_int,
            ~h=brickHeight |> float_of_int,
            ctx,
          );
          fillStyle(ctx, "#0095DD");
          Canvas.Canvas2d.fill(ctx);
          Canvas.Canvas2d.closePath(ctx);
        | Hidden => ()
        };
      };
    };
  };

  let drawScore = () => {
    Canvas.Canvas2d.font(ctx, "16px Arial");
    fillStyle(ctx, "#0095DD");
    Canvas.Canvas2d.fillText(
      "Score: " ++ (score^ |> string_of_int),
      ~x=8.0,
      ~y=20.0,
      ctx,
    );
  };

  let drawLives = () => {
    Canvas.Canvas2d.font(ctx, "16px Arial");
    fillStyle(ctx, "#0095DD");
    Canvas.Canvas2d.fillText(
      "Lives: " ++ (lives^ |> string_of_int),
      ~x=Canvas.CanvasElement.width(canvas) - 65 |> float_of_int,
      ~y=20.0,
      ctx,
    );
  };

  let rec draw = () => {
    Canvas.Canvas2d.clearRect(
      ~x=0.0,
      ~y=0.0,
      ~w=Canvas.CanvasElement.width(canvas) |> float_of_int,
      ~h=Canvas.CanvasElement.height(canvas) |> float_of_int,
      ctx,
    );
    drawBricks();
    drawBall();
    drawPaddle();
    drawScore();
    drawLives();
    collisionDetection();
    if (x^
        + dx^ > Canvas.CanvasElement.width(canvas)
        - ballRadius
        || x^
        + dx^ < ballRadius) {
      dx := - dx^;
    };
    if (y^ + dy^ < ballRadius) {
      dy := - dy^;
    } else if (y^ + dy^ > Canvas.CanvasElement.height(canvas) - ballRadius) {
      if (x^ > paddleX^ && x^ < paddleX^ + paddleWidth) {
        dy := - dy^;
      } else {
        lives := lives^ - 1;
        if (lives^ <= 0) {
          window |> Window.alert("GAME OVER");
          Location.reload(location);
        } else {
          x := Canvas.CanvasElement.width(canvas) / 2;
          y := Canvas.CanvasElement.height(canvas) - 30;
          dx := 2;
          dy := (-2);
          paddleX := (Canvas.CanvasElement.width(canvas) - paddleWidth) / 2;
        };
      };
    };
    if (rightPressed^
        && paddleX^ < Canvas.CanvasElement.width(canvas)
        - paddleWidth) {
      paddleX := paddleX^ + 7;
    } else if (leftPressed^ && paddleX^ > 0) {
      paddleX := paddleX^ - 7;
    };
    x := x^ + dx^;
    y := y^ + dy^;
    requestAnimationFrame(_ => draw());
  };

  switch (document |> Document.getElementById("runButton")) {
  | Some(button) =>
    Dom.Element.addClickEventListener(
      _ => {
        draw();
        Element.setAttribute("disabled", "true", button);
      },
      button,
    )

  | None => ()
  };
  None;
};
let _ = program();
