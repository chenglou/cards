/* TODO: fix zoom then drag */
/* TODO: fix drag outside and back and release wrong coordinates */
[@bs.val] external window : Dom.window = "";
[@bs.val] external document : Dom.document = "";
[@bs.send]
external addEventListener : (Dom.window, string, 'event => unit) => unit = "";
[@bs.get] external outerWidth : Dom.window => float = "";
[@bs.get] external outerHeight : Dom.window => float = "";
/* sensitive to zoom */
[@bs.get] external innerWidth : Dom.window => float = "";
[@bs.send]
external getElementById : (Dom.document, string) => option(Dom.element) = "";
[@bs.send] external contains : (Js.t({..}), 'target) => bool = "";

type coords = {
  x: float,
  y: float,
};

/* constants */
let p1InitialCoords = {x: 200., y: 200.};
let p2InitialCoords = {x: 450., y: 300.};

let p1Width = 400.;
let p1Height = 300.;
let p2Width = 200.;
let p2Height = 350.;

let p1Bg = "./bg1.png";
let p2Bg = "./bg2.jpg";

type pane =
  | One
  | Two;
type state = {
  currentDrag: option(coords),
  lastDrag: (pane, coords),
  p1PermaOffset: coords,
  p2PermaOffset: coords,
  glow: bool,
  ambientOcclusion: bool,
  blur: bool,
  grow: bool,
  lightSource: bool,
  overlapCrossfade: bool,
  transition: bool,
};

type action =
  | Down(pane, coords)
  | Up
  | Drag(coords)
  | ToggleGlow
  | ToggleAmbientOcclusion
  | ToggleBlur
  | ToggleGrow
  | ToggleLightSource
  | ToggleOverlapCrossfade
  | ToggleTransition
  | ToggleAll;

/* utils */
let allTogglesOn = state =>
  state.glow
  && state.ambientOcclusion
  && state.blur
  && state.grow
  && state.lightSource
  && state.overlapCrossfade
  && state.transition;
let updateAllToggles = (state, value) => {
  ...state,
  glow: value,
  ambientOcclusion: value,
  blur: value,
  grow: value,
  lightSource: value,
  overlapCrossfade: value,
  transition: value,
};

let addCoords = ({x: a, y: b}, {x: c, y: d}) => {x: a +. c, y: b +. d};
let minusCoords = ({x: a, y: b}, {x: c, y: d}) => {x: a -. c, y: b -. d};
let divCoords = ({x, y}, n) => {x: x /. n, y: y /. n};

let component = ReasonReact.reducerComponent("Page");

let make = _children => {
  ...component,
  initialState: () => {
    lastDrag: (Two, {x: 0., y: 0.}),
    currentDrag: None,
    p1PermaOffset: {
      x: 0.,
      y: 0.,
    },
    p2PermaOffset: {
      x: 0.,
      y: 0.,
    },
    glow: true,
    ambientOcclusion: true,
    blur: true,
    grow: true,
    lightSource: true,
    overlapCrossfade: true,
    transition: true,
  },
  reducer: (action, state) =>
    switch (action) {
    | Down(pane, coords) =>
      ReasonReact.Update({
        ...state,
        lastDrag: (pane, coords),
        currentDrag: Some(coords),
      })
    | Up =>
      switch (state.currentDrag, state.lastDrag) {
      | (None, _) => ReasonReact.NoUpdate
      | (Some(a), (pane, b)) =>
        let offset = minusCoords(a, b);
        switch (pane) {
        | One =>
          ReasonReact.Update({
            ...state,
            lastDrag: (pane, a),
            currentDrag: None,
            p1PermaOffset: offset |. addCoords(state.p1PermaOffset),
          })
        | Two =>
          ReasonReact.Update({
            ...state,
            lastDrag: (pane, a),
            currentDrag: None,
            p2PermaOffset: offset |. addCoords(state.p2PermaOffset),
          })
        };
      }
    | Drag(coords) =>
      ReasonReact.Update({...state, currentDrag: Some(coords)})
    | ToggleGlow => ReasonReact.Update({...state, glow: ! state.glow})
    | ToggleAmbientOcclusion =>
      ReasonReact.Update({
        ...state,
        ambientOcclusion: ! state.ambientOcclusion,
      })
    | ToggleBlur => ReasonReact.Update({...state, blur: ! state.blur})
    | ToggleGrow => ReasonReact.Update({...state, grow: ! state.grow})
    | ToggleLightSource =>
      ReasonReact.Update({...state, lightSource: ! state.lightSource})
    | ToggleOverlapCrossfade =>
      ReasonReact.Update({
        ...state,
        overlapCrossfade: ! state.overlapCrossfade,
      })
    | ToggleTransition =>
      ReasonReact.Update({...state, transition: ! state.transition})
    | ToggleAll =>
      if (allTogglesOn(state)) {
        ReasonReact.Update(updateAllToggles(state, false));
      } else {
        ReasonReact.Update(updateAllToggles(state, true));
      }
    },
  didMount: self => {
    let cb =
      self.handle((event, self) => {
        let x = event##screenX;
        let y = event##screenY;
        if (self.state.currentDrag !== None) {
          self.send(Drag({x, y}));
        };
      });
    window |. addEventListener("mousemove", cb);
  },
  render: ({state} as self) => {
    let windowWidth = window |. outerWidth;
    let windowHeight = window |. outerHeight;
    let zoomLevel =
      (
        document
        |. getElementById("measurer")
        |. Belt.Option.getExn
        |. ReactDOMRe.domElementToObj
      )##offsetWidth
      /. (window |. innerWidth);

    let transitionTiming = state.transition ? 0.2 : 0.;

    let (p1Depth, p2Depth) =
      switch (state.lastDrag) {
      | (Two, _) => (1, 2)
      | (One, _) => (2, 1)
      };
    let (p1Blur, p2Blur) =
      switch (state.blur, state.lastDrag) {
      | (false, _) => (0., 0.)
      | (true, (Two, _)) => (0.5, 0.)
      | (true, (One, _)) => (0., 0.5)
      };
    let (p1Scale, p2Scale) =
      switch (state.grow, state.lastDrag) {
      | (false, _) => (1., 1.)
      | (true, (Two, _)) => (1., 1.12)
      | (true, (One, _)) => (1.12, 1.)
      };
    let draggingOffset =
      switch (state.currentDrag, state.lastDrag) {
      | (None, _) => {x: 0., y: 0.}
      | (Some(a), (_, b)) => minusCoords(a, b)
      };
    let p1xy = p1InitialCoords |. addCoords(state.p1PermaOffset);
    let {x: p1x, y: p1y} =
      switch (state.lastDrag) {
      | (One, _) =>
        p1xy |. addCoords(draggingOffset) |. divCoords(zoomLevel)
      | _ => p1xy |. divCoords(zoomLevel)
      };
    let p2xy = p2InitialCoords |. addCoords(state.p2PermaOffset);
    let {x: p2x, y: p2y} =
      switch (state.lastDrag) {
      | (Two, _) =>
        p2xy |. addCoords(draggingOffset) |. divCoords(zoomLevel)
      | _ => p2xy |. divCoords(zoomLevel)
      };
    let (p1ShadowX, p1ShadowY, p1ShadowBlur) =
      switch (state.lastDrag) {
      | (One, _) => (
          state.lightSource ?
            (windowWidth /. 2. -. p1x) *. (-40.) /. windowWidth : 0.,
          state.lightSource ?
            (windowHeight /. 2. -. p1y -. 1000.) *. (-40.) /. windowHeight : 0.,
          30,
        )
      | _ => (0., 0., 3)
      };
    let (p2ShadowX, p2ShadowY, p2ShadowBlur) =
      switch (state.lastDrag) {
      | (Two, _) => (
          state.lightSource ?
            (windowWidth /. 2. -. p2x) *. (-40.) /. windowWidth : 0.,
          state.lightSource ?
            (windowHeight /. 2. -. p2y -. 1000.) *. (-40.) /. windowHeight : 0.,
          30,
        )
      | _ => (0., 0., 3)
      };
    let p1Dragged =
      switch (state.lastDrag) {
      | (One, _) => true
      | _ => false
      };
    let p2Dragged =
      switch (state.lastDrag) {
      | (Two, _) => true
      | _ => false
      };
    /* */
    let p1NudgeXDueToScale = (p1Scale -. 1.) /. 2. *. p1Width;
    let p1NudgeYDueToScale = (p1Scale -. 1.) /. 2. *. p1Height;
    let p2NudgeXDueToScale = (p2Scale -. 1.) /. 2. *. p2Width;
    let p2NudgeYDueToScale = (p2Scale -. 1.) /. 2. *. p2Height;

    let (
      p1ProjectedShadowToP2Top,
      p1ProjectedShadowToP2Right,
      p1ProjectedShadowToP2Bottom,
      p1ProjectedShadowToP2Left,
    ) = {
      let p2LocalToGlobalLeft = p2x -. p2NudgeXDueToScale;
      let p1LocalToGlobalLeft = p1x -. p1NudgeXDueToScale;
      let leftDelta = p1LocalToGlobalLeft -. p2LocalToGlobalLeft;

      let p2LocalToGlobalRight =
        p2x -. p2NudgeXDueToScale +. p2Width *. p2Scale;
      let p1LocalToGlobalRight =
        p1x -. p1NudgeXDueToScale +. p1Width *. p1Scale;
      let rightDelta = p2LocalToGlobalRight -. p1LocalToGlobalRight;

      let p2LocalToGlobalBottom =
        p2y -. p2NudgeYDueToScale +. p2Height *. p2Scale;
      let p1LocalToGlobalBottom =
        p1y -. p1NudgeYDueToScale +. p1Height *. p1Scale;
      let bottomDelta = p2LocalToGlobalBottom -. p1LocalToGlobalBottom;

      let p2LocalToGlobalTop = p2y -. p2NudgeYDueToScale;
      let p1LocalToGlobalTop = p1y -. p1NudgeYDueToScale;
      let topDelta = p1LocalToGlobalTop -. p2LocalToGlobalTop;
      (
        -. topDelta -. p1NudgeYDueToScale,
        -. rightDelta -. p1NudgeXDueToScale,
        -. bottomDelta -. p1NudgeYDueToScale,
        -. leftDelta -. p1NudgeXDueToScale,
      );
    };
    let (
      p2ProjectedShadowToP1Top,
      p2ProjectedShadowToP1Right,
      p2ProjectedShadowToP1Bottom,
      p2ProjectedShadowToP1Left,
    ) = {
      let p1LocalToGlobalLeft = p1x -. p1NudgeXDueToScale;
      let p2LocalToGlobalLeft = p2x -. p2NudgeXDueToScale;
      let leftDelta = p2LocalToGlobalLeft -. p1LocalToGlobalLeft;

      let p1LocalToGlobalRight =
        p1x -. p1NudgeXDueToScale +. p1Width *. p1Scale;
      let p2LocalToGlobalRight =
        p2x -. p2NudgeXDueToScale +. p2Width *. p2Scale;
      let rightDelta = p1LocalToGlobalRight -. p2LocalToGlobalRight;

      let p1LocalToGlobalBottom =
        p1y -. p1NudgeYDueToScale +. p1Height *. p1Scale;
      let p2LocalToGlobalBottom =
        p2y -. p2NudgeYDueToScale +. p2Height *. p2Scale;
      let bottomDelta = p1LocalToGlobalBottom -. p2LocalToGlobalBottom;

      let p1LocalToGlobalTop = p1y -. p1NudgeYDueToScale;
      let p2LocalToGlobalTop = p2y -. p2NudgeYDueToScale;
      let topDelta = p2LocalToGlobalTop -. p1LocalToGlobalTop;
      (
        -. topDelta -. p2NudgeYDueToScale,
        -. rightDelta -. p2NudgeXDueToScale,
        -. bottomDelta -. p2NudgeYDueToScale,
        -. leftDelta -. p2NudgeXDueToScale,
      );
    };

    /* styles */
    let p1Outer =
      ReactDOMRe.Style.make(
        ~position="absolute",
        ~top="0px",
        ~left="0px",
        ~zIndex=string_of_int(p1Depth),
        ~transform={j|translate($(p1x)px, $(p1y)px)|j},
        (),
      );
    let p2Outer =
      ReactDOMRe.Style.make(
        ~position="absolute",
        ~top="0px",
        ~left="0px",
        ~zIndex=string_of_int(p2Depth),
        ~transform={j|translate($(p2x)px, $(p2y)px)|j},
        (),
      );
    /* */
    let p1Overlay =
      ReactDOMRe.Style.make(
        ~position="absolute",
        ~top="0px",
        ~left="0px",
        ~zIndex=string_of_int(p1Depth + 10),
        ~transition={j|opacity $(transitionTiming)s|j},
        ~transform={j|translate($(p1x)px, $(p1y)px)|j},
        ~opacity=p1Dragged && state.overlapCrossfade ? "1" : "0",
        (),
      );
    let p2Overlay =
      ReactDOMRe.Style.make(
        ~position="absolute",
        ~top="0px",
        ~left="0px",
        ~zIndex=string_of_int(p2Depth + 10),
        ~transition={j|opacity $(transitionTiming)s|j},
        ~transform={j|translate($(p2x)px, $(p2y)px)|j},
        ~opacity=p2Dragged && state.overlapCrossfade ? "1" : "0",
        (),
      );
    /* */
    let p1Inner =
      ReactDOMRe.Style.make(
        ~cursor=
          p1Dragged && state.currentDrag === None ?
            "-webkit-grab" : "-webkit-grabbing",
        ~width={j|$(p1Width)px|j},
        ~height={j|$(p1Height)px|j},
        ~position="absolute",
        ~borderRadius="8px",
        ~backgroundImage={j|url($(p1Bg))|j},
        ~backgroundSize="cover",
        ~backgroundColor="black",
        ~transition={j|all $(transitionTiming)s|j},
        ~transform={j|scale($p1Scale)|j},
        ~filter={j|blur($(p1Blur)px)|j},
        (),
      );
    let p2Inner =
      ReactDOMRe.Style.make(
        ~cursor=
          p2Dragged && state.currentDrag === None ?
            "-webkit-grab" : "-webkit-grabbing",
        ~width={j|$(p2Width)px|j},
        ~height={j|$(p2Height)px|j},
        ~position="absolute",
        ~borderRadius="8px",
        ~backgroundImage={j|url($(p2Bg))|j},
        ~backgroundSize="cover",
        ~backgroundColor="black",
        ~transition={j|all $(transitionTiming)s|j},
        ~transform={j|scale($p2Scale)|j},
        ~filter={j|blur($(p2Blur)px)|j},
        (),
      );
    /* */
    let p1ClipPath = {j|inset($(p1ProjectedShadowToP2Top)px $(p1ProjectedShadowToP2Right)px $(p1ProjectedShadowToP2Bottom)px $(p1ProjectedShadowToP2Left)px round 8px)|j};
    let p1ProjectedShadowOnP2 =
      ReactDOMRe.Style.make(
        ~width={j|$(p1Width)px|j},
        ~height={j|$(p1Height)px|j},
        ~position="absolute",
        ~borderRadius="8px",
        ~background="black",
        ~transition=p1Dragged && state.transition ? "opacity 0.45s" : "none",
        ~boxShadow={j|$(p1ShadowX)px $(p1ShadowY)px 100px black|j},
        ~clipPath=p1ClipPath,
        ~opacity=p1Dragged ? "1" : "0",
        (),
      )
      |. ReactDOMRe.Style.unsafeAddProp("webkitClipPath", p1ClipPath);

    let p2ClipPath = {j|inset($(p2ProjectedShadowToP1Top)px $(p2ProjectedShadowToP1Right)px $(p2ProjectedShadowToP1Bottom)px $(p2ProjectedShadowToP1Left)px round 8px)|j};
    let p2ProjectedShadowOnP1 =
      ReactDOMRe.Style.make(
        ~width={j|$(p2Width)px|j},
        ~height={j|$(p2Height)px|j},
        ~position="absolute",
        ~borderRadius="8px",
        ~background="black",
        ~transition=p2Dragged && state.transition ? "opacity 0.45s" : "none",
        ~boxShadow={j|$(p2ShadowX)px $(p2ShadowY)px 100px black|j},
        ~clipPath=p2ClipPath,
        ~opacity=p2Dragged ? "1" : "0",
        (),
      )
      |. ReactDOMRe.Style.unsafeAddProp("webkitClipPath", p2ClipPath);
    /* */
    let p1Glow =
      ReactDOMRe.Style.make(
        ~width={j|$(p1Width)px|j},
        ~height={j|$(p1Height)px|j},
        ~borderRadius="8px",
        ~backgroundImage={j|url($(p1Bg))|j},
        ~backgroundSize="cover",
        ~transition={j|all $(transitionTiming)s|j},
        ~position="absolute",
        ~transform={j|translate($(p1ShadowX)px, $(p1ShadowY)px)|j},
        ~filter={j|blur($(p1ShadowBlur)px) brightness(80%) saturate(200%)|j},
        (),
      );
    let p2Glow =
      ReactDOMRe.Style.make(
        ~width={j|$(p2Width)px|j},
        ~height={j|$(p2Height)px|j},
        ~borderRadius="8px",
        ~backgroundImage={j|url($(p2Bg))|j},
        ~backgroundSize="cover",
        ~transition={j|all $(transitionTiming)s|j},
        ~position="absolute",
        ~transform={j|translate($(p2ShadowX)px, $(p2ShadowY)px)|j},
        ~filter={j|blur($(p2ShadowBlur)px) brightness(80%) saturate(200%)|j},
        (),
      );

    let makeToggle = (stateValue, action, description) =>
      <label
        style=(
          ReactDOMRe.Style.make(
            ~display="flex",
            ~cursor="pointer",
            ~userSelect="none",
            (),
          )
        )>
        <input
          type_="checkbox"
          checked=stateValue
          onChange=(_ => self.send(action))
        />
        (ReasonReact.string(description))
      </label>;

    <div>
      <div
        style=(
          ReactDOMRe.Style.make(
            ~fontFamily="-apple-system, BlinkMacSystemFont, sans-serif",
            (),
          )
        )>
        <div>(ReasonReact.string("Only works on desktop for now"))</div>
        <p></p>
        (makeToggle(state.glow, ToggleGlow, "Back glow (self shadow)"))
        (
          makeToggle(
            state.ambientOcclusion,
            ToggleAmbientOcclusion,
            "Ambient occlusion (projected shadow)",
          )
        )
        (makeToggle(state.blur, ToggleBlur, "Blur (far away item)"))
        (makeToggle(state.grow, ToggleGrow, "Grow"))
        (
          makeToggle(
            state.lightSource,
            ToggleLightSource,
            "Light source (shadow position)",
          )
        )
        (
          makeToggle(
            state.overlapCrossfade,
            ToggleOverlapCrossfade,
            "Overlap crossfade",
          )
        )
        (makeToggle(state.transition, ToggleTransition, "Transition"))
        <p />
        (makeToggle(allTogglesOn(state), ToggleAll, "2030"))
      </div>
      <div style=p1Outer>
        (
          state.ambientOcclusion ?
            <div style=p1ProjectedShadowOnP2 /> : ReasonReact.null
        )
        (state.glow ? <div style=p1Glow /> : ReasonReact.null)
        <div style=p1Inner />
      </div>
      <div style=p2Outer>
        (
          state.ambientOcclusion ?
            <div style=p2ProjectedShadowOnP1 /> : ReasonReact.null
        )
        (state.glow ? <div style=p2Glow /> : ReasonReact.null)
        <div style=p2Inner />
      </div>
      /* */
      <div
        style=p1Overlay
        onMouseDown=(
          e => {
            let x = ReactEventRe.Mouse.screenX(e) |. float_of_int;
            let y = ReactEventRe.Mouse.screenY(e) |. float_of_int;
            self.send(Down(One, {x, y}));
          }
        )
        onMouseUp=(_e => self.send(Up))>
        <div style=p1Inner />
      </div>
      <div
        style=p2Overlay
        onMouseDown=(
          e => {
            let x = ReactEventRe.Mouse.screenX(e) |. float_of_int;
            let y = ReactEventRe.Mouse.screenY(e) |. float_of_int;
            self.send(Down(Two, {x, y}));
          }
        )
        onMouseUp=(_e => self.send(Up))>
        <div style=p2Inner />
      </div>
    </div>;
    /* */
  },
};
