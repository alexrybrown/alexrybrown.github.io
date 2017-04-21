module Asteroids exposing (..)


-- Using color to create colors for the screen and ship
import Color exposing (..)
-- Using collage to draw onto the screen
import Collage exposing (..)
-- Using element to create our container and render to Html
import Element exposing (..)
-- Using html to create our program
import Html exposing (..)
-- Using keyboard to track key ups and key downs
import Keyboard exposing (KeyCode)
-- Using animation frame to track time diffs
import AnimationFrame
-- Using time as the type for animation frame diffs
import Time exposing (Time)


main : Program Never Model Msg
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type alias Model =
  { ship : Ship
  }

-- Any thing that can move around the screen.
type alias PhysicsObject a =
  { a |
    x : Float,
    y : Float,
    velocity : Float,
    acceleration : Float,
    rotation : Int
  }

type alias Ship =
  { x : Float
  , y : Float
  , velocity : Float
  , acceleration : Float
  , rotation : Int
  }


spaceship : Ship
spaceship =
  { x = 0
  , y = 0
  , velocity = 0
  , acceleration = 0
  , rotation = 0
  }


model : Model
model =
  { ship = spaceship
  }

-- INIT

init : (Model, Cmd Msg)
init =
  (model, Cmd.none)

-- UPDATE

type Msg =
  Delta Time
  | KeyDown KeyCode
  | KeyUp KeyCode


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    KeyDown keyCode ->
      (keyDown keyCode model, Cmd.none)

    KeyUp keyCode ->
      (keyUp keyCode model, Cmd.none)

    Delta dt ->
      (applyPhysics dt model, Cmd.none)


keyDown : KeyCode -> Model -> Model
keyDown keyCode model =
  case keyCode of
    -- Space
    -- 32 ->
    --   { model | ship = fireShot model.ship }
    -- ArowLeft
    37 ->
      let
        { ship } = model
        newShip =
          updateAccelerationShip -0.0001 ship
      in
        { model | ship = newShip }
    -- ArrowRight
    39 ->
      let
        { ship } = model
        newShip =
          updateAccelerationShip 0.0001 ship
      in
        { model | ship = newShip }
    -- ArrowUp
    38 ->
      let
        { ship } = model
        newShip =
          updateRotationShip 1 ship
      in
        { model | ship = newShip }
    -- ArrowDown
    40 ->
      let
        { ship } = model
        newShip =
          updateRotationShip -1 ship
      in
        { model | ship = newShip }

    _ ->
      model


-- Currently no key up messages are needed.
keyUp : KeyCode -> Model -> Model
keyUp keyCode model =
  case keyCode of

    _ ->
      model


-- Applies velocity and position to our objects.
applyPhysics : Float -> Model -> Model
applyPhysics dt model =
  { model | ship = applyPhysicsHelper dt model.ship }

applyPhysicsHelper : Float -> PhysicsObject a -> PhysicsObject a
applyPhysicsHelper dt phyObj =
  let
    calculatedVelocity =
      phyObj.velocity + phyObj.acceleration * dt

    newVelocity =
        if calculatedVelocity >= 0 && calculatedVelocity <= 0.1 then
          calculatedVelocity
        else if calculatedVelocity < 0 then
          0
        else
          0.1

    calculatedXPosition =
      phyObj.x + dt * cos (degrees (toFloat phyObj.rotation)) * newVelocity

    newXPosition =
      if calculatedXPosition >= -500 && calculatedXPosition <= 500 then
        calculatedXPosition
      else if calculatedXPosition < -500 then
        500
      else
        -500

    calculatedYPosition =
      phyObj.y + dt * sin (degrees (toFloat phyObj.rotation)) * newVelocity

    newYPosition =
      if calculatedYPosition >= -500 && calculatedYPosition <= 500 then
        calculatedYPosition
      else if calculatedYPosition < -500 then
        500
      else
        -500
  in
    { phyObj |
        x = newXPosition,
        y = newYPosition,
        velocity = newVelocity
    }

updateAccelerationShip : Float -> Ship -> Ship
updateAccelerationShip addAcceleration ship =
  let
    calculatedAcceleration =
      ship.acceleration + addAcceleration

    newAcceleration =
      if calculatedAcceleration >= -0.001 && calculatedAcceleration <= 0.001 then
        calculatedAcceleration
      else if calculatedAcceleration < -0.001 then
        -0.001
      else
        0.001
  in
    { ship | acceleration = newAcceleration }


-- Update the rotation of rotatable things like our ship.
updateRotation : Int -> Model -> Model
updateRotation addRot model =
  { model | ship = updateRotationShip addRot model.ship }

updateRotationShip : Int -> Ship -> Ship
updateRotationShip addRot ship =
  { ship | rotation = (ship.rotation + addRot) % 360 }


-- fireShot : Ship -> Ship
-- fireShot ship =
--     { ship | shots = ship.shots + 1 }



-- VIEW

-- Draws a black box and a circle that you can control
view : Model -> Html Msg
view model =
  let
    { ship } = model
  in
    toHtml <|
      container 1000 1000 middle <|
        collage 1000 1000
          [ rect 1000 1000
              |> filled (rgb 0 0 0)
          , oval 15 15
              |> filled white
              |> move (ship.x, ship.y)
          ]


-- SUBSCRIPTIONS
-- Keeping track of time and key presses.
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ AnimationFrame.diffs Delta
    , Keyboard.downs KeyDown
    , Keyboard.ups KeyUp
    ]
