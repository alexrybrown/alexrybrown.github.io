module Asteroids exposing (..)

import Color exposing (..)
import Collage exposing (..)
import Element exposing (..)
import Html exposing (..)
import Keyboard exposing (KeyCode)
import AnimationFrame
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

type alias Dimensions =
  { width : Int}


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
    Delta dt ->
      (applyPhysics dt model, Cmd.none)

    KeyDown keyCode ->
      (keyDown keyCode model, Cmd.none)

    KeyUp keyCode ->
      (keyUp keyCode model, Cmd.none)


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


keyUp : KeyCode -> Model -> Model
keyUp keyCode model =
  case keyCode of
    -- ArrowLeft
    -- 37 ->
    --   updateAcceleration 0 model
    -- ArrowRight
    -- 39 ->
    --   updateAcceleration 0 model

    _ ->
      model


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

view : Model -> Html Msg
-- view model =
--   Html.text (toString model)
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

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ AnimationFrame.diffs Delta
    , Keyboard.downs KeyDown
    , Keyboard.ups KeyUp
    ]
