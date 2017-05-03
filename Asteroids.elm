module Asteroids exposing (..)


-- Using color to create colors for the screen and ship
import Color exposing (..)
-- Using collage to draw onto the screen
import Collage exposing (..)
-- Using element to create our container and render to Html
import Element exposing (..)
-- Using html to create our program
import Html exposing (..)
import Html.Events exposing (..)
-- Using keyboard to track key ups and key downs
import Keyboard exposing (KeyCode)
-- Using animation frame to track time diffs
import AnimationFrame
-- Using time as the type for animation frame diffs
import Time exposing (Time)
import Set exposing (Set)
import Random


main : Program Never Model Msg
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

generateInitialSeed = Random.generate InitialSeed (Random.int Random.minInt Random.maxInt)

type alias Model =
  { menu : Menu
  , ship : Ship
  , asteroids : List Asteroid
  , stars : List Star
  , keyDowns : Set KeyCode
  , windowHeight : Int
  , windowWidth : Int
  , seed : Random.Seed
  , message : String
  }

-- Any thing that is contained in a list and needs to be drawn
type alias Drawable a =
  { a |
    x : Float,
    y : Float,
    shapeDetails : ShapeDetails
  }

-- Generic physics object
type alias PhysicsObject a =
  { a |
    x : Float,
    y : Float,
    velocity : Float,
    rotation : Int
  }

type alias ShapeDetails =
  { shape : Float -> Float -> Shape
  , shapeColor : Color
  , shapeWidth : Int
  , shapeHeight : Int
  }

type alias Menu =
  { mainMenu : Bool
  , paused : Bool
  , endGame : Bool
  }

type alias Ship =
  { x : Float
  , y : Float
  , velocity : Float
  , acceleration : Float
  , rotation : Int
  , bullets : List Bullet
  , shotFired : Bool
  , shapeDetails : ShapeDetails
  }

type alias Bullet =
  { x : Float
  , y : Float
  , velocity : Float
  , rotation : Int
  , shapeDetails : ShapeDetails
  }

type alias Asteroid =
  { x : Float
  , y : Float
  , velocity : Float
  , rotation : Int
  , shapeDetails : ShapeDetails
  }

type alias Star =
  { x : Float
  , y : Float
  , shapeDetails : ShapeDetails
  }


menuChoices : Menu
menuChoices =
  { mainMenu = True
  , paused = False
  , endGame = False
  }


spaceshipShape : ShapeDetails
spaceshipShape =
  { shape = oval
  , shapeColor = white
  , shapeWidth = 15
  , shapeHeight = 15
  }


bulletShape : ShapeDetails
bulletShape =
  { shape = oval
  , shapeColor = red
  , shapeWidth = 5
  , shapeHeight = 5
  }


asteroidShape : ShapeDetails
asteroidShape =
  { shape = oval
  , shapeColor = grey
  , shapeWidth = 60
  , shapeHeight = 60
  }


starShape : ShapeDetails
starShape =
  { shape = oval
  , shapeColor = yellow
  , shapeWidth = 2
  , shapeHeight = 2
  }


spaceship : Ship
spaceship =
  { x = 0
  , y = 0
  , velocity = 0
  , acceleration = 0
  , rotation = 0
  , bullets = []
  , shotFired = False
  , shapeDetails = spaceshipShape
  }


initialModel : Model
initialModel =
  { menu = menuChoices
  , ship = spaceship
  , asteroids = []
  , stars = []
  , keyDowns = Set.empty
  , windowHeight = 500
  , windowWidth = 500
  , seed = Random.initialSeed 0
  , message = ""
  }


-- INIT

init : (Model, Cmd Msg)
init =
  (initialModel, generateInitialSeed)

-- UPDATE

type Msg =
  Delta Time
  | KeyDown KeyCode
  | KeyUp KeyCode
  | KeyPresses KeyCode
  | InitialSeed Int
  | StartGame


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    InitialSeed val ->
      ({ model | seed = Random.initialSeed val }, Cmd.none)

    StartGame ->
      (createStars 200 (createAsteroids 10 initialModel), Cmd.none)

    KeyDown keyCode ->
      ({ model | keyDowns = Set.insert keyCode model.keyDowns }, Cmd.none)

    KeyUp keyCode ->
      ({ model | keyDowns = Set.remove keyCode model.keyDowns }, Cmd.none)

    KeyPresses keyCode ->
      (keyPress keyCode model, Cmd.none)

    Delta dt ->
      if not model.menu.mainMenu && not model.menu.paused && not model.menu.endGame then
        (checkCollisions (applyPhysics dt (checkKeys model)), Cmd.none)
      else
        (model, Cmd.none)


createStars : Int -> Model -> Model
createStars num model =
  if num == 0 then
    let
      { menu } = model
      newMenu =
        { menu |
          mainMenu = False,
          paused = False,
          endGame = False}
    in
      { model | menu = newMenu }
  else
    let
      (newX, seedX) = (Random.step (Random.float -250 250) model.seed)
      (newY, seedY) = (Random.step (Random.float -250 250) seedX)
      stars =
        { x = newX
         , y = newY
         , shapeDetails = starShape
         } :: model.stars
    in
      createStars (num - 1)
        ({ model |
            stars = stars,
            seed = seedY })


createAsteroids : Int -> Model -> Model
createAsteroids num model =
  if num == 0 then
    model
  else
    let
      (newX, seedX) = (Random.step (Random.float -250 250) model.seed)
      (newY, seedY) = (Random.step (Random.float -250 250) seedX)
      (newVelocity, seedVelocity) = (Random.step (Random.float 0 0.1) seedY)
      (newRotation, seedRotation) = (Random.step (Random.int 0 359) seedVelocity)
      asteroids =
        { x = newX
         , y = newY
         , velocity = newVelocity
         , rotation = newRotation
         , shapeDetails = asteroidShape
         } :: model.asteroids
    in
      createAsteroids (num - 1)
        ({ model |
            asteroids = asteroids,
            seed = seedRotation })


keyPress : KeyCode -> Model -> Model
keyPress keyCode model =
  case keyCode of
    -- Esc
    -- If the esc key is pressed we pause or unpause the game
    27 ->
      let
        { menu } = model
        paused = not model.menu.paused
      in
        { model | menu = { menu | paused = paused } }

    -- Space
    -- If the space button has been pressed we can fire another shot.
    32 ->
      if Set.member 32 model.keyDowns && not model.ship.shotFired then
        { model | ship = fireShot model.ship }
      else
        model

    _ ->
      model


-- Checks which keys are down and applies proper functions
checkKeys : Model -> Model
checkKeys model =
  let
    { ship } = model
    -- Check to rest ship shot fired
    newShip =
      if not (Set.member 32 model.keyDowns) && model.ship.shotFired then
        { ship | shotFired = False }
      else
        ship
    -- Check the rest of our keys
    newModel = checkKeysHelper { model | ship = newShip } (Set.toList model.keyDowns)
  in
    newModel


checkKeysHelper : Model -> List KeyCode -> Model
checkKeysHelper model keyCodes =
  if List.isEmpty keyCodes then
    model
  else
    let
      keyCode = Maybe.withDefault -1 (List.head keyCodes)
      rest = Maybe.withDefault [] (List.tail keyCodes)
    in
      if not (keyCode == -1) then
        case keyCode of
          -- ArrowLeft
          37 ->
            let
              { ship } = model
              newShip =
                updateAccelerationShip -0.0001 ship
            in
              checkKeysHelper { model | ship = newShip } rest
          -- ArrowRight
          39 ->
            let
              { ship } = model
              newShip =
                updateAccelerationShip 0.0001 ship
            in
              checkKeysHelper { model | ship = newShip } rest
          -- ArrowUp
          38 ->
            let
              { ship } = model
              newShip =
                updateRotationShip 3 ship
            in
              checkKeysHelper { model | ship = newShip } rest
          -- ArrowDown
          40 ->
            let
              { ship } = model
              newShip =
                updateRotationShip -3 ship
            in
              checkKeysHelper { model | ship = newShip } rest

          _ ->
            checkKeysHelper model rest
        else
          checkKeysHelper model rest


-- Applies velocity and position to our objects.
applyPhysics : Float -> Model -> Model
applyPhysics dt model =
  let
    { ship, asteroids } = model
    newShip = applyPhysicsShip dt ship model
    { bullets } = newShip
    newBullets = (checkBulletOutOfBounds model (applyObjectPhysics model dt bullets) [])
    newAsteroids = (checkAsteroidOutOfBounds model (applyObjectPhysics model dt asteroids) [])
  in
    { model |
      ship = { newShip | bullets = newBullets },
      asteroids = newAsteroids }


applyPhysicsShip : Float -> Ship -> Model -> Ship
applyPhysicsShip dt ship model =
  let
    calculatedVelocity =
      ship.velocity + ship.acceleration * dt

    newVelocity =
        if calculatedVelocity >= 0 && calculatedVelocity <= 0.15 then
          calculatedVelocity
        else if calculatedVelocity < 0 then
          0
        else
          0.15

    calculatedXPosition =
      1/2 * ship.acceleration * dt * dt + cos (degrees (toFloat ship.rotation)) * newVelocity * dt + ship.x

    newXPosition =
      if calculatedXPosition >= -(toFloat model.windowWidth) / 2 && calculatedXPosition <= (toFloat model.windowWidth) / 2 then
        calculatedXPosition
      else if calculatedXPosition < -(toFloat model.windowWidth) / 2 then
        (toFloat model.windowWidth) / 2
      else
        -(toFloat model.windowWidth) / 2

    calculatedYPosition =
      1/2 * ship.acceleration * dt * dt + sin (degrees (toFloat ship.rotation)) * newVelocity * dt + ship.y

    newYPosition =
      if calculatedYPosition >= -(toFloat model.windowHeight) / 2 && calculatedYPosition <= (toFloat model.windowHeight) / 2 then
        calculatedYPosition
      else if calculatedYPosition < -(toFloat model.windowHeight) / 2 then
        (toFloat model.windowHeight) / 2
      else
        -(toFloat model.windowHeight) / 2
  in
    { ship |
        x = newXPosition,
        y = newYPosition,
        velocity = newVelocity
    }


applyObjectPhysics : Model -> Float -> List (PhysicsObject a) -> List (PhysicsObject a)
applyObjectPhysics model dt physObjs =
  List.map (applyObjectPhysicsHelper dt) physObjs


applyObjectPhysicsHelper : Float -> PhysicsObject a -> PhysicsObject a
applyObjectPhysicsHelper dt physObj =
  let
    { velocity } = physObj
    calculatedXPosition =
      cos (degrees (toFloat physObj.rotation)) * velocity * dt + physObj.x

    calculatedYPosition =
      sin (degrees (toFloat physObj.rotation)) * velocity * dt + physObj.y
  in
    { physObj
    | x = calculatedXPosition
    , y = calculatedYPosition
    }


checkBulletOutOfBounds : Model -> List Bullet -> List Bullet -> List Bullet
checkBulletOutOfBounds model bullets acc =
  case bullets of
    bullet::rest ->
      if bullet.x >= -(toFloat model.windowWidth) / 2 && bullet.x <= (toFloat model.windowWidth) / 2  &&
          bullet.y >= -(toFloat model.windowHeight) / 2 && bullet.y <= (toFloat model.windowHeight) / 2  then
        checkBulletOutOfBounds model rest (bullet :: acc)
      else
        checkBulletOutOfBounds model rest acc
    [] -> acc


checkAsteroidOutOfBounds : Model -> List Asteroid -> List Asteroid -> List Asteroid
checkAsteroidOutOfBounds model asteroids acc =
  case asteroids of
    asteroid::rest ->
      let
        x =
          if asteroid.x < -(toFloat model.windowWidth) / 2 then
            (toFloat model.windowWidth) / 2
          else if asteroid.x > (toFloat model.windowWidth) / 2 then
            -(toFloat model.windowWidth) / 2
          else
            asteroid.x
        y =
          if asteroid.y < -(toFloat model.windowHeight) / 2 then
            (toFloat model.windowHeight) / 2
          else if asteroid.y > (toFloat model.windowHeight) / 2 then
            -(toFloat model.windowHeight) / 2
          else
            asteroid.y
      in
        checkAsteroidOutOfBounds model rest ({ asteroid | x = x, y = y} :: acc)
    [] -> acc


updateAccelerationShip : Float -> Ship -> Ship
updateAccelerationShip addAcceleration ship =
  let
    calculatedAcceleration =
      ship.acceleration + addAcceleration

    newAcceleration =
      if ship.velocity <= 0 && calculatedAcceleration <= 0 then
        0
      else if calculatedAcceleration >= -0.001 && calculatedAcceleration <= 0.001 then
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


-- Create a new bullet and add it to our list of bullets for our ship.
fireShot : Ship -> Ship
fireShot ship =
  let
    { bullets } = ship
    newBullets =
      { x = ship.x
      , y = ship.y
      , velocity = 0.3
      , rotation = ship.rotation
      , shapeDetails = bulletShape
      } :: bullets
  in
    { ship
    | bullets = newBullets
    , shotFired = True
    }


-- Check collisions between records in our model
checkCollisions : Model -> Model
checkCollisions model =
  let
    { ship, menu } = model
    (asteroids, bullets) = checkAsteroidBulletCollision model.asteroids model.ship.bullets []
    (endGame, message) =
      if checkShipCollision ship asteroids then
        (True, "You Lose")
      else if List.isEmpty asteroids then
        (True, "You Win!")
      else
        (False, "")
  in
    { model
    | asteroids = asteroids
    , message = message
    , menu = { menu | endGame = endGame }
    , ship = { ship | bullets = bullets } }


checkAsteroidBulletCollision : List Asteroid -> List Bullet -> List Asteroid -> (List Asteroid, List Bullet)
checkAsteroidBulletCollision asteroids bullets asteroidsAcc =
  case asteroids of
    asteroid::rest ->
      -- For a given asteroid check if any bullets are in it
      let
        (keepAsteroid, newBullets) = checkBulletsInAsteroid asteroid bullets []
      in
        if keepAsteroid then
          checkAsteroidBulletCollision rest newBullets (asteroid :: asteroidsAcc)
        else
          checkAsteroidBulletCollision rest newBullets asteroidsAcc
    [] ->
      (asteroidsAcc, bullets)


checkBulletsInAsteroid : Asteroid -> List Bullet -> List Bullet -> (Bool, List Bullet)
checkBulletsInAsteroid asteroid bullets bulletsAcc =
  case bullets of
    bullet::rest ->
      -- If this bullet is in the asteroid, return all the other bullets
      if drawableInDrawable bullet asteroid then
        (False, bulletsAcc ++ rest)
      else
        checkBulletsInAsteroid asteroid rest (bullet :: bulletsAcc)
    [] ->
      (True, bulletsAcc)


checkShipCollision : Ship -> List Asteroid -> Bool
checkShipCollision ship asteroids =
  case asteroids of
    asteroid::rest ->
      if drawableInDrawable ship asteroid then
        True
      else
        checkShipCollision ship rest
    [] -> False


-- Check to see if a given drawable is in another drawable
drawableInDrawable : Drawable a -> Drawable b -> Bool
drawableInDrawable drawableOne drawableTwo =
  let
    radius = ((toFloat drawableTwo.shapeDetails.shapeWidth) / 2)^2
    distance = (drawableOne.x - drawableTwo.x)^2 + (drawableOne.y - drawableTwo.y)^2
  in
    distance <= radius


-- VIEW

-- Draws a black box and a circle that you can control
view : Model -> Html Msg
view model =
  -- Html.text (toString model)
  let
    { ship, menu, asteroids, stars } = model
    { bullets } = ship
  in
    if menu.mainMenu then
      div []
        [ div [] [ Html.text "Asteroids" ]
        , div []
          [ button [ Html.Events.onClick StartGame ] [ Html.text "Play" ]]
        ]
    else if menu.endGame || menu.paused then
      div []
        [ div [] [ Html.text model.message ]
        , div []
          [ button [ Html.Events.onClick StartGame ] [ Html.text "Restart" ]]
        ]
    else
      toHtml <|
        container model.windowWidth model.windowHeight middle <|
          collage model.windowWidth model.windowHeight
            ([ rect (toFloat model.windowWidth) (toFloat model.windowHeight)
                |> filled (rgb 0 0 0)]
              ++ (showDrawables yellow (oval 2 2) stars [])
              ++ (showDrawables red (oval 5 5) bullets [])
              ++ [oval 15 15
                  |> filled white
                  |> move (ship.x, ship.y)]
              ++ (showDrawables grey (oval 60 60) asteroids []))


showDrawables : Color -> Shape -> List (Drawable a) -> List Form -> List Form
showDrawables shapeColor shape drawables acc =
  case drawables of
    drawable::rest ->
      showDrawables shapeColor shape rest ((move (drawable.x, drawable.y) (filled shapeColor shape)) :: acc)
    [] -> acc


-- SUBSCRIPTIONS
-- Keeping track of time and key presses.
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ AnimationFrame.diffs Delta
    , Keyboard.downs KeyDown
    , Keyboard.ups KeyUp
    , Keyboard.presses KeyPresses
    ]
