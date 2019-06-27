import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Random
import Array exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
-- import Int

-- MAIN
main = 
  Browser.element {
    init = init,
    subscriptions = subscriptions,
    update = update,
    view = view
  }
-- MODEL 
type alias Model  =  {
  dieFace: Int
  }

images : Array String
images = 
  Array.fromList [
    "https://etc.usf.edu/clipart/42100/42158/die_01_42158_mth.gif",
    "https://etc.usf.edu/clipart/42100/42159/die_02_42159_mth.gif",
    "https://etc.usf.edu/clipart/42100/42160/die_03_42160_mth.gif",
    "https://etc.usf.edu/clipart/42100/42161/die_04_42161_mth.gif",
    "https://etc.usf.edu/clipart/42100/42162/die_05_42162_mth.gif",
    "https://etc.usf.edu/clipart/42100/42164/die_06_42164_mth.gif"
  ]

init : () -> (Model, Cmd Msg)
init _ =
  (
    Model 3,
    Cmd.none
  )

-- UPDATE

type Msg = Roll | NewFace Int

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of
    Roll -> 
      (
        model, 
        Random.generate NewFace (Random.int 1 6)
      )
    NewFace newFace ->
      (
        Model newFace,
        Cmd.none
      )
-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

-- VIEW

view : Model    -> Html Msg
view model =
  div []
  [
    h1 [] [Html.text (String.fromInt model.dieFace)],
    dieImage (model.dieFace),
    dieSvg (model.dieFace),
    button [onClick Roll] [Html.text "Roll"]
  ]

dieSvg: Int -> Html.Html msg
dieSvg face = 
  svg [
    Svg.Attributes.width "100",
    Svg.Attributes.height "100",
    viewBox "0 0 100 100"
  ] [
    g [
        color "#8876ff"
    ](
      rect [
        x "0", y "0",
        Svg.Attributes.width "100",
        Svg.Attributes.height "100",
        rx "15", ry "15",
        fill "currentColor",
        stroke "blue",
        fillOpacity "0.5"
      ] [] :: 
        dotsSvg face
    )
  ]

dotsSvg : Int -> List (Svg msg)
dotsSvg face =  
  List.concat
  [
    if (face > 1) then 
      [
        topLeft |> dot,
        topLeft |> right |> right |> down |> down |> dot
      ]
    else [],

    if (face > 3) then 
      [
        topLeft |> down |> down |> dot,
        topLeft |> right |> right |> dot
      ]
    else [],

    if (face > 5) then 
      [
        topLeft |> down |> dot,
        topLeft |> right |> right |> down |> dot
      ]
    else [],

    -- Middle circle
    if (modBy 2 face > 0) then 
      [topLeft |> down |> right |> dot]
    else []
  ]

stride : Float
stride = 100 / 3

uncurry : (a -> b -> c) -> (a, b) ->  c
uncurry f (p1, p2) = f p1 p2

topLeft = (100 / 6, 100 / 6)

right : (Float, Float) -> (Float, Float)
right (x, y) =  (x + stride, y)

down : (Float, Float) -> (Float, Float)
down  (x, y) = (x, y + stride)

dot : (Float, Float) -> Svg msg
dot (x,y) =
    g [
      color "green"
    ][
      circle [
        cx (String.fromFloat x),
        cy (String.fromFloat y),
        r "10",
        fill "currentColor"
      ][]
    ]

dieImage : Int -> Html.Html msg
dieImage face = 
  img [src (dieUrl face)][]

dieUrl : Int -> String
dieUrl dieFace =
   Maybe.withDefault "" (Array.get (dieFace - 1) ( images))