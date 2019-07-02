import Browser
import Html exposing (..)
import Task
import Time

-- MAIN

main = 
    Browser.element {
        init = init,
        view = view,
        update = update,
        subscriptions = subscriptions
    }

-- MODEL
type alias Model = {
        zone : Time.Zone,
        time: Time.Posix
    }

init : () -> (Model, Cmd Msg)
init _ =
    (
        Model Time.utc (Time.millisToPosix 0),
        Task.perform AdjustTimeZone Time.here
    )

-- UPDATE
type Msg = 
    Tick Time.Posix 
    | AdjustTimeZone Time.Zone

update: Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    case msg of
        Tick newTime -> 
            (
                {model | time = newTime},
                Cmd.none
            )

        AdjustTimeZone newZone -> 
            (
                {model | zone = newZone},
                Cmd.none
            )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick

-- VIEW

view : Model -> Html Msg
view model = 
    let
        hour = getTime model Time.toHour
        minute = getTime model Time.toMinute
        second = getTime model Time.toSecond
    in
        h1 [] [text (hour ++ ":" ++ minute ++ ":" ++ second)]

getTime: Model -> (Time.Zone -> Time.Posix -> Int) -> String
getTime model f =
    String.fromInt (f model.zone model.time)