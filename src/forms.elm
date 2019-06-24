import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)


-- MAIN

main = 
    Browser.sandbox {init = init, update = update, view = view}


-- MODEL

type alias Model =
    {
        name: String,
        password: String,
        passwordAgain: String,
        validated: Bool
    }

init: Model
init = 
    Model "" "" "" False

-- UPDATE

type Msg =
    Name String | Password String | PasswordAgain String | Validate

update : Msg -> Model -> Model
update msg model = 
    case msg of
        Name name -> 
            { model | name = name }

        Password password ->
            { model | password = password }

        PasswordAgain password ->
            { model | passwordAgain = password }
        
        Validate ->
            { model | validated = not model.validated }

-- VIEW
view : Model -> Html Msg
view model = 
    div [] [
        viewInput "text" "Name" model.name Name,
        viewInput "password" "Password" model.password Password,
        viewInput "password" "Re-enter Password" model.passwordAgain PasswordAgain,
        button [onClick Validate ] [text "validate"] ,
        if model.validated then viewValidation model
        else div [][]
    ]

viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [type_ t, placeholder p, value v, onInput toMsg] []

viewValidation : Model -> Html msg
viewValidation model =
    if String.length model.password < 8 then   
        error "Password has to be greater than 8 characters!"
    else if model.password == model.passwordAgain then
        ok
    else
        error " Passwords do not match !"

ok = div [ style "color" "green" ] [ text "OK" ]
error err = div [ style "color" "red" ] [ text err ]