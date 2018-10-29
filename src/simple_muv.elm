import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)

main =
   Browser.sandbox {init = init, update = update, view = view}

-------------MODEL 

type alias Model = 
   {  color1 : Color
   ,  color2 : Color
   ,  color3 : Color
   ,  status : Status
   ,  counter : Int
   } 

type Color
    = Red 
    | Green 
    | Purple
    | Blank

type Status 
    = Same
    | Different

init : Model 
init = 
  Model Blank Blank Blank Same 0

----------- UPDATE

type Msg = 
  Card Int Color | Button Status

update : Msg -> Model -> Model 
update msg model = 
    case msg of 
        Card n color -> if (modBy 3 n) == 0 then { model | color1 = color, counter = (modBy 3 (n+1)) }
                                                else if (modBy 3 n == 1) then { model | color2 = color, counter = (modBy 3 (n+1))}
                                                                         else { model | color3 = color, counter = (modBy 3 (n+1))}
        Button status -> { model | status = status }
        


-------------VIEW
view : Model -> Html Msg
view model = 
   div []
      [
          button [ onClick (Card model.counter Red) ] [ text "Red"]
      ,   button [ onClick (Card model.counter Purple) ] [ text "Purple"]
      ,   button [ onClick (Card model.counter Green) ] [ text "Green"]
      ,   button [ onClick (Button (colorTest model.color1 model.color2 model.color3)) ] [text "Check"]
      ,   div [] [text (if model.status == Same then "Same!" else "Different!")] 
      ]

colorTest : Color -> Color -> Color -> Status 
colorTest c1 c2 c3 = if (c1 == c2 && c2 == c3) then Same
                                               else Different
