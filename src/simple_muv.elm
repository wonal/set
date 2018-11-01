import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import List exposing(map)

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
        Card n color -> let addOne = (modBy 3 (n+1)) in case (modBy 3 n) of 
                            0 -> { model | color1 = color, counter = addOne}
                            1 -> { model | color2 = color, counter = addOne}
                            3 -> { model | color3 = color, counter = addOne}
                            _ -> { model | color3 = color, counter = addOne}
        Button status -> { model | status = status }
        


-------------VIEW
view : Model -> Html Msg
view model = 
   div []
       (
       (map (\color -> (button [ onClick (Card model.counter color)] [text (show color)])) [Red, Purple, Green, Red, Purple, Green]) ++
       [div [] [button [ onClick (Button (colorTest model.color1 model.color2 model.color3)) ] [text "Check"]]] ++
       [div [] [text (if model.status == Same then "Same!" else "Different!")]])

colorTest : Color -> Color -> Color -> Status 
colorTest c1 c2 c3 = if (c1 == c2 && c2 == c3) then Same
                                               else Different

show : Color -> String
show c = 
    case c of 
        Red -> "Red"
        Purple -> "Purple"
        Green -> "Green"
        Blank -> "_"