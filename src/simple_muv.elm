import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import List exposing(map)

main =
   Browser.sandbox {init = init, update = update, view = view}

-------------MODEL 

type alias Model = 
   {  card1 : Card
   ,  card2 : Card
   ,  card3 : Card
   ,  status : Status
   ,  counter : Int
   } 

type alias Card = 
    {   
        color  : Color
    ,   shape  : Shape 
    }
type Color
    = Red 
    | Green 
    | Purple
    | NoColor

type Shape
    = Round
    | Diamond
    | Wave
    | NoShape

type Attribute a1 a2 a3
    = Attribute a1 a2 a3

blankCard : Card
blankCard = 
    {
        color = NoColor
    ,   shape = NoShape
    }

type Status 
    = Same
    | Different

init : Model 
init = 
  Model blankCard blankCard blankCard Same 0

----------- UPDATE

type Msg = 
  C Int Color Shape | Selected Model

update : Msg -> Model -> Model 
update msg model = 
    case msg of 
        C n color shape -> let addOne = (modBy 3 (n+1)) in case (modBy 3 n) of 
                            0 -> {card1 = (setColor color model.card1), card2 = model.card2, card3 = model.card3, status = model.status, counter = addOne}
                            1 -> {card1 = model.card1, card2 = (setColor color model.card2), card3 = model.card3, status = model.status, counter = addOne}
                            2 -> {card1 = model.card1, card2 = model.card2, card3 = (setColor color model.card3), status = model.status, counter = addOne}
                            _ -> {card1 = model.card1, card2 = model.card2, card3 = (setColor color model.card3), status = model.status, counter = addOne}
        Selected m -> { model | status = checkAttributes m}

setColor : Color -> Card -> Card
setColor newColor card = 
    { card | color = newColor}

checkAttributes : Model -> Status
checkAttributes m = let colorCheck = sameAttribute m.card1.color m.card2.color m.card3.color in 
                        let shapeCheck = sameAttribute m.card1.shape m.card2.shape m.card3.shape in 
                                 if colorCheck && shapeCheck then Same    
                                                             else if not colorCheck && not shapeCheck then Same 
                                                                                                      else Different

sameAttribute : a -> a -> a -> Bool
sameAttribute a1 a2 a3 = if a1 == a2 && a2 == a3 then True else False

-------------VIEW
view : Model -> Html Msg
view model = 
   div []
       (
       (map (\(color,shape) -> (button [ onClick (C model.counter color shape)] [text (showColor color ++ showShape shape)])) [(Red, Diamond), (Purple, Wave), (Green, Round), (Red, Diamond), (Purple, Wave), (Green, Round),(Green,Round)]) ++
       [div [] [button [ onClick (Selected model) ] [text "Check"]]] ++
       [div [] [text (if model.status == Same then "Same!" else "Different!")]])

showColor : Color -> String
showColor c = 
    case c of 
        Red -> "Red"
        Purple -> "Purple"
        Green -> "Green"
        NoColor -> "_"

showShape : Shape -> String
showShape s = 
    case s of
        Round -> "Round"
        Diamond -> "Diamond"
        Wave -> "Wave"
        NoShape -> "_" 