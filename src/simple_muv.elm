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

 -----------------UPDATE

type Msg = 
  ClickedCard Int Color Shape | Selected Model

update : Msg -> Model -> Model 
update msg model = 
    case msg of 
        ClickedCard n c shape -> let addOne = (modBy 3 (n+1)) in case (modBy 3 n) of 
                            0 -> {card1 = (setAttributes c shape model.card1), card2 = model.card2, card3 = model.card3, status = model.status, counter = addOne}
                            1 -> {card1 = model.card1, card2 = (setAttributes c shape model.card2), card3 = model.card3, status = model.status, counter = addOne}
                            2 -> {card1 = model.card1, card2 = model.card2, card3 = (setAttributes c shape model.card3), status = model.status, counter = addOne}
                            _ -> {card1 = model.card1, card2 = model.card2, card3 = (setAttributes c shape model.card3), status = model.status, counter = addOne}
        Selected m -> { model | status = checkAttributes m}

setAttributes : Color -> Shape -> Card -> Card
setAttributes newColor newShape card = 
    { card | color = newColor, shape = newShape}

checkAttributes : Model -> Status
checkAttributes m = let sameColor = allSame m.card1.color m.card2.color m.card3.color in 
                    let sameShape = allSame m.card1.shape m.card2.shape m.card3.shape in 
                    let differentColor = allDifferent m.card1.color m.card2.color m.card3.color in 
                    let differentShape = allDifferent m.card1.shape m.card2.shape m.card3.shape in 
                            if sameColor && sameShape then Same    
                                        else if differentColor && differentShape then Same 
                                                                                 else Different

allSame : a -> a -> a -> Bool
allSame a1 a2 a3 = if a1 == a2 && a2 == a3 then True else False

allDifferent : a -> a -> a -> Bool
allDifferent a1 a2 a3 = if a1 /= a2 && a2 /= a3 && a1 /= a3 then True else False 

-------------VIEW
view : Model -> Html Msg
view model = 
   div []
       (
       (map (\(color,shape) -> (button [ onClick (ClickedCard model.counter color shape)] [text (showColor color ++ showShape shape)])) 
                            [(Red, Round), (Purple, Diamond), (Green, Wave), (Red, Diamond), (Purple, Wave), (Green, Round),(Green,Round)]) ++
       [div [] [button [ onClick (Selected model) ] [text "Check"]]] ++
       [div [] [text (if model.status == Same then "Same!" else "Different!")]] ++
       [div [] [text (showColor model.card1.color ++ showShape model.card1.shape ++ " 2: " ++ showColor model.card2.color ++ showShape model.card2.shape ++ " 3:" ++ showColor model.card3.color ++ showShape model.card3.shape)]])

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