import Html.App as Html
import Svg exposing (..)
import Svg.Attributes as Attr exposing (..)
import AnimationFrame exposing (..)
import Basics.Extra exposing (never)
import Time
import Window
import Task


type alias Model = 
    { res : Maybe Window.Size
    , tick : Int 
    }


model =
    { res = Nothing
    , tick = 0
    }


main =
    Html.program
        { view = view
        , update = update
        , init = init
        , subscriptions = subscriptions
        }


view : Model -> Svg Msg
view model = Svg.text "Fuck"


init = model => Task.perform never Resize Window.size


update : Msg -> Model -> (Model, Cmd Msg)
update msg ({res, tick} as model) =
    case msg of
        NoOp
        -> model
        => Cmd.none
        
        Tick time
        -> { model | tick = tick + 1 }
        => Cmd.none

        Resize res
        -> { model | res = Just res }
        => Cmd.none


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Window.resizes Resize
        , AnimationFrame.diffs Tick
        ]


type Msg
    = Resize Window.Size
    | Tick Time.Time
    | NoOp
    
    
(=>) = (,)