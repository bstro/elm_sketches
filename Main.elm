import Html.App as Html
import Svg exposing (..)
import Svg.Attributes as Attr exposing (..)
import AnimationFrame exposing (..)
import Basics.Extra exposing (never)
import Time
import Window
import Task
import Math.Matrix4 exposing (..)
import Math.Vector3 exposing (..)

type alias Model = 
    { res : Maybe Window.Size
    , tick : Int 
    }


size = 20


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
view ({res, tick} as model) =
    case res of
        Nothing -> Svg.text "Nothing"
        
        Just {width, height} ->
            let
                w = (toFloat width) / size
                h = (toFloat height) / size
                vb = "0" ++ " " ++ "0" ++ " " ++ toString w ++ " " ++ toString h
                
                rows = [1..size]
                columns = [1..size]
                
            in
                Svg.svg 
                [ viewBox vb
                , Attr.width (toString width ++ "px")
                , Attr.height (toString height ++ "px")
                , Attr.style "overflow: hidden; position: absolute;" 
                ]
                [
                    Svg.text' [ x (toString <| w/2), y (toString <| h/2), fontSize "1", textAnchor "middle" ] [Svg.text <| toString model] 
                ]
                 -- elements
    


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