import Html exposing (Html)
import Html.App as Html
import Html.Attributes as Attr
import WebGL exposing (..)
import AnimationFrame exposing (..)
import Basics.Extra exposing (never)
import List.Extra exposing (andThen)
import List exposing (concat)
import Time
import Window
import Mouse
import Task
import Random exposing (initialSeed)
import Dict exposing (Dict)
import Math.Matrix4 exposing (..)
import Math.Vector3 exposing (..)
import Noise exposing (..)

type alias Model = 
    { res : Maybe Window.Size
    , pos : Maybe Mouse.Position
    , terrain : Maybe (List (Vertex, Vertex, Vertex))
    , tick : Int
    }


type Msg
    = Resize Window.Size
    | MouseMove Mouse.Position
    | Init Window.Size
    | Tick Time.Time
    | NoOp


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Window.resizes Resize
        , Mouse.moves MouseMove
        , AnimationFrame.diffs Tick
        ]


model =
    { res = Nothing
    , pos = Nothing
    , tick = 0
    , terrain = Nothing
    }


main =
    Html.program
        { view = view
        , update = update
        , init = init
        , subscriptions = subscriptions
        }
    
    
init = model => Task.perform never Init Window.size


update msg ({res, tick} as model) =
    case msg of
        NoOp
        -> model
        => Cmd.none
        
        Init ({width, height} as res)
        ->
        let
            (perm, newSeed) = permutationTable (initialSeed 42)
        in 
            { model
            | res = Just res
            , terrain = Just <| triangle (0 , 0) (1 , 1) (1 , -1) 
            }
        => Cmd.none
        
        MouseMove pos
        -> { model | pos = Just pos }
        => Cmd.none
        
        Tick time
        -> { model | tick = tick + 1 }
        => Cmd.none

        Resize res
        -> { model | res = Just res }
        => Cmd.none

    
(=>) : a -> b -> (a, b)
(=>) = (,)


type alias Vertex = { position : Vec3, color : Vec3 }
type alias Varying = { vColor : Vec3 }

 
triangle : (Float, Float) -> (Float, Float) -> (Float, Float) -> List (Vertex, Vertex, Vertex)
triangle (x1, y1) (x2, y2) (x3, y3) =
    [
        ( Vertex (vec3 x1 y1 0) (vec3 0 0 0)
        , Vertex (vec3 x2 y2 0) (vec3 0 0 0)
        , Vertex (vec3 x3 y3 0) (vec3 0 0 0)
        )
    ]


-- n max val =
--     let
--         min = 2
--     in
--         (val - min) / (max - min) 

n max unscaled =
    let
        maxAllowed = 1
        minAllowed = -1
        min = 0
    in
        (maxAllowed - minAllowed) * (unscaled-min) / (max-min) + minAllowed


view ({res, tick} as model) =
    case res of
        Nothing ->
            Html.text "Nothing"
            
        Just {height, width} ->
            let
                rows =
                    40
                columns = 
                    40
                pxPerRow =
                    (toFloat height) / rows
                pxPerCol =
                    (toFloat width) / columns
                getc =
                    n columns
                getr = 
                    n rows
                mesh = 
                    Triangle <| concat <| [0..rows] `andThen` \r -> [1..columns] `andThen` \c -> [
                        triangle
                            (getc <| c-1  , getr <| r+1)
                            (getc <| c    , getr <| r+1)
                            (getc <| c-1  , getr <| r)
                            
                    ] 
                log = 
                    Debug.log "mesh is" mesh
                    
            in
                WebGL.toHtml
                    [ Attr.width width, Attr.height height ] -- Attr.style [("position", "absolute"), ("left", "-50%")] ]
                    [ render vertexShader fragmentShader mesh {} ]


vertexShader : Shader Vertex {} Varying
vertexShader = [glsl|
    attribute vec3 position; 
    attribute vec3 color;
    varying vec3 vColor;
    
    void main () {
        gl_Position = vec4(position, 1.0);
        vColor = color;
    }
|]

fragmentShader : Shader {} {} Varying
fragmentShader = [glsl|
    precision mediump float;
    
    varying vec3 vColor;
    
    void main () {
        gl_FragColor = vec4(vColor, 1.0);
    }
|]
