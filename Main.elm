import Html exposing (Html)
import Html.App as Html
import Html.Attributes as Attr
import WebGL exposing (..)
import AnimationFrame exposing (..)
import Basics.Extra exposing (never)
import List.Extra exposing (andThen)
import Time
import Window
import Mouse
import Task
import Random exposing (initialSeed)
import Dict exposing (Dict)
import Math.Matrix4 exposing (..)
import Math.Vector3 exposing (..)
import Noise exposing (..)


type alias Vertex = 
    { position : Vec3
    , color : Vec3
    }
    

type alias Model = 
    { res : Maybe Window.Size
    , pos : Maybe Mouse.Position
    , terrain : Maybe (List (Vertex))
    , tick : Int
    }


type Msg
    = Resize Window.Size
    | MouseMove Mouse.Position
    | Init Window.Size
    | Tick Time.Time
    | NoOp


size = 16.18


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


mesh : Drawable Vertex
mesh = Triangle
  [ ( Vertex (vec3 0  0 0) (vec3 1 0 0)
    , Vertex (vec3 1  1 0) (vec3 0 1 0)
    , Vertex (vec3 1 -1 0) (vec3 0 0 1)
    )
  ]


-- view : Model -> Svg Msg
view ({res, tick} as model) =
    case res of
        Nothing -> Html.text "Nothing"
        Just {height, width} ->
            WebGL.toHtml
                [ Attr.width width, Attr.height height ]
                [ render vertexShader fragmentShader mesh {} ]
    

init = model => Task.perform never Init Window.size


update msg ({res, tick} as model) =
    case msg of
        NoOp
        -> model
        => Cmd.none
        
        Init ({width, height} as res)
        ->
        let
            w = width // round size
            h = height // round size
            (perm, newSeed) = permutationTable (initialSeed 42)
        in 
            { model
            | res = Just res
            , terrain =
                Just 
                [ Vertex (vec3 0  0  0) (vec3 1  0  0)
                , Vertex (vec3 1  1  0) (vec3 0  1  0)
                , Vertex (vec3 1 -1  0) (vec3 0  0  1)
                ] 
                -- Just <| [1..h] `andThen` \x -> [1..w] `andThen` \y -> 
                --             [(x => y => Vertex (vec3 0 0 0) (vec3 0 0 0) )]
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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Window.resizes Resize
        , Mouse.moves MouseMove
        , AnimationFrame.diffs Tick
        ]
    
    
(=>) : a -> b -> (a, b)
(=>) = (,)

vertexShader : Shader { attr | position: Vec3, color: Vec3 } {} { vcolor: Vec3 }
vertexShader = [glsl| 
    attribute vec3 position;
    attribute vec3 color;
    varying vec3 vcolor;
    
    void main () {
        gl_Position = vec4(position, 1.0);
        vcolor = color;
    }
|]

fragmentShader : Shader {} u { vcolor: Vec3 }
fragmentShader = [glsl|
    
    precision mediump float;
    varying vec3 vcolor;
    
    void main () {
        gl_FragColor = vec4(vcolor, 1.0);
    }
|]