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
    , terrain : Maybe (List (Attribute, Attribute, Attribute))
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


model : Model
model =
    { res = Nothing
    , pos = Nothing
    , tick = 0
    , terrain = Nothing
    }


main : Program Never
main =
    Html.program
        { view = view
        , update = update
        , init = init
        , subscriptions = subscriptions
        }
    

init : (Model, Cmd Msg)
init = model => Task.perform never Init Window.size


update : Msg -> Model -> (Model, Cmd Msg)
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


rows = 3

columns = 3


getc : Float -> Float
getc = norm columns


getr : Float -> Float
getr = norm rows


mesh : Drawable Attribute
mesh = 
    Triangle <| concat <| [0..rows] `andThen` \r -> [1..columns] `andThen` \c -> [
        triangle
            (getc <| c-1  , getr <| r+1)
            (getc <| c    , getr <| r+1)
            (getc <| c-1  , getr <| r  )
            ,
        triangle
            (getc <| c-1  , getr <| r  )
            (getc <| c    , getr <| r+1)
            (getc <| c    , getr <| r)
    ]


view : Model -> Html Msg
view ({res, tick, pos} as model) =
    case res of
        Nothing ->
            Html.text "Nothing"
            
        Just {height, width} ->
            case pos of
                Nothing -> Html.text "Nothing"
                Just {x, y} ->
                    WebGL.toHtml
                        [ Attr.width width, Attr.height height ] -- Attr.style [("position", "absolute"), ("left", "-50%")] ]
                        [ render vertexShader fragmentShader mesh {rotate = rotation ((*) 0.005 <| toFloat y) ((*) 0.005 <| toFloat x) } ]


type alias Uniform = { rotate : Mat4 }

type alias Attribute = 
    { position : Vec3
    , color : Vec3
    }

type alias Varying =
    { vColor : Vec3 
    }


rotation : Float -> Float -> Mat4
rotation x y = makeRotate (x*y) (vec3 1 1 1) 

 
triangle : (Float, Float) -> (Float, Float) -> (Float, Float) -> List (Attribute, Attribute, Attribute)
triangle (x1, y1) (x2, y2) (x3, y3) =
    [
        ( Attribute (vec3 x1 y1 0) (vec3 0 1 1)  
        , Attribute (vec3 x2 y2 0) (vec3 1 1 0)
        , Attribute (vec3 x3 y3 0) (vec3 1 0 1)
        )
    ]
 

norm : Float -> Float -> Float
norm max val =
    let
        newMax = 1
        newMin = -1
        min = 0
    in
        (newMax - newMin) * (val-min) / (max-min) + newMin


vertexShader : Shader Attribute Uniform Varying
vertexShader = [glsl|
    attribute vec3 position; 
    attribute vec3 color;
    
    uniform mat4 rotate;
    
    varying vec3 vColor;
    
    void main () {
        gl_Position = rotate * vec4(position, 1.0);
        vColor = color;
    }
|]

fragmentShader : Shader {} Uniform Varying
fragmentShader = [glsl|
    precision mediump float;
    varying vec3 vColor;
    
    void main () {
        gl_FragColor = vec4(vColor, 1.0);
    }
|]
