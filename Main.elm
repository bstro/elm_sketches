import Html exposing (Html)
import Html.App as Html exposing (program)
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
    , tick : Int
    , texture : Maybe Texture
    }


type Msg
    = Resize Window.Size
    | TexturesError Error
    | TexturesLoaded (Maybe Texture)
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
    , texture = Nothing
    }


main : Program Never
main =
    program
    { view = view
    , update = update
    , init = init
    , subscriptions = subscriptions
    }
    

init : (Model, Cmd Msg)
init = model => 
    Cmd.batch
      [ Window.size |> Task.perform never Init
      , fetchTextures |> Task.perform TexturesError TexturesLoaded
      ]


fetchTextures : Task.Task Error (Maybe Texture)
fetchTextures =
  loadTexture "./terrain3.jpg"
    `Task.andThen` \tex -> Task.succeed (Just tex)

update : Msg -> Model -> (Model, Cmd Msg)
update msg ({res, tick} as model) =
    case msg of
        NoOp
        -> model
        => Cmd.none

        TexturesError err
        -> model
        => Cmd.none
        
        
        TexturesLoaded texture
        -> { model | texture = texture }
        => Cmd.none
        

        Init ({width, height} as res)
        ->
            { model
            | res = Just res
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


mesh : Drawable Attribute
mesh = 
    Triangle <| concat <| [0..size] `andThen` \r -> [1..size] `andThen` \c -> [
        triangle r c
            (c-1  , r+1)
            (c    , r+1)
            (c-1  , r  )
            ,
        triangle r c
            (c-1  , r  )
            (c    , r+1)
            (c    , r  )
    ]


view : Model -> Html Msg
view ({res, tick, pos, texture} as model) =
    case res of
        Nothing ->
            Html.text "Nothing"
            
        Just {height, width} ->
            case pos of
                Nothing -> Html.text "Nothing"
                Just {x, y} ->
                    case texture of
                        Nothing -> Html.text "Nothing"
                        Just tex -> 
                            let 
                                uniform =
                                    { texture = tex
                                    , rotate = rotation (toFloat x) (toFloat y) (toFloat width) (toFloat height)
                                    , scaling = scaling 1
                                    , size = size
                                    , camera = camera width height
                                    , screenWidth = toFloat width
                                    , screenHeight = toFloat height
                                    , mx = toFloat x
                                    , my = toFloat y
                                    }
                            in
                            WebGL.toHtml
                                [ Attr.width width, Attr.height height ] -- Attr.style [("position", "absolute"), ("left", "-50%")] ]
                                [ render vertexShader fragmentShader mesh uniform ]

size = 50

type alias Attribute = 
    { position : Vec3
    , color : Vec3
    , row : Float
    , col : Float
    }

type alias Varying =
    { vColor : Vec3 
    }


type alias Uniform =
    { texture : Texture
    , rotate : Mat4
    , scaling : Mat4
    , camera : Mat4
    , size : Float
    , screenWidth : Float
    , screenHeight : Float
    , mx : Float
    , my : Float
    }

normalize max val =
    let
        newMax = 1.0
        newMin = -1.0
        min = 0
    in
        (newMax - newMin) * (val-min) / (max-min) + newMin

camera : Int -> Int -> Mat4
camera w h = makePerspective 90 (toFloat w / toFloat h) 0.01 100

rotation : Float -> Float -> Float -> Float -> Mat4
rotation x y w h =
    let
        nx = (normalize w x)
        ny = (normalize h y)
    in
    makeRotate 1 (vec3 ny nx 1)

scaling : Float -> Mat4
scaling t = makeScale (vec3 t t t)  

triangle : Float -> Float -> (Float, Float) -> (Float, Float) -> (Float, Float) -> List (Attribute, Attribute, Attribute)
triangle row col (x1, y1) (x2, y2) (x3, y3) =
    [
        ( Attribute (vec3 x1 y1 0) (vec3 0 1 1) row col  
        , Attribute (vec3 x2 y2 0) (vec3 1 1 1) row col
        , Attribute (vec3 x3 y3 0) (vec3 1 0 1) row col
        )
    ]


vertexShader : Shader Attribute Uniform Varying
vertexShader = [glsl|
    attribute vec3 position;
    attribute vec3 color;
    attribute float row;
    attribute float col;
        
    uniform float size;
    uniform mat4 rotate;
    uniform mat4 scaling;
    uniform sampler2D texture;
    uniform mat4 camera;
    uniform float screenWidth;
    uniform float screenHeight;
    uniform float mx;
    uniform float my;
    
    varying vec3 vColor;
    
    vec3 newVec;
                
    float n(float val) {
        float newMax = 1.0;
        float newMin = -1.0;
        float min = 0.0;
        float max = size;
        return (newMax - newMin) * (val-min) / (max-min) + newMin;
    }
    
    highp float rand(vec2 co)
    {
        highp float a = 12.9898;
        highp float b = 78.233;
        highp float c = 43758.5453;
        highp float dt= dot(co.xy ,vec2(a,b));
        highp float sn= mod(dt,3.14);
        return fract(sin(sn) * c);
    }
    
    void main () {
        float offset = texture2D(texture, vec2(row,col)).r;
        newVec = vec3(sin(n(position.x)), sin(n(position.y)), sin(n(rand(vec2(position.x*mx, position.y*my))*2.0)*0.5)); 
        gl_Position = camera * rotate * vec4(newVec, 1);    
        vColor = color;
    }
|]

fragmentShader : Shader {} Uniform Varying
fragmentShader = [glsl|
    precision mediump float;
    varying vec3 vColor;
    uniform sampler2D texture;
    
    void main () {
        gl_FragColor = vec4(vColor, 1.0);
    }
|]
