module Main exposing (..)

import AnimationFrame
import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Style
import Style.Properties exposing (..)
import Time exposing (Time)


type alias Model =
    { style : Style.Animation
    , content : String
    }


type Msg
    = Show
    | Animate Time


init : ( Model, Cmd Msg )
init =
    ( { style =
            Style.init
                [ Left 0 Px
                , Opacity 0.2
                ]
      , content = "😊"
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    AnimationFrame.times Animate


view : Model -> Html Msg
view model =
    div []
        [ div
            [ style
                ([ ( "position", "absolute" )
                 , ( "top", "45px" )
                 , ( "font-size", "85px" )
                 ]
                    ++ (Style.render model.style)
                )
            ]
            [ text model.content ]
        , button [ onClick Show ] [ text "Start!" ]
        ]


update msg model =
    case msg of
        Show ->
            ( { model
                | style =
                    (List.foldl
                        (\i anim ->
                            let
                                animation =
                                    if i == 0 then
                                        anim
                                    else
                                        anim |> Style.andThen
                            in
                                animation
                                    |> Style.spring { stiffness = 300, damping = 18 }
                                    |> Style.to [ Left 150 Px, Opacity 1 ]
                                    |> Style.andThen
                                    |> Style.spring { stiffness = 300, damping = 18 }
                                    |> Style.delay (0.5 * Time.second)
                                    |> Style.to [ Left 0 Px, Opacity 0.3 ]
                        )
                        Style.animate
                        [1..5]
                    )
                        |> Style.on model.style
                    --Style.repeat 4
                    --    |> Style.spring { stiffness = 300, damping = 18 }
                    --    |> Style.to [ Left 150 Px, Opacity 1 ]
                    --    |> Style.andThen
                    --    |> Style.spring { stiffness = 300, damping = 18 }
                    --    |> Style.delay (0.5 * Time.second)
                    --    |>
                    --        Style.to [ Left 0 Px, Opacity 0.3 ]
                    --    |> Style.on model.style
              }
            , Cmd.none
            )

        Animate time ->
            ( { model | style = Style.tick time model.style }, Cmd.none )


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
