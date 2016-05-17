module Main exposing (..)

{-| Recreating the menu found here: https://github.com/nashvail/ReactPathMenu
Make using:
   elm-make FlowerMenu.elm --output elm.js
   open index.html


-}

import Time exposing (second, Time)
import Html.App
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
-- import Svg exposing (..)
-- import Svg.Attributes exposing (..)
import AnimationFrame
import Style
import Style.Properties exposing (..)
import Color exposing (black, rgb)


type alias Model =
  { submenus : List Submenu
  , style : Style.Animation
  , open : Bool
  , message : Maybe Message
  }

type alias Submenu =
  { style : Style.Animation
  , icon : String
  }

type alias Message =
  { label : String
  , style : Style.Animation
  }

type Msg
  = Toggle
  | ShowMessage String
  | Animate Time

update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
  case message of
    Toggle ->
      if model.open then
        let
          submenus =
              List.indexedMap
                (\i submenu ->
                  { submenu
                      | style =
                          Style.animate
                            |> Style.delay (toFloat i * 5.0e-2 * second)
                            |> Style.spring
                                  { stiffness = 400
                                  , damping = 28
                                  }
                            |> Style.to
                                [ TranslateY 0 Px ]
                            |> Style.on submenu.style
                  }
                ) model.submenus
        in
          ( { model
                | submenus = submenus
                , open = False
                , style =
                    Style.animate
                        |> Style.spring
                                { stiffness = 500
                                , damping = 30
                                }
                        |> Style.to
                            [ Rotate -0.125 Turn
                            ]
                        |> Style.on model.style
            }
          , Cmd.none
          )
      else
        let
          submenus =
            List.indexedMap
              (\i submenu ->
                  { submenu
                      | style =
                          Style.animate
                              |> Style.delay (toFloat i * 2.5e-2 * second)
                              |> Style.spring
                                    { stiffness = 400
                                    , damping = 28
                                    }
                              |> Style.to
                                  [ TranslateY 100 Px
                                  ]
                              |> Style.on submenu.style
                  }
              )
              model.submenus

          parentStyle =
              Style.animate
                |> Style.spring
                        { stiffness = 500
                        , damping = 30
                        }
                |> Style.to
                    [ Rotate 0 Turn
                    ]
                |> Style.on model.style
        in
          ( { model
              | submenus = submenus
              , style = parentStyle
              , open = True
            }
          , Cmd.none
          )

    ShowMessage str ->
      let
        message =
          { label = str
          , style =
              Style.init
                [ Display Block
                , Opacity 0
                ]
          }

        msgStyle =
          Style.animate
              |> Style.to
                  [ Opacity 1
                  ]
              |> Style.andThen
              |> Style.to
                  [ Opacity 0
                  ]
              |> Style.andThen
              |> Style.set
                  [ Display None
                  ]
              |> Style.on message.style

        msg =
          { message | style = msgStyle }
      in
        ( { model
            | message = Just msg
          }
        , Cmd.none
        )

    Animate time ->
      ( { model
            | style = Style.tick time model.style
            , message =
                Maybe.map
                (\msg ->
                    let
                      newMsgStyle =
                            Style.tick time msg.style
                    in
                      { msg | style = newMsgStyle }
                ) model.message
            , submenus =
                List.map
                  (\submenu ->
                      let
                        newStyle =
                          Style.tick time submenu.style
                      in
                        { submenu | style = newStyle }
                  ) model.submenus
        }
      , Cmd.none
      )




view : Model -> Html Msg
view model =
  let
    icon =
      i [ class "fa fa-close fa-3x"
        , style (Style.render model.style)
        ] []

    message =
      Maybe.withDefault (div [] [])
        <| Maybe.map viewMessage model.message

    submenus =
      List.map viewSubmenu model.submenus
  in
    div
      [ class "main-button"
      , onClick Toggle
      ]
      ( icon :: message :: submenus )


viewSubmenu : Submenu -> Html Msg
viewSubmenu submenu =
  div
    [ class "child-button"
    , style (Style.render submenu.style)
    , onClick (ShowMessage submenu.icon)
    ]
    [ i [ class ("fa  fa-lg fa-" ++ submenu.icon) ] []
    ]


viewMessage : Message -> Html Msg
viewMessage msg =
  div
    [ class "message"
    , style (Style.render msg.style)
    ]
    [ text msg.label ]


{-| In Turns
-}
fanAngle : Float
fanAngle =
  0.11


createSubmenu : String -> Int -> Int -> Submenu
createSubmenu icon total i =
  let
    adjustment =
      0.5 - (((toFloat total - 1) / 2.0) * fanAngle)

    angle =
      (toFloat i * fanAngle) + adjustment
  in
    { icon = icon
    , style =
        Style.init
          [ Rotate angle Turn
          , TranslateY 0 Px
          , Rotate (-1 * angle) Turn
            -- Counter rotation so the icon is upright
          ]
    }


iconCache : List String
iconCache =
  [ "pencil", "at", "camera", "bell", "comment", "bolt", "ban", "code" ]


icons : List String
icons =
  List.take 5 iconCache


init =
  ( { open = False
  , submenus =
      List.indexedMap
        (\i icon -> createSubmenu icon (List.length icons) i)
        icons
  , style =
      Style.init
        [ Rotate -0.125 Turn ]
  , message = Nothing
  }, Cmd.none
  )



subscriptions : Model -> Sub Msg
subscriptions model =
    AnimationFrame.times Animate


main =
    Html.App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
