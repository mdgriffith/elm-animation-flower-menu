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
import AnimationFrame
import Style
import Style.Properties exposing (..)
import Style.Sheet
import Animation
import Msg exposing (..)


type alias Model =
    { submenus : List Submenu
    , open : Bool
    , message : Maybe String
    , sheet : Style.Sheet.Model Animation.Class Msg
    }


type alias Submenu =
    { icon : String
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Toggle ->
            if model.open then
                ( { model
                    | open = False
                    , sheet = Style.Sheet.update Animation.close model.sheet
                  }
                , Cmd.none
                )
            else
                ( { model
                    | open = True
                    , sheet = Style.Sheet.update Animation.open model.sheet
                  }
                , Cmd.none
                )

        ShowMessage str ->
            ( { model
                | message = Just str
                , sheet = Style.Sheet.update Animation.showMsg model.sheet
              }
            , Cmd.none
            )

        Print str ->
            let
                _ =
                    Debug.log "print" str
            in
                ( model, Cmd.none )

        Animate time ->
            let
                ( newSheet, messages ) =
                    Style.Sheet.tick time model.sheet
            in
                List.foldl
                    (\msg ( model, cmds ) ->
                        let
                            ( new, newCmds ) =
                                update msg model
                        in
                            ( new, Cmd.batch [ cmds, newCmds ] )
                    )
                    ( { model
                        | sheet = newSheet
                      }
                    , Cmd.none
                    )
                    messages


view : Model -> Html Msg
view model =
    let
        icon =
            i
                [ class "fa fa-close fa-3x"
                , style (Style.Sheet.render model.sheet Animation.Menu)
                ]
                []

        message =
            case model.message of
                Nothing ->
                    div [] []

                Just msg ->
                    div
                        [ class "message"
                        , style (Style.Sheet.render model.sheet Animation.Message)
                        ]
                        [ text msg ]

        submenus =
            List.indexedMap (viewSubmenu model.sheet) model.submenus
    in
        div
            [ class "main-button"
            , onClick Toggle
            ]
            (icon :: message :: submenus)


viewSubmenu : Style.Sheet.Model Animation.Class Msg -> Int -> Submenu -> Html Msg
viewSubmenu sheet id submenu =
    div
        [ class "child-button"
        , style (Style.Sheet.render sheet (Animation.Submenu id))
        , onClick (ShowMessage submenu.icon)
        ]
        [ i [ class ("fa  fa-lg fa-" ++ submenu.icon) ] []
        ]


icons : List String
icons =
    List.take 5 [ "pencil", "at", "camera", "bell", "comment", "bolt", "ban", "code" ]


init =
    ( { open = False
      , submenus =
            List.map (\icon -> { icon = icon }) icons
      , message = Nothing
      , sheet =
            Animation.init <|
                [ Animation.Menu
                , Animation.Message
                ]
                    ++ List.map Animation.Submenu [0..List.length icons]
      }
    , Cmd.none
    )


main =
    Html.App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = (\_ -> AnimationFrame.times Animate)
        }
