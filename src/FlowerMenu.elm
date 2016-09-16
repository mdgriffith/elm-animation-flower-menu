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
import Animation exposing (turn, px)
import Animation.Messenger
import Color


type Msg
    = Toggle
    | ClickSubmenu Int
    | Animate Animation.Msg


type alias Model =
    { submenus : List Submenu
    , open : Bool
    , message : ( String, Animation.State )
    , menu : Animation.Messenger.State Msg
    }


type alias Submenu =
    { icon : String
    , style : Animation.State
    }


onSubmenuStyle : (Int -> Animation.State -> Animation.State) -> List Submenu -> List Submenu
onSubmenuStyle fn submenus =
    List.indexedMap
        (\i submenu ->
            { submenu
                | style =
                    fn i submenu.style
            }
        )
        submenus


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Toggle ->
            if model.open then
                -- Close the menu
                let
                    newMenu =
                        Animation.interrupt
                            [ Animation.to
                                [ Animation.rotate (turn -0.125)
                                ]
                            ]
                            model.menu

                    newSubmenus =
                        onSubmenuStyle
                            (\i style ->
                                Animation.interrupt
                                    [ Animation.wait (toFloat i * 5.0e-2 * second)
                                    , Animation.to [ Animation.translate (px 0) (px 0) ]
                                    ]
                                    style
                            )
                            model.submenus
                in
                    ( { model
                        | open = False
                        , submenus = newSubmenus
                        , menu = newMenu
                      }
                    , Cmd.none
                    )
            else
                -- open the menu
                let
                    newMenu =
                        Animation.interrupt
                            [ Animation.to
                                [ Animation.rotate (turn 0)
                                ]
                            ]
                            model.menu

                    newSubmenus =
                        onSubmenuStyle
                            (\i style ->
                                Animation.interrupt
                                    [ Animation.wait (toFloat i * 5.0e-2 * second)
                                    , Animation.to [ Animation.translate (px 0) (px 100) ]
                                    ]
                                    style
                            )
                            model.submenus
                in
                    ( { model
                        | open = True
                        , submenus = newSubmenus
                        , menu = newMenu
                      }
                    , Cmd.none
                    )

        ClickSubmenu i ->
            let
                msg =
                    Maybe.withDefault "whoops" <|
                        Maybe.map .icon <|
                            List.head <|
                                List.drop i model.submenus

                newMessageAnim =
                    Animation.interrupt
                        [ Animation.to
                            [ Animation.opacity 1
                            ]
                        , Animation.wait (1 * second)
                        , Animation.to
                            [ Animation.opacity 0
                            ]
                        ]
                        (snd model.message)
            in
                ( { model
                    | message =
                        ( msg, newMessageAnim )
                  }
                , Cmd.none
                )

        Animate time ->
            let
                ( newMenu, menuCmds ) =
                    Animation.Messenger.update time model.menu

                newSubmenus =
                    onSubmenuStyle
                        (\i style ->
                            Animation.update time style
                        )
                        model.submenus

                messageAnim =
                    Animation.update time (snd model.message)
            in
                ( { model
                    | menu = newMenu
                    , submenus = newSubmenus
                    , message = ( fst model.message, messageAnim )
                  }
                , menuCmds
                )


view : Model -> Html Msg
view model =
    let
        icon =
            i
                (Animation.render model.menu
                    ++ [ class "fa fa-close fa-3x"
                       ]
                )
                []

        message =
            div
                (Animation.render (snd model.message)
                    ++ [ class "message"
                       ]
                )
                [ text (fst model.message) ]
    in
        div
            [ class "main-button"
            , onClick Toggle
            ]
            (icon :: message :: List.indexedMap viewSubmenu model.submenus)


viewSubmenu : Int -> Submenu -> Html Msg
viewSubmenu index submenu =
    div
        (Animation.render submenu.style
            ++ [ class "child-button"
               , onClick (ClickSubmenu index)
               ]
        )
        [ i [ class ("fa  fa-lg fa-" ++ submenu.icon) ] []
        ]


icons : List String
icons =
    List.take 5 [ "pencil", "at", "camera", "bell", "comment", "bolt", "ban", "code" ]


makeSubmenu i icon =
    { icon = icon
    , style =
        Animation.styleWith (Animation.spring { stiffness = 400, damping = 28 }) <|
            let
                adjustment =
                    0.5 - (((toFloat (List.length icons) - 1) / 2.0) * fanAngle)

                angle =
                    (toFloat i * fanAngle) + adjustment
            in
                [ Animation.rotate (turn angle)
                , Animation.translate (px 0) (px 0)
                , Animation.rotate (turn (-1 * angle))
                , Animation.backgroundColor Color.lightGrey
                  -- Counter rotation so the icon is upright
                ]
    }


{-| In Turns
-}
fanAngle : Float
fanAngle =
    0.11


init =
    ( { open = False
      , menu =
            Animation.styleWith (Animation.spring { stiffness = 400, damping = 28 })
                [ Animation.rotate (turn -0.125)
                ]
      , submenus =
            List.indexedMap makeSubmenu icons
      , message =
            ( ""
            , Animation.style
                [ Animation.display Animation.block
                , Animation.opacity 0
                ]
            )
      }
    , Cmd.none
    )


main =
    Html.App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


{-| We have two subscriptions to our animations because we're using both Animation.State and Animation.Messenger.State, which can't both live in the same list.

-}
subscriptions model =
    Sub.batch
        [ Animation.subscription Animate
            (snd model.message :: List.map .style model.submenus)
        , Animation.subscription Animate
            [ model.menu ]
        ]
