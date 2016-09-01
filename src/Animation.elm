module Animation exposing (..)

import Style
import Style.Properties exposing (..)
import Style.Sheet
import Time exposing (second, Time)
import Msg exposing (..)


type Class
    = Submenu Int
    | Menu
    | Message


{-| In Turns
-}
fanAngle : Float
fanAngle =
    0.11


isSubmenu : Class -> Bool
isSubmenu id =
    case id of
        Submenu _ ->
            True

        _ ->
            False


init : List Class -> List ( Class, Style.Animation Msg )
init ids =
    Style.Sheet.initWith
        { spring =
            { stiffness = 170
            , damping = 26
            }
        }
        (initStyle ids)
        ids



--initStyle : List Class -> Class -> Style.Style


initStyle ids id =
    case id of
        Menu ->
            [ Rotate -0.125 Turn ]

        Message ->
            [ Display Block
            , Opacity 0
            ]

        Submenu i ->
            let
                submenus =
                    (List.length <| List.filter isSubmenu ids) - 1

                adjustment =
                    0.5 - (((toFloat submenus - 1) / 2.0) * fanAngle)

                angle =
                    (toFloat i * fanAngle) + adjustment
            in
                [ Rotate angle Turn
                , TranslateY 0 Px
                , Rotate (-1 * angle) Turn
                  -- Counter rotation so the icon is upright
                ]


open : Class -> Style.Animation Msg -> Style.Animation Msg
open id style =
    case id of
        Menu ->
            Style.interrupt [ rotate (turn 0) ] styleModel

        Submenu i ->
            Style.interrupt
                [ wait (toFloat i * 2.5e-2 * second)
                , translateY (px 100)
                ]
                styleModel

        _ ->
            style


close : Class -> Style.Animation Msg -> Style.Animation Msg
close id style =
    case id of
        Menu ->
            Style.animate
                |> Style.to
                    [ Rotate -0.125 Turn
                    ]
                |> Style.send (Print "finished closing")
                |> Style.on style

        Submenu i ->
            Style.animate
                |> Style.wait (toFloat i * 5.0e-2 * second)
                |> Style.to [ TranslateY 0 Px ]
                |> Style.on style

        _ ->
            style


showMsg : Class -> Style.Animation Msg -> Style.Animation Msg
showMsg id style =
    case id of
        Message ->
            Style.animate
                |> Style.to
                    [ Opacity 1
                    ]
                |> Style.to
                    [ Opacity 0
                    ]
                |> Style.set
                    [ Display None
                    ]
                |> Style.on style

        _ ->
            style
