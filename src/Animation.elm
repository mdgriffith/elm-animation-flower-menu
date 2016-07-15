module Animation exposing (..)

import Style
import Style.Properties exposing (..)
import Style.Sheet
import Time exposing (second, Time)


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



--


init : List Class -> List ( Class, Style.Animation )
init ids =
    Style.Sheet.init (initStyle ids) ids



--initStyle : List Class -> Class -> Style.Animation


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


open : Class -> Style.Animation -> Style.Animation
open id style =
    case id of
        Menu ->
            Style.animate
                |> Style.spring
                    { stiffness = 500
                    , damping = 30
                    }
                |> Style.to
                    [ Rotate 0 Turn
                    ]
                |> Style.on style

        Submenu i ->
            Style.animate
                |> Style.delay (toFloat i * 2.5e-2 * second)
                |> Style.spring
                    { stiffness = 400
                    , damping = 28
                    }
                |> Style.to
                    [ TranslateY 100 Px
                    ]
                |> Style.on style

        _ ->
            style


close : Class -> Style.Animation -> Style.Animation
close id style =
    case id of
        Menu ->
            Style.animate
                |> Style.spring
                    { stiffness = 500
                    , damping = 30
                    }
                |> Style.to
                    [ Rotate -0.125 Turn
                    ]
                |> Style.on style

        Submenu i ->
            Style.animate
                |> Style.delay (toFloat i * 5.0e-2 * second)
                |> Style.spring
                    { stiffness = 400
                    , damping = 28
                    }
                |> Style.to [ TranslateY 0 Px ]
                |> Style.on style

        _ ->
            style


showMsg : Class -> Style.Animation -> Style.Animation
showMsg id style =
    case id of
        Message ->
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
                |> Style.on style

        _ ->
            style
