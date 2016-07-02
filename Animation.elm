module Animation exposing (..)

import Style
import Style.Properties exposing (..)
import Time exposing (second, Time)


(=>) =
    (,)


type Id
    = Submenu Int
    | Menu
    | Message


type AnimationMsg
    = Open
    | Close
    | ShowMessage


type alias StyleSheet =
    List ( Id, Style.Animation )


{-| In Turns
-}
fanAngle : Float
fanAngle =
    0.11


sheet : Int -> List ( Id, Style.Animation )
sheet submenus =
    --List.map (\( id, x ) -> ( id, Style.init x ))
    [ Menu => Style.init [ Rotate -0.125 Turn ]
    , Message
        => Style.init
            [ Display Block
            , Opacity 0
            ]
    ]
        ++ (List.map
                (\i ->
                    Submenu i
                        => let
                            adjustment =
                                0.5 - (((toFloat submenus - 1) / 2.0) * fanAngle)

                            angle =
                                (toFloat i * fanAngle) + adjustment
                           in
                            Style.init
                                [ Rotate angle Turn
                                , TranslateY 0 Px
                                , Rotate (-1 * angle) Turn
                                  -- Counter rotation so the icon is upright
                                ]
                )
                [0..submenus]
           )


update anim sheet =
    List.map
        (\( id, x ) ->
            ( id, styleSheetUpdateBy anim id x )
        )
        sheet


styleSheetUpdateBy : AnimationMsg -> Id -> Style.Animation -> Style.Animation
styleSheetUpdateBy anim id style =
    case anim of
        Open ->
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

        Close ->
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

        ShowMessage ->
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


tick time sheet =
    List.map (\( i, x ) -> ( i, Style.tick time x )) sheet


on sheet id anim =
    List.map
        (\( id2, x ) ->
            if id == id2 then
                ( id2, anim |> Style.on x )
            else
                ( id2, x )
        )
        sheet


styleSheetUpdate fn sheet =
    List.map
        (\( id, x ) ->
            ( id, fn id x )
        )
        sheet


render sheet id =
    let
        matching =
            List.filter (\x -> fst x == id) sheet
    in
        case List.head matching of
            Nothing ->
                []

            Just style ->
                Style.render <| snd style
