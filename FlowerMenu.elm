module Main (..) where

{-| Recreating the menu found here: https://github.com/nashvail/ReactPathMenu
Make using:
   elm-make FlowerMenu/FlowerMenu.elm --output FlowerMenu/elm.js
   open FlowerMenu/index.html


-}

import StartApp
import Task
import Signal exposing (Signal, Address)
import Effects exposing (Effects, Never)
import Time exposing (second)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Animation as UI
import Html.Animation.Properties exposing (..)


type alias Model =
  { submenus : List Submenu
  , style : UI.Animation
  , open : Bool
  , message : Maybe Message
  }


type alias Submenu =
  { style : UI.Animation
  , icon : String
  }


type alias Message =
  { label : String
  , style : UI.Animation
  }


type Action
  = Toggle
  | ShowMessage String
  | Animate AnimationTarget


type AnimationTarget
  = OnMenu UI.Action
  | OnSubmenu Int UI.Action
  | OnMessage UI.Action


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    Toggle ->
      if model.open then
        let
          ( submenus, submenuFx ) =
            UI.stagger
              (\total i ->
                UI.animate
                  |> UI.delay (i * 5.0e-2 * second)
                  |> UI.spring UI.fastAndLoose
                  |> UI.props
                      [ TranslateY (UI.to 0) Px
                      ]
              )
              |> onAllSubmenus model.submenus

          ( newMenu, menuFx ) =
            UI.animate
              |> UI.props
                  [ Rotate (UI.to -0.125) Turn
                  ]
              |> onMenu model
        in
          ( { newMenu
              | submenus = submenus
              , open = False
            }
          , Effects.batch
              [ submenuFx
              , menuFx
              ]
          )
      else
        let
          ( submenus, submenuFx ) =
            UI.stagger
              (\total i ->
                UI.animate
                  |> UI.delay (i * 2.5e-2 * second)
                  |> UI.spring UI.fastAndLoose
                  |> UI.props
                      [ TranslateY (UI.to 100) Px
                      ]
              )
              |> onAllSubmenus model.submenus

          ( newMenu, menuFx ) =
            UI.animate
              |> UI.props
                  [ Rotate (UI.to 0) Turn
                  ]
              |> onMenu model
        in
          ( { newMenu
              | submenus = submenus
              , open = True
            }
          , Effects.batch
              [ submenuFx
              , menuFx
              ]
          )

    ShowMessage str ->
      let
        message =
          { label = str
          , style =
              UI.init
                [ Display Block
                , Opacity 0
                ]
          }

        ( msgStyle, fx ) =
          UI.animate
            |> UI.props
                [ Opacity (UI.to 1)
                ]
            |> UI.andThen
            |> UI.props
                [ Opacity (UI.to 0)
                ]
            |> UI.andThen
            |> UI.set
                [ Display None
                ]
            |> UI.on message.style

        msg =
          { message | style = msgStyle }
      in
        ( { model
            | message = Just msg
          }
        , Effects.map (\a -> Animate (OnMessage a)) fx
        )

    Animate target ->
      case target of
        OnMenu action ->
          onMenu model action

        OnSubmenu i action ->
          let
            ( submenus, fx ) =
              onSubmenu i model.submenus action
          in
            ( { model | submenus = submenus }
            , fx
            )

        OnMessage action ->
          case model.message of
            Nothing ->
              ( model, Effects.none )

            Just message ->
              let
                ( msgStyle, fx ) =
                  UI.update action message.style

                newMsg =
                  { message | style = msgStyle }
              in
                ( { model | message = Just newMsg }
                , Effects.map (\a -> Animate (OnMessage a)) fx
                )


view : Address Action -> Model -> Html
view address model =
  let
    icon =
      i [ class "fa fa-close fa-3x"
        , style (UI.render model.style)
        ] []

    message =
      Maybe.withDefault (div [] [])
        <| Maybe.map viewMessage model.message

    submenus =
      List.map (viewSubmenu address) model.submenus
  in
    div
      [ class "main-button"
      , onClick address Toggle
      ]
      ( icon :: message :: submenus )


viewSubmenu : Address Action -> Submenu -> Html
viewSubmenu address submenu =
  div
    [ class "child-button"
    , style (UI.render submenu.style)
    , onClick address (ShowMessage submenu.icon)
    ]
    [ i [ class ("fa  fa-lg fa-" ++ submenu.icon) ] []
    ]


viewMessage : Message -> Html
viewMessage msg =
  div
    [ class "message"
    , style (UI.render msg.style)
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
        UI.init
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
  { open = False
  , submenus =
      List.indexedMap
        (\i icon -> createSubmenu icon (List.length icons) i)
        icons
  , style =
      UI.init
        [ Rotate -0.125 Turn ]
  , message = Nothing
  }


app =
  StartApp.start
    { init = ( init, Effects.none )
    , update = update
    , view = view
    , inputs = []
    }


main =
  app.html


port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks


onMenu =
  UI.forwardTo
    (\a -> Animate (OnMenu a))
    .style
    (\w style -> { w | style = style })


onAllSubmenus =
  UI.forwardToAll
    (\i a -> Animate <| OnSubmenu i a)
    .style
    -- widget style getter
    (\w style -> { w | style = style })

onSubmenu =
  UI.forwardToIndex
    (\i a -> Animate <| OnSubmenu i a)
    .style
    -- widget style getter
    (\w style -> { w | style = style })

