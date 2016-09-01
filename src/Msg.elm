module Msg exposing (..)

import Time exposing (Time)


type Msg
    = Toggle
    | ShowMessage String
    | Print String
    | Animate Time
