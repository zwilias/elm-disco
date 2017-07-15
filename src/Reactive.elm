module Reactive exposing (Query, program)

import Html
import Reactive.View as View exposing (View)


type alias Query store result =
    store -> result


program :
    { init : ( store, Cmd msg )
    , update : msg -> store -> ( store, Cmd msg )
    , view : View store msg
    }
    -> Program Never store msg
program { init, update, view } =
    Html.program
        { init = init
        , update = update
        , view = flip View.apply view
        , subscriptions = always Sub.none
        }
