module Main exposing (..)

import Reactive exposing (Query, program)
import Reactive.View exposing (..)


type alias Store =
    { todo : List String
    , otherThing : String
    }


q :
    { items : Query Store (List String)
    , intro : Query Store String
    }
q =
    { items = .todo >> List.reverse
    , intro = .otherThing
    }


view : View Store msg
view =
    with q.items <|
        \items ->
            List.map renderTodo items |> div []


renderTodo : String -> View Store msg
renderTodo item =
    with q.intro <|
        \intro ->
            div []
                [ text intro
                , text item
                ]


init : ( Store, Cmd msg )
init =
    { todo = [ "hello", "world" ]
    , otherThing = "hi"
    }
        ! []


main =
    program
        { init = init
        , update = \_ store -> store ! []
        , view = view
        }
