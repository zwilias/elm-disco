module Main exposing (..)

import Browser
import Disco exposing (..)
import Html exposing (Html)


type alias Store =
    { todo : List String
    , otherThing : String
    }


type alias SubStore =
    { first : String
    , second : String
    }


q :
    { items : Query Store (List String)
    , intro : Query Store String
    , foo : Query Store SubStore
    , hardcoded : Query Store String
    }
q =
    { items = .todo >> List.reverse
    , intro = .otherThing
    , foo =
        \store ->
            { first =
                store.todo
                    |> List.head
                    |> Maybe.withDefault "bar"
            , second = "nope"
            }
    , hardcoded = always "there"
    }


view : View Store msg
view =
    with q.items <|
        \items ->
            div []
                [ items
                    |> List.map (renderTodo >> List.singleton >> li [])
                    |> ul []
                , split q.foo subView
                , usingMultiple
                ]


renderTodo : String -> View Store msg
renderTodo item =
    with q.intro <|
        \intro ->
            div []
                [ p [] [ text intro ]
                , p [] [ text item ]
                ]


subView : View SubStore msg
subView =
    with identity <|
        \{ first, second } ->
            div []
                [ p [] [ text <| "first: " ++ first ]
                , p [] [ text <| "second: " ++ second ]
                ]


usingMultiple : View Store msg
usingMultiple =
    with2 q.intro q.hardcoded <|
        \foo bar ->
            text (foo ++ " - " ++ bar)


initialStore : Store
initialStore =
    { todo = [ "hello", "world" ]
    , otherThing = "hi"
    }



-- Main browser program


main : Program () () ()
main =
    Browser.element
        { init = \_ -> ( (), Cmd.none )
        , update = \_ _ -> ( (), Cmd.none )
        , subscriptions = \_ -> Sub.none
        , view = \_ -> render view initialStore
        }
