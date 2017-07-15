module Reactive.View exposing (..)

import Html exposing (Html)


type View store msg
    = View (store -> Html msg)


with : (store -> a) -> (a -> View store msg) -> View store msg
with extract toView =
    View <|
        \store ->
            toView (extract store) |> apply store


div : List (Html.Attribute msg) -> List (View store msg) -> View store msg
div =
    wrap Html.div


text : String -> View store msg
text content =
    View <| \_ -> Html.text content


wrap :
    (List (Html.Attribute msg)
     -> List (Html msg)
     -> Html msg
    )
    -> List (Html.Attribute msg)
    -> List (View store msg)
    -> View store msg
wrap htmlTag attrs children =
    View <|
        \store ->
            htmlTag attrs (List.map (apply store) children)


apply : store -> View store msg -> Html msg
apply store (View view) =
    view store
