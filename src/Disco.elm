module Disco exposing (..)

import Html exposing (Html)


type alias Query store result =
    store -> result


type View store msg
    = View (store -> Html msg)


with : (store -> a) -> (a -> View store msg) -> View store msg
with extract toView =
    View <|
        \store ->
            toView (extract store) |> apply store


split : (store -> partial) -> View partial msg -> View store msg
split extract (View render) =
    View <| extract >> render


map : (subMsg -> msg) -> View partial subMsg -> View partial msg
map tagger (View render) =
    View <| render >> Html.map tagger


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


render : View store msg -> store -> Html msg
render =
    flip apply


div : List (Html.Attribute msg) -> List (View store msg) -> View store msg
div =
    wrap Html.div


text : String -> View store msg
text content =
    View <| \_ -> Html.text content


ul : List (Html.Attribute msg) -> List (View store msg) -> View store msg
ul =
    wrap Html.ul


li : List (Html.Attribute msg) -> List (View store msg) -> View store msg
li =
    wrap Html.li
