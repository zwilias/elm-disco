module Disco
    exposing
        ( Query
        , View
        , div
        , li
        , map
        , render
        , split
        , text
        , ul
        , with
        , wrap
        )

{-| Exploratory package for views where the model isn't explicitly passed around
and data extraction from the store is decoupled from the view logic.


# Building blocks

@docs View, Query, with, split, map


# DOM

@docs render, wrap, div, text, ul, li

-}

import Html exposing (Html)


{-| A query is just a function that extracts some data from a store.
-}
type alias Query store data =
    store -> data


{-| A view is a function that renders some data, which it can extract from a
store. That store is also made available to its children, again without having
explicit, direct access to this store.
-}
type View store msg
    = View (store -> Html msg)


{-| Allows building a view that actually _uses_ some of the data from the store,
using a `Query` to extract data to use.
-}
with : Query store data -> (data -> View store msg) -> View store msg
with extract toView =
    View <|
        \store ->
            toView (extract store) |> apply store


{-| Include a `View` that works on a different data-model, by using a query to
transform/extract the new data-model from the current data-model.
-}
split : Query store partial -> View partial msg -> View store msg
split extract (View render) =
    View <| extract >> render


{-| The equivalent of `Html.map` to map the `msg` part of the view.
-}
map : (subMsg -> msg) -> View partial subMsg -> View partial msg
map tagger (View render) =
    View <| render >> Html.map tagger


{-| Lift a regular old `Html` node into the world of `Disco`.
-}
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


{-| Given a view and a store, render it to plain old `Html`. This allows
embedding disco views in regular views, and is also useful for passing your view
to `Html.program` and friends.
-}
render : View store msg -> store -> Html msg
render =
    flip apply


{-| The disco equivalent of `Html.div`.
-}
div : List (Html.Attribute msg) -> List (View store msg) -> View store msg
div =
    wrap Html.div


{-| The disco equivalent of `Html.text`.
-}
text : String -> View store msg
text content =
    View <| \_ -> Html.text content


{-| The disco equivalent of `Html.ul`.
-}
ul : List (Html.Attribute msg) -> List (View store msg) -> View store msg
ul =
    wrap Html.ul


{-| The disco equivalent of `Html.li`.
-}
li : List (Html.Attribute msg) -> List (View store msg) -> View store msg
li =
    wrap Html.li
