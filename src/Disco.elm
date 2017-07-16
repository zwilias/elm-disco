module Disco
    exposing
        ( Query
        , View
        , a
        , address
        , article
        , aside
        , b
        , blockquote
        , br
        , button
        , canvas
        , caption
        , code
        , col
        , colgroup
        , dd
        , div
        , dl
        , dt
        , em
        , fieldset
        , figcaption
        , figure
        , footer
        , form
        , h1
        , h2
        , h3
        , h4
        , h5
        , h6
        , header
        , hr
        , i
        , iframe
        , img
        , input
        , label
        , legend
        , li
        , main_
        , map
        , math
        , nav
        , ol
        , option
        , p
        , pre
        , progress
        , render
        , section
        , select
        , span
        , split
        , strong
        , sub
        , sup
        , table
        , tbody
        , td
        , textarea
        , tfoot
        , th
        , thead
        , tr
        , u
        , ul
        , with
        , with2
        , with3
        , with4
        , with5
        , with6
        , wrap
        )

{-| Exploratory package for views where the model isn't explicitly passed around
and data extraction from the store is decoupled from the view logic.


# Building blocks

@docs View, Query, with, split, map


# More args

@docs with2, with3, with4, with5, with6


# DOM

@docs render, wrap


## Tags


### Headers

@docs h1, h2, h3, h4, h5, h6


### Grouping content

@docs div, p, hr, pre, blockquote


### Text

@docs span, a, code, em, strong, i, b, u, sub, sup, br


### Lists

@docs ol, ul, li, dl, dt, dd


### Embedded content

@docs img, iframe, canvas, math


### Forms

@docs form, input, textarea, button, select, option, label, fieldset, legend


### Sections

@docs section, nav, article, aside, header, footer, address, main_


### Figures

@docs figure, figcaption


### Tables

@docs table, caption, colgroup, col, tbody, thead, tfoot, tr, td, th


### Weird stuff

@docs progress

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


{-| View with 2 pieces of data.
-}
with2 :
    Query store data1
    -> Query store data2
    -> (data1 -> data2 -> View store msg)
    -> View store msg
with2 extract1 extract2 toView =
    View <|
        \store ->
            toView (extract1 store) (extract2 store) |> apply store


{-| View with 3 pieces of data.
-}
with3 :
    Query store data1
    -> Query store data2
    -> Query store data3
    -> (data1 -> data2 -> data3 -> View store msg)
    -> View store msg
with3 extract1 extract2 extract3 toView =
    View <|
        \store ->
            toView (extract1 store) (extract2 store) (extract3 store) |> apply store


{-| View with 4 pieces of data.
-}
with4 :
    Query store data1
    -> Query store data2
    -> Query store data3
    -> Query store data4
    -> (data1 -> data2 -> data3 -> data4 -> View store msg)
    -> View store msg
with4 extract1 extract2 extract3 extract4 toView =
    View <|
        \store ->
            toView
                (extract1 store)
                (extract2 store)
                (extract3 store)
                (extract4 store)
                |> apply store


{-| View with 5 pieces of data.
-}
with5 :
    Query store data1
    -> Query store data2
    -> Query store data3
    -> Query store data4
    -> Query store data5
    -> (data1 -> data2 -> data3 -> data4 -> data5 -> View store msg)
    -> View store msg
with5 extract1 extract2 extract3 extract4 extract5 toView =
    View <|
        \store ->
            toView
                (extract1 store)
                (extract2 store)
                (extract3 store)
                (extract4 store)
                (extract5 store)
                |> apply store


{-| View with 6 pieces of data.
-}
with6 :
    Query store data1
    -> Query store data2
    -> Query store data3
    -> Query store data4
    -> Query store data5
    -> Query store data6
    -> (data1 -> data2 -> data3 -> data4 -> data5 -> data6 -> View store msg)
    -> View store msg
with6 extract1 extract2 extract3 extract4 extract5 extract6 toView =
    View <|
        \store ->
            toView
                (extract1 store)
                (extract2 store)
                (extract3 store)
                (extract4 store)
                (extract5 store)
                (extract6 store)
                |> apply store


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
    View <| apply >> flip List.map children >> htmlTag attrs


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



-- DOM elements


{-| -}
text : String -> View store msg
text input =
    View <| always <| Html.text input


{-| -}
h1 : List (Html.Attribute msg) -> List (View store msg) -> View store msg
h1 =
    wrap Html.h1


{-| -}
h2 : List (Html.Attribute msg) -> List (View store msg) -> View store msg
h2 =
    wrap Html.h2


{-| -}
h3 : List (Html.Attribute msg) -> List (View store msg) -> View store msg
h3 =
    wrap Html.h3


{-| -}
h4 : List (Html.Attribute msg) -> List (View store msg) -> View store msg
h4 =
    wrap Html.h4


{-| -}
h5 : List (Html.Attribute msg) -> List (View store msg) -> View store msg
h5 =
    wrap Html.h5


{-| -}
h6 : List (Html.Attribute msg) -> List (View store msg) -> View store msg
h6 =
    wrap Html.h6


{-| -}
div : List (Html.Attribute msg) -> List (View store msg) -> View store msg
div =
    wrap Html.div


{-| -}
p : List (Html.Attribute msg) -> List (View store msg) -> View store msg
p =
    wrap Html.p


{-| -}
hr : List (Html.Attribute msg) -> View store msg
hr attrs =
    wrap Html.hr attrs []


{-| -}
pre : List (Html.Attribute msg) -> List (View store msg) -> View store msg
pre =
    wrap Html.pre


{-| -}
blockquote : List (Html.Attribute msg) -> List (View store msg) -> View store msg
blockquote =
    wrap Html.blockquote


{-| -}
span : List (Html.Attribute msg) -> List (View store msg) -> View store msg
span =
    wrap Html.span


{-| -}
a : List (Html.Attribute msg) -> List (View store msg) -> View store msg
a =
    wrap Html.a


{-| -}
code : List (Html.Attribute msg) -> List (View store msg) -> View store msg
code =
    wrap Html.code


{-| -}
em : List (Html.Attribute msg) -> List (View store msg) -> View store msg
em =
    wrap Html.em


{-| -}
strong : List (Html.Attribute msg) -> List (View store msg) -> View store msg
strong =
    wrap Html.strong


{-| -}
i : List (Html.Attribute msg) -> List (View store msg) -> View store msg
i =
    wrap Html.i


{-| -}
b : List (Html.Attribute msg) -> List (View store msg) -> View store msg
b =
    wrap Html.b


{-| -}
u : List (Html.Attribute msg) -> List (View store msg) -> View store msg
u =
    wrap Html.u


{-| -}
sub : List (Html.Attribute msg) -> List (View store msg) -> View store msg
sub =
    wrap Html.sub


{-| -}
sup : List (Html.Attribute msg) -> List (View store msg) -> View store msg
sup =
    wrap Html.sup


{-| -}
br : List (Html.Attribute msg) -> View store msg
br attrs =
    wrap Html.br attrs []


{-| -}
ol : List (Html.Attribute msg) -> List (View store msg) -> View store msg
ol =
    wrap Html.ol


{-| -}
ul : List (Html.Attribute msg) -> List (View store msg) -> View store msg
ul =
    wrap Html.ul


{-| -}
li : List (Html.Attribute msg) -> List (View store msg) -> View store msg
li =
    wrap Html.li


{-| -}
dl : List (Html.Attribute msg) -> List (View store msg) -> View store msg
dl =
    wrap Html.dl


{-| -}
dt : List (Html.Attribute msg) -> List (View store msg) -> View store msg
dt =
    wrap Html.dt


{-| -}
dd : List (Html.Attribute msg) -> List (View store msg) -> View store msg
dd =
    wrap Html.dd


{-| -}
img : List (Html.Attribute msg) -> View store msg
img attrs =
    wrap Html.img attrs []


{-| -}
iframe : List (Html.Attribute msg) -> List (View store msg) -> View store msg
iframe =
    wrap Html.iframe


{-| -}
canvas : List (Html.Attribute msg) -> List (View store msg) -> View store msg
canvas =
    wrap Html.canvas


{-| -}
math : List (Html.Attribute msg) -> List (View store msg) -> View store msg
math =
    wrap Html.math


{-| -}
form : List (Html.Attribute msg) -> List (View store msg) -> View store msg
form =
    wrap Html.form


{-| -}
input : List (Html.Attribute msg) -> List (View store msg) -> View store msg
input =
    wrap Html.input


{-| -}
textarea : List (Html.Attribute msg) -> List (View store msg) -> View store msg
textarea =
    wrap Html.textarea


{-| -}
button : List (Html.Attribute msg) -> List (View store msg) -> View store msg
button =
    wrap Html.button


{-| -}
select : List (Html.Attribute msg) -> List (View store msg) -> View store msg
select =
    wrap Html.select


{-| -}
option : List (Html.Attribute msg) -> List (View store msg) -> View store msg
option =
    wrap Html.option


{-| -}
label : List (Html.Attribute msg) -> List (View store msg) -> View store msg
label =
    wrap Html.label


{-| -}
fieldset : List (Html.Attribute msg) -> List (View store msg) -> View store msg
fieldset =
    wrap Html.fieldset


{-| -}
legend : List (Html.Attribute msg) -> List (View store msg) -> View store msg
legend =
    wrap Html.legend


{-| -}
section : List (Html.Attribute msg) -> List (View store msg) -> View store msg
section =
    wrap Html.section


{-| -}
nav : List (Html.Attribute msg) -> List (View store msg) -> View store msg
nav =
    wrap Html.nav


{-| -}
article : List (Html.Attribute msg) -> List (View store msg) -> View store msg
article =
    wrap Html.article


{-| -}
aside : List (Html.Attribute msg) -> List (View store msg) -> View store msg
aside =
    wrap Html.aside


{-| -}
header : List (Html.Attribute msg) -> List (View store msg) -> View store msg
header =
    wrap Html.header


{-| -}
footer : List (Html.Attribute msg) -> List (View store msg) -> View store msg
footer =
    wrap Html.footer


{-| -}
address : List (Html.Attribute msg) -> List (View store msg) -> View store msg
address =
    wrap Html.address


{-| -}
main_ : List (Html.Attribute msg) -> List (View store msg) -> View store msg
main_ =
    wrap Html.main_


{-| -}
figure : List (Html.Attribute msg) -> List (View store msg) -> View store msg
figure =
    wrap Html.figure


{-| -}
figcaption : List (Html.Attribute msg) -> List (View store msg) -> View store msg
figcaption =
    wrap Html.figcaption


{-| -}
table : List (Html.Attribute msg) -> List (View store msg) -> View store msg
table =
    wrap Html.table


{-| -}
caption : List (Html.Attribute msg) -> List (View store msg) -> View store msg
caption =
    wrap Html.caption


{-| -}
colgroup : List (Html.Attribute msg) -> List (View store msg) -> View store msg
colgroup =
    wrap Html.colgroup


{-| -}
col : List (Html.Attribute msg) -> List (View store msg) -> View store msg
col =
    wrap Html.col


{-| -}
tbody : List (Html.Attribute msg) -> List (View store msg) -> View store msg
tbody =
    wrap Html.tbody


{-| -}
thead : List (Html.Attribute msg) -> List (View store msg) -> View store msg
thead =
    wrap Html.thead


{-| -}
tfoot : List (Html.Attribute msg) -> List (View store msg) -> View store msg
tfoot =
    wrap Html.tfoot


{-| -}
tr : List (Html.Attribute msg) -> List (View store msg) -> View store msg
tr =
    wrap Html.tr


{-| -}
td : List (Html.Attribute msg) -> List (View store msg) -> View store msg
td =
    wrap Html.td


{-| -}
th : List (Html.Attribute msg) -> List (View store msg) -> View store msg
th =
    wrap Html.th


{-| -}
progress : List (Html.Attribute msg) -> List (View store msg) -> View store msg
progress =
    wrap Html.progress
