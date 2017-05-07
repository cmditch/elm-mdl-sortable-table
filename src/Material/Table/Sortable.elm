module Material.Table.Sortable
    exposing
        ( view
        , config
        , customConfig
        , stringColumn
        , intColumn
        , floatColumn
        , customColumn
        , defaultCustomizations
        , State
        , Customizations
        , initialSort
        , Config
        , Sorter(..)
        , Header
        , Row
        )

{--|
blah blah

|-}

import Html exposing (Html, Attribute, text)
import Html.Keyed
import Material.Table as Table exposing (Order(..))
import Material.Options as Options exposing (css)


{--|
blah blah
-}


type alias State =
    { sortColumn : String
    , sortOrder : Table.Order
    }


type Config data msg
    = Config
        { toId : data -> String
        , toMsg : State -> msg
        , columns : List (ColumnData data msg)
        , customizations : Customizations data msg
        }


type alias ColumnData data msg =
    { name : String
    , viewData : data -> Html msg
    , sorter : Sorter data
    }



{--|

Custom styling for <table>, <tbody>, <th>, <tr>, and optional caption/footer.
Custom styling uses elm-mdl functions, e.g. Options.cs
The exception for this is tbodyAttrs, which must use Html.Attributes because it's wrapped
in Html.Keyed

--}


type alias Customizations data msg =
    { tableAttrs : List (Options.Property {} msg)
    , tbodyAttrs : List (Attribute msg)
    , rowAttrs : data -> List (Options.Property Row msg)
    , thead : List ( String, Maybe Table.Order, List (Options.Property Header msg) ) -> Html msg
    , tfoot : Maybe (Html msg)
    }


type alias Header =
    { numeric : Bool
    , sorted : Maybe Table.Order
    }


type alias Row =
    { selected : Bool }


defaultCustomizations : Customizations data msg
defaultCustomizations =
    { tableAttrs = []
    , tbodyAttrs = []
    , rowAttrs = defaultRowAttrs
    , thead = defaultHead
    , tfoot = Nothing
    }


config :
    { toId : data -> String
    , toMsg : State -> msg
    , columns : List (Column data msg)
    }
    -> Config data msg
config { toId, toMsg, columns } =
    Config
        { toId = toId
        , toMsg = toMsg
        , columns = List.map (\(Column cData) -> cData) columns
        , customizations = defaultCustomizations
        }


customConfig :
    { toId : data -> String
    , toMsg : State -> msg
    , columns : List (Column data msg)
    , customizations : Customizations data msg
    }
    -> Config data msg
customConfig { toId, toMsg, columns, customizations } =
    Config
        { toId = toId
        , toMsg = toMsg
        , columns = List.map (\(Column cData) -> cData) columns
        , customizations = customizations
        }


defaultRowAttrs : data -> List (Options.Property Row msg)
defaultRowAttrs _ =
    []



{--|
Default css for <th> is {cursor: pointer, user-select: none}
--}


defaultHead : List ( String, Maybe Table.Order, List (Options.Property Header msg) ) -> Html msg
defaultHead headers =
    let
        noSelect =
            [ css "user-select" "none" ]

        newHeaders =
            headers
                |> List.map (\( name, order, style ) -> ( name, order, style ++ noSelect ))
    in
        Table.thead [] (List.map tHeadHelper newHeaders)


tHeadHelper : ( String, Maybe Table.Order, List (Options.Property Header msg) ) -> Html msg
tHeadHelper header =
    let
        ( name, order, attrs ) =
            header

        orderStyle =
            case order of
                Nothing ->
                    []

                Just ascOrDesc ->
                    [ Table.sorted ascOrDesc ]
    in
        Table.th (orderStyle ++ attrs) [ text name ]


tHeadInfo : State -> (State -> msg) -> ColumnData data msg -> ( String, Maybe Table.Order, List (Options.Property Header msg) )
tHeadInfo { sortColumn, sortOrder } toMsg { name, viewData, sorter } =
    let
        order =
            if name == sortColumn then
                Just sortOrder
            else
                Nothing

        onClick =
            case sorter of
                None ->
                    []

                _ ->
                    if name == sortColumn then
                        [ Options.onClick (toMsg <| State name (rotate sortOrder)) ]
                    else
                        [ Options.onClick (toMsg <| State name Descending) ]

        pointer =
            case sorter of
                None ->
                    []

                _ ->
                    [ css "cursor" "pointer" ]
    in
        ( name, order, onClick ++ pointer )


initialSort : String -> Table.Order -> State
initialSort header order =
    State header order


sort : State -> List (ColumnData data msg) -> List data -> List data
sort { sortColumn, sortOrder } columns data =
    let
        column =
            columns
                |> List.filter (\column -> column.name == sortColumn)
                |> List.head
    in
        case column of
            Nothing ->
                data

            Just column ->
                applySorter sortOrder column data


applySorter : Table.Order -> ColumnData data msg -> List data -> List data
applySorter order { sorter } data =
    case sorter of
        None ->
            data

        BasicSort sorter ->
            if order == Descending then
                sorter data
            else
                sorter data |> List.reverse

        CustomSort sorter ->
            sorter order data


view : Config data msg -> State -> List data -> Html msg
view (Config { toId, toMsg, columns, customizations }) state data =
    let
        sortedData =
            sort state columns data

        thead =
            customizations.thead (List.map (tHeadInfo state toMsg) columns)

        rows =
            List.map (viewRow customizations.rowAttrs toId columns) sortedData
    in
        Table.table ([] ++ customizations.tableAttrs)
            [ thead
            , Html.Keyed.node "tbody" ([] ++ customizations.tbodyAttrs) rows
            , Maybe.withDefault (text "") customizations.tfoot
            ]


viewRow :
    (data -> List (Options.Property Row msg))
    -> (data -> String)
    -> List (ColumnData data msg)
    -> data
    -> ( String, Html msg )
viewRow toAttr toId columns data =
    ( toId data
    , Table.tr (toAttr data) (List.map (\column -> viewCell column data) columns)
    )


viewCell : ColumnData data msg -> data -> Html msg
viewCell { viewData } data =
    Table.td [] [ viewData data ]


type Column data msg
    = Column (ColumnData data msg)


customColumn : String -> (data -> Html msg) -> Sorter data -> Column data msg
customColumn name dataHtml sorter =
    Column
        { name = name
        , viewData = dataHtml
        , sorter = sorter
        }


stringColumn : String -> (data -> String) -> Column data msg
stringColumn name recordField =
    Column
        { name = name
        , viewData = text << recordField
        , sorter = BasicSort (List.sortBy recordField)
        }


intColumn : String -> (data -> Int) -> Column data msg
intColumn name recordField =
    Column
        { name = name
        , viewData = text << toString << recordField
        , sorter = BasicSort (List.sortBy recordField)
        }


floatColumn : String -> (data -> Float) -> Column data msg
floatColumn name recordField =
    Column
        { name = name
        , viewData = text << toString << recordField
        , sorter = BasicSort (List.sortBy recordField)
        }


rotate : Table.Order -> Table.Order
rotate ord =
    case ord of
        Ascending ->
            Descending

        Descending ->
            Ascending


type Sorter data
    = None
    | BasicSort (List data -> List data)
    | CustomSort (Table.Order -> List data -> List data)
