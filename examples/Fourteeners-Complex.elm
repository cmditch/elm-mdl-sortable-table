module Presidents exposing (..)

import Html exposing (Html, div, h1, input, text)
import Dict exposing (Dict)
import Material
import Material.Scheme
import Material.Layout as Layout
import Material.Color as Color
import Material.Options as Options exposing (css)
import Material.Toggles as Toggles
import Material.Textfield as Textfield
import Material.Table as Table
import Material.Table.Sortable as Sortable exposing (Sorter(..), Header, defaultCustomizations)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { mountains : List Mountain
    , tableState : Sortable.State
    , mdl : Material.Model
    }


init : ( Model, Cmd Msg )
init =
    { mountains = fourteeners
    , tableState = Sortable.initialSort "Range" Table.Descending
    , mdl = Material.model
    }
        ! []



-- VIEW


view : Model -> Html Msg
view model =
    Material.Scheme.topWithScheme Color.Blue Color.Orange <|
        Layout.render Mdl
            model.mdl
            [ Layout.fixedHeader ]
            { header = [ header model ]
            , drawer = []
            , tabs =
                ( [], [] )
            , main = [ mountainTable model ]
            }


header : Model -> Html Msg
header model =
    Layout.row []
        [ Layout.title [] [ text "elm-mdl | Sortable Table " ]
        , Layout.spacer
        , Layout.title [] [ text <| (toString <| climbedCount model.mountains) ++ "  Mountains Climbed" ]
        ]


mountainTable : Model -> Html Msg
mountainTable model =
    Sortable.view (config model) model.tableState model.mountains


config : Model -> Sortable.Config Mountain Msg
config model =
    Sortable.customConfig
        { toId = toString << .id
        , toMsg = SetTableState
        , columns =
            [ Sortable.stringColumn "Name" .name
            , Sortable.customColumn "Height" heightField (BasicSort <| List.sortBy .height)
            , Sortable.customColumn "Range" (.range >> text) (CustomSort rangeSort)
            , Sortable.customColumn "One Word Description" (commentField model) None
            , Sortable.customColumn "Climbed" (climbedToggle model) None
            ]
        , customizations = myCustomizations
        }



{--
Customizations Availaable

type alias Customizations data msg =
    { tableAttrs : List (Options.Property {} msg)
    , tbodyAttrs : List (Attribute msg)
    , rowAttrs : data -> List (Options.Property Row msg)
    , thead : List ( String, Maybe Order, List (Options.Property Header msg) ) -> Html msg
    , tfoot : Maybe (Html msg)
    }

--}


myCustomizations : Sortable.Customizations Mountain Msg
myCustomizations =
    let
        rowColor =
            css "background-color" "aliceblue"

        rowAttrs =
            \mnt ->
                if mnt.climbed then
                    [ rowColor ]
                else
                    []

        tableAttrs =
            [ css "width" "70%", css "margin" "0 auto" ]

        customHeads =
            \headers -> Table.thead [] (List.map thHelper headers)
    in
        { defaultCustomizations
            | tableAttrs = tableAttrs
            , rowAttrs = rowAttrs
            , thead = customHeads
            , tfoot = Just tfoot
        }



{--
  Must declare 'Header' type to satisfy elm-mdl
--}


thHelper : ( String, Maybe Table.Order, List (Options.Property Header Msg) ) -> Html Msg
thHelper header =
    let
        ( name, maybeOrder, attrs ) =
            header

        orderStyle =
            case maybeOrder of
                Nothing ->
                    [ css "user-select" "none" ]

                Just order ->
                    [ Table.sorted order
                    , css "cursor" "pointer"
                    , css "user-select" "none"
                    ]
    in
        Table.th (orderStyle ++ attrs) [ text name ]


tfoot : Html Msg
tfoot =
    let
        center =
            css "text-align" "center"
    in
        Table.tfoot []
            [ Table.td [ center ] [ text "elm is fun" ]
            , Table.td [ center ] [ text "elm-mdl is pretty" ]
            , Table.td [ center ] [ text "who uses footers anyways?" ]
            ]


climbedToggle : Model -> Mountain -> Html Msg
climbedToggle model mountain =
    Toggles.switch Mdl
        [ mountain.id ]
        model.mdl
        [ Options.onToggle (ToggleMountain mountain.id)
        , Toggles.ripple
        , Toggles.value mountain.climbed
        ]
        []


commentField : Model -> Mountain -> Html Msg
commentField model mountain =
    Textfield.render Mdl
        [ mountain.id ]
        model.mdl
        [ Options.onInput (UpdateMountain mountain.id)
        , Textfield.value mountain.thought
        , Textfield.error ("One word only")
            |> Options.when
                (String.split " " mountain.thought
                    |> List.length
                    |> (<) 1
                )
        , Textfield.disabled
            |> Options.when (not mountain.climbed)
        ]
        []



{--
  Sometimes you want to sort things by Int or Date,
  but you need to format the string in a special way.
--}


heightField : Mountain -> Html Msg
heightField =
    .height >> toString >> decorateHeight >> text



{--
Complex subsorting is availble with CustomSort
We're sorting the Mountain Ranges alphabetically, but always keeping the height descending

CustomSort hands you the Table.Order and lets you handle it yourself,
while Basic sort will reverse the list for you depending on Table.Order
--}


rangeSort : Table.Order -> List Mountain -> List Mountain
rangeSort order mountains =
    (groupBy .range mountains)
        |> Dict.toList
        |> List.sortBy Tuple.first
        |> List.concatMap (Tuple.second >> List.sortBy .height >> keepListAscending order)
        |> if order == Table.Ascending then
            List.reverse
           else
            identity



-- UPDATE


type Msg
    = SetTableState Sortable.State
    | UpdateMountain Int String
    | ToggleMountain Int
    | Mdl (Material.Msg Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetTableState newState ->
            ( { model | tableState = newState }
            , Cmd.none
            )

        UpdateMountain id thought ->
            let
                newMountains =
                    model.mountains
                        |> List.map
                            (\mnt ->
                                if id == mnt.id then
                                    { mnt | thought = thought }
                                else
                                    mnt
                            )
            in
                { model | mountains = newMountains } ! []

        ToggleMountain id ->
            let
                newMountains =
                    model.mountains
                        |> List.map
                            (\mnt ->
                                if id == mnt.id then
                                    { mnt | climbed = not mnt.climbed }
                                else
                                    mnt
                            )
            in
                { model | mountains = newMountains } ! []

        Mdl msg_ ->
            Material.update Mdl msg_ model



-- MOUNTAINS


type alias Mountain =
    { id : Int
    , name : String
    , height : Int
    , climbed : Bool
    , thought : String
    , range : String
    }


fourteeners : List Mountain
fourteeners =
    [ Mountain 1 "Torreys Peak" 14267 False "" "Front"
    , Mountain 2 "Mt. Evans" 14264 True "Crowded" "Front"
    , Mountain 3 "Longs Peak" 14255 True "Cold" "Front"
    , Mountain 4 "Pikes Peak" 14110 False "" "Front"
    , Mountain 5 "Mt. Bierstadt" 14060 True "Goats" "Front"
    , Mountain 6 "Quandary Peak" 14265 False "" "Tenmile"
    , Mountain 7 "Mt. Lincoln" 14286 True "Windy" "Mosquito"
    , Mountain 8 "Mt. Cameron" 14238 False "" "Mosquito"
    , Mountain 9 "Mt. Bross" 14172 True "Sunset" "Mosquito"
    , Mountain 10 "Mt. Democrat" 14148 False "" "Mosquito"
    , Mountain 11 "Mt. Sherman" 14036 False "" "Mosquito"
    , Mountain 12 "Mt. Elbert" 14433 True "Sleet" "Sawatch"
    , Mountain 13 "Mt. Massive" 14421 False "" "Sawatch"
    , Mountain 14 "Mt. Harvard" 14420 True "Ouch" "Sawatch"
    , Mountain 15 "La Plata Peak" 14336 False "" "Sawatch"
    , Mountain 16 "Mt. Antero" 14269 False "" "Sawatch"
    , Mountain 17 "Mt. Shavano" 14229 True "Sore" "Sawatch"
    , Mountain 18 "Mt. Belford" 14197 False "" "Sawatch"
    , Mountain 19 "Mt. Princeton" 14197 False "" "Sawatch"
    , Mountain 20 "Grays Peak" 14270 True "Stellar" "Front"
    , Mountain 21 "Mt. Yale" 14196 True "Beautiful" "Sawatch"
    , Mountain 22 "Tabeguache Peak" 14155 False "" "Sawatch"
    , Mountain 23 "Mt. Oxford" 14153 False "" "Sawatch"
    , Mountain 24 "Mt. Columbia" 14073 True "Timeless" "Sawatch"
    , Mountain 25 "Missouri Mountain" 14067 False "" "Sawatch"
    , Mountain 26 "Mt. of the Holy Cross" 14005 False "" "Sawatch"
    , Mountain 27 "Huron Peak" 14003 False "" "Sawatch"
    , Mountain 28 "Castle Peak" 14265 False "" "Elk"
    , Mountain 29 "Maroon Peak" 14156 True "Gorgeous" "Elk"
    , Mountain 30 "Capitol Peak" 14130 False "" "Elk"
    , Mountain 31 "Snowmass Mountain" 14092 False "" "Elk"
    , Mountain 32 "Conundrum Peak" 14060 True "Wow" "Elk"
    , Mountain 33 "Pyramid Peak" 14018 False "" "Elk"
    , Mountain 34 "North Maroon Peak" 14014 True "Srsly?" "Elk"
    , Mountain 35 "Uncompahgre Peak" 14309 False "" "San Juan"
    , Mountain 36 "Mt. Wilson" 14246 False "" "San Juan"
    , Mountain 37 "El Diente Peak" 14159 True "Brutal" "San Juan"
    , Mountain 38 "Mt. Sneffels" 14150 False "" "San Juan"
    , Mountain 39 "Mt. Eolus" 14083 True "Itchy" "San Juan"
    , Mountain 40 "Blanca Peak" 14345 True "Worthwhile" "Sangres"
    , Mountain 41 "Windom Peak" 14082 True "Marmots" "San Juan"
    , Mountain 42 "Sunlight Peak" 14059 False "" "San Juan"
    , Mountain 43 "Handies Peak" 14048 False "" "San Juan"
    , Mountain 44 "North Eolus" 14039 True "Where?" "San Juan"
    , Mountain 45 "Redcloud Peak" 14034 False "" "San Juan"
    , Mountain 46 "Wilson Peak" 14017 True "Lost" "San Juan"
    , Mountain 47 "Wetterhorn Peak" 14015 False "" "San Juan"
    , Mountain 48 "San Luis Peak" 14014 False "" "San Juan"
    , Mountain 49 "Sunshine Peak" 14001 False "" "San Juan"
    , Mountain 50 "Crestone Peak" 14294 False "" "Sangres"
    , Mountain 51 "Crestone Needle" 14197 False "" "Sangres"
    , Mountain 52 "Kit Carson Peak" 14165 True "Unforgettable" "Sangres"
    , Mountain 53 "Challenger Point" 14081 False "" "Sangres"
    , Mountain 54 "Humboldt Peak" 14064 False "" "Sangres"
    , Mountain 55 "Culebra Peak" 14047 True "Amazing" "Sangres"
    , Mountain 56 "Mt. Lindsey" 14042 False "" "Sangres"
    , Mountain 57 "Ellingwood Point" 14042 False "" "Sangres"
    , Mountain 58 "Little Bear Peak" 14037 True "Big" "Sangres"
    ]


emptyMountain : Mountain
emptyMountain =
    Mountain 0 "" 0 False "" ""


decorateHeight : String -> String
decorateHeight height =
    String.left 2 height ++ "," ++ (String.right 3 height) ++ "'"


climbedCount : List Mountain -> Int
climbedCount =
    List.length << List.filter .climbed


keepListAscending : Table.Order -> (List data -> List data)
keepListAscending order =
    case order of
        Table.Descending ->
            List.reverse

        Table.Ascending ->
            identity


groupBy : (v -> comparable) -> List v -> Dict comparable (List v)
groupBy keyFromElem list =
    let
        reducer elem dict =
            let
                key =
                    keyFromElem elem

                values =
                    Maybe.withDefault [] (Dict.get key dict)
            in
                Dict.insert key (List.append values [ elem ]) dict
    in
        List.foldl reducer Dict.empty list
