module Main exposing (main)

import Html exposing (Html, div, text, table, tr, td)
import Html.Attributes exposing (style)
import Random


main : Program Never Model Msg
main =
    Html.program
        { init = init ! []
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { generation : Int
    , speed : Int
    , lifegame : LifeGame
    }


init : Model
init =
    { generation = 0
    , speed = 0
    , lifegame = { w = 100, h = 100, table = List.repeat 100 (List.repeat 100 Dead) }
    }


type Cell
    = Alive
    | Dead


type alias LifeGame =
    { w : Int
    , h : Int
    , table : List (List Cell)
    }


type EdgeStrategy
    = Zero
    | Loop


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


randomCell : Random.Generator Cell
randomCell =
    Random.map
        (\b ->
            if b then
                Alive
            else
                Dead
        )
        Random.bool


randomRow : Int -> Random.Generator (List Cell)
randomRow n =
    Random.list n randomCell


randomTable : Int -> Int -> Random.Generator (List (List Cell))
randomTable m n =
    Random.list m (randomRow n)


extendTableWithZero : LifeGame -> List (List Cell)
extendTableWithZero lifegame =
    [ List.repeat (lifegame.w + 2) Dead ]
        ++ List.map (\l -> [ Dead ] ++ l ++ [ Dead ]) lifegame.table
        ++ [ List.repeat (lifegame.w + 2) Dead ]


extendTableWithLoop : LifeGame -> List (List Cell)
extendTableWithLoop lifegame =
    lifegame.table
        |> List.map extendListWithLoop
        |> extendListWithLoop


extendListWithLoop : List a -> List a
extendListWithLoop list =
    let
        n =
            List.length list
    in
        List.drop (n - 1) list ++ list ++ List.take 1 list


groupWithNeighbors : List (List Cell) -> List (List (List Cell))
groupWithNeighbors table =
    table
        |> List.map (window 3)
        |> window 3
        |> List.map (zipList List.concat)


evaluateWithNeighbors : List Cell -> Cell
evaluateWithNeighbors selfAndNeighbors =
    let
        self =
            selfAndNeighbors |> List.drop 4 |> List.head

        neighbors =
            List.take 4 selfAndNeighbors ++ List.drop 5 selfAndNeighbors

        aliveCount =
            List.filter (\c -> c == Alive) neighbors |> List.length
    in
        case ( self, aliveCount ) of
            ( Just Alive, 2 ) ->
                Alive

            ( _, 3 ) ->
                Alive

            _ ->
                Dead


transpose : List (List a) -> List (List a)
transpose list =
    if List.all List.isEmpty list then
        []
    else
        List.filterMap List.head list :: transpose (List.filterMap List.tail list)


zipList : (List a -> a) -> List (List a) -> List a
zipList f list =
    if List.all List.isEmpty list then
        []
    else
        (List.filterMap List.head list |> f) :: zipList f (List.filterMap List.tail list)


window : Int -> List a -> List (List a)
window n list =
    if List.length list < n then
        []
    else
        List.take n list :: window n (List.drop 1 list)


view : Model -> Html Msg
view model =
    div []
        [ text "Hello, World!"
        , text (toString <| transpose [ [ 1, 2, 3 ], [ 4, 5, 6 ], [ 7, 8, 9 ] ])
        , text (toString <| window 3 [ 1, 2, 3, 4, 5, 6, 7 ])
        , viewTable model.lifegame
        ]


viewTable : LifeGame -> Html Msg
viewTable lifegame =
    table [] <|
        List.map viewRow lifegame.table


viewRow : List Cell -> Html Msg
viewRow row =
    tr [] <|
        List.map viewCell row


viewCell : Cell -> Html Msg
viewCell cell =
    case cell of
        Alive ->
            td [ style [ ( "backgroundColor", "white" ) ] ] [ text "*" ]

        Dead ->
            td [ style [ ( "backgroundColor", "black" ) ] ] [ text " " ]
