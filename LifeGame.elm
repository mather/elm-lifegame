module Main exposing (main)

import Html exposing (Html, div, text, button)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Html.Lazy exposing (lazy)
import Time exposing (every, second)
import Random


main : Program Never Model Msg
main =
    Html.program
        { init = init ! [ initializeTableByRandom init.lifegame.w init.lifegame.h ]
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { generation : Int
    , speed : Int
    , edge : EdgeStrategy
    , lifegame : LifeGame
    }


init : Model
init =
    { generation = 0
    , speed = 2
    , edge = DeadPadding
    , lifegame = emptyLifeGame 50 100
    }


emptyLifeGame : Int -> Int -> LifeGame
emptyLifeGame m n =
    { w = n
    , h = m
    , table = List.repeat m (List.repeat n Dead)
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
    = DeadPadding
    | Loop


type Msg
    = NoOp
    | InitializeLifeGame LifeGame
    | NextGen


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        InitializeLifeGame lifegame ->
            { model | lifegame = lifegame, generation = 0 } ! []

        NextGen ->
            { model
                | lifegame = nextGen model.edge model.lifegame
                , generation = model.generation + 1
            }
                ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.speed > 0 then
        every (second / (toFloat model.speed)) (\t -> NextGen)
    else
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
randomTable w h =
    Random.list h (randomRow w)


randomLifeGame : Int -> Int -> Random.Generator LifeGame
randomLifeGame w h =
    Random.map
        (\table ->
            { w = w
            , h = h
            , table = table
            }
        )
        (randomTable w h)


initializeTableByRandom : Int -> Int -> Cmd Msg
initializeTableByRandom m n =
    Random.generate InitializeLifeGame (randomLifeGame m n)



-- extend


edgeFramedTable : EdgeStrategy -> LifeGame -> List (List Cell)
edgeFramedTable edge =
    case edge of
        DeadPadding ->
            edgeFramedTableWithDeadPadding

        Loop ->
            edgeFramedTableWithLoop


edgeFramedTableWithDeadPadding : LifeGame -> List (List Cell)
edgeFramedTableWithDeadPadding lifegame =
    [ List.repeat (lifegame.w + 2) Dead ]
        ++ List.map (\l -> [ Dead ] ++ l ++ [ Dead ]) lifegame.table
        ++ [ List.repeat (lifegame.w + 2) Dead ]


edgeFramedTableWithLoop : LifeGame -> List (List Cell)
edgeFramedTableWithLoop lifegame =
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


nextGen : EdgeStrategy -> LifeGame -> LifeGame
nextGen edge lifegame =
    let
        nextTable =
            edgeFramedTable edge lifegame
                |> groupWithNeighbors
                |> List.map (List.map evaluateWithNeighbors)
    in
        { lifegame | table = nextTable }


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
        [ button [ onClick NextGen ] [ text "NextGen" ]
        , text <| "generation = " ++ toString model.generation
        , viewTable model.lifegame
        ]


viewTable : LifeGame -> Html Msg
viewTable lifegame =
    div [] <|
        List.map viewRow lifegame.table


viewRow : List Cell -> Html Msg
viewRow row =
    div [ style [ ( "clear", "both" ) ] ] <|
        List.map (lazy viewCell) row


viewCell : Cell -> Html Msg
viewCell cell =
    case cell of
        Alive ->
            div [ style [ ( "backgroundColor", "white" ), ( "height", "1ex" ), ( "width", "1ex" ), ( "float", "left" ) ] ] []

        Dead ->
            div [ style [ ( "backgroundColor", "black" ), ( "height", "1ex" ), ( "width", "1ex" ), ( "float", "left" ) ] ] []
