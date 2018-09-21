module Main exposing (main)

import Browser
import Html exposing (Html, div, text, button, input, select, option, span)
import Html.Attributes as Attr exposing (style, disabled, type_, min, max, value, name, checked)
import Html.Events exposing (onClick, onInput)
import Html.Lazy exposing (lazy)
import Time exposing (every)
import Random
import String exposing (toInt)


main : Program Never Model Msg
main =
    Browser.document
        { init = ( init, Cmd.batch [ initializeTableByRandom init.lifegame.w init.lifegame.h ] )
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { generation : Int
    , speed : Int
    , paused : Bool
    , edge : EdgeStrategy
    , lifegame : LifeGame
    }


init : Model
init =
    { generation = 0
    , speed = 2
    , paused = True
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
    | TogglePause
    | SetSpeed Int
    | SetEdgeStrategy EdgeStrategy
    | Initialize


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        InitializeLifeGame lifegame ->
            ( { model | lifegame = lifegame, generation = 0 }, Cmd.none )

        NextGen ->
            ( { model
                | lifegame = nextGen model.edge model.lifegame
                , generation = model.generation + 1
              }
            , Cmd.none
            )

        TogglePause ->
            ( { model | paused = not model.paused }, Cmd.none )

        SetSpeed i ->
            ( { model | speed = i }, Cmd.none )

        SetEdgeStrategy edge ->
            ( { model | edge = edge }, Cmd.none )

        Initialize ->
            ( model, Cmd.batch [ initializeTableByRandom model.lifegame.w model.lifegame.h ] )


secondInMillis : float
secondInMillis =
    1000


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.speed > 0 && not model.paused then
        every (secondInMillis / (toFloat model.speed)) (\t -> NextGen)
    else
        Sub.none


randomCell : Random.Generator Cell
randomCell =
    Random.uniform Alive [ Dead ]


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


groupWithNeighbors : List (List Cell) -> List (List (List (List Cell)))
groupWithNeighbors table =
    table
        |> List.map (window 3)
        |> window 3
        |> List.map transpose


evaluateWithNeighbors : List (List Cell) -> Cell
evaluateWithNeighbors nineCells =
    let
        selfAndNeighbors =
            List.concat nineCells

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


window : Int -> List a -> List (List a)
window n list =
    if List.length list < n then
        []
    else
        List.take n list :: window n (List.drop 1 list)


view : Model -> Html Msg
view model =
    div []
        [ viewLifeGame model.lifegame
        , viewController model
        , viewStatus model
        ]


viewController : Model -> Html Msg
viewController model =
    div [ style [ ( "clear", "both" ) ] ]
        [ initializeButton
        , singleStepButton model.paused
        , togglePauseButton model.paused
        , speedSlider model.speed
        , selectEdgeStrategy model.edge
        ]


initializeButton : Html Msg
initializeButton =
    button [ onClick Initialize ] [ text "ランダムに初期化する" ]


singleStepButton : Bool -> Html Msg
singleStepButton paused =
    button [ onClick NextGen, disabled <| not paused ] [ text "1世代進める" ]


togglePauseButton : Bool -> Html Msg
togglePauseButton paused =
    button [ onClick TogglePause ]
        [ text <|
            if paused then
                "自動再生"
            else
                "停止する"
        ]


speedSlider : Int -> Html Msg
speedSlider speed =
    let
        toMsg str =
            case toInt str of
                Ok i ->
                    SetSpeed i

                _ ->
                    NoOp
    in
        input
            [ type_ "range", Attr.min "1", Attr.max "10", onInput toMsg, value (String.fromInt speed) ]
            []


selectEdgeStrategy : EdgeStrategy -> Html Msg
selectEdgeStrategy edge =
    span []
        [ text "境界条件："
        , input [ type_ "radio", name "edgeStrategy", onClick <| SetEdgeStrategy DeadPadding, checked <| edge == DeadPadding ] []
        , text "死亡"
        , input [ type_ "radio", name "edgeStrategy", onClick <| SetEdgeStrategy Loop, checked <| edge == Loop ] []
        , text "ループ"
        ]


viewStatus : Model -> Html Msg
viewStatus model =
    div []
        [ text <| "世代数 = " ++ String.fromInt model.generation
        , text <| ", 自動再生スピード = " ++ String.fromInt model.speed
        ]


viewLifeGame : LifeGame -> Html Msg
viewLifeGame lifegame =
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
