-- Press a button to draw a random card.
--
-- Dependencies:
--   elm install elm/random
--


module Main exposing (main)

import Array
import Browser
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (..)
import Random
import Random.Array
import Random.List
import String
import CardModel exposing (CardSuit, getDeck, openCard)
import CardModel exposing (viewCardSuit)
import Html.Attributes exposing (class)


-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { cards : List CardSuit
    , board : Array.Array(List CardSuit)
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model (getDeck 54 False ++ getDeck 54 False) Array.empty
    , Cmd.none
    )


throwCardsOnBoard : List CardSuit -> Array.Array(List CardSuit)
throwCardsOnBoard cards =  
    Array.push (List.take 10 cards) Array.empty
    |>    Array.push (List.take 10 (List.drop 10 cards))
    |>    Array.push (List.take 10 (List.drop 20 cards))
    |>    Array.push (List.take 10 (List.drop 30 cards))
    |>    Array.push (List.take 6 (List.drop 40 cards))
    |>    Array.push (List.take 6 (List.drop 46 cards))
    |>    Array.push (List.take 6 (List.drop 52 cards))
    |>    Array.push (List.take 6 (List.drop 58 cards))
    |>    Array.push (List.take 6 (List.drop 64 cards))
    |>    Array.push (List.take 6 (List.drop 70 cards))

openLastCardsOnboard : Array.Array(List CardSuit) -> Array.Array(List CardSuit)
openLastCardsOnboard arr = 
    Array.map (\lcs -> List.head (List.reverse lcr) ) arr

-- UPDATE


type Msg
    = Shuffle
    | ShuffledCards (List CardSuit)
    | OpenCard CardSuit
    | Throw



--| NewCard (List CardSuit)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        i=1
    in
    case msg of
        Shuffle ->
            ( model
            , Random.generate ShuffledCards <| deckShuffleGenerator model.cards
            )
        Throw -> (Model (List.drop 76 model.cards) (throwCardsOnBoard model.cards), Cmd.none)
        ShuffledCards newCards ->
            ( Model newCards Array.empty
              -- ( Model <| List.map openCard newCards
            , Cmd.none
            )

        OpenCard card ->
            let
                items =
                    List.map
                        (\c ->
                            if card == c then
                                openCard c

                            else
                                c
                        )
                        model.cards
            in
            ( { model | cards = items, board = throwCardsOnBoard items }
            , Cmd.none
            )


deckShuffleGenerator : List CardSuit -> Random.Generator (List CardSuit)
deckShuffleGenerator cardDeck =
    List.map (\c -> {c | open = False}) cardDeck
    |> Random.List.shuffle


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [] <|
        [ button [ onClick Shuffle ] [ text "Shuffle" ]
        , text <| String.fromInt <| List.length model.cards
        , text <| viewMaybeCard <| List.head model.cards
        , viwDivArrayCards model.board
        , button [ onClick Throw ] [ text "ThrowCards" ]
        ]


viewDivCards : List CardSuit -> Html Msg
viewDivCards cards =
    List.map (\c -> div [ onClick (OpenCard c) ] [ text (viewCard c) ]) cards -- map all list to 
        |> div [] -- div for one column
        |> List.singleton
        |> div [ style "display" "grid", style "grid-template-columns" "repeat(10, 1fr)", style "font-size" "10em"] -- div of all board

viewColumnOfCards : List CardSuit -> Html Msg
viewColumnOfCards cards =
    List.map viewCardDiv cards -- map all list to 
        |> div [] -- div for one column

viwDivArrayCards: Array.Array(List CardSuit) -> Html Msg
viwDivArrayCards arr = 
    Array.toList arr
    |> List.map viewColumnOfCards
    |> div [ style "display" "grid", style "grid-template-columns" "repeat(10, 1fr)", style "font-size" "5em"] -- div of all board


viewCardDiv : CardSuit -> Html Msg
viewCardDiv card = div [ onClick (OpenCard card) ] [ text (viewCard card) ] 

viewMaybeCard : Maybe CardSuit -> String
viewMaybeCard mbcardSuit =
    case mbcardSuit of
        Just value ->
            viewCard value

        Nothing ->
            "[ ]"


viewCard : CardSuit -> String
viewCard card =
    viewCardSuit ( card.value, card.suit, card.open )