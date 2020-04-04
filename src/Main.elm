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



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type CardStack
    = List CardSuit


type alias Model =
    { cards : List CardSuit
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model <| getDeck 36
    , Cmd.none
    )


type Suit
    = Diamond
    | Club
    | Heart
    | Spade
    | Joker


type Card
    = Ace
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King


type alias CardSuit =
    { value : Card
    , suit : Suit
    , open : Bool
    }


openCard : CardSuit -> CardSuit
openCard card =
    { card
        | open =
            if card.open then
                False

            else
                True
    }



-- UPDATE


type Msg
    = Shuffle
    | ShuffledCards (List CardSuit)
    | OpenCard CardSuit



--| NewCard (List CardSuit)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Shuffle ->
            ( model
            , Random.generate ShuffledCards <| deckShuffleGenerator model.cards
              -- , Random.generate NewCard <| cardStackGenerator 10
            )

        ShuffledCards newCards ->
            ( Model newCards
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
            ( { model | cards = items }
            , Cmd.none
            )


cardBoardGenerator : Random.Generator (Array.Array (List CardSuit))
cardBoardGenerator =
    cardStackGenerator 6
        |> Random.Array.array 10


deckShuffleGenerator : List CardSuit -> Random.Generator (List CardSuit)
deckShuffleGenerator cardDeck =
    Random.List.shuffle cardDeck


cardStackGenerator : Int -> Random.Generator (List CardSuit)
cardStackGenerator stackNumber =
    Random.map2
        (\x y -> CardSuit x y True)
        cardGenerator
        suitGenerator
        |> Random.list stackNumber


cardGenerator : Random.Generator Card
cardGenerator =
    Random.uniform Ace
        [ Two
        , Three
        , Four
        , Five
        , Six
        , Seven
        , Eight
        , Nine
        , Ten
        , Jack
        , Queen
        , King
        , King
        ]


suitGenerator : Random.Generator Suit
suitGenerator =
    Random.uniform Diamond [ Club, Heart, Spade ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [] <|
        [ button [ onClick Shuffle ] [ text "Shuffle" ]
        , viewDivCards model.cards
        ]


viewDivCards : List CardSuit -> Html Msg
viewDivCards cards =
    List.map (\c -> div [ style "display" "inline-block", onClick (OpenCard c) ] [ text (viewCard c) ]) cards
        |> div [ style "font-size" "6em" ]


viewCards : List CardSuit -> String
viewCards cards =
    List.map viewCard cards
        |> String.join ""


viewCard : CardSuit -> String
viewCard card =
    viewCardSuit ( card.value, card.suit, card.open )


viewArrayOfCards : Array.Array (List CardSuit) -> String
viewArrayOfCards cards =
    Array.toList cards
        |> List.map viewCards
        |> String.join "<br>"


getDeck : Int -> List CardSuit
getDeck countOfCards =
    case countOfCards of
        54 ->
            [ CardSuit Ace Spade False
            , CardSuit Two Spade False
            , CardSuit Three Spade False
            , CardSuit Four Spade False
            , CardSuit Five Spade False
            , CardSuit Six Spade False
            , CardSuit Seven Spade False
            , CardSuit Eight Spade False
            , CardSuit Nine Spade False
            , CardSuit Ten Spade False
            , CardSuit Jack Spade False
            , CardSuit Queen Spade False
            , CardSuit King Spade False
            , CardSuit Ace Diamond False
            , CardSuit Two Diamond False
            , CardSuit Three Diamond False
            , CardSuit Four Diamond False
            , CardSuit Five Diamond False
            , CardSuit Six Diamond False
            , CardSuit Seven Diamond False
            , CardSuit Eight Diamond False
            , CardSuit Nine Diamond False
            , CardSuit Ten Diamond False
            , CardSuit Jack Diamond False
            , CardSuit Queen Diamond False
            , CardSuit King Diamond False
            , CardSuit Ace Club False
            , CardSuit Two Club False
            , CardSuit Three Club False
            , CardSuit Four Club False
            , CardSuit Five Club False
            , CardSuit Six Club False
            , CardSuit Seven Club False
            , CardSuit Eight Club False
            , CardSuit Nine Club False
            , CardSuit Ten Club False
            , CardSuit Jack Club False
            , CardSuit Queen Club False
            , CardSuit King Club False
            , CardSuit Ace Heart False
            , CardSuit Two Heart False
            , CardSuit Three Heart False
            , CardSuit Four Heart False
            , CardSuit Five Heart False
            , CardSuit Six Heart False
            , CardSuit Seven Heart False
            , CardSuit Eight Heart False
            , CardSuit Nine Heart False
            , CardSuit Ten Heart False
            , CardSuit Jack Heart False
            , CardSuit Queen Heart False
            , CardSuit King Heart False
            , CardSuit Ace Joker False
            , CardSuit Ace Joker False
            ]

        52 ->
            [ CardSuit Ace Spade False
            , CardSuit Two Spade False
            , CardSuit Three Spade False
            , CardSuit Four Spade False
            , CardSuit Five Spade False
            , CardSuit Six Spade False
            , CardSuit Seven Spade False
            , CardSuit Eight Spade False
            , CardSuit Nine Spade False
            , CardSuit Ten Spade False
            , CardSuit Jack Spade False
            , CardSuit Queen Spade False
            , CardSuit King Spade False
            , CardSuit Ace Diamond False
            , CardSuit Two Diamond False
            , CardSuit Three Diamond False
            , CardSuit Four Diamond False
            , CardSuit Five Diamond False
            , CardSuit Six Diamond False
            , CardSuit Seven Diamond False
            , CardSuit Eight Diamond False
            , CardSuit Nine Diamond False
            , CardSuit Ten Diamond False
            , CardSuit Jack Diamond False
            , CardSuit Queen Diamond False
            , CardSuit King Diamond False
            , CardSuit Ace Club False
            , CardSuit Two Club False
            , CardSuit Three Club False
            , CardSuit Four Club False
            , CardSuit Five Club False
            , CardSuit Six Club False
            , CardSuit Seven Club False
            , CardSuit Eight Club False
            , CardSuit Nine Club False
            , CardSuit Ten Club False
            , CardSuit Jack Club False
            , CardSuit Queen Club False
            , CardSuit King Club False
            , CardSuit Ace Heart False
            , CardSuit Two Heart False
            , CardSuit Three Heart False
            , CardSuit Four Heart False
            , CardSuit Five Heart False
            , CardSuit Six Heart False
            , CardSuit Seven Heart False
            , CardSuit Eight Heart False
            , CardSuit Nine Heart False
            , CardSuit Ten Heart False
            , CardSuit Jack Heart False
            , CardSuit Queen Heart False
            , CardSuit King Heart False
            ]

        36 ->
            [ CardSuit Ace Spade False
            , CardSuit Six Spade False
            , CardSuit Seven Spade False
            , CardSuit Eight Spade False
            , CardSuit Nine Spade False
            , CardSuit Ten Spade False
            , CardSuit Jack Spade False
            , CardSuit Queen Spade False
            , CardSuit King Spade False
            , CardSuit Ace Diamond False
            , CardSuit Six Diamond False
            , CardSuit Seven Diamond False
            , CardSuit Eight Diamond False
            , CardSuit Nine Diamond False
            , CardSuit Ten Diamond False
            , CardSuit Jack Diamond True
            , CardSuit Queen Diamond False
            , CardSuit King Diamond False
            , CardSuit Ace Club False
            , CardSuit Six Club False
            , CardSuit Seven Club False
            , CardSuit Eight Club False
            , CardSuit Nine Club False
            , CardSuit Ten Club False
            , CardSuit Jack Club False
            , CardSuit Queen Club False
            , CardSuit King Club False
            , CardSuit Ace Heart False
            , CardSuit Six Heart False
            , CardSuit Seven Heart False
            , CardSuit Eight Heart False
            , CardSuit Nine Heart False
            , CardSuit Ten Heart False
            , CardSuit Jack Heart False
            , CardSuit Queen Heart False
            , CardSuit King Heart False
            ]

        24 ->
            [ CardSuit Ace Spade False
            , CardSuit Nine Spade False
            , CardSuit Ten Spade False
            , CardSuit Jack Spade False
            , CardSuit Queen Spade False
            , CardSuit King Spade False
            , CardSuit Ace Diamond False
            , CardSuit Nine Diamond False
            , CardSuit Ten Diamond False
            , CardSuit Jack Diamond False
            , CardSuit Queen Diamond False
            , CardSuit King Diamond False
            , CardSuit Ace Club False
            , CardSuit Nine Club False
            , CardSuit Ten Club False
            , CardSuit Jack Club False
            , CardSuit Queen Club False
            , CardSuit King Club False
            , CardSuit Ace Heart False
            , CardSuit Nine Heart False
            , CardSuit Ten Heart False
            , CardSuit Jack Heart False
            , CardSuit Queen Heart False
            , CardSuit King Heart False
            ]

        _ ->
            []


viewCardSuit : ( Card, Suit, Bool ) -> String
viewCardSuit ( card, suit, open ) =
    case ( card, suit, open ) of
        ( _, _, False ) ->
            "🂠"

        ( _, Joker, _ ) ->
            "J"

        ( Ace, Spade, _ ) ->
            "🂡"

        ( Two, Spade, _ ) ->
            "🂢"

        ( Three, Spade, _ ) ->
            "🂢"

        ( Four, Spade, _ ) ->
            "🂢"

        ( Five, Spade, _ ) ->
            "🂥"

        ( Six, Spade, _ ) ->
            "🂦"

        ( Seven, Spade, _ ) ->
            "🂧"

        ( Eight, Spade, _ ) ->
            "🂨"

        ( Nine, Spade, _ ) ->
            "🂩"

        ( Ten, Spade, _ ) ->
            "🂪"

        ( Jack, Spade, _ ) ->
            "🂫"

        ( Queen, Spade, _ ) ->
            "🂭"

        ( King, Spade, _ ) ->
            "🂮"

        ( Ace, Diamond, _ ) ->
            "🃁"

        ( Two, Diamond, _ ) ->
            "🃂"

        ( Three, Diamond, _ ) ->
            "🃃"

        ( Four, Diamond, _ ) ->
            "🃄"

        ( Five, Diamond, _ ) ->
            "🃅"

        ( Six, Diamond, _ ) ->
            "🃆"

        ( Seven, Diamond, _ ) ->
            "🃇"

        ( Eight, Diamond, _ ) ->
            "🃈"

        ( Nine, Diamond, _ ) ->
            "🃉"

        ( Ten, Diamond, _ ) ->
            "🃊"

        ( Jack, Diamond, _ ) ->
            "🃋"

        ( Queen, Diamond, _ ) ->
            "🃍"

        ( King, Diamond, _ ) ->
            "🃎"

        ( Ace, Club, _ ) ->
            "🃑"

        ( Two, Club, _ ) ->
            "🃒"

        ( Three, Club, _ ) ->
            "🃓"

        ( Four, Club, _ ) ->
            "🃔"

        ( Five, Club, _ ) ->
            "🃕"

        ( Six, Club, _ ) ->
            "🃖"

        ( Seven, Club, _ ) ->
            "🃗"

        ( Eight, Club, _ ) ->
            "🃘"

        ( Nine, Club, _ ) ->
            "🃙"

        ( Ten, Club, _ ) ->
            "🃚"

        ( Jack, Club, _ ) ->
            "🃛"

        ( Queen, Club, _ ) ->
            "🃝"

        ( King, Club, _ ) ->
            "🃞"

        ( Ace, Heart, _ ) ->
            "🂱"

        ( Two, Heart, _ ) ->
            "🂲"

        ( Three, Heart, _ ) ->
            "🂳"

        ( Four, Heart, _ ) ->
            "🂴"

        ( Five, Heart, _ ) ->
            "🂵"

        ( Six, Heart, _ ) ->
            "🂶"

        ( Seven, Heart, _ ) ->
            "🂷"

        ( Eight, Heart, _ ) ->
            "🂸"

        ( Nine, Heart, _ ) ->
            "🂹"

        ( Ten, Heart, _ ) ->
            "🂺"

        ( Jack, Heart, _ ) ->
            "🂻"

        ( Queen, Heart, _ ) ->
            "🂽"

        ( King, Heart, _ ) ->
            "🂾"
