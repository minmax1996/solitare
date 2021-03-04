module CardModel exposing (..)


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
    { card | open = not card.open }


getDeck : Int -> Bool -> List CardSuit
getDeck countOfCards open=
    case countOfCards of
        54 ->
            [ CardSuit Ace Spade open
            , CardSuit Two Spade open
            , CardSuit Three Spade open
            , CardSuit Four Spade open
            , CardSuit Five Spade open
            , CardSuit Six Spade open
            , CardSuit Seven Spade open
            , CardSuit Eight Spade open
            , CardSuit Nine Spade open
            , CardSuit Ten Spade open
            , CardSuit Jack Spade open
            , CardSuit Queen Spade open
            , CardSuit King Spade open
            , CardSuit Ace Diamond open
            , CardSuit Two Diamond open
            , CardSuit Three Diamond open
            , CardSuit Four Diamond open
            , CardSuit Five Diamond open
            , CardSuit Six Diamond open
            , CardSuit Seven Diamond open
            , CardSuit Eight Diamond open
            , CardSuit Nine Diamond open
            , CardSuit Ten Diamond open
            , CardSuit Jack Diamond open
            , CardSuit Queen Diamond open
            , CardSuit King Diamond open
            , CardSuit Ace Club open
            , CardSuit Two Club open
            , CardSuit Three Club open
            , CardSuit Four Club open
            , CardSuit Five Club open
            , CardSuit Six Club open
            , CardSuit Seven Club open
            , CardSuit Eight Club open
            , CardSuit Nine Club open
            , CardSuit Ten Club open
            , CardSuit Jack Club open
            , CardSuit Queen Club open
            , CardSuit King Club open
            , CardSuit Ace Heart open
            , CardSuit Two Heart open
            , CardSuit Three Heart open
            , CardSuit Four Heart open
            , CardSuit Five Heart open
            , CardSuit Six Heart open
            , CardSuit Seven Heart open
            , CardSuit Eight Heart open
            , CardSuit Nine Heart open
            , CardSuit Ten Heart open
            , CardSuit Jack Heart open
            , CardSuit Queen Heart open
            , CardSuit King Heart open
            , CardSuit Ace Joker open
            , CardSuit Ace Joker open
            ]

        52 ->
            [ CardSuit Ace Spade open
            , CardSuit Two Spade open
            , CardSuit Three Spade open
            , CardSuit Four Spade open
            , CardSuit Five Spade open
            , CardSuit Six Spade open
            , CardSuit Seven Spade open
            , CardSuit Eight Spade open
            , CardSuit Nine Spade open
            , CardSuit Ten Spade open
            , CardSuit Jack Spade open
            , CardSuit Queen Spade open
            , CardSuit King Spade open
            , CardSuit Ace Diamond open
            , CardSuit Two Diamond open
            , CardSuit Three Diamond open
            , CardSuit Four Diamond open
            , CardSuit Five Diamond open
            , CardSuit Six Diamond open
            , CardSuit Seven Diamond open
            , CardSuit Eight Diamond open
            , CardSuit Nine Diamond open
            , CardSuit Ten Diamond open
            , CardSuit Jack Diamond open
            , CardSuit Queen Diamond open
            , CardSuit King Diamond open
            , CardSuit Ace Club open
            , CardSuit Two Club open
            , CardSuit Three Club open
            , CardSuit Four Club open
            , CardSuit Five Club open
            , CardSuit Six Club open
            , CardSuit Seven Club open
            , CardSuit Eight Club open
            , CardSuit Nine Club open
            , CardSuit Ten Club open
            , CardSuit Jack Club open
            , CardSuit Queen Club open
            , CardSuit King Club open
            , CardSuit Ace Heart open
            , CardSuit Two Heart open
            , CardSuit Three Heart open
            , CardSuit Four Heart open
            , CardSuit Five Heart open
            , CardSuit Six Heart open
            , CardSuit Seven Heart open
            , CardSuit Eight Heart open
            , CardSuit Nine Heart open
            , CardSuit Ten Heart open
            , CardSuit Jack Heart open
            , CardSuit Queen Heart open
            , CardSuit King Heart open
            ]

        36 ->
            [ CardSuit Ace Spade open
            , CardSuit Six Spade open
            , CardSuit Seven Spade open
            , CardSuit Eight Spade open
            , CardSuit Nine Spade open
            , CardSuit Ten Spade open
            , CardSuit Jack Spade open
            , CardSuit Queen Spade open
            , CardSuit King Spade open
            , CardSuit Ace Diamond open
            , CardSuit Six Diamond open
            , CardSuit Seven Diamond open
            , CardSuit Eight Diamond open
            , CardSuit Nine Diamond open
            , CardSuit Ten Diamond open
            , CardSuit Jack Diamond open
            , CardSuit Queen Diamond open
            , CardSuit King Diamond open
            , CardSuit Ace Club open
            , CardSuit Six Club open
            , CardSuit Seven Club open
            , CardSuit Eight Club open
            , CardSuit Nine Club open
            , CardSuit Ten Club open
            , CardSuit Jack Club open
            , CardSuit Queen Club open
            , CardSuit King Club open
            , CardSuit Ace Heart open
            , CardSuit Six Heart open
            , CardSuit Seven Heart open
            , CardSuit Eight Heart open
            , CardSuit Nine Heart open
            , CardSuit Ten Heart open
            , CardSuit Jack Heart open
            , CardSuit Queen Heart open
            , CardSuit King Heart open
            ]

        24 ->
            [ CardSuit Ace Spade open
            , CardSuit Nine Spade open
            , CardSuit Ten Spade open
            , CardSuit Jack Spade open
            , CardSuit Queen Spade open
            , CardSuit King Spade open
            , CardSuit Ace Diamond open
            , CardSuit Nine Diamond open
            , CardSuit Ten Diamond open
            , CardSuit Jack Diamond open
            , CardSuit Queen Diamond open
            , CardSuit King Diamond open
            , CardSuit Ace Club open
            , CardSuit Nine Club open
            , CardSuit Ten Club open
            , CardSuit Jack Club open
            , CardSuit Queen Club open
            , CardSuit King Club open
            , CardSuit Ace Heart open
            , CardSuit Nine Heart open
            , CardSuit Ten Heart open
            , CardSuit Jack Heart open
            , CardSuit Queen Heart open
            , CardSuit King Heart open
            ]

        _ ->
            []


viewCardSuit : ( Card, Suit, Bool ) -> String
viewCardSuit ( card, suit, open ) =
    case ( card, suit, open ) of
        ( _, _, False ) ->
            "🂠"

        ( _, Joker, _ ) ->
            "🃏"

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
