-- eightoff.hs, a game of solitare (eight off) by Samuel Fung

module EightOff where
    import System.Random
    import Data.List
    import Data.Maybe
    import Debug.Trace

    -- algebraic datatype for suit
    data Suit = Diamonds | Clubs | Hearts | Spades deriving (Eq, Ord, Enum, Show)

    -- algebraic datatype for pip
    data Pip = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine |Ten | Jack | Queen | King deriving (Eq, Ord, Enum, Show)
    
    -- datatype for card
    -- Use tuple of pip and suit to represent the card
    type Card = (Pip,Suit)

    -- datatype for deck
    -- A list of 52 cards
    type Deck = [Card]

    -- datatype for deck
    -- A list of cards
    type Stack = [Card]

    -- datatype for foundation
    -- A list of list of cards
    type Foundation = [Stack]

    -- datatype for Cell
    -- A list of cards
    -- Starts with 4 cards, and can have maximum of 8 cards
    type Cell = [Card]                          

    -- datatype for Tableau
    -- A list of list of cards
    -- Starts with 8 list of 6 cards
    type Tableau = [Stack]          
    
    -- datatype for EOBoard
    -- A board is a list of list of cards
    type EOBoard = (Foundation,Cell,Tableau) 

    -- pack -> Returns a list of 52 cards in order
    pack ::  Deck
    pack = [(pip,suit) | suit <- [Diamonds .. Spades], pip <- [Ace .. King]]

    -- sCard -> Returns the successor of the card, based on pip value
    sCard :: Card -> Card
    sCard (pip, suit)   = (succ pip, suit)

    -- pCard -> Returns the predecessor of the card, based on pip value
    pCard :: Card -> Card
    pCard (pip, suit)   = (pred pip, suit)

    -- isAce -> Returns true if the card given has pip value of Ace
    isAce :: Card -> Bool
    isAce (pip, _)      = pip == Ace

    -- isKing -> Returns true if the card given has pip value of King
    isKing :: Card -> Bool
    isKing (pip, _)     = pip == King

    -- shuffle -> Takes in an integer/seed to generate a different order of 52 cards
    shuffle :: Int -> Deck
    shuffle seed = [suit | (_, suit) <- slis]                                                       -- returns randomised deck
        where
            rlis        = take 52 (randoms (mkStdGen seed) :: [Int])                                -- create a list of 52 randomised integers
            zlis        = zip rlis pack                                                             -- zip that list with card tuple, eg. (47, (Ace, Diamonds))
            slis        = sort zlis                                                                 -- sort the zipped list according to the randomised integers
    
    -- eODeal -> Shuffles the card and splits it to 8 tableaus of 6 cards, and 4 cells
    -- Uses an integer/seed to generate a different shuffle at every deal
    eODeal :: Int -> EOBoard
    eODeal seed = ([],cellStack,tableauStack)
        where
            cellStack       = take 4 (shuffle seed)                                                 -- takes 4 cards from a shuffled deck
            tableauStack    = getTableau (shuffle seed)                                             -- produces 8 piles of 6 cards
    
    -- getTableau -> produces piles of 6 cards for EOBoard
    -- Calls itself recursively until there are only 6 cards (last pile) in the deck
    getTableau :: Stack -> Tableau
    getTableau [] = []
    getTableau deck@(h:t)
        | length deck < 6 = []                                                                      -- Stopping condition: only 6 cards (last/8th pile) left in the deck
        | otherwise = (take 6 deck):(getTableau (drop 6 deck))                                      -- takes 6 cards, then gets the next 6 cards recursively
    
    -- toFoundations -> checks if the card can move to Foundations, returns the updated board once a move is made
    toFoundations :: EOBoard -> EOBoard
    toFoundations oriBoard@(foundation,cell,tableau)
        | checkMove = toFoundations newBoard                                                         -- Returns a new board if move can be made
        | otherwise = oriBoard                                                                       -- Returns original board if move cannot be made
        where
            checkMove                       = any (checkFoundation foundation) cardSelected          -- Returns true if the card can move
            cardSelected                    = getHead tableau ++ cell                                -- Gets the card of all lists in the tableau & cell
            checkFoundation foundation card = isAce card || elem (pCard card) (map head foundation)  -- Returns true if the card is an Ace or the card's predeccessor is in foundations
            newBoard                        = foldr (moveAce) oriBoard cardSelected                  -- Generating a new board when the moves are made
            
    -- getHead -> Generates a new list of cards containing the heads from each individual list/stack
    getHead :: [[Card]] -> [Card]
    getHead llis = [head hlis | hlis <- llis, (not.null) llis]          -- Gets the head of the list if the list is not empty
    
    -- Takes a card and a list of lists of cards, and removes that card from the list
    removeHead :: Card -> [[Card]] -> [[Card]]
    removeHead _ [] = []
    removeHead card llis
        | elem card h         = (delete card h: t)                       -- Removes the card if it is head of the list
        | otherwise           = (h : removeHead card t)                  -- Calls itself recursively to check the next list
        where
            (h:t) = llis

    -- moveAce -> Checks if the card is an ace and moves it if it is
    moveAce :: Card -> EOBoard -> EOBoard
    moveAce aceCard oriBoard@(foundation,cell,tableau)
        | isAce aceCard = ([aceCard]:foundation,delete aceCard cell,removeHead aceCard tableau)                             -- Adds the ace card to foundations & removes it from the tableau/cell
        | otherwise     = moveCard aceCard oriBoard                                                                         -- Returns the original board if the card is not Ace
    
    -- moveCard -> Checks if the card's predecessor is already in foundations and moves it if it is
    moveCard :: Card -> EOBoard -> EOBoard
    moveCard card ([],cell,tableau) = ([],cell,tableau)
    moveCard card ((hFoundation:tFoundation),cell,tableau)
        | head hFoundation == pCard card = (((card:hFoundation):tFoundation),delete card cell,removeHead card tableau)      -- Adds the card to foundations & removes it from tableau/cell
        | otherwise                      = ((hFoundation:newFound),newCell,newTableau)                                      -- Recurse to the next list of cards to check if the predecessor is there
        where
            (newFound, newCell, newTableau) = moveCard card (tFoundation,cell,tableau)