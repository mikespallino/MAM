import Text.ParserCombinators.Parsec
import Data.Typeable
import Data.List
import qualified Data.Map as Map
import Data.Maybe


--
-- Parser for player input
--

-- Each line contains 1 or more cells, separated by a space
line :: GenParser Char st [String]
line = 
    do result <- cells
       return result
       
-- Build up a list of cells.  Try to parse the first cell, then figure out 
-- what ends the cell.
cells :: GenParser Char st [String]
cells = 
    do first <- cellContent
       next <- remainingCells
       return (first : next)

-- The cell either ends with a space, indicating that 1 or more cells follow,
-- or it doesn't, indicating that we're at the end of the cells for this line
remainingCells :: GenParser Char st [String]
remainingCells =
    (char ' ' >> cells)            -- Found space?  More cells coming
    <|> (return [])                -- No space?  Return [], no more cells

-- Each cell contains 0 or more characters, which must not be a space
cellContent :: GenParser Char st String
cellContent = 
    many (noneOf " ")

parseText :: String -> Either ParseError [String]
parseText input = parse line "(unknown)" input

--
-- HERE BE GAME STUFFS
--

data Player = Player {
    location :: String,
    inventory :: [String]
} deriving (Show)

data PlayerCommand = PlayerCommand {
    player :: Player,
    inputStr :: String,
    result :: String
} deriving (Show)

-- Valid Command data structure set
data ValidCommand = Look | LookAround | Take | Use | Move | Talk | Items | Inventory | Help | Exit | INVALID_COMMAND deriving (Show)

-- Take a string and return a Valid Command
verifyCommand :: String -> ValidCommand
verifyCommand cmd = do
    case cmd of
        "look"       -> Look
        "lookaround" -> LookAround
        "take"       -> Take
        "use"        -> Use
        "move"       -> Move
        "talk"       -> Talk
        "items"      -> Items
        "inventory"  -> Inventory
        "help"       -> Help
        "exit"       -> Exit
        _            -> INVALID_COMMAND

-- Parse user input and verify the first element is a valid command.
getCommand :: PlayerCommand -> PlayerCommand
getCommand plyCmd = do
    -- parse out player input string
    let input = parseText (inputStr plyCmd)
    -- get the command from the first word of input
    let cmd = verifyCommand (either (\_ -> "invalid") (head) (input))
    -- grab the rest of the command
    let params = either (\_ -> ["invalid"]) (tail) (input)
    -- store the location of the player
    let curloc = location (player plyCmd)
    -- get valid places to look at from player's location
    let validLookables = fromJust (Map.lookup curloc locations)
    -- get valid items to take from player's location
    let validTakables = fromJust (Map.lookup curloc items)
    case cmd of
        -- return the item description if it exists
        Look -> if Map.member (Data.List.intercalate " " params) validLookables then (PlayerCommand (player plyCmd) (inputStr plyCmd) (fromJust (Map.lookup (Data.List.intercalate " " params) validLookables))) else (PlayerCommand (player plyCmd) (inputStr plyCmd) (curloc ++ " doesn't have that place.")) 
        LookAround -> (PlayerCommand (player plyCmd) (inputStr plyCmd) (Data.List.intercalate ", " (Map.keys validLookables)))
        Take -> if elem (Data.List.intercalate " " params) (fromJust (Map.lookup curloc items)) then (PlayerCommand (Player (location (player plyCmd)) (inventory (player plyCmd) ++ [(Data.List.intercalate " "(params))])) (inputStr plyCmd) ("Took " ++ (Data.List.intercalate " " params) ++ ".")) else (PlayerCommand (player plyCmd) (inputStr plyCmd) ("You can't take that."))
        Use  ->  plyCmd
        Move -> if Map.member (Data.List.intercalate " " params) locations then
                    (PlayerCommand (Player (Data.List.intercalate " " params) (inventory (player plyCmd))) (Data.List.intercalate " " params) ("Moved to " ++ (Data.List.intercalate " " params)))
                else
                    (PlayerCommand (player plyCmd) (inputStr plyCmd) ("Invalid location provided."))
        Items -> (PlayerCommand (player plyCmd) (inputStr plyCmd) (Data.List.intercalate ", " (fromJust (Map.lookup curloc items))))
        Inventory -> (PlayerCommand (player plyCmd) (inputStr plyCmd) (Data.List.intercalate ", " (inventory (player plyCmd))))
        Help -> (PlayerCommand (player plyCmd) (inputStr plyCmd) helpStr)
        Exit -> (PlayerCommand (player plyCmd) (inputStr plyCmd) "Bye!")
        INVALID_COMMAND -> (PlayerCommand (player plyCmd) (inputStr plyCmd) ("Invalid command provided."))


locations = Map.fromList [("HeinsVille", Map.fromList [("Nuclear Reactor", "The Nuclear Reactor where Sandman works. The birth place of Metallic Atomic Mayo. It looks like there may be some footprints on the ground headed to the lunch room. Maybe you should take a look."),
                                                        ("Big Mac Bay", "It looks like there are a thousand islands out on the horizon."),
                                                        ("Hidden Valley","There is nothing of interest here.")]),
                          ("Nuclear Reactor", Map.fromList [("Lunch Room", "This is where the mayo was stolen. The footprints head off to Sandmans office.  Sandman's turkey sandwich lays in a state of dissarray. It lacks structure      without the mayo. Who could do such a thing?"),
                                                            ("Sandman's Office", "The footprints lead up to his locker. It looks like there is Swiss Cheese on the locker. This must be Reuben's handy work! He won't get away with it this time. He lives in a penthouse in Dill City if I remember correctly.")]),
                          ("Dill City",  Map.fromList[("CBS Headquarters", "TV news station. Left of mayo affiliation."),
                                                      ("Reuben's Penthouse", "For the leader of the crime syndicate in HeinsVille, this place seems pretty simple. Oh looks like the door is open!")]),
                          ("Reuben's Penthouse", Map.fromList[("Kitchen", "Ugh, this place is a mess... How can people just leave dishes in the sink?"),
                                                              ("Table", "A sandwhich? Could Reuben really be this stupid? And what is this piece of scrap paper?")]),
                          ("Saltropolis", Map.fromList[("Cobbler's main office", "There ain't even a window.")])]

items = Map.fromList [("HeinsVille", ["food", "dog"]),
                      ("Dill City", ["pickles", "paper"]),
                      ("Saltropolis", ["grain of salt", "sandwich"]),
                      ("Hidden Valley", ["Metallic Atomic Mayo"]),
                      ("Nuclear Reactor", ["swiss cheese"]),
                      ("Reuben's Penthouse", ["letter", "Reuben's sandwich"])]

helpStr = "\nValid Commands:\n\n\tlook <LOCATION>\t\t(Look at a location)\n\tlookaround\t\t(List the locations in your area)\n\ttake <ITEM_NAME>\t(Take an item in your location)\n\tuse <ITEM_NAME>\t\t(Use an item in your inventory)\n\tmove <LOCATION>\t\t(Move to a new location)\n\titems\t\t\t(List the items in your location)\n\tinventory\t\t(List the items in your inventory)\n\thelp\t\t\t(Show this message)\n"


play p1 plyCmd = do
    let resCmd = getCommand plyCmd
    putStrLn (result resCmd)
    putStr "-> "
    theCommand <- getLine
    if (theCommand /= "exit") then 
        if "Metallic Atomic Mayo" `elem` inventory (player resCmd) then
            putStrLn "You solved the mystery, great work!"
        else
            (play p1 (PlayerCommand (player resCmd) (theCommand) (result resCmd))) 
    else
        putStrLn "\nBye!"

main = do
    putStrLn "Welcome to Metallic Atomic Mayo!\n\n\n\t\t\tCBS NEWS: **MAYO STOLEN!**\n\n\t\"Sandman, who works at the Nuclear Reactor here in HeinsVille, had his   famous Metallic Atomic Mayo stolen! For those unaware, Sandman eats a sandwhich everyday at work and recently he knocked his jar of mayo into the reactor       creating Metallic Atomic Mayo!\n\tBecause Metallic Atomic Mayo doesn't decay, this became a very lucrative business. Sandman made millions off of his expiration date free product. For   obvious reasons he had to be protective of this amazing mayonnaise. Alas, his   protection contracted from the Secret Service was not enough. The mayo was      stolen by the most notorious villain in HeinsVille, Rueben.\"\n\n\nYou are a Private Investigator hired to find the MAM. You've been living in     HeinsVille for the last 5 years and have been very good friends with Sandman    since you moved here and that is why he contacted you.\n\nYou MUST find that MAYO.\n\nFor a list of commands, type \"help\"."
    putStr "-> "
    theCommand <- getLine
    let p1 = Player "HeinsVille" ["note"]
    let plyCmd = PlayerCommand p1 theCommand ""
    if (theCommand /= "exit") then (play p1 plyCmd) else putStrLn "\nBye!"
