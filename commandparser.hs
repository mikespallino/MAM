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
data ValidCommand = Look | Take | Use | Move | Talk | Items | Inventory | Help | INVALID_COMMAND deriving (Show)

-- Take a string and return a Valid Command
verifyCommand :: String -> ValidCommand
verifyCommand cmd = do
    case cmd of
        "look"      -> Look
        "take"      -> Take
        "use"       -> Use
        "move"      -> Move
        "talk"      -> Talk
        "items"     -> Items
        "inventory" -> Inventory
        "help"      -> Help
        _           -> INVALID_COMMAND

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
        Look -> if Map.member (concat params) validLookables then (PlayerCommand (player plyCmd) (inputStr plyCmd) (fromJust (Map.lookup (concat params) validLookables))) else (PlayerCommand (player plyCmd) (inputStr plyCmd) (curloc ++ " doesn't have that place.")) 
        Take -> if elem (concat params) (fromJust (Map.lookup curloc items)) then (PlayerCommand (Player (location (player plyCmd)) (inventory (player plyCmd) ++ [concat(params)])) (inputStr plyCmd) ("Took " ++ (concat params) ++ ".")) else (PlayerCommand (player plyCmd) (inputStr plyCmd) ("You can't take that."))
        Use  ->  plyCmd
        Move -> if Map.member (concat params) locations then
                    (PlayerCommand (Player (concat params) (inventory (player plyCmd))) (concat params) ("Moved to " ++ concat params))
                else
                    (PlayerCommand (player plyCmd) (inputStr plyCmd) ("Invalid location provided."))
        Items -> (PlayerCommand (player plyCmd) (inputStr plyCmd) (Data.List.intercalate ", " (fromJust (Map.lookup curloc items))))
        Inventory -> (PlayerCommand (player plyCmd) (inputStr plyCmd) (Data.List.intercalate ", " (inventory (player plyCmd))))
        Help -> (PlayerCommand (player plyCmd) (inputStr plyCmd) helpStr)
        INVALID_COMMAND -> (PlayerCommand (player plyCmd) (inputStr plyCmd) ("Invalid command provided."))


locations = Map.fromList [("HeinsVille", Map.fromList [("NuclearReactor", "What do you think a nuclear reactor looks like?"),
                                                        ("BigMacBay", "A heart attack waiting to happen."),
                                                        ("HiddenValley","Yes like the dressing, idiot.")]),
                          ("DillCity",  Map.fromList[("CBSHeadquarters", "TV news station. Left of mayo affiliation.")]),
                          ("Saltropolis", Map.fromList[("Cobbler's main office", "There ain't even a window.")])]

items = Map.fromList [("HeinsVille", ["food", "dog"])]

helpStr = "\nValid Commands:\n\tlook <LOCATION>\n\ttake <ITEM_NAME>\n\tuse <ITEM_NAME>\n\tmove <LOCATION>\n\titems\n\tinventory\n\thelp"


play p1 plyCmd = do
    let resCmd = getCommand plyCmd
    putStrLn (result resCmd)
    putStr "-> "
    theCommand <- getLine
    let newCmd = PlayerCommand (player resCmd) (theCommand) (result resCmd)
    play p1 newCmd

main = do
    putStrLn "Valid Commands:\n\tlook <LOCATION>\n\ttake <ITEM_NAME>\n\tuse <ITEM_NAME>\n\tmove <LOCATION>\n\titems\n\tinventory\n\t\t\tCBS NEWS: **MAYO STOLEN!**\n\t\"Sandman, who works at the Nuclear Reactor here in HeinsVille, had his   famous Metallic Atomic Mayo stolen! For those unaware, Sandman eats a sandwhich everyday at work and recently he knocked his jar of mayo into the reactor       creating Metallic Atomic Mayo!\n\tBecause Metallic Atomic Mayo doesn't decay, this became a very lucrative business. Sandman made millions off of his expiration date free product. For   obvious reasons he had to be protective of this amazing mayonnaise. Alas, his   protection contracted from the Secret Service was not enough. The mayo was      stolen by the most notorious villain in HeinsVille, Rueben.\"\n\nYou are a Private Investigator hired to find the MAM. You've been living in     HeinsVille for the last 5 years and have been very good friends with Sandman    since you moved here and that is why he contacted you.\n\nYou MUST find that MAYO. If you forget the commands, type \"help\"."
    putStr "-> "
    theCommand <- getLine
    let p1 = Player "HeinsVille" ["note"]
    let plyCmd = PlayerCommand p1 theCommand ""
    play p1 plyCmd
