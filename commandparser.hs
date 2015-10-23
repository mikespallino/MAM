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
data ValidCommand = Look | Take | Use | Move | Talk | INVALID_COMMAND deriving (Show)

-- Take a string and return a Valid Command
verifyCommand :: String -> ValidCommand
verifyCommand cmd = do
    case cmd of
        "look" -> Look
        "take" -> Take
        "use"  -> Use
        "move" -> Move
        "talk" -> Talk
        _      -> INVALID_COMMAND

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
        Talk -> plyCmd
        INVALID_COMMAND -> (PlayerCommand (player plyCmd) (inputStr plyCmd) ("Invalid command provided."))


locations = Map.fromList [("HeinsVille", Map.fromList [("NuclearReactor", "What do you think a nuclear reactor looks like?"),
                                                        ("BigMacBay", "A heart attack waiting to happen."),
                                                        ("HiddenValley","Yes like the dressing, idiot.")]),
                          ("DillCity",  Map.fromList[("CBSHeadquarters", "TV news station. Left of mayo affiliation.")]),
                          ("Saltropolis", Map.fromList[("Cobbler's main office", "There ain't even a window.")])]

items = Map.fromList [("HeinsVille", ["food", "dog"])]