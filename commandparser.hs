import Text.ParserCombinators.Parsec
import Data.Typeable
import Data.List
import qualified Data.Map as Map


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
getCommand :: String -> Maybe String
getCommand str = do
    let input = parseText str
    let cmd = verifyCommand (either (\_ -> "invalid") (head) (input))
    let params = either (\_ -> ["invalid"]) (tail) (input)
    case cmd of
        Look -> if Map.member (concat params) locations then Map.lookup (concat params) locations else Just "The Metallic Atomic Mayo universe doesn't have that place." 
        Take -> Just "TAKEN."
        Use  -> Just "USED."
        Move -> Just "MOVED."
        Talk -> Just "TALKED."
        INVALID_COMMAND -> Just "Invalid input provided."

locations = Map.fromList [("HeinsVille", "Main city, oddly resembles a ketchup bottle..."), 
                          ("NuclearReactor", "What do you think a nuclear reactor looks like?"),
                          ("BigMacBay", "A heart attack waiting to happen."),
                          ("HiddenValley","Yes like the dressing, idiot."),
                          ("DillCity", "The pickle came from here."), 
                          ("CBSHeadquarters", "TV news station. Left of mayo affiliation."),
                          ("Saltropolis", "Who doesn't love salt?"),
                          ("Cobbler's main office", "There ain't even a window."),
                          ("Sugaria", "I have no idea what this place is for yet.")]