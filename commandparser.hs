import Text.ParserCombinators.Parsec

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