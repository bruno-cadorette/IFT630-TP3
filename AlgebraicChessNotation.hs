import Data.List
import Data.Char
import Data.Maybe

algebricChessNotation (column:row) =
  (fromJust $ column `elemIndex` ['a'..'z'], digitToInt (head row) - 1)
