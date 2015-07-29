module ChessSerializer() where

import ChessPiece
import qualified Data.Map as Map
import Data.List.Split
import Data.Bits
import Data.Word
import Data.Maybe
import Data.Serialize
import qualified Data.ByteString as BStr

instance Serialize Board where
    put = put . encodeBoard
    get = fmap decodeBoard get

instance Serialize PieceColor    
 
instance Serialize ChessGame 
    
encodePiece :: Maybe ChessPiece -> Word8
encodePiece (Just (ChessPiece pType color))  =   fromIntegral(2 * fromEnum pType + fromEnum color + 1) 
encodePiece Nothing = 0

decodePiece :: Word8 -> Maybe ChessPiece
decodePiece 0 = Nothing
decodePiece n = 
    let 
        n' = fromIntegral n - 1
        color = toEnum $ fromIntegral n' `mod` 2
        pType = toEnum $ (n' - (fromEnum color)) `div` 2
    in  Just (ChessPiece pType color)

positions :: [Position]
positions = [(i,j)|i<-[0..7],j<-[0..7]]

encodeBoard :: Board -> BStr.ByteString
encodeBoard (Board board) = 
    BStr.pack $ map (\ [x,y] ->(shiftL (encodePiece x) 4) .|. encodePiece y) $ 
    chunksOf 2 $ map (\pos -> Map.lookup pos board  )  positions 

decodeBoard :: BStr.ByteString -> Board
decodeBoard str = 
    Board $ Map.fromList $ mapMaybe removeNoneValue $ concat $ 
    zipWith zip  (chunksOf 2 positions) (map unpackNumber $ BStr.unpack str)
    
removeNoneValue :: (a, Maybe b) -> Maybe (a,b)
removeNoneValue (k,v) = 
    case v of
    Just v' -> Just (k, v')
    Nothing -> Nothing

unpackNumber :: Word8 -> [Maybe ChessPiece]
unpackNumber n = 
    let 
        a = shiftR n 4
        b = n .&. 0xF
    in map decodePiece [a,b]