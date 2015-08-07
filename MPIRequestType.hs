{-# LANGUAGE DeriveGeneric #-}
module MPIRequestType (SenderRank, MPIRequestType(..),Action) where

import ChessPiece
import GHC.Generics
import Data.Serialize
import ChessSerializer

type Action = String
type SenderRank = Int
data MPIRequestType =
    GetCache ChessGame SenderRank |
    SetCache (ChessGame,Int) SenderRank |
    GetGameResult Integer (Action, ChessGame) |
    ReturnGameResult (Action, Int) SenderRank |
    CancelComputation deriving (Generic)

instance Serialize MPIRequestType
