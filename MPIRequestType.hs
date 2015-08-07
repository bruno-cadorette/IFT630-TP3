module MPIRequestType (SenderRank, MPIRequestType(..)) where

type SenderRank = Int
data MPIRequestType =
    GetCache ChessGame SenderRank |
    SetCache (ChessGame,Int) SenderRank |
    GetGameResult Integer (Action, ChessGame) |
    ReturnGameResult (Action, ChessGame) SenderRank |
    CancelComputation deriving (Generic)