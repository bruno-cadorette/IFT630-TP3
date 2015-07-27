module HashCache (getHashTable,insertH,getValue,getList) where

import qualified Data.HashTable.IO as H
type HashTable k v = H.BasicHashTable k v

getHashTable :: IO (HashTable String Int)
getHashTable = do
    ht <- H.new
    return ht

insertH ht key val =
     H.insert ht key val
getValue ht key =
    H.lookup ht key

getList ht =
    H.toList ht
