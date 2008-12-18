{-# LANGUAGE MagicHash, ForeignFunctionInterface #-}

module Data.Unique.Id (

  Id, hashedId, IdSupply, initIdSupply, splitIdSupply, idFromSupply

 ) where

import GHC.Exts
import GHC.IOBase ( unsafeDupableInterleaveIO )

newtype Id = Id { hashedId :: Int }

data IdSupply = IdSupply Int# IdSupply IdSupply

initIdSupply :: Char -> IO IdSupply
initIdSupply (C# c) =
 case uncheckedIShiftL# (ord# c) (unboxedInt 24) of
  mask ->
   let mk_supply =
        unsafeDupableInterleaveIO (
         genSymZh  >>= \ (I# u) ->
         mk_supply >>= \ l ->
         mk_supply >>= \ r ->
         return (IdSupply (word2Int# (or# (int2Word# mask) (int2Word# u))) l r))
    in mk_supply

splitIdSupply :: IdSupply -> (IdSupply,IdSupply)
splitIdSupply (IdSupply _ l r) = (l,r)

idFromSupply :: IdSupply -> Id
idFromSupply (IdSupply n _ _) = Id (I# n)

instance Eq Id where Id (I# x) == Id (I# y) = x ==# y

instance Ord Id
 where
  Id (I# x) <  Id (I# y) = x <#  y
  Id (I# x) <= Id (I# y) = x <=# y

  compare (Id (I# x)) (Id (I# y)) =
   if x ==# y then EQ else if x <# y then LT else GT

instance Show Id
 where
  showsPrec _ i s = case unpackId i of (c,n) -> c:show n++s




unboxedInt :: Int -> Int#
unboxedInt (I# x) = x

foreign import ccall unsafe "genSymZh" genSymZh :: IO Int

unpackId :: Id -> (Char,Int)
unpackId (Id (I# i)) =
 let tag = C# (chr# (uncheckedIShiftRL# i (unboxedInt 24)))
     num = I# (word2Int# (and# (int2Word# i) 
                               (int2Word# (unboxedInt 16777215))))
  in (tag, num)
