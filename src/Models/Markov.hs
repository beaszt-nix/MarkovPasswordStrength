{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Models.Markov where

import qualified Data.Trie.Text                as TT
import           Data.Trie.Text.Convenience     ( insertWith'
                                                , unionWith'
                                                )
import qualified Data.Text                     as T
import           Data.Ratio                     ( numerator
                                                , denominator
                                                , (%)
                                                )
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Trans.State
import           Data.Maybe
import           Control.Parallel
import           Data.Semigroup                 ( Sum(..) )
import           Data.List                      ( inits )
import           Data.Binary                    ( Binary(..) )
import           Control.Exception

data MarkovState a = MarkovState {
                      trie :: TT.Trie a,
                      alphabet :: [T.Text]
                   } deriving Show

instance Binary a => Binary (MarkovState a) where
  get = do
    alphabet' <- Data.Binary.get
    trie'     <- Data.Binary.get
    return MarkovState { trie = trie', alphabet = alphabet' }
  put ms = do
    Data.Binary.put $ alphabet ms
    Data.Binary.put $ trie ms

type EvalMarkov = StateT (MarkovState Integer) IO

getNGrams :: Int -> T.Text -> [T.Text]
getNGrams _ "" = []
getNGrams n t  = (\(a, b) -> a : getNGrams n b) $ T.splitAt n t

getUptoNGrams n text =
  let res = getNGrams n text
      x   = head res
      xs  = tail res
  in  if length res == 0 then res else concat [(tail $ T.inits x), xs]

addPassw :: Int -> T.Text -> EvalMarkov ()
addPassw n text = do
  s <- Control.Monad.Trans.State.get
  let k = getUptoNGrams n text
      t = foldr addOne (trie s) k
  k `pseq` t `pseq` modify' $ \s -> s { trie = t }
  where addOne !a !b = insertWith' (+) a 1 b

probability :: T.Text -> EvalMarkov Rational
probability text = do
  ms <- Control.Monad.Trans.State.get
  let t     = trie ms
      sigma = alphabet ms
  let numera = lookupSub text t
      prefix = T.init text
      denomin =
        getSum $ foldMap (\s -> Sum $ lookupSub (T.concat [prefix, s]) t) sigma
  if denomin == 0 then return 0 else return $ numera % denomin
  where lookupSub s trie = fromMaybe 0 $ TT.lookup s trie

passwordStrength :: Int -> T.Text -> EvalMarkov Double
passwordStrength n text = do
  inside <- product <$> ((sequence . map probability . getUptoNGrams n) $ text)
  return (den' inside - num' inside)
 where
  den' = logBase 2 . fromInteger . denominator
  num' = logBase 2 . fromInteger . numerator

emptyMarkovState :: T.Text -> IO (MarkovState Integer)
emptyMarkovState text = return MarkovState
  { trie     = TT.empty
  , alphabet = map (\c -> T.pack [c]) $ T.unpack text
  }

mergeMarkovState
  :: MarkovState Integer -> MarkovState Integer -> MarkovState Integer
mergeMarkovState a b =
  let at = trie a
      bt = trie b
  in  at `par` bt `pseq` a { trie = unionWith' (+) at bt }
