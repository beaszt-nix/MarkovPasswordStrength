{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module PassPredict.Utils
  ( createWeights
  , loadWeights
  , writeWeights
  )
where

import           Models.Markov
import           Control.Monad                  ( mapM_
                                                , forM_
                                                )
import           Control.Monad.Trans.State      ( execStateT
                                                , evalStateT
                                                )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Data.List                      ( splitAt )
import           Data.Maybe                     ( fromMaybe )
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Parallel
import           Data.Binary                    ( encodeFile
                                                , decodeFile
                                                )
import           System.Directory               ( getDirectoryContents )
import           GHC.Conc                       ( getNumCapabilities )

procFile :: FilePath -> EvalMarkov ()
procFile f = do
  res <- liftIO $ (T.lines <$> TIO.readFile f)
  liftIO $ print f
  forM_ res $ addPassw 5

procFiles :: MarkovState Integer -> [FilePath] -> IO (MarkovState Integer)
procFiles ms [] = print "files finished" >> return ms
procFiles ms ls = do
  let (current, next) = splitAt 100 ls
  f <- execStateT (forM_ current procFile) ms
  f `pseq` procFiles f next

writeWeights :: String -> MarkovState Integer -> IO ()
writeWeights s = encodeFile (s ++ ".wt")

loadWeights :: FilePath -> IO (MarkovState Integer)
loadWeights fpath = decodeFile fpath

createWeights :: FilePath -> T.Text -> IO (MarkovState Integer)
createWeights fpath alphabet = do
  res <- map (fpath ++) . drop 2 <$> getDirectoryContents fpath
  ms  <- emptyMarkovState alphabet
  n   <- getNumCapabilities
  let len = div (length res) n
  pars <- mapM (procFiles ms) $ chunksOf len res
  let k = foldr mergeMarkovState ms pars
  k `pseq` return k

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n x  = let (l, r) = splitAt n x in l : chunksOf n r
