{-# LANGUAGE OverloadedStrings  #-}

module Main where

import           PassPredict.Utils
import           Models.Markov
import           System.Directory               ( getDirectoryContents )
import           Data.Maybe                     ( fromMaybe )
import           System.IO
import qualified Data.Text.IO                  as TIO
import qualified Data.Text                     as T
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Trans.State


read' :: Maybe String -> EvalMarkov T.Text
read' prompt = do
  let k = concat ["PassPredict ", fromMaybe "" prompt, ":> "]
  liftIO $ putStr k
  liftIO $ hFlush stdout
  liftIO $ TIO.getLine

eval :: T.Text -> EvalMarkov (Maybe Double)
eval input = do
  case T.words input of
    ["addPassword", pw] -> do
      addPassw 5 pw
      return Nothing
    ["loadWeights", path] -> do
      res <- liftIO $ loadWeights (read $ T.unpack path)
      put res
      return Nothing
    ["writeWeights", str] -> do
      res <- get
      liftIO $ writeWeights (read $ T.unpack str) res
      return Nothing
    ["createWeights", str, charset] -> do
      res <- liftIO $ createWeights (T.unpack str) (read $ T.unpack charset)
      put res
      return Nothing
    ["passwordStrength", text] ->
      Just <$> passwordStrength 5 (read $ T.unpack text)
    ["exit"]  -> return $ Just (-1)
    otherwise -> return Nothing


loop :: EvalMarkov ()
loop = do
  line <- read' Nothing
  res  <- eval line
  case res of
    Just d -> if d < 0
      then return ()
      else (liftIO $ putStrLn ("Password Strength is " ++ show d)) >> loop
    Nothing -> loop

input :: String -> IO T.Text
input s = do
  putStr s
  hFlush stdout
  TIO.getLine

main :: IO ()
main = do
  let
    alph =
      "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!@#$%^&*( "
  res  <- emptyMarkovState alph >>= execStateT loop
  yOrn <- input "Save model?"
  if yOrn == "y"
    then do
      fname <- input "Enter Output Name "
      writeWeights (T.unpack fname) res
    else return ()
