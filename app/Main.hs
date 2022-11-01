{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Foldable (Foldable (foldl'))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Void
import Replace.Megaparsec
import System.IO
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

main :: IO ()
main = do
  content <- openFileWithEncoding "./EIJIRO-1448.TXT" "cp932"

  let defLines = T.lines content
  let res = T.unlines [streamEdit (match pronunciation) solve defLine | defLine <- defLines]

  writeUTF "./EIJIRO-1448-Jones-Phonetic-Alphabet.TXT" res

pronunciation :: Parsec Void T.Text T.Text
pronunciation = do
  chunk "【発音】" <|> chunk "【発音！】"
  let isEnd c = '、' /= c
  takeWhile1P Nothing isEnd

solve :: (T.Text, b) -> T.Text
solve (s, _) = replaceWithList s replacementRule

replacementRule :: [(T.Text, T.Text)]
replacementRule = [("`", ""), ("`", ""), ("э", "ə"), ("∫", "ʃ"), ("Λ", "ʌ"), ("ｏ", "ɔ"), ("δ", "ð"), ("α", "ɑ"), ("з", "ʒ"), ("ae'", "ǽ"), ("ae", "æ"), ("a'", "á"), ("e'", "é"), ("i'", "í"), ("o'", "ó"), ("u'", "ú"), ("ɑ'", "ɑ́"), ("ɔ'", "ɔ́"), ("ʌ'", "ʌ́"), ("ə'", "ə́")]

replaceWithList :: Foldable t => T.Text -> t (T.Text, T.Text) -> T.Text
replaceWithList = foldl' replaceWithTuple

replaceWithTuple :: T.Text -> (T.Text, T.Text) -> T.Text
replaceWithTuple s (n, h) = T.replace n h s

openFileWithEncoding :: FilePath -> String -> IO T.Text
openFileWithEncoding filepath encoding = do
  h <- openFile filepath ReadMode
  textEncoding <- mkTextEncoding encoding
  hSetEncoding h textEncoding
  TIO.hGetContents h

writeUTF :: FilePath -> T.Text -> IO ()
writeUTF filepath text = do
  h <- openFile filepath WriteMode
  hSetEncoding h utf8
  TIO.hPutStr h text
  hClose h