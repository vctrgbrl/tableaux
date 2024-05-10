module Util
  ( split,
  )
where

split :: String -> Char -> [String]
split text spliter = _split text spliter ""

_split :: String -> Char -> String -> [String]
_split [] spliter ""  = []
_split [] spliter str  = [str]
_split (c : text) spliter str
  | c == spliter = str:_split text spliter ""
  | c /= spliter = _split text spliter (str ++ [c])