module MiniCSV (
    -- * Encoding
    csvEncodeTable,
    csvEncodeRow,
    csvEncodeField,

    -- * Decoding
    csvDecodeTable,
) where

-------------------------------------------------------------------------------
-- Encoding
-------------------------------------------------------------------------------

csvEncodeTable :: [[String]] -> String
csvEncodeTable = concatMap (\r -> csvEncodeRow r ++ "\r\n")

csvEncodeRow :: [String] -> String
csvEncodeRow []     = ""
csvEncodeRow [x]    = csvEncodeField x
csvEncodeRow (x:xs) = csvEncodeField x ++ "," ++ csvEncodeRow xs

csvEncodeField :: String -> String
csvEncodeField xs
  | any (`elem` xs) ",\"\n\r"
  = '"' : go xs -- opening quote

  | otherwise = xs
  where
    go []         = '"' : []
    go ('"' : ys) = '"' : '"' : go ys
    go (y : ys)   = y : go ys

-------------------------------------------------------------------------------
-- Decoding
-------------------------------------------------------------------------------

-- | Decode CSV trying to recover as much as possible.
csvDecodeTable :: String -> [[String]]
csvDecodeTable []                 = []
csvDecodeTable ('\r' : '\n' : cs) = csvDecodeTable cs
csvDecodeTable ('\r'        : cs) = csvDecodeTable cs
csvDecodeTable ('\n'        : cs) = csvDecodeTable cs
csvDecodeTable (','         : cs) = csvDecodeField ("" :) cs
csvDecodeTable ('"'         : cs) = csvDecodeEscapedField id id cs
csvDecodeTable (c           : cs) = csvDecodeUnescapedField (c :) id cs

csvDecodeField :: ([String] -> [String]) -> String -> [[String]]
csvDecodeField accR ('\r' : '\n' : cs) = accR [""] : csvDecodeTable cs
csvDecodeField accR ('\r'        : cs) = accR [""] : csvDecodeTable cs
csvDecodeField accR ('\n'        : cs) = accR [""] : csvDecodeTable cs
csvDecodeField accR ('"'         : cs) = csvDecodeEscapedField id accR cs
csvDecodeField accR (','         : cs) = csvDecodeField (accR . ("" :)) cs
csvDecodeField accR (c           : cs) = csvDecodeUnescapedField (c :) accR cs
csvDecodeField accR []                 = [accR []]

csvDecodeEscapedField :: (String -> String) -> ([String] -> [String]) -> String -> [[String]]
csvDecodeEscapedField accF accR ('"' : '"' : cs) = csvDecodeEscapedField (accF . ('"' :)) accR cs
csvDecodeEscapedField accF accR ('"' : cs)       = csvDecodeAfterEscapedField (accR . (accF "" :)) cs
csvDecodeEscapedField accF accR (c   : cs)       = csvDecodeEscapedField (accF . (c :))   accR cs
csvDecodeEscapedField accF accR []               = [accR [accF ""]]

-- expected: EOF, EOL or ,
csvDecodeAfterEscapedField :: ([String] -> [String]) -> String -> [[String]]
csvDecodeAfterEscapedField accR [] = [accR []]
csvDecodeAfterEscapedField accR ('\r' : '\n' : cs) = accR [] : csvDecodeTable cs
csvDecodeAfterEscapedField accR ('\r'        : cs) = accR [] : csvDecodeTable cs
csvDecodeAfterEscapedField accR ('\n'        : cs) = accR [] : csvDecodeTable cs
csvDecodeAfterEscapedField accR (','         : cs) = csvDecodeField accR cs
csvDecodeAfterEscapedField accR (_           : cs) = csvDecodeAfterEscapedField accR cs

csvDecodeUnescapedField :: (String -> String) -> ([String] -> [String]) -> String -> [[String]]
csvDecodeUnescapedField accF accR (','         : cs) = csvDecodeField (accR . (accF "" :)) cs
csvDecodeUnescapedField accF accR ('\r' : '\n' : cs) = accR [accF ""] : csvDecodeTable cs
csvDecodeUnescapedField accF accR ('\r'        : cs) = accR [accF ""] : csvDecodeTable cs
csvDecodeUnescapedField accF accR ('\n'        : cs) = accR [accF ""] : csvDecodeTable cs
csvDecodeUnescapedField accF accR (c           : cs) = csvDecodeUnescapedField (accF . (c :)) accR cs
csvDecodeUnescapedField accF accR []                 = [accR [accF ""]]
