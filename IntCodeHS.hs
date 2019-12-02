{-# LANGUAGE ForeignFunctionInterface #-}

module IntCodeHS where

import Foreign
import Foreign.C.Types

splitBy :: Char -> String -> [CInt]
splitBy c s = case break (== c) s of
                (t, s') -> (read t):(case s' of
                                       []    -> []
                                       _:s'' -> splitBy c s'')

readProg :: IO [CInt]
readProg = splitBy ',' <$> getLine

foreign import ccall "intcode.h runcode"
  c_runcode :: Ptr CInt -> CInt -> CInt -> CInt -> IO CInt

runCode :: [CInt] -> CInt -> CInt -> IO CInt
runCode prog noun verb = do
  withArrayLen prog $ \len prog' -> c_runcode prog' (fromIntegral len) noun verb
