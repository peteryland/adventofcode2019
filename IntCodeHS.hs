{-# LANGUAGE ForeignFunctionInterface #-}

module IntCodeHS where

import Foreign
-- import Foreign.C.Types

splitBy :: Char -> String -> [Int64]
splitBy c s = case break (== c) s of
                (t, s') -> (read t):(case s' of
                                       []    -> []
                                       _:s'' -> splitBy c s'')

readProg :: IO [Int64]
readProg = splitBy ',' <$> getLine

foreign import ccall "intcode.h runcode_basic"
  c_runcode_basic :: Ptr Int64 -> Int64 -> Int64 -> Int64 -> IO Int64

foreign import ccall "intcode.h runcode"
  c_runcode :: Ptr Int64 -> Int64 -> Ptr Int64 -> IO Int64

-- foreign import ccall "intcode.h runcode_new"
  -- c_runcode_new :: Ptr Int64 -> Int64 -> Ptr Int64 -> IO (Ptr Int64)

-- foreign import ccall "intcode.h resume"
  -- c_resume :: Ptr Int64 -> Ptr Int64 -> IO (Ptr Int64)

runCodeBasic :: [Int64] -> Int64 -> Int64 -> IO Int64
runCodeBasic prog noun verb = do
  withArrayLen prog $ \len prog' -> c_runcode_basic prog' (fromIntegral len) noun verb

runCode :: [Int64] -> [Int64] -> IO Int64
runCode prog input = do
  withArrayLen prog $ \len prog' -> withArrayLen input $ \_ input' -> c_runcode prog' (fromIntegral len) input'

-- runCodeNew :: [Int64] -> [Int64] -> IO (Maybe Int64)
-- runCodeNew prog input = do
  -- withArrayLen prog $ \len prog' -> withArrayLen input $ \_ input' -> do
    -- out <- c_runcode_new prog' (fromIntegral len) input'
    -- if out == nullPtr
    -- then return Nothing
    -- else Just <$> peek out

-- resume :: Int64 -> [Int64] -> IO (Maybe Int64)
-- resume state input = do
  -- withArrayLen input $ \_ input' -> do
    -- out <- c_resume (Ptr state) input'
    -- if out == nullPtr
    -- then return Nothing
    -- else Just <$> peek out
