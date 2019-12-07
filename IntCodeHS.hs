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

foreign import ccall "intcode.h runcode_basic"
  c_runcode_basic :: Ptr CInt -> CInt -> CInt -> CInt -> IO CInt

foreign import ccall "intcode.h runcode"
  c_runcode :: Ptr CInt -> CInt -> Ptr CInt -> IO CInt

-- foreign import ccall "intcode.h runcode_new"
  -- c_runcode_new :: Ptr CInt -> CInt -> Ptr CInt -> IO (Ptr CInt)

-- foreign import ccall "intcode.h resume"
  -- c_resume :: Ptr CInt -> Ptr CInt -> IO (Ptr CInt)

runCodeBasic :: [CInt] -> CInt -> CInt -> IO CInt
runCodeBasic prog noun verb = do
  withArrayLen prog $ \len prog' -> c_runcode_basic prog' (fromIntegral len) noun verb

runCode :: [CInt] -> [CInt] -> IO CInt
runCode prog input = do
  withArrayLen prog $ \len prog' -> withArrayLen input $ \_ input' -> c_runcode prog' (fromIntegral len) input'

-- runCodeNew :: [CInt] -> [CInt] -> IO (Maybe CInt)
-- runCodeNew prog input = do
  -- withArrayLen prog $ \len prog' -> withArrayLen input $ \_ input' -> do
    -- out <- c_runcode_new prog' (fromIntegral len) input'
    -- if out == nullPtr
    -- then return Nothing
    -- else Just <$> peek out

-- resume :: CInt -> [CInt] -> IO (Maybe CInt)
-- resume state input = do
  -- withArrayLen input $ \_ input' -> do
    -- out <- c_resume (Ptr state) input'
    -- if out == nullPtr
    -- then return Nothing
    -- else Just <$> peek out
