import IntCodeHS

main = do
  p <- readProg
  print =<< runCodeBasic p 12 2
