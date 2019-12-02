import IntCodeHS

main = do
  p <- readProg
  print =<< runCode p 12 2
