import IntCodeHS

runCode' p i j = do
  r <- runCodeBasic p i j
  return (r, i, j)

main = do
  p <- readProg
  results <- sequenceA $ runCode' p <$> [0..100] <*> [0..100]
  let (_, i', j') = head $ filter (\(r, _, _) -> r == 19690720) results
  putStrLn $ show i' ++ show j'
