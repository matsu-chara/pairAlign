module Main where 
  import PairwiseAlignment

  a = [G, A, T, T, A]
  b = [G, A, A, T, T, C]

  main = 
    print $ calcF a b