[![CircleCI](https://circleci.com/gh/davidsmts/num-linalg/tree/master.svg?style=svg)](https://circleci.com/gh/davidsmts/num-linalg/tree/master)

# num-linalg
Numerical algebraic computation in Haskell
aka collection of weird bugs or error messages

# Current state
Just a work in progress

# Get Started
- Get Stack : https://docs.haskellstack.org/en/stable/README/
- stack setup
- stack build
- stack exec num-linalg-exe
- ghci LU.hs -i ../Matrix ../Vector

# Contents
Up Until now only with Integer
- transpose
- matrix multiplication
- handy functions to create matrix / vector
- LU Splitting
- Unit
- certain diagonal
- quadratic matrix

# ToDo's
- Handy Shit:
  - Use Num Type instead of fixating on Int :(
  - Basic Matrix math with easy to use operators
  - Create Matrix:
    - Lower Triangle
    - Upper Triangle
    - Diagonal zero
    - split function that gives out two triangulars
    
- Direct linear solving:
  - to come
  
- Matrix factorization:
  - QR
  - LU
  - Cholesky
  -
  
- Problems with eigenvalues:
  - vector iteration
  - QR iteration
  
- Fault analysis:
