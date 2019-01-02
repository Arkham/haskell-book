module Employee where

data Employee = Coder
              | Manager
              | Veep
              | CEO
              deriving (Eq, Ord, Show)

reportBoss :: Employee -> Employee -> IO ()
reportBoss e e' =
  putStrLn $ show e ++
    " is the boss of " ++
      show e'

employeeRank :: Employee -> Employee -> IO ()
employeeRank =
  employeeCustomRank compare

employeeCustomRank :: (Employee -> Employee -> Ordering )
                   -> Employee
                   -> Employee
                   -> IO ()
employeeCustomRank compareFn e e' =
  case compareFn e e' of
    GT -> reportBoss e e'
    EQ -> putStrLn "Neither employee is the boss"
    LT -> reportBoss e' e

coderUberAlles :: Employee -> Employee -> Ordering
coderUberAlles Coder Coder = EQ
coderUberAlles Coder _ = GT
coderUberAlles _ Coder = LT
coderUberAlles e e' = compare e e'
