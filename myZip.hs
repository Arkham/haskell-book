module MyZip where

myZip :: [a] -> [b] -> [(a, b)]
myZip = myZipWith (,)

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith fn as bs = reverse $ myZipWith' fn as bs []

myZipWith' :: (a -> b -> c) -> [a] -> [b] -> [c] -> [c]
myZipWith' _ [] _ acc = acc
myZipWith' _ _ [] acc = acc
myZipWith' fn (a:as) (b:bs) acc =
  myZipWith' fn as bs (fn a b : acc)
