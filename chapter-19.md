# Applying structure

## Monoid

### Concatenating html fragments

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty

import Data.Monoid (mconcat)

main = scotty 3000 $
    get "/:word" $ do
        beam <- param "word"
        html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
```

### Concatenating connections params

```haskell
import Database.Persist.Postgresql (withPostgresqlConn)
import Web.Heroku (dbConnParams)
import Data.Monoid ((<>))

runDb :: SqlPersist (ResourceT IO) a -> IO a
runDb query = do
    params <- dbConnParams
    let connStr = foldr (\(k,v) t ->
        t <> (encodeUtf8 $ k <> "=" <> v <> " ")) "" params
    runResourceT . withPostgresqlConn connStr $ runSqlConn query
```

### Concatenating key configs

```haskell
import XMonad
import XMonad.Actions.Volume
import Data.Map.Lazy (fromList)
import Data.Monoid (mappend)

main = do
  xmonad def { keys =
    \c -> fromList [
      ((0, xK_F6),
        lowerVolume 4 >> return ()),
      ((0, xK_F7),
        raiseVolume 4 >> return ())
    ] `mappend` keys defaultConfig c
  }
```

We are merging functions?

```haskell
instance Monoid b => Monoid (a -> b)
```

See an example

```haskell
Prelude> import Data.Monoid
Prelude Data.Monoid> f = const (Sum 1)
Prelude Data.Monoid> g = const (Sum 2)
Prelude Data.Monoid> f 9001
Sum {getSum = 1}
Prelude Data.Monoid> g 9001
Sum {getSum = 2}
Prelude Data.Monoid> :t f <> g
f <> g :: Num a => b -> Sum a
Prelude Data.Monoid> f <> g $ 9001
Sum {getSum = 3}
```

Let's look at `fromList`

```haskell
Prelude> import qualified Data.Map as M
Prelude M> :t M.fromList
M.fromList :: Ord k => [(k, a)] -> M.Map k a
Prelude M> let f = M.fromList [('a', 1)]
Prelude M> let g = M.fromList [('b', 2)]
Prelude M> :t f
f :: Num a => M.Map Char a
Prelude M> import Data.Monoid
Prelude M Data.Monoid> f <> g
fromList [('a',1),('b',2)]
Prelude M Data.Monoid> :t f <> g
f <> g :: Num a => M.Map Char a
```

## Functor

### Lifting over IO

```haskell
import Data.Time.Clock

offsetCurrentTime :: NominalDiffTime -> IO UTCTime
offsetCurrentTime offset =
  fmap (addUTCTime (offset * 24 * 3600)) $ getCurrentTime

-- addUTCTime :: NominalDiffTime -> UTCTime -> UTCTime
-- getCurrentTime :: IO UTCTime
```

### Another example of lifting over IO

```haskell
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUIDv4

textUuid :: IO Text
textUuid =
fmap (T.pack . UUID.toString) UUIDv4.nextRandom

-- pack :: String -> Text
-- toString :: UUID -> String
-- nextRandom :: IO UUID
```

### Lifting over web app monads. WTF

```haskell
userAgent :: AppHandler (Maybe UserAgent)
UserAgent =
  (fmap . fmap) userAgent' getRequest

userAgent' :: Request -> Maybe UserAgent
UserAgent' =
  getHeader "User-Agent" req
```

## Applicative

### hgrev

```haskell
jsonSwitch :: Parser (a -> a)
jsonSwitch =
  infoOption $(hgRevStateTH jsonFormat) $ long "json"
  <> short 'J'
  <> help
       "Display JSON version information"

parserInfo :: ParserInfo (a -> a)
parserInfo =
  info (helper <*> verSwitch <* jsonSwitch) fullDesc
```

What is `<*`?

```haskell
Prelude> :t (<*)
(<*) :: Applicative f => f a -> f b -> f a
```

It looks very similar to `const`

```haskell
Prelude> :t const
const :: a -> b -> a
```

Let's see an example

```haskell
Prelude> (Just (+1)) <* (Just (*2)) <*> Just 4
Just 5
```

### More parsing

```haskell
parseJSON :: Value -> Parser a
(.:) :: FromJSON a
  => Object
  -> Text
  -> Parser a

instance FromJSON Payload where
  parseJSON (Object v) =
    Payload <$> v .: "from"
            <*> v .: "to"
            <*> v .: "subject"
            <*> v .: "body"
            <*> v .: "offset_seconds"
    parseJSON v = typeMismatch "Payload" v
```

## Something different

```haskell
module Web.Shipping.Utils ((<||>)) where

import Control.Applicative (liftA2)

(<||>) :: (a -> Bool)
       -> (a -> Bool)
       -> a
       -> Bool
(<||>) = liftA2 (||)
```

we can see it in action

```haskell
Prelude> f 9001 = True; f _ = False
Prelude> g 42 = True; g _ = False
Prelude> :t f
f :: (Eq a, Num a) => a -> Bool
Prelude> f 42
False
Prelude> f 9001
True
Prelude> g 42
True
Prelude> g 9001
False
Prelude> (f <||> g) 0
False
Prelude> (f <||> g) 42
True
Prelude> (f <||> g) 9001
True
```

## Monad

### Opening a network socket

```haskell
import Network.Socket

openSocket :: FilePath -> IO Socket
openSocket p = do
  sock <- socket AF_UNIX
                 Stream
                 defaultProtocol
  connect sock sockAddr
  return sock
    where sockAddr = SockAddrUnix . encodeString $ p
```

Let's look at the types

```haskell
Prelude Network.Socket> :t socket
socket :: Family -> SocketType -> ProtocolNumber -> IO Socket
Prelude Network.Socket> :t connect
connect :: Socket -> SockAddr -> IO ()
Prelude Network.Socket> :t SockAddrUnix
SockAddrUnix :: String -> SockAddr
```

### Binding over failure in initialization

## Shawty
