# Building projects

Exposing functions manually

```
module Hello
  ( sayHello )
  where
```

Importing single functions

```
import Data.Bool (bool)
```

Qualified imports

```
import qualified Data.Bool
import qualified Data.Bool as B
```

Change Prelude prompt

```
Prelude> :set prompt "Lambda> "
```

do notation and return

```
Prelude> :t return
return :: Monad m => a -> m a

-- this fails
twoo :: IO Bool
twoo = do c <- getChar
          c' <- getChar
          (c == c')

-- this succeeds
twoo :: IO Bool
twoo = do c <- getChar
          c' <- getChar
          return (c == c')
```
