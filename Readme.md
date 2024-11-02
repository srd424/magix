
# Magix

Build, cache and run possibly compiled scripts with dependencies using the Nix
package manager.

-   Magix is simple and stupid.
-   Magix is a tiny wrapper around `nix-build`.


# Supported languages


## Bash

```bash
#!/usr/bin/env magix
#!magix bash
#!runtimeInputs jq

jq --help
```


## Haskell

```haskell
#!/usr/bin/env magix
#!magix haskell
#!haskellPackages bytestring
#!ghcFlags -threaded

{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString qualified as BS

main :: IO ()
main = BS.putStr "Hello, World!\n"
```

## Python

```python
#!/usr/bin/env magix
#!magix python
#!pythonPackages numpy

from numpy import array

xs = array([1,2,3])
print(xs)
```

# Next steps

-   Property-based testing (e.g., generate arbitrary directives or even scripts).
    When creating arbitrary directives, one could test if the resulting
    Nix expressions are syntactically correct.
-   We create random caches and hashes during tests. We could write an Arbitrary
    instance for `Config` to simplify this process.

