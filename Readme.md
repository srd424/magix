
# Magix

Build, cache and run (possibly compiled) scripts with dependencies using the Nix
package manager.

-   Magix is simple and stupid.
-   Magix is a tiny wrapper around `nix-build`.


# Next steps

-   Inspect cache more carefully. For example, the links may be broken.
-   Probably improve hashing. For example, the hash should change when the Nixpkgs
    version used to build the derivation changes.
-   Nix expression and replacement test helpers.
-   Property-based testing (e.g., generate arbitrary directives or even scripts).
-   Option to define cache directory.
-   Python, make it happen.

