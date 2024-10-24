
# Magix

Build, cache and run (possibly compiled) scripts with dependencies using the Nix
package manager.

-   Magix is simple and stupid.
-   Magix is a tiny wrapper around `nix-build`.


# Next steps

-   Use random temporary cache directory for tests.
-   We create random hashes when running tests. Can we use the HSpec seed for
    reproducability? Maybe we have to write an Arbitrary instance.
-   More tests for builder: Inspect cache more carefully. For example, the links may be broken.
-   Nix expression and replacement test helpers.
-   Property-based testing (e.g., generate arbitrary directives or even scripts).
-   Python, make it happen.

