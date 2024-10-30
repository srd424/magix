
# Magix

Build, cache and run (possibly compiled) scripts with dependencies using the Nix
package manager.

-   Magix is simple and stupid.
-   Magix is a tiny wrapper around `nix-build`.


# Next steps

-   Property-based testing (e.g., generate arbitrary directives or even scripts).
-   We create random caches and hashes during tests. We could write an Arbitrary
    instance for `Config` to simplify this process.

