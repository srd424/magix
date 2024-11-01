
# Magix

Build, cache and run (possibly compiled) scripts with dependencies using the Nix
package manager.

-   Magix is simple and stupid.
-   Magix is a tiny wrapper around `nix-build`.


# Next steps

-   Test edge cases when parsing directives. For example, test horizontal space
    after directives before the newline; or horizontal space between `#!` and the
    directive.
-   Property-based testing (e.g., generate arbitrary directives or even scripts).
-   We create random caches and hashes during tests. We could write an Arbitrary
    instance for `Config` to simplify this process.

