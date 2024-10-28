
# Magix

Build, cache and run (possibly compiled) scripts with dependencies using the Nix
package manager.

-   Magix is simple and stupid.
-   Magix is a tiny wrapper around `nix-build`.


# Next steps

-   Build process should use a lock. Use `filelock` and `withFileLock`. The
    library uses `flock`, which is part of `util-linux` (has to be added to
    flake). There should be a test checking the lock.
-   More tests for builder: Inspect cache more carefully. For example, what
    happens when the links are broken?
-   Nix expression and replacement test helpers.
-   Property-based testing (e.g., generate arbitrary directives or even scripts).
-   Python, make it happen.


# Other ideas

-   We create random caches and hashes during tests. We could write an Arbitrary
    instance for `Config` to simplify this process.

