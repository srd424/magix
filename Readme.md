
# Magix

Build, cache and run compiled scripts using the Nix package manager. Magix is
simple and stupid. Magix is a tiny wrapper around `nix-build`.


# Ideas

-   More tests.
-   Magix does not work with scripts with file names containing spaces. We can
    make it work by using a layer of indirection: Create a link with a path
    without spaces to the original script. Refer to the link in the Nix
    expressions, not the original script.

