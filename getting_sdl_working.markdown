`ghc-pkg` is what `cabal` uses under the hood. You can use it to view or edit info about your packages. For example, view the config of a package using:

    ghc-pkg describe PACKAGENAME

In my case, the `ld-options` for the `SDL` package were configured wrongly. So I had to change them. Here were the steps:

1. Export config to a file

    ghc-pkg describe SDL > sdl.pkg

2. Edit the file

    vim sdl.pkg

3. Update the config in `ghc-pkg`:

    ghc-pkg update --global sdl.pkg

Now check your packages with

    ghc-pkg list SDL

you might see two packages! One `--user` and one `--global`.
You can see which is which with

    ghc-pkg list --user SDL
    ghc-pkg list --global SDL

Now unregister the user package:

    ghc-pkg unregister --user SDL

And then

    ghc-pkg describe SDL

should have your changes!
