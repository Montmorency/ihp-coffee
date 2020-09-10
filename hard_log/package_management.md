nix package for RSA required by req:
https://gist.github.com/mpscholten/b4c88c5e18b67a70c247d0312cc02094
Then the instruction for including a new package.
https://ihp.digitallyinduced.com/Guide/package-management.html#using-a-different-version-of-a-haskell-package

Which essentiall boils down to installing cabal2nix

```
    nix-env -i cabal2nix
```

Choose a new nix package definition for older version of google-oauth2:

```
    cabal2nix cabal://google-oauth2-0.2.2
```

This outputs a new nix package definition which should be saved to 

```
    Config/nix/haskell-packages/google-oauth2.nix
```

IHP keeps an eye on this directory and will use the package
definitions stored there automatically.

If you get an Encountered missing or private dependencies 
this is usually due to a version mismatch between what the package expects
and what nix provides: you may need to jailbreak the package in 
Config/nix/nixpkgs-config.nix:

```
    doJailbreakPackages = ["google-oauth"...];
```

The package version does not necessarily need to be an older copy of a package!

When it is installed nix-shell it then

```
    make -B .envrc
```

To ensure the new package is visible to all IHP tools

You can also turn on dontCheckPackages =[] so nix doesn't run the test suites.

For projects not in IHP's cachix you may want to maintain your own next to 
digitallyinduced so that you dont have to build from source every time.

