## Haskell at Chalmers

We use the Glasgow Haskell Compiler (GHC) for this course.  GHC 7.10.2 are
already available on the Chalmers computers, but in order to get access to them,
you need to make sure to have the directory

```bash
/chalmers/sw/unsup64/phc/b/binh
```

in your path. One way to achieve this is to run these commands in a terminal:

```bash
echo 'setenv PATH "/chalmers/sw/unsup64/phc/b/binh:"$PATH' >> $HOME/.tcshrc
echo 'export PATH=/chalmers/sw/unsup64/phc/b/binh:$PATH'   >> $HOME/.bashrc
```

Restart the terminal to make sure that the new path is visible.

Note that if you run these commands several times, redundant lines will be added
to your `~/.tcshrc` and `~/.bashrc` files. If so, you may want to clean them up
afterwards.

Check that the setup works by running
```bash
> ghc --version
The Glorious Glasgow Haskell Compilation System, version 7.10.2
```
