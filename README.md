# Use `libev`-enabled LWT on M1 Mac:

``` sh
env LIBEV_CFLAGS="-I/opt/homebrew/include" LIBEV_LIBS="-L/opt/homebrew/lib -lev" opam reinstall conf-libev lwt
```

