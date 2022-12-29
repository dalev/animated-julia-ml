# Animated Julia fractal 

The rendering is done on the CPU, but parallelized using `Domainslib`.

Run either:
- `dune exec --release animated_julia -- --mode=animate`
- `dune exec --release animated_julia -- --mode=follow-mouse`

Or, see a couple of other options:
- `dune exec --release animated_julia -- --help`

