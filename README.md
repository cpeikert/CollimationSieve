# Collimation Sieve

This Haskell code simulates and benchmarks a generalization of
Kuperberg's quantum *collimation sieve* algorithm for arbitrary cyclic
groups. We have used it to estimate the quantum (in)security of CSIDH
("Commutative SIDH"), a proposed "post quantum" drop-in replacement
for noninteractive Diffie-Hellman-style key agreement and
encryption. See
[our paper](https://web.eecs.umich.edu/~cpeikert/pubs/csidh-sieve.pdf)
for further details.

## Building

1. Make sure you have a recent version of
[Haskell stack installed](https://docs.haskellstack.org/en/stable/install_and_upgrade/). (Warning:
pre-1.0 versions of stack will crash with a parsing error.)

2. Clone this repository and do `stack build`. Go have a hot chocolate
while several packages build (just this one time).

## Execution

* Run the sieve with

        stack run <log N> <log L> <log S> [threshold]
  where `N` is the group order (use `log N=0` for the exact CSIDH-512
  group order), `L` is the desired phase vector length, `S` is the
  desired range size, and `threshold < 1` (optional, defaults to 0.25)
  is the factor that determines whether a phase vector is too short
  (i.e., any vector shorter than `threshold * L` is discarded).

* Small parameters like `log N=150`, `log L=16`, `log S=16` will cause
  the sieve to finish relatively quickly (and use relatively little
  memory), and will give you an idea of how it works and how to
  interpret the output.

* `stack run` will use as many CPU cores as it deems appropriate,
  which may not give the best performance. To specify the number of
  cores and other options, use `stack exec` with RTS
  options. (Caution: `stack run` silently ignores RTS options!)  For
  example,

        stack exec -- collimation-sieve-exe <args> +RTS -N4 -s -RTS
  runs on 4 cores, and outputs various memory and garbage-collection
  statistics at the end.

* **WARNING:** a length limit in Haskell's `vector` package
  effectively imposes a length limit of 2^30 on the sieve's vectors,
  so the sieve will not run reliably for `log L >= 26` unless
  `threshold` is increased somewhat, e.g., to `0.4` (and even this is
  not a guarantee). We are working on a tweak to address this, but in
  any case note that `log L >= 26` can use 100 GB of RAM or more.
