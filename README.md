# prng

## Description
 Pseudo random number generation library for
 sampling and random permutations

 Module `Data.PRNG` exports `RNG` typeclass.
 Includes instances of [System.Random.Mersenne.Pure64](https://hackage.haskell.org/package/mersenne-random-pure64-0.2.2.0/docs/System-Random-Mersenne-Pure64.html)
 and 
 [System.Random.MWC](https://hackage.haskell.org/package/mwc-random-0.13.6.0/docs/System-Random-MWC.html)

## Compilation
- Benchmarks between Mersenne Twister and MWC

```stack build && stack exec prng-exe -- --output rng.html && open rng.html```

- Test

```stack test```





