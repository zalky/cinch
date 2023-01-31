<img src="https://i.imgur.com/GH71uSi.png" title="zalky" align="right" width="250"/>

# Cinch

[![Clojars Project](https://img.shields.io/clojars/v/io.zalky/cinch?labelColor=blue&color=green&style=flat-square&logo=clojure&logoColor=fff)](https://clojars.org/io.zalky/cinch)

Utility functions that make things a cinch.

Just include it in your `deps.edn`:

```clj
{:deps {io.zalky/cinch {:mvn/version "0.1.0"}}}
```

Contains all the basics you always seem to need, some highlights:

- `get-source-dirs`: Get project source paths from classpath

- `merge-hierarchies`: Merges Clojure hierarchies, extrapolating
  transitive relationships and checking for cyclical dependencies

- `index-comparator`: Given a vector, returns a comparator by element
  indices

- `merge-deep`: the obligatory merge deep, but this one merges vector
  elements positionally

And more...

## License

Cinch is distributed under the terms of the Apache License 2.0.
