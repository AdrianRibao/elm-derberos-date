# elm-derberos-date

The latest elm 0.19 version introduced some [important changes](https://github.com/elm/compiler/blob/master/upgrade-docs/0.19.md) in the [Time API](https://package.elm-lang.org/packages/elm/time/latest/). This new API uses the [Posix time](https://en.wikipedia.org/wiki/Unix_time) as an internal representation of the time.

The functions provided in this library use the raw serialized time for the calculations.

This library was created for the advanced booking system of [Derberos](https://derberos.digital/en).

It is based on the wonderful paper from Howard Hinnant ([chrono-Compatible Low-Level Date Algorithms](http://howardhinnant.github.io/date_algorithms.html)).

Inspired also by [Elm date extra](https://package.elm-lang.org/packages/rluiten/elm-date-extra/9.4.0)
