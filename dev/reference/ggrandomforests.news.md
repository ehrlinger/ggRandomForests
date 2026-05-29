# Display the ggRandomForests NEWS file

Opens the package change log in the system pager so users can read the
version history without leaving their R session. The function reads
`NEWS.md` from the installed package root (the canonical change log that
R also surfaces via
[`utils::news()`](https://rdrr.io/r/utils/news.html)). `inst/NEWS` was
removed in v2.7.1 to eliminate the duplicate-source-of-truth maintenance
hole that left the legacy file frozen at v2.4.0; if any installation
still ships an `inst/NEWS`, this function falls back to it.

## Usage

``` r
ggrandomforests.news(...)
```

## Arguments

- ...:

  Currently unused; reserved for future arguments.

## Value

Called for its side-effect of opening the NEWS file in the system pager
(`file.show`). Returns `invisible(NULL)`.
