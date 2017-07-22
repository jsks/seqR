## seqR - Sequence Analysis in R

To get started, install with devtools:

```r
devtools::install_github("jsks/seqR", build_vignettes = T)

vignette("intro", package = "seqR")
```

### TODO

- [ ] Modify functions to be flexible regrading index start position (currently assumes everything starts at 0).
- [ ] Unit tests for the graphing functions using the `vdiffr` pkg.
- [ ] Clean up documentation (ex: `table` vs `matrix` etc etc)
