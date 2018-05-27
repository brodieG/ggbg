
# nocov start
.onLoad <- function(libname, pkgname) {

  .default.opts <- list(ggbg.signif=11L)
  existing.opts <- options()
  options(.default.opts[setdiff(names(.default.opts), names(existing.opts))])
}
# nocov end
