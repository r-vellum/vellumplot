.onLoad <- function(libname, pkgname) {
  # Register S7 classes/methods (the as_vellum_scene method on PlotSpec, the
  # print externals) so dispatch works once the package is installed.
  S7::methods_register()
  # Add the grammar lint rules to vellum's registry. Done here, not at build
  # time, so they survive a `load_all()` -- and so a plain `vellum::vl_lint()`
  # on a compiled plot reports the encoding problems too, which is the whole
  # point of the registry being open to a layer above. See R/lint.R.
  .register_lint_rules()
}
