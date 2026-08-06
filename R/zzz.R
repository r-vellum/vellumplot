# Symbols that appear unquoted inside `after_stat()` (stat-output columns) and the
# `.data` pronoun in `vwaffle()` -- non-standard evaluation R CMD check cannot see
# through. Declared here so it does not flag them as undefined globals.
utils::globalVariables(c("n", "lab", ".olab", ".data"))

.onLoad <- function(libname, pkgname) {
  # Register S7 classes/methods (the as_vellum_scene method on PlotSpec, the
  # print externals) so dispatch works once the package is installed.
  S7::methods_register()
  # Add the grammar lint rules to vellum's registry. Done here, not at build
  # time, so they survive a `load_all()` -- and so a plain `vellum::vl_lint()`
  # on a compiled plot reports the encoding problems too, which is the whole
  # point of the registry being open to a layer above. See R/lint.R.
  .register_lint_rules()
  # Adaptive knit output (crisp inline SVG in HTML; default device elsewhere).
  .register_knit_print()
}
