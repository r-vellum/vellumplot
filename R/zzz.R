.onLoad <- function(libname, pkgname) {
  # Register S7 classes/methods (the as_vellum_scene method on PlotSpec, the
  # print externals) so dispatch works once the package is installed.
  S7::methods_register()
}
