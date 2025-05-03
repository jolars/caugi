#' @useDynLib caugi, .registration = TRUE
#' @importFrom cpp11 cpp_register
.onLoad <- function(libname, pkgname) {
  message("[caugi] ⟳ .onLoad: registering C++ routines")
  cpp_register()
}
