# zzz.R
# ::prprcss::
# 2021 E.D. Gennatas lambdamd.org

prprcss.version <- packageVersion("prprcss")

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste0(
    "  .:", pkgname, " ", prprcss.version, ": Welcome, ",
    Sys.getenv("USER"), "\n  https://lambdamd.org"
  ))
}

#' \pkg{prprcss}: Biomedical Image Preprocessing
#'
#' @docType package
#' @name prprcss-package
#' @import graphics grDevices methods stats utils rtemis data.table ANTsR

NULL
