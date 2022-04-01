# test_wynton.R
# ::prprcss::
# 2022 E. D. Gennatas lambdamd.org

#' Test our software works on Wynton
#' 
#' Tests ANTsX loads
#' 
#' @export 

test_wynton <- function() {
    sessionInfo()
    library(ANTsRCore)
    library(ANTsR)
    library(ANTsRNet)

    sink(file = paste0("~/.prprcss_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".log"))
    cat("R.home():\n")
    R.home()
    cat("sessionInfo():\n")
    sessionInfo()
    sink()
} # prprcss::test_sge
