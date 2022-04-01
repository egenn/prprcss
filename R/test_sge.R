# test_sge.R
# ::prprcss::
# 2022 E. D. Gennatas lambdamd.org

#' Test ANTsX works on the SGE cluster
#' 
#' @export 

test_sge <- function() {
    sessionInfo()
    library(ANTsRCore)
    library(ANTsR)
    library(ANTsRNet)

    sink(file = paste0("~/.prprcss_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".log"))
    sessionInfo()
    sink()
} # prprcss::test_sge
