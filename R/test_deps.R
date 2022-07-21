# test_deps.R
# ::prprcss::
# 2022 E. D. Gennatas lambdamd.org

#' Test dependencies can load
#' 
#' @export 

test_deps <- function() {
    
    print(sessionInfo())
    cat("Loading ANTs libraries...\n")
    library(ANTsRCore)
    library(ANTsR)
    library(ANTsRNet)


    logfile <- paste0("/wynton/protected/home/gennatas/egenn/.test/.prprcss_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".log")
    cat("Writing diagnostics to ", logfile, "\n", sep = "")
    sink(file = logfile)
    cat("R.home():\n")
    print(R.home())
    cat("\n")
    cat("sessionInfo():\n")
    print(sessionInfo())
    sink()
} # prprcss::test_deps

test_deps <- function() {
    
    print(sessionInfo())
    cat("Loading ANTs libraries...\n")
    library(ANTsRCore)
    library(ANTsR)
    library(ANTsRNet)

    logfile <- paste0("/wynton/protected/home/gennatas/egenn/.test/.prprcss_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".log")
    cat("Writing diagnostics to ", logfile, "\n", sep = "")
    sink(file = logfile)
    cat("R.home():\n")
    print(R.home())
    cat("\n")
    cat("sessionInfo():\n")
    print(sessionInfo())
    sink()
} # prprcss::test_deps