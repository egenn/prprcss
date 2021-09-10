# dkt.R
# ::prprcss::
# E.D. Gennatas lambdamd.org

#' DKT parcellation using \pkg{ANTsRNet}
#' 
#' Simple wrapper to perform parcellation and write to file.
#' 
#' @param x Character path to T1 file
#' @param doPreprocessing Logical: see \code{ANTsRNet::desikanKillianyTourvilleLabeling}
#' @param returnProbabilityImages Logical: see \code{ANTsRNet::desikanKillianyTourvilleLabeling}
#' @param writeseg Logical: If TRUE, write segmentation to file. Default = TRUE
#' @param preprocdir Character: Path to directory to save segmentation, if \code{writeseg = TRUE}
#' @param verbose Logical: If TRUE, write messages to console

dkt <- function(x,
                doPreprocessing = TRUE,
                returnProbabilityImages = FALSE,
                writeseg = TRUE,
                preprocdir = file.path(dirname(x), "preprocess_t1"),
                verbose = TRUE,) {

    dkt <- ANTsRNet::desikanKillianyTourvilleLabeling(
            ANTsRCore::check_ants(x)
            doPreprocessing = doPreprocessing,
            returnProbabilityImages = returnProbabilityImages,
            verbose = verbose)
    name <- gsub("\\..*", "", basename(x))
    antsrout <- file.path(datadir_spin, preprocdir, paste0(name, "_DKT.ANTsRsession"))
    # save.ANTsR(antsrout, objects = "dkt")
    # if (dir.exists(antsrout)) msg("Saved parcellation to", antsrout)
    # fileout <- file.path(, preprocdir, paste0(name, "_DKT.ANTsRsession"))
    if (writeseg) {
        if (!dir.exists(preprocdir)) {
            dir.create(preprocdir)
            if (verbose) msg("Created", preprocdir)
        }
        fileout <- file.path(preprocdir, paste0(name, "_DKT.nii.gz"))
        ANTsRCore::antsImageWrite(dkt[["segmentationImage"]],
            filename = )
    }
    
    invisible(dkt)

} # prprcss::dkt
