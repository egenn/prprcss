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
#' 
#' @export

dkt <- function(x,
                doPreprocessing = TRUE,
                returnProbabilityImages = FALSE,
                writeseg = TRUE,
                preprocdir = file.path(dirname(x), "preprocess_t1"),
                verbose = TRUE) {

    dkt <- ANTsRNet::desikanKillianyTourvilleLabeling(
            t1 = ANTsRCore::check_ants(x)
            doPreprocessing = doPreprocessing,
            returnProbabilityImages = returnProbabilityImages,
            verbose = verbose)

    name <- gsub("\\..*", "", basename(x))

    if (writeseg) {
        if (!dir.exists(preprocdir)) {
            dir.create(preprocdir)
            if (verbose) msg("Created", preprocdir)
        }
        fileout <- file.path(preprocdir, paste0(name, "_DKT.nii.gz"))
        ANTsRCore::antsImageWrite(dkt[["segmentationImage"]],
            filename = fileout)
        if (file.exists(fileout)) {
            if (verbose) msg("Saved", fileout)
        } else {
            warning("Failed to save file", fileout)
        }
    }
    
    invisible(dkt)

} # prprcss::dkt
