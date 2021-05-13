# preprocess_t1.R
# ::prprcss::
# 2021 E.D. Gennatas lambdamd.org
# PRs for ANTsR: brainmask, plot title color

#' Preprocess T1 volume using \pkg{ANTsR} and \pkg{ANTsRNet}
#'
#' @param x Character vector of paths to nifti files OR list of \code{antsImage} volumes
#' @param id Character, vector: Names of volumes in \code{x}. Optional; if NULL, defaults to
#' \code{basename(x)} if \code{x} is character, or names of list
#' @param winsorize_prob Float, vector, length 2: Probabilities to define quantiles to winsorize
#' image. Default = c(.01, .99), i.e. any values below the 1st percentile / aboe the 99th percentile
#' will be set to the value corresponding to the 1st /99th percentile respectively.
#' @param template_brain Path to image or antsImage object for template brain, i.e. skull-stripped
#' volume
#' @param transform_type Character: Registration transform type to use. Default = "SyN"
#' @param atropos_initialization Character: Atropos initialization. Default = "KMeans[3]"
#' @param atropos_mrf Character: Atropos MRF parameters. Default = "[0,1x1x1]" (no smoothing)
#' @param atropos_convergence Character: Atropos convergence parameters. Default = "[3,0]"
#' @param outdir Character: Path to output directory to save transformations and output volumes
#' @param verbose Logical: If TRUE, print messages to console
#' @param vistrace Integer 0-2: If > 0, print plots of images at different preprocessing stages
#' @param vistrace_slices Integer: How many slices to plot if \code{vistrace > 0}
#'
#' @author E.D. Gennatas
#' @export

preprocess_t1 <- function(x,
                          id = NULL,
                          winsorize_prob = c(.01, .99),
                          template_brain = NULL,
                          transform_type = "SyN",
                          atropos_initialization = "KMeans[3]",
                          atropos_mrf = "[0,1x1x1]",
                          atropos_convergence = "[3,0]",
                          outdir = NULL,
                          verbose = TRUE,
                          vistrace = 0,
                          vistrace_slices = c(90, 130, 170)) {

    if (is.null(id)) {
        if (is.list(x)) {
            id <- names(x)
        } else {
            id <- gsub("\\..*", "", basename(x))
        }
        if (is.null(id)) id <- paste0("Image", seq_along(x))
    }
    if (is.null(template_brain)) {
        template_brain <- ANTsRCore::antsImageRead(
            system.file("data", "sri24_brain.nii.gz", package = "prprcss"))
        template_brain_name <- "sri24"
    } else {
        if (is.character(template_brain)) {
            template_brain <- ANTsRCore::antsImageRead(template_brain)
        }
    }

    for (i in seq_along(x)) {

        ## Read image ====
        # if (verbose) msg0("Reading image ", x[i], "...")
        if (is.character(x[[i]])) {
            t1 <- ANTsRCore::antsImageRead(x[[i]])
        } else {
            if (!inherits(x[[i]], "antsImage")) stop("Input is not antsImage")
        }

        # Output directory
        if (is.null(outdir)) outdir <- paste0(dirname(x[i]), "/preprocess_t1")

        if (vistrace > 0) {
            plot(t1, axis = 3, slices = vistrace_slices, title.img = "Raw T1")
        }

        ## Winsorize ====
        quantiles <- quantile(t1, winsorize_prob)
        if (verbose) {
            msg(paste0("Winsorizing: low = ",
                quantiles[1], ", high = ", quantiles[2]))
        }
        t1[t1 < quantiles[1]] <- quantiles[1]
        t1[t1 > quantiles[2]] <- quantiles[2]
        if (vistrace > 1) {
            plot(t1, axis = 3, slices = vistrace_slices,
                title.img = "Winsorized T1", title.line = -3)
        }

        ## N4 bias correction ====
        if (verbose) msg("Running N4 bias correction...")
        t1 <- ANTsR::abpN4(t1, verbose = verbose)
        if (vistrace > 0) {
            plot(t1, axis = 3, slices = vistrace_slices,
                title.img = "N4-corrected T1", title.line = -3)
        }

        ## Brain extraction ====
        t1_brain_mask <- ANTsRNet::brainExtraction(t1, modality = "t1")
        # PR: input arg test warning
        t1_brain_mask <- ANTsRCore::getMask(t1_brain_mask, lowThresh = .5, highThresh = 1)
        if (vistrace > 0) {
            plot(t1_brain_mask, axis = 3, slices = vistrace_slices,
                title.img = "Brain mask", title.line = -3)
        }
        t1_brain <- t1 * t1_brain_mask
        if (vistrace > 0) {
            plot(t1_brain, axis = 3, slices = vistrace_slices,
                title.img = "T1 brain", title.line = -3)
        }

        ## Syn to template brain ====
        dir.create(outdir, showWarnings = FALSE)
        t1_brain_to_template_brain <- ANTsRCore::antsRegistration(
            fixed = template_brain,
            moving = t1_brain,
            typeofTransform = transform_type,
            outprefix = paste0(outdir, "/", id[i], "_brain_to_",
                template_brain_name, "_", transform_type),
            printArgs = verbose,
            verbose = verbose)

        ## Apply transformation ====
        t1_brain_W <- ANTsRCore::antsApplyTransforms(
            fixed = template_brain,
            moving = t1_brain,
            transformlist = t1_brain_to_template_brain$fwdtransforms,
            verbose = verbose)

        ## Normalize ====
        t1_brain_Wn <- ANTsRCore::iMath(
            img = t1_brain_W,
            operation = "Normalize")

        if (vistrace > 1) {
            plot(t1_brain_Wn, axis = 3, slices = vistrace_slices,
                title.img = "Warped, Normalized T1", title.line = -3)
        }

        ## Write to file ====
        ANTsRCore::antsImageWrite(image = t1_brain_Wn,
            filename = paste0(outdir, "/", id[i], "_rsWn.nii.gz"))

        ## Atropos Segmentation ====
        t1_3class <- atropos(a = t1_brain,
                        x = t1_brain_mask,
                        i = atropos_initialization,
                        m = atropos_mrf,
                        c = atropos_convergence,
                        verbose = verbose)

        if (vistrace > 0) {
            plot(t1_3class$probabilityimages[[2]], axis = 3,
                slices = vistrace_slices,
                title.img = "Native GM", title.line = -3)
        }

        ## Apply trasnformation to GM ====
        gmW <- ANTsRCore::antsApplyTransforms(
            fixed = template_brain,
            moving = t1_3class$probabilityimages[[2]],
            transformlist = t1_brain_to_template_brain$fwdtransforms,
            verbose = verbose)

        if (vistrace > 0) {
            plot(gmW, axis = 3,
                 # slices = vistrace_slices,
                title.img = "Warped GM", title.line = -3)
        }

        ## Write to file
        ANTsRCore::antsImageWrite(image = t1_3class$probabilityimages[[2]],
            filename = paste0(outdir, "/", id[i], "_rsGMW.nii.gz"))

    } # /loop through input images

} # prprcss::preprocess_t1
