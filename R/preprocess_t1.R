# preprocess_t1.R
# ::prprcss::
# 2021 E.D. Gennatas lambdamd.org
# PRs for ANTsR: brainmask, plot title color

#' Preprocess T1 volume using \pkg{ANTsR} and \pkg{ANTsRNet}
#'
#' @param x Character vector of paths to nifti files OR list of \code{antsImage} volumes
#' @param id Character, vector: Names of volumes in \code{x}. Optional; if NULL, defaults to
#' \code{basename(x)} if \code{x} is character, or names of list
#' @param intensityTruncation iMath intensity truncation parameters
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
                          intensityTruncation = c(0.025, 0.975, 256),
                          template_brain = NULL,
                          transform_type = "antsRegistrationSyN[s]",
                          atropos_initialization = "KMeans[3]",
                          atropos_mrf = "[0,1x1x1]",
                          atropos_convergence = "[3,0]",
                          do_warpgm = FALSE,
                          outdir = NULL,
                          verbose = TRUE,
                          vistrace = 0,
                          vistrace_slices = c(90, 130, 170),
                          itk_cores = NULL) {

    if (!is.null(itk_cores)) {
        Sys.setenv(ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS = itk_cores)
    }
    if (is.null(template_brain)) {
        template_brain <- ANTsRCore::antsImageRead(
            system.file("data", "antsmni.nii.gz", package = "prprcss"))
            template_brain_name <- "mni"
    } else {
        template_brain <- ANTsRCore::check_ants(template_brain)
        template_brain_name <- filename(template_brain@filename)
    }
    
    for (i in seq_along(x)) {

        ## Read image ====
        # if (verbose) msg0("Reading image ", x[i], "...")
        t1 <- ANTsRCore::check_ants(x[i])
        .id <- if (is.null(id[i])) filename(t1@filename) else id[i]

        # Output directory
        if (is.null(outdir)) {
            outdir <- file.path(dirname(x[i]), "preprocess_t1")
        } else {
            outdir <- normalizePath(outdir)
        }

        if (vistrace > 0) {
            plot(t1, axis = 3, slices = vistrace_slices, title.img = "Raw T1")
        }

        ## N4 bias correction ====
        if (verbose) msg("Running N4 bias correction...")
        t1 <- ANTsR::abpN4(t1, 
                intensityTruncation = intensityTruncation,
                verbose = verbose)
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
        transform_type_name <- if (grepl("antsRegistrationSyN", transform_type)) {
            "SyN"
        } else {
            transform_type
        }
        dir.create(outdir, showWarnings = FALSE)
        t1_brain_to_template_brain <- ANTsRCore::antsRegistration(
            fixed = template_brain,
            moving = t1_brain,
            typeofTransform = transform_type,
            outprefix = file.path(outdir, paste0(.id, "_brain_to_",
                template_brain_name, "_", transform_type_name)),
            printArgs = verbose,
            verbose = verbose)
# > t1_brain_to_template_brain$invtransforms
# [1] "/tmp/Rtmp7NkZw9/file2faf3ba99bfc0GenericAffine.mat" 
# [2] "/tmp/Rtmp7NkZw9/file2faf3ba99bfc1InverseWarp.nii.gz"
# > t1_brain_to_template_brain$fwdtransforms
# [1] "/tmp/Rtmp7NkZw9/file2faf3ba99bfc1Warp.nii.gz"      
# [2] "/tmp/Rtmp7NkZw9/file2faf3ba99bfc0GenericAffine.mat"

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

        ## +++Write rsWn to file ====
        ANTsRCore::antsImageWrite(image = t1_brain_Wn,
            filename = file.path(outdir, paste0(.id, "_rsWn.nii.gz")))

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

        ## +++Write native GM to file ====
        ANTsRCore::antsImageWrite(image = t1_3class$probabilityimages[[2]],
            filename = file.path(outdir, paste0(.id, "_rsGM.nii.gz")))

        ## Warp GM ====
        if (do_warpgm) {
            ## Apply transformation to GM ====
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

            ## +++Write gmW to file ====
            ANTsRCore::antsImageWrite(image = gmW,
                filename = file.path(outdir, paste0(.id, "_rsGMW.nii.gz")))
        }

    } # /loop through input images

} # prprcss::preprocess_t1

filename <- function(x) gsub("\\..*", "", basename(x))
