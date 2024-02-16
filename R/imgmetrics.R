# imgmetrics
# ::prprcss::
# 2021 E.D. Gennatas lambdamd.org

#' Signal-to-Noise Ratio
#'
#' SNR = mean(image)/sd(image)
#'
#' @param x Character or antsImage: path to image OR ANTs image
#' @param verbose Logical: If TRUE, print messages to console. Default = TRUE
#'
#' @return List with snr and image mean, sd
#'
#' @author E.D. Gennatas
#' @export

snr <- function(x, verbose = TRUE) {
  img <- ANTsRCore::check_ants(x)
  img_mean <- mean(img)
  img_sd <- sd(img)
  out <- list(
    snr = img_mean / img_sd,
    mean = img_mean,
    sd = img_sd
  )
  if (verbose) printls(out, color = crayon::cyan)
  invisible(out)
} # prprcss::snr


#' Contrast-to-Noise Ratio
#'
#' CNR = (mean(brain) - mean(background))/sd(background)
#'
#' @param x Character or antsImage: path to image OR ANTs image
#' @param modality Character: "t1", "t1v0", "t1nobrainer", "t1combined", "t2",
#' "flair", "bold", "fa", "t1t2infant", "t1infant", "t2infant". Default = "t1"
#' Passed to \code{ANTsRNet::brainExtraction}
#' @param lowThresh Double: Passed to \code{ANTsRCore::getMask}
#' @param highThresh Double: Passed to \code{ANTsRCore::getMask}
#' @param cleanup Double: Passed to \code{ANTsRCore::getMask}. Default = 2
#' @param verbose Logical: If TRUE, print messages to console. Default = TRUE
#'
#' @return List with cnr, snr, img/brain/background mean and sd
#'
#' @author E.D. Gennatas
#' @export

cnr <- function(x,
                modality = "t1",
                lowThresh = NULL,
                highThresh = NULL,
                cleanup = 2,
                verbose = TRUE) {
  img <- ANTsRCore::check_ants(x)
  if (is.null(lowThresh)) lowThresh <- mean(img)
  if (is.null(highThresh)) highThresh <- max(img)
  brainmask <- ANTsRNet::brainExtraction(img,
    modality = modality,
    verbose = verbose
  )
  brain <- img * brainmask
  headmask <- ANTsRCore::getMask(img,
    lowThresh = lowThresh,
    highThresh = highThresh,
    cleanup = cleanup
  )
  head <- img * headmask
  background <- img * (1 - headmask)
  img_mean <- mean(img)
  img_sd <- sd(img)
  snr <- img_mean / img_sd
  brain_mean <- mean(brain)
  brain_sd <- sd(brain)
  background_mean <- mean(background)
  background_sd <- sd(background)
  cnr <- (brain_mean - background_mean) / background_sd
  out <- list(
    cnr = cnr,
    snr = snr,
    img_mean = img_mean,
    img_sd = img_sd,
    brain_mean = brain_mean,
    brain_sd = brain_sd,
    background_mean = background_mean,
    background_sd = background_sd
  )

  if (verbose) printls(out, color = crayon::cyan)
  invisible(out)
} # prprcss::cnr
