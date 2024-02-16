# labelstats.R
# ::prprcss::
# 2021 E.D. Gennatas lambdamd.org

#' Get label stats for a list of images using \pkg{ANTsR}
#'
#' @param x Character vector of nifti paths OR list of \code{antsImage} objects
#' @param labeled_nifti Character of path to labeled nifti OR \code{antsImage} object
#' @param labelkey Character vector: Label names
#' @param exclude_label_index Integer vector: 1-based index of labels to exclude. Default = 1,
#' which would exclude the first label that is commonly the background. Set to NULL to not exclude
#' any labels
#' @param verbose Logical: If TRUE, print messages to console. Default = TRUE
#'
#' @author E.D. Gennatas
#' @export

labelstats1 <- function(x, labeled_nifti,
                        labelkey = NULL,
                        exclude_label_index = 1,
                        verbose = TRUE) {
  labeled_nifti <- ANTsRCore::check_ants(labeled_nifti)
  if (is.null(labelkey)) {
    labelkey <- paste0("Label_", seq_len(length(unique(labeled_nifti))) - 1)
  }
  id <- character(length(x))
  nimgs <- length(x)

  # labelStats ====
  .ls <- vector("list", nimgs)
  for (i in seq_along(x)) {
    if (verbose) msg0("Working on image ", i, " of ", nimgs, "...")
    img <- ANTsRCore::check_ants(x[i])
    id[i] <- filename(img@filename)
    .ls[[i]] <- ANTsRCore::labelStats(img, labeled_nifti)
  }
  names(.ls) <- id

  dat <- lapply(
    seq_along(.ls),
    function(i) data.table(ID = id[i], .ls[[i]])
  )
  dat <- do.call(rbind, dat)
  dat_wide <- dcast(dat, ID ~ LabelValue, value.var = "Mean")
  names(dat_wide)[2:ncol(dat_wide)] <- labelkey
  if (!is.null(exclude_label_index)) {
    dat_wide <- dat_wide[, -c(exclude_label_index + 1)]
  }
  dat_wide
} # prprcss::labelstats
