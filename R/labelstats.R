# labelstats_native.R
# ::prprcss::
# 2021 E.D. Gennatas lambdamd.org

#' Get label stats for a list of images using \pkg{ANTsR}
#'
#' @param x Character vector of nifti paths OR list of \code{antsImage} objects
#' @param labeled_nifti Character of path to labeled nifti OR \code{antsImage} object in same space as \code{x}
#' @param labelkey Character or Data frame: Either path to CSV or data.frame containing two columns: 
#' "LabelID" (integer) and "LabelName" (character).
#' Output will contain all labels in \code{labelkey} whether they have values or not,
#' and will exclude any labels found in \code{labeled_nifti} but not in \code{labelkey}, this
#' may often be the background label (commonly LabelID 0), for example.
#' @param exclude_label_index Integer vector: 1-based index of labels to exclude. Default = NULL,
#' i.e. do not exclude any labels. May be best to include a \code{labelkey} that excludes unwanted
#' labels (e.g. the background)
#' any labels
#' @param verbose Logical: If TRUE, print messages to console. Default = TRUE
#' 
#' @author E.D. Gennatas
#' @export

labelstats <- function(x, labeled_nifti,
                              labelkey = NULL,
                              exclude_label_index = NULL,
                              do_save_native_label = TRUE,
                              verbose = TRUE) {

  labeled_nifti <- ANTsRCore::check_ants(labeled_nifti)
  labeled_nifti_filename <- filename(labeled_nifti@filename)
  if (is.character(labelkey)) {
      stopifnot(file.exists(labelkey))
      labelkey <- read.csv(labelkey)
  }
  stopifnot(colnames(labelkey) == c("LabelID", "LabelName"))
  nlabels <- nrow(labelkey)
  nimgs <- length(x)
  # init id vector
  id <- character(nimgs)
  # init labelstats list
  .ls <- vector("list", nimgs)

  # labelStats ====
  for (i in seq_along(x)) {
    if (verbose) msg0("Working on image ", i, " of ", nimgs, "...")
    img <- ANTsRCore::check_ants(x[i])
    name <- gsub("_.*", "", filename(img@filename))
    id[i] <- filename(img@filename)
    dirpath <- dirname(x[i])
    # native label path
    native_label_path <- file.path(dirpath, paste0(name, "_", labeled_nifti_filename, "_nativeMasked.nii.gz"))
    if (!file.exists(native_label_path)) {
      affine <- file.path(dirpath, dir(dirpath, paste0(name, ".*0GenericAffine")))
      warp <- file.path(dirpath, dir(dirpath, paste0(name, ".*1InverseWarp")))
        labeled_nifti_native <- ANTsRCore::antsApplyTransforms(
            fixed = img,
            moving = labeled_nifti,
            transformlist = c(affine, warp),
            interpolator = "genericLabel",
            verbose = verbose)
        
        # mask native label by native GM
        native_mask <- ANTsRCore::getMask(img, cleanup = 0)
        labeled_nifti_native_masked <- labeled_nifti_native * native_mask
        
        if (do_save_native_label) {
          ## +++Write native labels to file ====
          if (verbose) msg("Writing masked native label file to disk...")
            ANTsRCore::antsImageWrite(
                image = labeled_nifti_native_masked,
                filename = native_label_path)
          if (file.exists(native_label_path)) {
            msg("Saved", native_label_path)
          } else {
            warning("Failed to save", native_label_path)
          }
        }
    } else {
      labeled_nifti_native_masked <- ANTsRCore::check_ants(native_label_path)
    }
    .ls[[i]] <- ANTsRCore::labelStats(img, labeled_nifti_native_masked)
  }
  names(.ls) <- id
  
  if (verbose) msg("Merging labelstats of", nimgs, "volumes...")
  # Some labels may be missing from native space:
  # Fix by merging with all unique Label IDs before rbinding
  dat <- data.table(LabelID = sort(unique(labeled_nifti)))
  if (!is.null(labelkey)) {
      if (all(c("LabelID", "LabelName") %in% names(labelkey))) {
          dat <- merge(dat, labelkey, by = "LabelID")
      }
  }
  datlabels <- lapply(.ls, function(l) 
      merge(dat, l, by.x = "LabelID", by.y = "LabelValue", all = TRUE))
  datlabels <- lapply(seq_along(datlabels),
                function(i) data.table(ImageID = id[i], datlabels[[i]]))
  datlabels <- do.call(rbind, datlabels)
  if (is.null(labelkey)) {
      dat_wide <- dcast(datlabels, ImageID ~ LabelID,
      value.var = c("Mean", "Volume"))
  } else {
      dat_wide <- dcast(datlabels, ImageID ~ LabelID + LabelName,
      value.var = c("Mean", "Volume"))
  }
  
  if (!is.null(exclude_label_index)) {
    dat_wide <- dat_wide[, -c(exclude_label_index + 1)]
  }
  dat_wide

} # prprcss::labelstats_native
