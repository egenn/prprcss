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
                       labelkey,
                       include_labels = c("labelkey", "img", "both"),
                       exclude_label_index = NULL,
                       mask_label_by_img = TRUE,
                       do_save_masked_label = FALSE,
                       verbose = TRUE) {

  # Arguments ====
  include_labels <- match.arg(include_labels)
  stopifnot(all(c("LabelID", "LabelName") %in% names(labelkey)))

  labeled_nifti <- ANTsRCore::check_ants(labeled_nifti)
  labeled_nifti_filename <- filename(labeled_nifti@filename)
  if (is.character(labelkey)) {
      stopifnot(file.exists(labelkey))
      labelkey <- read.csv(labelkey)
  }

  nlabels <- NROW(labelkey)
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
    
    # mask label by img
    if (mask_label_by_img) {
        mask <- ANTsRCore::getMask(img, cleanup = 0)
        labeled_nifti_masked <- labeled_nifti * mask

        if (do_save_masked_label) {
          ## +++Write native labels to file ====
          if (verbose) msg("Writing masked label file to disk...")
            label_path <- file.path(dirpath, paste0(name, "_", labeled_nifti_filename, "_masked.nii.gz"))
            ANTsRCore::antsImageWrite(
                image = labeled_nifti_masked,
                filename = label_path)
          if (file.exists(label_path)) {
            msg("Saved", label_path)
          } else {
            warning("Failed to save", label_path)
          }
        }
        .ls[[i]] <- ANTsRCore::labelStats(img, labeled_nifti_masked)
    } else {
      .ls[[i]] <- ANTsRCore::labelStats(img, labeled_nifti)
    }
  }
  names(.ls) <- id
  
  if (verbose) msg("Merging labelstats of", nimgs, "volumes...")
  # Some labels may be missing from individual imgs:
  # Fix by merging with all unique Label IDs before rbinding
  dat <- data.table(LabelID = sort(unique(labeled_nifti)))
  dat <- if (include_labels == "labelkey") {
        merge(dat, labelkey, by = "LabelID", all.x = FALSE, all.y = TRUE)
      } else if (include_labels == "img") {
        merge(dat, labelkey, by = "LabelID", all.x = TRUE, all.y = FALSE)
      } else {
        merge(dat, labelkey, by = "LabelID", all.x = TRUE, all.y = TRUE)
      }

  datlabels <- lapply(.ls, function(l) 
      merge(dat, l, by.x = "LabelID", by.y = "LabelValue", 
            all.x = TRUE, all.y = FALSE))

  # Add ImageID
  # todo: in-place append to 1st column
  datlabels <- lapply(seq_along(datlabels),
                function(i) data.table(ImageID = id[i], datlabels[[i]]))
  datlabels <- do.call(rbind, datlabels)
  # Outputs ImageID, {Mean, Volume}_LabelName: 1 + 2xNlabels
  dat_wide <- dcast(datlabels, ImageID ~ LabelID + LabelName,
          value.var = c("Mean", "Volume"))
  
  if (!is.null(exclude_label_index)) {
    # 1st column is ImageID, labels start at 2nd column
    dat_wide <- dat_wide[, -c(exclude_label_index + 1)]
  }
  dat_wide

} # prprcss::labelstats
