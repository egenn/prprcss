# which_preprocess.R
# ::prprcss::
# E.D. Gennatas lambdamd.org

#' Check which images have been preprocessed
#'
#' @param root_dir Character: Directory where raw T1 volumes are stored
#' @param raw_suffix Character: Suffix of raw T1 volumes. Default = ".nii.gz"
#' @param preprocess_dir Character: Name of preprocessing directory. Default = "preprocess_t1"
#' @param target_file_suffix Character: Suffix of preprocessed volumes. Default = "rsGM.nii.gz"
#' @param verbose Logical: If TRUE, print messages to console. Default = TRUE
#'
#' @author E.D. Gennatas
#' @export

which_preprocess <- function(root_dir,
                             raw_suffix = ".nii.gz",
                             preprocess_dir = "preprocess_t1",
                             target_file_suffix = "rsGM.nii.gz",
                             verbose = TRUE) {
  ids_available <- gsub(raw_suffix, "", dir(root_dir, raw_suffix))
  ids_preprocessed <- gsub(
    "_.*", "",
    dir(file.path(root_dir, preprocess_dir), target_file_suffix)
  )
  ids_topreprocess <- ids_available[!ids_available %in% ids_preprocessed]
  n_available <- length(ids_available)
  n_preprocessed <- length(ids_preprocessed)
  n_topreprocess <- length(ids_topreprocess)

  msg0(n_preprocessed,
    " (", ddSci(n_preprocessed / n_available * 100, 1),
    "%) preprocessed .:. ",
    n_topreprocess,
    " (", ddSci(n_topreprocess / n_available * 100, 1),
    "%) remaining to preprocess",
    color = crayon::cyan
  )

  invisible(list(
    n_available = n_available,
    n_preprocessed = n_preprocessed,
    n_topreprocess = n_topreprocess,
    ids_preprocessed = ids_preprocessed,
    ids_topreprocess = ids_topreprocess
  ))
} # prprcss::which_preprocess
