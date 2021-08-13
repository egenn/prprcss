# Check preprocess_t1

which_preprocess <- function(root_dir,
                             raw_suffix = ".nii.gz",
                             preprocess_dir = "preprocess_t1",
                             target_file_suffix = "rsGM.nii.gz",
                             verbose = TRUE) {
    
    ids_available <- gsub(raw_suffix, "", dir(root_dir, raw_suffix))
    ids_preprocessed <- gsub("_.*", "",
        dir(file.path(root_dir, preprocess_dir), target_file_suffix))
    ids_topreprocess <- ids_available[!ids_available %in% ids_preprocessed]
    n_available <- length(ids_available)
    n_preprocessed <- length(ids_preprocessed)
    n_topreprocess <- length(ids_topreprocess)

    msg0(n_preprocessed, 
        " (",ddSci(n_preprocessed/n_available*100, 1),
        "%) preprocessed .:. ",
        n_topreprocess,
        " (", ddSci(n_topreprocess/n_available*100, 1),
        "%) remaining to preprocess",
        color = crayon::cyan)

    invisible(list(n_available = n_available,
            n_preprocessed = n_preprocessed,
            n_topreprocess = n_topreprocess))
    
}
