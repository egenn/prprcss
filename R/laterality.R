# laterality_aal116.R
# ::prprcss::
# 2021 E.D. Gennatas lambdamd.org

#' Calculate Laterality Index from AAL 116 ROIs
#'
#' @param x \code{data.frame} or similar with columns that include the ALL label
#' names ending in "_L" and "_R" for the paired regions
#' @param idname Character: Name of column holding cases IDs. Default = "ID"
#' @param select_pattern: Character: Select columns that match this pattern
#' @param verbose Logical: If TRUE, print messages to console. Default = TRUE
#'
#' @author E.D. Gennatas
#' @export

aal_paired_regions <- c(
  "Precentral", "Frontal_Sup", "Frontal_Sup_Orb",
  "Frontal_Mid", "Frontal_Mid_Orb", "Frontal_Inf_Oper", "Frontal_Inf_Tri",
  "Frontal_Inf_Orb", "Rolandic_Oper", "Supp_Motor_Area", "Olfactory",
  "Frontal_Sup_Medial", "Frontal_Med_Orb", "Rectus", "Insula", "Cingulum_Ant",
  "Cingulum_Mid", "Cingulum_Post", "Hippocampus", "ParaHippocampal",
  "Amygdala", "Calcarine", "Cuneus", "Lingual", "Occipital_Sup",
  "Occipital_Mid", "Occipital_Inf", "Fusiform", "Postcentral", "Parietal_Sup",
  "Parietal_Inf", "SupraMarginal", "Angular", "Precuneus",
  "Paracentral_Lobule", "Caudate", "Putamen", "Pallidum", "Thalamus",
  "Heschl", "Temporal_Sup", "Temporal_Pole_Sup", "Temporal_Mid",
  "Temporal_Pole_Mid", "Temporal_Inf", "Cerebelum_Crus1", "Cerebelum_Crus2",
  "Cerebelum_3", "Cerebelum_4_5", "Cerebelum_6", "Cerebelum_7b",
  "Cerebelum_8", "Cerebelum_9", "Cerebelum_10"
)


aal3_paired_regions <- c(
  "Precentral", "Frontal_Sup_2", "Frontal_Mid_2", "Frontal_Inf_Oper",
  "Frontal_Inf_Tri", "Frontal_Inf_Orb_2", "Rolandic_Oper", "Supp_Motor_Area",
  "Olfactory", "Frontal_Sup_Medial", "Frontal_Med_Orb", "Rectus",
  "OFCmed", "OFCant", "OFCpost", "OFClat", "Insula", "Cingulate_Ant",
  "Cingulate_Mid", "Cingulate_Post", "Hippocampus", "ParaHippocampal",
  "Amygdala", "Calcarine", "Cuneus", "Lingual", "Occipital_Sup",
  "Occipital_Mid", "Occipital_Inf", "Fusiform", "Postcentral",
  "Parietal_Sup", "Parietal_Inf", "SupraMarginal", "Angular", "Precuneus",
  "Paracentralobule", "Caudate", "Putamen", "Pallidum", "Thalamus",
  "Heschl", "Temporal_Sup", "Temporal_Pole_Sup", "Temporal_Mid",
  "Temporal_Pole_Mid", "Temporal_Inf", "Cerebellum_Crus1", "Cerebellum_Crus2",
  "Cerebellum_3", "Cerebellum_4_5", "Cerebellum_6", "Cerebellum_7b",
  "Cerebellum_8", "Cerebellum_9", "Cerebellum_10", "Thal_AV", "ThalP",
  "Thal_VA", "Thal_VL", "Thal_VPL", "Thal_IL", "Thal_Re", "Thal_MDm",
  "Thal_MDl", "ThalGN", "Thal_MGN", "Thal_PuI", "Thal_PuM", "Thal_PuA",
  "Thal_PuL", "ACC_sub", "ACC_pre", "ACC_sup", "N_Acc", "VTA",
  "SN_pc", "SN_pr", "Red_N", "LC"
)

laterality <- function(x, idname = "ID",
                       atlas = c("aal3", "aal"),
                       select_pattern = NULL,
                       verbose = TRUE) {
  # Args ====
  atlas <- match.arg(atlas)

  regions <- switch(atlas,
    aal3 = aal3_paired_regions,
    aal = aal_paired_regions
  )

  if (verbose) {
    msg(
      "Calculating laterality index for",
      length(regions), "paired regions..."
    )
  }
  x <- as.data.table(x)
  if (!is.null(select_pattern)) {
    selected <- c(idname, grep(select_pattern, names(x), value = TRUE))
    x <- x[, ..selected]
  }
  lat <- cbind(
    data.table(ID = x[, ..idname]),
    matrix(NA, nrow(x), length(regions))
  )
  setnames(lat, names(lat), c(idname, paste0(regions, "_LI")))
  for (r in regions) {
    id_left <- grep(paste0(r, "_L$"), names(x))
    id_right <- grep(paste0(r, "_R$"), names(x))
    if (length(id_left) + length(id_right) == 2) {
      vleft <- x[[grep(paste0(r, "_L$"), names(x))]]
      vright <- x[[grep(paste0(r, "_R$"), names(x))]]
      lat[[paste0(r, "_LI")]] <- (vleft - vright) / (vleft + vright)
    } else {
      lat[[paste0(r, "_LI")]] <- NA
    }
  }

  if (verbose) msg("Done")
  lat
} # prprcss::laterality_aal116
