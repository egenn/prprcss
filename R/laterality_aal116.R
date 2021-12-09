# laterality_aal116.R
# ::prprcss::
# 2021 E.D. Gennatas lambdamd.org

#' Calculate Laterality Index from AAL 116 ROIs
#'
#' @param x \code{data.frame} or similar with column that include the ALL label names ending in "_L"
#' and "_R" for the paired regions
#' @author E.D. Gennatas
#' @export

aal_paired_regions <- c("Precentral", "Frontal_Sup", "Frontal_Sup_Orb", "Frontal_Mid",
                        "Frontal_Mid_Orb", "Frontal_Inf_Oper", "Frontal_Inf_Tri", "Frontal_Inf_Orb",
                        "Rolandic_Oper", "Supp_Motor_Area", "Olfactory", "Frontal_Sup_Medial",
                        "Frontal_Med_Orb", "Rectus", "Insula", "Cingulum_Ant", "Cingulum_Mid",
                        "Cingulum_Post", "Hippocampus", "ParaHippocampal", "Amygdala",
                        "Calcarine", "Cuneus", "Lingual", "Occipital_Sup", "Occipital_Mid",
                        "Occipital_Inf", "Fusiform", "Postcentral", "Parietal_Sup", "Parietal_Inf",
                        "SupraMarginal", "Angular", "Precuneus", "Paracentral_Lobule",
                        "Caudate", "Putamen", "Pallidum", "Thalamus", "Heschl", "Temporal_Sup",
                        "Temporal_Pole_Sup", "Temporal_Mid", "Temporal_Pole_Mid", "Temporal_Inf",
                        "Cerebelum_Crus1", "Cerebelum_Crus2", "Cerebelum_3", "Cerebelum_4_5",
                        "Cerebelum_6", "Cerebelum_7b", "Cerebelum_8", "Cerebelum_9",
                        "Cerebelum_10")

laterality_aal116 <- function(x, idname = "ID",
                              verbose = TRUE) {

  if (verbose) msg("Calculating laterality index for", length(aal_paired_regions),
                   "paired AAL regions...")
  setDT(x)
  id <- x[, ..idname]
  lat <- cbind(data.table(ID = x[, ..idname]), matrix(NA, nrow(x), length(aal_paired_regions)))
  setnames(lat, names(lat), c(idname, paste0(aal_paired_regions, "_LI")))
  for (r in aal_paired_regions) {
    vleft <- x[[grep(paste0(r, "_L$"), names(x))]]
    vright <- x[[grep(paste0(r, "_R$"), names(x))]]
    lat[[paste0(r, "_LI")]] <- (vleft - vright)/(vleft + vright)
  }
  if (verbose) msg("Done")
  lat

} # prprcss::laterality_aal116
