dktgraylabs <- read.csv("~/sw/prprcss/inst/data/dktlabs.csv")
dktgraylabs
exclude <- c("background", "CSF", "vessel",
              getnames(dktgraylabs$LabelName, "ventricle"),
              getnames(dktgraylabs$LabelName, "white"))
dktgraylabs <- dktgraylabs[!dktgraylabs$LabelName %in% exclude, ]
rownames(dktgraylabs) <- seq(nrow(dktgraylabs))
write.csv(dktgraylabs, "~/sw/prprcss/inst/data/dktgraylabs.csv",
            row.names = FALSE)
