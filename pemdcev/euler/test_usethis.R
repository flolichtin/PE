args <- commandArgs(trailingOnly = TRUE)
OUT <- args[1]

my.iris <- head(iris)

saveRDS(my.iris, file = paste(OUT, "my-iris.rds", sep = "/"))
usethis::use_data(my.iris, overwrite = TRUE)
