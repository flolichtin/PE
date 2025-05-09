args <- commandArgs(trailingOnly = TRUE)

NCORES <- as.numeric(args[1])
OUT <- args[2]

sink(paste(OUT, "test.txt", sep = "/"))
cat("hello test\n")
cat("NCORES: ", NCORES, "\n")
cat("OUT: ", OUT, "\n")
sink()
