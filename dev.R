
## write date_pe and data_w to inst/csv
devtools::load_all()

write.table(data_pe, "./inst/csv/data_pe.csv", sep = ";", row.names = FALSE)
write.table(data_w, "./inst/csv/data_w.csv", sep = ";", row.names = FALSE)
