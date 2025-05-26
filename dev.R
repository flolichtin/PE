
## write date_pe and data_w to inst/csv
devtools::load_all()

write.table(data_pe, "./inst/csv/data_pe.csv", sep = ";", row.names = FALSE)
write.table(data_w, "./inst/csv/data_w.csv", sep = ";", row.names = FALSE)


iris_ <- head(iris, 3)
iris_$test <- list(c(1, 2, 3))

is_list <- data_w %>% map(is.list) %>% unlist()
is_list[is_list]
