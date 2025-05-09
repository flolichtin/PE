## code to prepare `colors` dataset goes here

colors <- RColorBrewer::brewer.pal(n = 6, name = "Set2")

Heimisc::plot_colors(colors)

usethis::use_data(colors, overwrite = TRUE)
