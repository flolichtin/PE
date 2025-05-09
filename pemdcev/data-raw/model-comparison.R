## code to prepare `model.comparison` dataset goes here

devtools::load_all()

library(tidyverse)
library(kableExtra)

rm(list = ls())


model.comparison <- list()

model <- apollo.17

model.tex <- apollo_tex(model)
typeof(model.tex)
gof.names <- model.tex@gof.names
gof <- model.tex@gof
gof <- setNames(gof, gof.names)



extract_gofs <- function(model) {
  model.tex <- apollo_tex(model)
  gof.names <- model.tex@gof.names
  gof <- model.tex@gof
  gof <- setNames(gof, gof.names)
  gof
}



l <- list()

l$model.1 <- extract_gofs(apollo.base.gamma)  # base
l$model.2 <- extract_gofs(apollo.11.outside)  # full
l$model.3 <- extract_gofs(apollo.12)  # reduced
l$model.4 <- extract_gofs(apollo.17)  # interaction

model.comp.df <- as.data.frame(l)
model.comparison$model.comp.df <- model.comp.df

input <- model.comp.df
names(input) <- paste0("Model ", 1:ncol(input))

tab <-
  input %>%
  kbl(format = "latex",
      caption = "Model comparison",
      label = "model_comparison",
      booktabs = TRUE) %>%
  kable_styling(font_size = 10)

tab


usethis::use_data(model.comparison, overwrite = TRUE)
