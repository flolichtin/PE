## code to prepare `data_flo` dataset goes here

devtools::load_all()

model <- apollo.17
screen_reg <- texreg::screenreg(apollo_tex(model))

me <- pemdcev::marginal.effects$all_me
ie <- pemdcev::income.effects
inter <- readRDS("./tmp/interaction-effects.rds")

df <- list()
df$model <- model
df$estimates <- screen_reg
df$marginal_effects <- me
df$income_effects <- ie
df$interaction_effects <- inter

saveRDS(df, "data_flo.rds")

data_flo <- df
usethis::use_data(data_flo, overwrite = TRUE)
