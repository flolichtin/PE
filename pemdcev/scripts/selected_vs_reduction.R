devtools::load_all()

df <-
  pemdcev::pe_data_list$main %>%
  select(ID, matches("_s$|_r$"), -matches("^tot_"), -matches("^undercomp_"), -matches("^overcomp")) %>%
  pivot_longer(-ID) %>%
  mutate(strategy = stringr::str_remove(name, "_[a-z]$"),
         what = stringr::str_extract(name, "[a-z]$")) %>%
  select(-name) %>%
  pivot_wider(names_from = what, values_from = value) %>%
  mutate(rn0 = as.numeric(r != 0))

df %>%
  group_by(strategy) %>%
  summarise(mean_s = mean(s),
            mean_rn0 = mean(rn0))


df2 <- PE::data_pe %>% select(matches("__selected$|__select$|reduction$"))
