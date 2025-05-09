## code to prepare `model.test` dataset goes here

"
* Intercept only model
* Testing rmdcev
"

devtools::load_all()

library(rmdcev)

rm(list = ls())

strategies <-
  pemdcev::pe_data_list$main %>%
  select(ID, matches("_r$"), -undercomp_r, -tot_r)

strategies %>%
  select(-ID, -undercomp.w.offset_r) %>%
  rowSums()

# we need alt, choice (_r), price and income (1)
keep <- rowSums(select(strategies, -ID)) <= 1
table(keep)
keep <- strategies$ID[keep]

data.mdcev <-
  strategies %>%
  filter(ID %in% keep) %>%
  pivot_longer(-ID) %>%
  mutate(alt = stringr::str_remove_all(name, "_[a-z]$"),
         choice = value,
         price = 1,
         income = 1) %>%
  select(id = ID, alt, choice, price, income)

aggregate(choice ~ alt, data = data.mdcev, FUN = mean)

avail <-
  pemdcev::pe_data_list$main %>%
  select(ID, matches("_v$")) %>%
  filter(ID %in% keep) %>%
  pivot_longer(-ID) %>%
  mutate(alt = stringr::str_remove_all(name, "_[a-z]$"),
         avail = as.numeric(value)) %>%
  select(id = ID, alt, avail)

aggregate(avail ~ alt, data = avail, FUN = mean)

data.mdcev <-
  data.mdcev %>%
  left_join(avail, by = c("id", "alt"))

# undercomp.w.offset is always avail
data.mdcev[is.na(data.mdcev$avail), "avail"] <- 1

data.mdcev.p <- mdcev.data(data.mdcev,
                           id.var = "id",
                           alt.var = "alt",
                           choice = "choice",
                           income = "income")

row.names(data.mdcev.p) <- NULL

"
...
add asc col for each alt (except for the reference -> which is rplc.r.sll)
include in formula below
compare to ~ 0 and psi_ascs = 1
"

mdcev_mle <- mdcev(~ 0,
                   data = data.mdcev.p,
                   model = "gamma",
                   algorithm = "MLE",
                   print_iterations = TRUE,
                   psi_ascs = 1)  # same as NULL

summary(mdcev_mle)

mdcev_asc <- mdcve(~ asc_) ...



usethis::use_data(model.test, overwrite = TRUE)
