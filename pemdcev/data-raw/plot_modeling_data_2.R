## code to prepare `plot_modeling_data_2` dataset goes here

devtools::load_all()

library(tidyverse)

dat.m.2 <- pemdcev::modeling_data_2$pe
avail <- pemdcev::modeling_data_2$avail
dat.avail <- as.data.frame(avail)
dat.avail$ID <- dat.m.2$ID


dat.lng <-
  dat.m.2 %>%
  pivot_longer(-ID) %>%
  rename(reduc = value)

dat.avail.lng <-
  dat.avail %>%
  pivot_longer(-ID) %>%
  rename(avail = value)

dat <-
  dat.lng %>%
  left_join(dat.avail.lng, by = c("ID", "name")) %>%
  mutate(avail = ifelse(is.na(avail), 1, avail),
         chosen.avail = as.numeric(avail == 1 & reduc != 0))

main.avail <-
  dat %>%
  group_by(name) %>%
  mutate(N = n()) %>%
  ungroup() %>%
  group_by(name, avail, N) %>%
  count() %>%
  ungroup() %>%
  filter(avail == 1) %>%
  mutate(perc.avail = n / N) %>%
  select(name, perc.avail)

main.reduc <-
  dat %>%
  filter(reduc != 0) %>%
  select(ID, name, reduc)

main.chosen.avail <-
  dat %>%
  filter(avail == 1) %>%
  group_by(name) %>%
  mutate(N = n()) %>%
  ungroup() %>%
  group_by(name, chosen.avail, N) %>%
  count() %>%
  ungroup() %>%
  filter(chosen.avail == 1) %>%
  mutate(perc.chosen.avail = n / N) %>%
  select(name, perc.chosen.avail)

main <-
  main.reduc %>%
  left_join(main.avail, by = "name") %>%
  left_join(main.chosen.avail, by = "name") %>%
  group_by(name) %>%
  mutate(m = median(reduc)) %>%
  ungroup() %>%
  mutate(name = ifelse(name == "budget", "ovrll.rdctn", name),
         name = factor(name),
         name = reorder(name, perc.chosen.avail)) %>%
  mutate(across(matches("perc"), function(x) 100 * x))

main %>%
  ggplot(aes(x = name)) +
  # geom_violin(aes(y = reduc), width = 2) +
  geom_boxplot(aes(y = reduc), outlier.shape = 1, outlier.alpha = 0.5,
               notch = FALSE, fill = "lightblue") +
  geom_point(aes(y = perc.avail, shape = "available", col = "available"), size = 4) +
  geom_point(aes(y = perc.chosen.avail, shape = "chosen | available", col = "chosen | available"), size = 4) +
  labs(x = "Strategy", y = "%", shape = "", colour = "",
       # title = "Extensive and intensive choice margin",
       subtitle = paste0("N = ", length(unique(main$ID)))) +
  scale_shape_manual(values = c(15, 17)) +
  scale_color_manual(values = c("#4b2461", "darkorange")) +
  theme_bw(base_family = "serif") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))


exp.reduc <-
  main %>%
  mutate(perc.chosen = (perc.avail / 100 * perc.chosen.avail / 100)) %>%
  group_by(name, perc.chosen) %>%
  summarise(mean.reduc = mean(reduc)) %>%
  ungroup() %>%
  mutate(exp.reduc = perc.chosen * mean.reduc,
         exp.reduc = paste0(round(exp.reduc, digits = 2), "%")) %>%
  select(name, exp.reduc)

main %>%
  ggplot(aes(x = name)) +
  # geom_violin(aes(y = reduc), width = 2) +
  geom_boxplot(aes(y = reduc), outlier.shape = 1, outlier.alpha = 0.5,
               notch = FALSE, fill = "grey50", col = "black") +
  geom_point(aes(y = perc.avail, shape = "available", col = "available"), size = 4) +
  geom_point(aes(y = perc.chosen.avail, shape = "chosen | available", col = "chosen | available"), size = 4) +
  geom_text(data = exp.reduc, mapping = aes(y = 105, label = exp.reduc),
            angle = 45, size = 3) +
  ylim(0, 107) +
  labs(x = "Strategy", y = "%", shape = "", colour = "",
       # title = "Extensive and intensive choice margin",
       subtitle = paste0("N = ", length(unique(main$ID)), "\nExpected reductions* as percentage values\n*effectively chosen times mean reduction values")) +
  scale_shape_manual(values = c(15, 17)) +
  scale_color_manual(values = c("#4b2461", "darkorange")) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("plot_modeling_data_2.png", width = 6, height = 6)

# base R
dev.off()
strats <- levels(main$name)
nstrats <- length(strats)
par(mai = par()$mai + c(0.5, 0, 0, 0))
boxplot(reduc ~ name, data = main,
        xlab = "", ylab = "%", las = 2, ylim = c(0, 107), yaxt = "n")
axis(2, at = seq(0, 100, 20), las = 1)
abline(h = 100)
points(x = main$name, y = main$perc.avail, pch = 15, cex = 1.5, col = "lightblue2")
points(x = main$name, y = main$perc.chosen.avail, pch = 17, cex = 1.5, col = "darkorange")
text(x = seq(nstrats)-0.3, y = 100, labels = exp.reduc$exp.reduc, srt = 45, adj = -0.4, cex = 0.75)
mtext("Expected reductions* as percentage values", line = 1, adj = 0)
mtext("*effectively chosen times mean reduction", adj = 0, cex = 0.75)
legend(x = 0, y = 100, c("avail", "chosen | avail"), col = c("lightblue2", "darkorange"), bty = "n",
       pch = c(15, 17))

usethis::use_data(plot_modeling_data_2, overwrite = TRUE)
