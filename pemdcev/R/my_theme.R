my_theme <- function(base_size = getOption("pemdcev.base_size", 12), base_family = getOption("pemdcev.base_family",
                                                                                             "serif"))
{
  theme_bw(base_family = base_family, base_size = base_size) +
    theme(plot.background = element_blank(), panel.grid = element_blank(),
          panel.background = element_blank(), panel.border = element_blank(),
          axis.line = element_line(linewidth = 0.4), axis.ticks = element_line(linewidth = 0.3),
          strip.background = element_blank(), strip.text = element_text(size = rel(0.9)),
          strip.placement = "outside", panel.spacing = unit(1.5,
                                                            "lines"), legend.position = "right", legend.background = element_blank(),
          legend.text = element_text(size = 13, hjust = 0),
          legend.key = element_blank())
}
