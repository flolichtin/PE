reorder_facts <- function(input) {
  input %>%
    mutate(title = factor(title),
           title = forcats::fct_relevel(title,
                                        "Change diet (behav.)",
                                        "Red. long flights (behav.)",
                                        "Red. medium flights (behav.)",
                                        "Red. short flights (behav.)",
                                        "Red. room temperature (behav.)",
                                        "Red. or comp. car travel (behav.)",
                                        "Repl. or sell car (both)",
                                        "CO2 certificates (tech.)",
                                        "CO2 offset (tech.)",
                                        "Install heat pump (tech.)",
                                        "Install solar panels (tech.)",
                                        "Install ventilation (tech.)",
                                        "Insulate facade (tech.)",
                                        "Insulate roof (tech.)",
                                        "Repl. windows (tech.)",
                                        "Target not reached (other)"))
}
