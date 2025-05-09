mean_probs <- function(model, db, dim = c("expe", "disc")) {
  dim <- match.arg(dim)
  list2env(model, envir = environment())

  apollo_inputs <- apollo::apollo_validateInputs(
    apollo_beta = apollo_beta,
    apollo_fixed = apollo_fixed,
    database = db,
    silent = TRUE
  )

  p <- apollo::apollo_prediction(model, apollo_probabilities, apollo_inputs)

  .mean_probs <- function(p, dim = c("expe", "disc")) {
    dim <- match.arg(dim)
    dim. <- paste0("_", dim, "_")
    p. <-
      p %>%
      select(ID, matches(dim.))

    p.. <-
      p. %>%
      pivot_longer(-ID) %>%
      mutate(strat = stringr::str_extract(name, "^[^_]+"),
             stat = stringr::str_extract(name, "[^_]+$")) %>%
      select(ID, strat, stat, value) %>%
      pivot_wider(names_from = stat, values_from = value)

    out <-
      p.. %>%
      group_by(strat) %>%
      summarise(mean = mean(mean),
                sd = 1 / n() * sqrt(sum(sd^2))) %>%  # standard deviation of the mean of independent RVs
      ungroup()

    return(out)
  }

  mp <- .mean_probs(p, dim = dim)
  return(mp)
}


model_vars_key <- function(key, model.vars) {
  mvk <-
    model.vars %>%
    filter(x == key) %>%
    as.list()
  mvk
}


prep_database <- function(db, key, model.vars) {
  db.before <- db
  db.after <- db

  mvk <- model_vars_key(key, model.vars)

  if (mvk$type == "continuous") {
    # dirty
    if (key == "zzz_log_re_area") {
      x <- db.after[, key]
      db.after[, key] <- log(1 + 1 / (x + 0.001))
    } else {
      db.after[, key] <- db.after[, key] + as.numeric(mvk$increase)
    }
  } else if (mvk$type == "indicator") {
    db.before[, key] <- 0  # deactivate
    db.after[, key] <- 1  # activate
  } else if(mvk$type == "likert") {
    db.after[, key] <- db.after[, key] + 1
  } else {
    stop("Unknown variable type")
  }

  list(
    db.before = db.before,
    db.after = db.after
  )
}


#' @export
marginal_effects <- function(key, model, model.vars, db, dim = c("expe", "disc")) {
  dim <- match.arg(dim)
  dbs <- prep_database(db, key, model.vars)
  mpb <- mean_probs(model, dbs$db.before, dim)
  mpa <- mean_probs(model, dbs$db.after, dim)
  mvk <- model_vars_key(key, model.vars)

  me <- data.frame(
    variable = mvk$variable,
    x = mvk$x,
    note = mvk$note,
    strat = factor(mpb$strat),
    me = mpa$mean - mpb$mean
  ) %>%
    filter(strat != "outside") %>%
    arrange(strat)

  class(me) <- c("marginal.effects", "data.frame")
  me
}



#' @method plot marginal.effects
#' @export
plot.marginal.effects <- function(marginal.effects) {
  title <- unique(marginal.effects$variable)
  subtitle <- unique(marginal.effects$note)
  p <-
    marginal.effects %>%
    ggplot(aes(x = me, y = strat, fill = me)) +
    geom_col(col = "grey") +
    geom_vline(xintercept = 0, linewidth = 2) +
    scale_fill_gradient2(
      low = colors[4],
      mid = "white",
      high = colors[3],
      midpoint = 0
    ) +
    labs(x = "Marginal effect [pp]",
         y = "Strategies",
         title = title,
         subtitle = subtitle) +
    # ggthemes::theme_base() +
    theme(legend.position = "none")

  p
}

