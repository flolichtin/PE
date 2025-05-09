apollo_pval <- function(model)
{
  round(1 - stats::pnorm(abs(model$estimate)/model$robse), digits = 4)
}

#' @export
apollo_tex <- function(model, output_file = NULL, drop_gofs = FALSE, excl_fixed = TRUE, ...)
{
  if(drop_gofs)
  {
    gof.names <- character(0)
    gof <- numeric(0)
  }
  else
  {
    gofs <- c("Number of respondents" = model$nIndivs,
              # "Number of observations" = model$nObs,
              "Number of parameters" = length(model$apollo_beta) - length(model$apollo_fixed),
              "LL(final)" = model$finalLL,
              "AIC" = model$AIC,
              "BIC" = model$BIC
              # "Inter individual draws" = model$apollo_draws$interNDraws
              # "Intra individual draws" = model$apollo_draws$intraNDraws
    )
    gof.names <- names(gofs)
    gof <- unname(gofs)
  }

  input <- data.frame(
    coef.names = names(model$estimate),
    coef = unname(model$estimate),
    se = unname(model$robse),
    pvalues = apollo_pval(model)
  )

  if (excl_fixed)
  {
    input <- dplyr::filter(input, !is.na(pvalues))
  }

  texmod <- texreg::createTexreg(coef.names = input$coef.names,
                                 coef = input$coef,
                                 se = input$se,

                                 pvalues = input$pvalues,

                                 gof.names = gof.names,
                                 gof = gof,
                                 ...
  )

  texmod
}

#' @export
fashion_show <- function(models, output_file = NULL, ...)
{
  catwalk <-
    map(.x = models, ~ apollo_tex(.x))

  multitex <-
    texreg::texreg(catwalk, custom.model.names = names(catwalk), dcolumn = TRUE, ...)

  if(!is.null(output_file))
    cat(multitex, file = output_file, append=FALSE)

  multitex
}

