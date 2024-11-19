# Default parameters for the pathway app
default_parameters <- c("(1/U_{weight})",
                        "P_{prevalence}",
                        "(1 - P_{sorting})",
                        "(1 - RRO_{effectiveness})")

# Default parameter names for the pathway app
default_parameters_names <- c("$U_{weight}$",
                              "$P_{prevalence}$",
                              "$P_{sorting}$",
                              "$RRO_{effectiveness}$")

# Distribution names for the pathway app
distribution_names <- c(
  "Beta (including scaled Beta)" = "beta",
  "Binomial (including Bernoulli)" = "binom",
  "Cauchy" = "cauchy",
  "Chi-squared" = "chisq",
  "Exponential" = "exp",
  "F" = "f",
  "Gamma" = "gamma",
  "Geometric (special case of the negative binomial)" = "geom",
  # "Hypergeometric" = "hyper",
  "Log-normal" = "lnorm",
  # "Multinomial" = "multinom",
  "Negative binomial" = "nbinom",
  "Normal" = "norm",
  "Poisson" = "pois",
  "Student's t" = "t",
  "Uniform" = "unif",
  "Weibull" = "weibull"
)

#' Creates a panel of conditional input elements based on the distribution type.
#'
#' This function generates a dynamic panel of conditional input elements
#' depending on the specified distribution type.
#'
#' @param id_dist Identifier for the distribution type.
#' 
#' @return A tagList object containing conditional input elements for the specified 
#' distribution.
#' 
#' @noRd
#' @keywords internal
distribution_panel <- function(ns, id_dist){
  tagList(
    conditionalPanel(condition=(paste0("input.dist", id_dist, "== 'beta'")),
                     ns = ns,
                     textInput(ns(paste0('par_beta_', id_dist)),
                               'shape1, shape2, min, max',
                               "1, 1, 0, 1")
    ),
    conditionalPanel(condition=(paste0("input.dist", id_dist, "== 'binom'")),
                     ns = ns,
                     textInput(ns(paste0('par_binom_',id_dist)), 'trials, probability',
                               "1, 0.5")),
    conditionalPanel(condition=(paste0("input.dist", id_dist, "== 'cauchy'")),
                     ns = ns,
                     textInput(ns(paste0('par_cauchy_',id_dist)), 'location, scale',
                               "0, 1")),
    conditionalPanel(condition=(paste0("input.dist", id_dist, "== 'chisq'")),
                     ns = ns,
                     textInput(ns(paste0('par_chisq_',id_dist)), 'df',
                               "1")),
    conditionalPanel(condition=(paste0("input.dist", id_dist, "== 'exp'")),
                     ns = ns,
                     textInput(ns(paste0('par_exp_',id_dist)), 'rate',
                               "1")),
    conditionalPanel(condition=(paste0("input.dist", id_dist, "== 'f'")),
                     ns = ns,
                     textInput(ns(paste0('par_f_',id_dist)), 'df1, df2',
                               "1, 1")),
    conditionalPanel(condition=(paste0("input.dist", id_dist, "== 'gamma'")),
                     ns = ns,
                     textInput(ns(paste0('par_gamma_',id_dist)), 'shape, scale',
                               "1, 1")),
    conditionalPanel(condition=(paste0("input.dist", id_dist, "== 'geom'")),
                     ns = ns,
                     textInput(ns(paste0('par_geom_',id_dist)), 'probability',
                               "0.5")),
    conditionalPanel(condition=(paste0("input.dist", id_dist, "== 'lnorm'")),
                     ns = ns,
                     textInput(ns(paste0('par_lnorm_',id_dist)), 'meanlog, sdlog',
                               "0, 1")),
    conditionalPanel(condition=(paste0("input.dist", id_dist, "== 'nbinom'")),
                     ns = ns,
                     textInput(ns(paste0('par_nbinom_',id_dist)), 'trials, probability',
                               "1, 0.5")),
    conditionalPanel(condition=(paste0("input.dist", id_dist, "== 'norm'")),
                     ns = ns,
                     textInput(ns(paste0('par_norm_',id_dist)), 'mean, sd',
                               "0, 1")),
    conditionalPanel(condition=(paste0("input.dist", id_dist, "== 'pois'")),
                     textInput(ns(paste0('par_pois_',id_dist)), 'lambda',
                               "1")),
    conditionalPanel(condition=(paste0("input.dist", id_dist, "== 't'")),
                     ns = ns,
                     textInput(ns(paste0('par_t_',id_dist)), 'df',
                               "1")),
    conditionalPanel(condition=(paste0("input.dist", id_dist, "== 'unif'")),
                     ns = ns,
                     textInput(ns(paste0('par_unif_',id_dist)), 'min, max',
                               "0, 1")),
    conditionalPanel(condition=(paste0("input.dist", id_dist, "== 'weibull'")),
                     ns = ns,
                     textInput(ns(paste0('par_weibull_', id_dist)), 'shape, scale',
                               "1, 1"))
  )#tagList
}


#' Generates random numbers from a specified distribution.
#'
#' This function generates random numbers from a specified distribution based on
#' the input parameters and number of iterations.
#'
#' @param dist Distribution type (e.g., 'beta', 'binom', 'norm').
#' @param input_pars Comma-separated string of distribution parameters.
#' @param n_iter Number of iterations to generate random numbers.
#' 
#' @return A numeric vector of random numbers generated from the specified distribution.
#' 
#' @noRd
#' @keywords internal
n_from_dist <- function(dist, input_pars, n_iter){
  set.seed(123)
  #character comma separated to numeric
  pars <- as.numeric((unlist(strsplit(input_pars, ","))))
  if(dist == 'beta'){
    n <- rbeta(n_iter, pars[1], pars[2])
    # Scaled
    if(pars[3]!=0 | pars[4]!=1){
      n <- pars[3] + n *(pars[4]-pars[3])
    }else{n}
  }else if(dist == 'binom'){
    n <- rbinom(n_iter, pars[1], pars[2])
  }else if(dist == 'cauchy'){
    n <- rcauchy(n_iter, pars[1], pars[2])
  }else if(dist == 'chisq'){
    n <- rchisq(n_iter, pars[1])
  }else if(dist == 'exp'){
    n <- rexp(n_iter, pars[1])
  }else if(dist == 'f'){
    n <- rf(n_iter, pars[1], pars[2])
  }else if(dist == 'gamma'){
    n <- rgamma(n_iter, shape=pars[1], scale=pars[2])
  }else if(dist == 'geom'){
    n <- rgeom(n_iter, pars[1])
  }else if(dist == 'lnorm'){
    n <- rlnorm(n_iter, pars[1], pars[2])
  }else if(dist == 'nbinom'){
    n <- rnbinom(n_iter, pars[1], pars[2])
  }else if(dist == 'norm'){
    n <- rnorm(n_iter, pars[1], pars[2])
  }else if(dist == 'pois'){
    n <- rpois(n_iter, pars[1])
  }else if(dist == 't'){
    n <- rt(n_iter, pars[1])
  }else if(dist == 'unif'){
    n <- runif(n_iter, pars[1], pars[2])
  }else if(dist == 'weibull'){
    n <- rweibull(n_iter, pars[1], pars[2])
  }
  return(n)
}

#' Generates quantiles from a specified distribution.
#'
#' This function generates quantiles (0.05, 0.25, 0.5, 0.75, 0.95) from a 
#' specified distribution based on the input parameters.
#'
#' @param dist Distribution type (e.g., 'beta', 'binom', 'norm').
#' @param input_pars Comma-separated string of distribution parameters.
#' 
#' @return A numeric vector.
#' 
#' @noRd
#' @keywords internal
q_from_dist <- function(dist, input_pars){
  set.seed(123)
  p <- c(0.05, 0.25, 0.5, 0.75, 0.95)
  #character comma separated to numeric
  pars <- as.numeric((unlist(strsplit(input_pars, ","))))
  if(dist == 'beta'){
    n <- qbeta(p, pars[1], pars[2])
     # Scaled
    if(pars[3]!=0 | pars[4]!=1){
      n <- pars[3] + n *(pars[4]-pars[3])
    }else{n}
  }else if(dist == 'binom'){
    n <- qbinom(p, pars[1], pars[2])
  }else if(dist == 'cauchy'){
    n <- qcauchy(p, pars[1], pars[2])
  }else if(dist == 'chisq'){
    n <- qchisq(p, pars[1])
  }else if(dist == 'exp'){
    n <- qexp(p, pars[1])
  }else if(dist == 'f'){
    n <- qf(p, pars[1], pars[2])
  }else if(dist == 'gamma'){
    n <- qgamma(p, shape=pars[1], scale=pars[2])
  }else if(dist == 'geom'){
    n <- qgeom(p, pars[1])
  }else if(dist == 'lnorm'){
    n <- qlnorm(p, pars[1], pars[2])
  }else if(dist == 'nbinom'){
    n <- qnbinom(p, pars[1], pars[2])
  }else if(dist == 'norm'){
    n <- qnorm(p, pars[1], pars[2])
  }else if(dist == 'pois'){
    n <- qpois(p, pars[1])
  }else if(dist == 't'){
    n <- qt(p, pars[1])
  }else if(dist == 'unif'){
    n <- qunif(p, pars[1], pars[2])
  }else if(dist == 'weibull'){
    n <- qweibull(p, pars[1], pars[2])
  }
  df <- data.frame("Q0.05"=round(n[1],4),
                   "Q0.25"=round(n[2],4),
                   "Q0.5" =round(n[3],4),
                   "Q0.75"=round(n[4],4),
                   "Q0.95"=round(n[5],4))
  return(df)
}

