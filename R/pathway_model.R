#' Pathway model
#' 
#' @description
#' Estimate the amount of infested commodities that can enter into the
#' countries of interest imported from third countries where the pest is present,
#' and produce the number of potential founder populations (\eqn{NPFP}) that can establish 
#' and spread.
#' 
#' @details
#' ### IDs - country or region identification codes:
#' The use of ISO 3166 (alpha-2) codes 
#' ([ISO 3166 Maintenance Agency](https://www.iso.org/iso-3166-country-codes.html)), 
#' or NUTS codes in the case of European countries 
#' [NUTS - Nomenclature of territorial units for statistics](https://ec.europa.eu/eurostat/web/nuts), 
#' as country or region identifiers (\code{IDs_col}) is recommended 
#' for subsequent compatibility with other functions of the  [qPRAentry] package.
#' 
#' ### Parameter distributions:
#' The following distributions are supported. For details on their parameters, refer 
#' to the linked R documentation:
#'
#' \tabular{lll}{
#' **Distribution** \tab \verb{ } \tab**Documentation** \cr
#' "beta"    \tab \verb{ } \tab [rbeta()] (Beta distribution) \cr
#' "binom"   \tab \verb{ } \tab [rbinom()] (Binomial distribution) \cr
#' "cauchy"  \tab \verb{ } \tab [rcauchy()] (Cauchy distribution) \cr
#' "chisq"   \tab \verb{ } \tab [rchisq()] (Chi-squared distribution) \cr
#' "exp"     \tab \verb{ } \tab [rexp()] (Exponential distribution) \cr
#' "f"       \tab \verb{ } \tab [rf()] (F distribution) \cr
#' "gamma"   \tab \verb{ } \tab [rgamma()] (Gamma distribution) \cr
#' "geom"    \tab \verb{ } \tab [rgeom()] (Geometric distribution) \cr
#' "lnorm"   \tab \verb{ } \tab [rlnorm()] (Log-normal distribution) \cr
#' "nbinom"  \tab \verb{ } \tab [rnbinom()] (Negative Binomial distribution) \cr
#' "norm"    \tab \verb{ } \tab [rnorm()] (Normal distribution) \cr
#' "pois"    \tab \verb{ } \tab [rpois()] (Poisson distribution) \cr
#' "t"       \tab \verb{ } \tab [rt()] (Student's t distribution) \cr
#' "unif"    \tab \verb{ } \tab [runif()] (Uniform distribution) \cr
#' "weibull" \tab \verb{ } \tab [rweibull()] (Weibull distribution) \cr
#' }
#' 
#' For example, to specify a normal distribution with mean 0 and standard deviation 1:\cr
#' \code{list(dist = "norm", mean = 0, sd = 1)}
#'
#' Ensure that all parameters required by the chosen distribution are included.
#' 
#' @param ntrade_data A data frame with the quantity of potentially infested commodities
#' imported from third countries where the pest is present. It can be calculated
#' using [ntrade()] function.
#' @param IDs_col A string specifying the column name in \code{ntrade_data} with the 
#' country or region IDs of interest. See details on 
#' **IDs - country or region identification codes**.
#' @param values_col A string specifying the column name in \code{ntrade_data} with 
#' the \eqn{N_{trade}} values (quantity of potentially infested commodity imports) 
#' to be used in the pathway model.
#' @param expression A string of characters representing the equation for the pathway model.
#' This expression must not include \eqn{N_{trade}}, since it is added multiplicatively to the
#' entered equation by default. This equation is then added multiplicatively to:
#' \deqn{NPFP = N_{trade_i} \cdot \; ``expression"}
#' @param parameters A named list specifying the distributions for each parameter 
#' used in \code{expression}. Each element of the list must be another list containing:
#' \itemize{
#'   \item \code{dist}: A string indicating the distribution name (e.g., "norm", "beta").
#'   \item Additional arguments required by the specified distribution (e.g., \code{mean}, 
#'   \code{sd} for "norm").
#' }
#' See the **Parameter distributions** section for a list of available distributions 
#' and examples on how to specify them.
#' @param niter The number of iterations to generate random samples from the distributions.
#' Default 100 iterations.
#'
#' @return A dataframe with the statistics (mean, SD, minimum, first quartile, 
#' median, third quartile, and maximum) resulting from the iterations of number of 
#' founder populations \eqn{NPFP} for each country/region and for the total (i.e., 
#' the results for the set of all countries/regions).
#'
#' @seealso [ntrade()]
#' 
#' @export
#'
#' @examples
#' ## Example using Northern American countries and ntrade simulated data
#' data("datatrade_NorthAm")
#' # Extract country IDs and simulate ntrade data
#' IDs <- datatrade_NorthAm$internal_production$reporter
#' df <- data.frame(IDs = IDs,
#'                  ntrade_values = abs(rnorm(length(IDs), 10000, 2000)))
#' # Expression for the pathway model using 3 parameters
#' eq <- "(1/P1)*P2*P3"
#' # Distribution for each parameter
#' parameters <- list(
#'   P1 = list(dist = "beta", shape1 = 0.5, shape2 = 1),
#'   P2 = list(dist = "gamma", shape = 1.5, scale = 100),
#'   P3 = list(dist = "lnorm", mean = 5, sd = 2)
#' )
#' # Run pathway_model()
#' res_pathway <- pathway_model(ntrade_data = df,
#'                              IDs_col = "IDs",
#'                              values_col = "ntrade_values",
#'                              expression = eq,
#'                              parameters = parameters,
#'                              niter = 100)
#' head(res_pathway)
#' # summary of the total for all countries
#' res_pathway[res_pathway$IDs == "Total",]
#' # plot
#' plot_countries(res_pathway, "IDs", "Median")
#' 
pathway_model <- function(ntrade_data, IDs_col, values_col,
                          expression, parameters, niter=100){
  
  # Check if the specified columns exist in the dataframe
  if (!all(c(IDs_col, values_col) %in% names(ntrade_data))) {
    stop("The dataframe must contain the columns specified in IDs_col and values_col")
  }
  # Check if parameters is a list
  if (!is.list(parameters)) {
    stop("parameters must be a list")
  }
  
  # Check if parameter names are in the expression
  if(!all(sapply(names(parameters), function(x) grepl(x, expression)))){
    stop("The parameters in 'expression' do not match the parameters specified in 'parameters'")
  }
  
  param_samples <- lapply(parameters, function(distr) {
    dist_name <- paste0("r", distr$dist)
    if (!exists(dist_name)) {
      stop(paste("The distribution function", dist_name, "is not valid"))
    }
    do.call(dist_name, c(list(niter), distr[-1]))
  })
  
  names(param_samples) <- names(parameters)
  mat_samp <- as.matrix(do.call(cbind, param_samples))
  
  # Generic function to evaluate the expression in each matrix row
  eval_expression <- function(row, expression) {
    variables <- names(row)
    for (i in seq_along(variables)) {
      assign(variables[i], row[i])
    }
    eval(parse(text = expression))
  }
  
  res_samp <- apply(mat_samp, 1, function(row) eval_expression(row, expression))
  names(res_samp) <- paste0("res_", 1:niter)
  
  res_df <- map2(res_samp, 1:niter,
                 function(x, i){
                   res <- ntrade_data[values_col] * x
                   setNames(res, paste0("res", i))
                 }) %>%
    bind_cols() %>% 
    bind_cols(select(ntrade_data, !!IDs_col)) %>%
    relocate(!!IDs_col)
  
  total_df <- res_df %>%
    summarise(across(where(is.numeric), sum)) %>% 
    mutate(!!IDs_col := "Total")
  
  res_df <- res_df %>% 
    bind_rows(total_df) %>% 
    rowwise(!!IDs_col) %>% 
    summarise(Mean = mean(c_across(where(is.numeric))),
              SD = sd(c_across(where(is.numeric))),
              Min = min(c_across(where(is.numeric))),
              Q0.25 = quantile(c_across(where(is.numeric)), 0.25),
              Median = median(c_across(where(is.numeric))),
              Q0.75 = quantile(c_across(where(is.numeric)), 0.75),
              Max = max(c_across(where(is.numeric))),
              .groups = "keep")
  
  return(res_df)
}
