#' Pathway model
#'
#' Estimate the amount of infested commodities that can enter into the
#' countries of interest imported from third countries where the pest is present,
#' and produce the number of potential founder populations (NPFP) that can establish and spread.
#'
#' @param ntrade_data A data frame with the quantity of potentially infested commodities
#' imported from third countries where the pest is present. It can be calculated
#' using \code{\link{ntrade}} function.
#' @param IDs_col Column name in \code{ntrade_data} with the country IDs of interest.
#' @param values_col Column name in \code{ntrade_data} with the \eqn{N_{trade}} values
#' (quantity of potentially infested commodity imports) to be used in the pathway model.
#' @param expression A string of characters representing the equation for the pathway model.
#' This expression must not include \eqn{N_{trade}}, since it is added multiplicatively to the
#' entered equation by default. This equation is then added multiplicatively to:
#' \deqn{NPFP = N_{trade_i} \cdot \; ...}
#' @param parameters Named list specifying the distributions for each parameter 
#' included in \code{expression}. Each list element should be a list containing 
#' the distribution name (e.g., \code{dist = "norm"}) and its arguments (e.g., 
#' \code{mean = 0, sd = 1}). The available distributions are:
#' \tabular{ll}{
#' "beta" \tab   see \code{\link[stats]{rbeta}}\cr
#'   \tab \cr
#' "binom" \tab  see \code{\link[stats]{rbinom}}\cr
#'   \tab \cr
#' "cauchy" \tab  see \code{\link[stats]{rcauchy}}\cr
#'   \tab \cr
#' "chisq" \tab  see \code{\link[stats]{rchisq}}\cr
#'   \tab \cr
#' "exp" \tab    see \code{\link[stats]{rexp}}\cr
#'   \tab \cr
#' "f"  \tab     see \code{\link[stats]{rf}}\cr
#'   \tab \cr
#' "gamma" \tab  see \code{\link[stats]{rgamma}}\cr
#'   \tab \cr
#' "geom" \tab   see \code{\link[stats]{rgeom}}\cr
#'   \tab \cr
#' "lnorm" \tab  see \code{\link[stats]{rlnorm}}\cr
#'   \tab \cr
#' "nbinom" \tab  see \code{\link[stats]{rnbinom}}\cr
#'   \tab \cr
#' "norm"  \tab  see \code{\link[stats]{rnorm}}\cr
#'   \tab \cr
#' "pois" \tab   see \code{\link[stats]{rpois}}\cr
#'   \tab \cr
#' "t"  \tab     see \code{\link[stats]{rt}}\cr
#'   \tab \cr
#' "unif" \tab   see \code{\link[stats]{runif}}\cr
#'   \tab \cr
#' "weibull" \tab see \code{\link[stats]{rweibull}}\cr
#' }
#' @param niter The number of iterations to generate random samples from the distributions.
#' Default 100 iterations.
#'
#' @return A dataframe with the statistics (mean, SD, minimum, first quartile, 
#' median, third quartile, and maximum) of founder population \eqn{NPFP} for each
#' country or region and for the total. [To be completed]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # pathway model equation (is multiplied by Ntrade value)
#' eq <- "(1/p1)*p2*p3"
#' # distribution for each parameter
#' parameters <- list(
#'   p1 = list(dist = "beta", shape1 = 0.5, shape2 = 1),
#'   p2 = list(dist = "gamma", shape = 1.5, scale = 100),
#'   p3 = list(dist = "norm", mean = 5, sd = 2)
#' )
#' res <- pathway_model(ntrade_data = Nt_df,
#'                      IDs_col = "IDs",
#'                      values_col = "value",
#'                      expression = eq,
#'                      parameters = parameters,
#'                      niter = 100)
#' head(res)
#' # Total
#' res %>% filter(IDs == "Total")
#' }
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
    do.call(dist_name, c(list(niter), dist[-1]))
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
