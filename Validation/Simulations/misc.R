#' Colorize function
#'
#' To modify color of the text in an html output.
#'
#' @param x input text string
#' @param color color of the text string
#'
#' @return
#' @export
#'
#' @examples
colorize <- function(x, color) {

  if (knitr::is_latex_output()) {
    sprintf("\\textcolor{%s}{%s}", color, x)
  } else if (knitr::is_html_output()) {
    sprintf("<span style='color: %s;'>%s</span>", color,
            x)
  } else x
}


#' Copy arguments into env and re-bind any function's lexical scope to bindTargetEnv .
#'
#' See http://winvector.github.io/Parallel/PExample.html for example use.
#'
#'
#' Used to send data along with a function in situations such as parallel execution
#' (when the global environment would not be available).  Typically called within
#' a function that constructs the worker function to pass to the parallel processes
#' (so we have a nice lexical closure to work with).
#'
#' @param bindTargetEnv environment to bind to
#' @param objNames additional names to lookup in parent environment and bind
#' @param names of functions to NOT rebind the lexical environments of
bindToEnv <- function(bindTargetEnv=parent.frame(),objNames,doNotRebind=c()) {
  # Bind the values into environment
  # and switch any functions to this environment!
  for(var in objNames) {
    val <- get(var,envir=parent.frame())
    if(is.function(val) && (!(var %in% doNotRebind))) {
      # replace function's lexical environment with our target (DANGEROUS)
      environment(val) <- bindTargetEnv
    }
    # assign object to target environment, only after any possible alteration
    assign(var,val,envir=bindTargetEnv)
  }
}


#' Function to hide print console statements
#'
#' Others' function would have built-in print statements to
#' console that we would like to hide from our markdowns.
#'
#' @param x Others' functions
#'
#' @return
#' @export
#'
#' @examples
quiet <- function(x) {

  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}

#' Round a data frame's numeric vectors by specified decimal place
#'
#' @param x numeric data frame
#' @param digits placement of rounding
#'
#' @return
#' @export
#'
#' @examples
round_df <- function(x, digits) {

  # round all numeric variables
  # x: data frame
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

# take in a vector and convert it to a filename
#' @param vec vector such as c(0, 0.5, 0.2)
#'
#' @return string 00502
convert.vec.to.filename <- function(vec)
{
  out <- paste(vec, collapse = '')
  out <- stringr::str_remove_all(out, "\\.")
  return(out)
}

# generate correlation matrix when correlation rho is fixed
#'
#' @param M dimension of matrix
#' @param rho.scalar fixed rho value
#'
#' @return rho.matrix, M x M matrix with rho.scalar as diag
#' @export
#'
#' @examples
gen_corr_matrix = function(M, rho.scalar)
{
  rho.matrix = diag(M) + rho.scalar * matrix(1, M, M) - rho.scalar * diag(M)
  return(rho.matrix)
}

# generate covariance matrix between two variables
#'
#' @param D dimension of matrix
#' @param var1.vec vector of variances of first variable
#' @param var2.vec vector of variances of second variable
#' @param rho.matrix matrix of correlations
#'
#' @return Sigma matrix of covariance
#' @export
#'
#' @examples
gen_cov_matrix = function(D, var1.vec, var2.vec, rho.matrix) {
  Sigma <- matrix(NA, D, D)
  for(k in 1:D) {
    for(l in 1:D) {
      Sigma[k,l] = rho.matrix[k,l] * sqrt(var1.vec[k]) * sqrt(var2.vec[l])
    }
  }
  return(Sigma)
}

#' generate simple default schools and districts IDs for individual students for
#' simulations. This assumes equal sized schools in equal sized districts
#'
#' @param K number of districts
#' @param J number of schools per district
#' @param n.j number of individuals per school
#'
#' @return list(S.jk, S.k) of school and district assignments for each individual
#' @export
#'
#' @examples

gen_simple_assignments <- function(J, K, n.j){

  N <- n.j * J * K

  # vector of assignments to schools
  S.jk = rep(NA, N)
  start.index = 1
  end.index = n.j
  for(j in 1:(K*J))
  {
    S.jk[start.index:end.index] = j
    start.index = end.index + 1
    end.index = end.index + n.j
  }

  S.k = rep(NA, N)
  start.index = 1
  n.k = N/K
  end.index = n.k
  for(k in 1:K)
  {
    S.k[start.index:end.index] = k
    start.index = end.index + 1
    end.index = end.index + n.k
  }
  stopifnot( all( !is.na( S.jk ) ) )
  stopifnot( all( !is.na( S.k ) ) )

  return(list(S.jk = S.jk, S.k = S.k))
}

#' read in simulation and user parameters
#' and return corresponding file name
#'
#' @param design
#' @param user.params.list
#' @param sim.params.list
#'
#' @return params.file.base
gen_params_file_base <- function(user.params.list, sim.params.list, design)
{
  params.file.base <- paste0(
    design, "_",
    sim.params.list[['S']] * sim.params.list[['Q']], "_S_",
    user.params.list[['M']], "_M_",
    sim.params.list[['B']], "_B_",
    convert.vec.to.filename(user.params.list[['ATE_ES']]),"_ATES_",
    user.params.list[['J']], "_J_",
    user.params.list[['n.j']], "_nj_",
    convert.vec.to.filename(user.params.list[['rho.default']]), "_rho_",
    convert.vec.to.filename(user.params.list[['omega.2']]), "_omega2_",
    convert.vec.to.filename(user.params.list[['R2.1']]),"_R21_",
    convert.vec.to.filename(user.params.list[['R2.2']]),"_R22_"
  )
  return(params.file.base)
}


#' takes in parameters, finds relevant pre-compiled results file, and returns a plot
#'
#' @param params.file.base the base string of the results file you want
#'
#' @return results_plot

gen.power.results.plot <- function(params.file.base, design)
{
  power.file <- find_file(params.file.base, type = 'power')
  if(length(power.file) == 0)
  {
    warning(paste('Results not yet computed for given parameters:', params.file.base))
    results_plot <- NULL
  } else
  {
    power_results <- readRDS(power.file)
    results_plot <- ggplot(power_results, aes(x = MTP, y = value, color = method)) +
      geom_point() +
      geom_line() +
      facet_wrap(~power_type, labeller = label_both) +
      ylab('Power') +
      ggtitle(paste('Design:', design)) +
      ylim(0, 1)
  }

  return(results_plot)
}

#' check for existing validation file
#'
#' @param params.file.base
#'
#' @return comparison.file

find_file <- function(params.file.base, type)
{
  results.files <- list.files(here::here("Validation/data"), full.names = TRUE)
  results.files <- results.files[grep(params.file.base, results.files)]
  results.files <- results.files[grep('comparison', results.files)]
  # return file
  ret.file <- results.files[grep(type, results.files)]
  return(ret.file)
}