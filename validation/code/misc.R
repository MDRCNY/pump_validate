library(ggplot2)

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

#' read in simulation and user parameters
#' and return corresponding file name
#'
#' @param d_m
#' @param user.params.list
#' @param sim.params.list
#'
#' @return params.file.base
gen_params_file_base <- function(user.params.list, sim.params.list, d_m)
{
  if('WY-SS' %in% sim.params.list[['MTP']] | 'WY-SD' %in% sim.params.list[['MTP']])
  {
    B <- sim.params.list[['B']]
  } else
  {
    B <- NULL
  }
  
  if(is.null(sim.params.list[['Q']]))
  {
    sim.params.list[['Q']] <- 1
  }
  
  params.file.base <- paste0(
    d_m, "_",
    sim.params.list[['S']] * sim.params.list[['Q']], "_S_",
    user.params.list[['M']], "_M_",
    B, "_B_",
    convert.vec.to.filename(user.params.list[['MDES']]),"_MDES_",
    user.params.list[['J']], "_J_",
    user.params.list[['K']], "_K_",
    user.params.list[['nbar']], "_nbar_",
    convert.vec.to.filename(user.params.list[['rho.default']]), "_rho_",
    convert.vec.to.filename(user.params.list[['omega.2']]), "_omega2_",
    convert.vec.to.filename(user.params.list[['omega.3']]), "_omega3_",
    convert.vec.to.filename(user.params.list[['R2.1']]),"_R21_",
    convert.vec.to.filename(user.params.list[['R2.2']]),"_R22_",
    convert.vec.to.filename(user.params.list[['R2.3']]),"_R23_",
    convert.vec.to.filename(user.params.list[['ICC.2']]),"_ICC2_",
    convert.vec.to.filename(user.params.list[['ICC.3']]),"_ICC3_"
  )
  return(params.file.base)
}


#' takes in parameters, finds relevant pre-compiled results file, and returns a plot
#'
#' @param params.file.base the base string of the results file you want
#'
#' @return results_plot

gen.power.results.plot <- function(params.file.base, d_m)
{
  power.file <- find_file(params.file.base, type = 'power')
  if(length(power.file) == 0)
  {
    warning(paste('Results not yet computed for given parameters:', params.file.base))
    results.plot <- NULL
  } else
  {
    power.results <- readRDS(power.file)
    
    power.results$compare = TRUE
    power.results$compare[power.results$method == 'sim'] = FALSE
    
    results.plot <- ggplot(power.results, aes(x = MTP, y = value, color = method)) +
      geom_point(aes(size = compare)) +
      geom_line() +
      scale_size_manual(values = c('TRUE' = 1.5, 'FALSE' = 0)) +
      guides(size = FALSE) +
      facet_wrap(~power_type, labeller = label_both) +
      ylab('Power') +
      ggtitle(paste('d_m:', d_m)) +
      ylim(0, 1)
  }

  return(results.plot)
}

#' check for existing validation file
#'
#' @param params.file.base
#'
#' @return comparison.file

find_file <- function(params.file.base, type, intermediate = FALSE)
{
  if(intermediate)
  {
    results.files <- list.files(here::here("validation/output/intermediate_results"), full.names = TRUE)
  } else
  {
    results.files <- list.files(here::here("validation/output"), full.names = TRUE)
  }
  
  results.files <- results.files[grep(params.file.base, results.files)]
  results.files <- results.files[grep('comparison', results.files)]
  # return file
  ret.file <- results.files[grep(type, results.files)]
  return(ret.file)
}