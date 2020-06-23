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
gen.corr.matrix = function(M, rho.scalar)
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
gen.cov.matrix = function(D, var1.vec, var2.vec, rho.matrix) {
  Sigma <- matrix(NA, D, D)
  for(k in 1:D) {
    for(l in 1:D) {
      Sigma[k,l] = rho.matrix[k,l] * sqrt(var1.vec[k]) * sqrt(var2.vec[l])
    }
  }
  return(Sigma)
}

# generate simple defaults for simulations
# this assumes equal sized schools in equal sized districts
#'
#' @param K number of districts
#' @param J number of schools
#' @param n.jk number of individuals per school
#'
#' @return list(S.j, S.k) of school and district assignments for each individual
#' @export
#'
#' @examples

gen_simple_assignments <- function(J, K, n.jk){

  N <- n.jk * J

  # vector of assignments to schools
  S.j = rep(NA, N)
  start.index = 1
  end.index = n.jk
  for(j in 1:J)
  {
    S.j[start.index:end.index] = j
    start.index = end.index + 1
    end.index = end.index + n.jk
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

  return(list(S.j = S.j, S.k = S.k))
}
