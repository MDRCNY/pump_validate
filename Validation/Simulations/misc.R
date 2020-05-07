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


