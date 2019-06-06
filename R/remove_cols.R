#' Remove Cols Function
#'
#' Given a dataframe and a number x, prints the first x non-na values of each column (though prints first 5 values if no number "x" is specified).
#' @param df A DataFrame
#' @param x Number of non-na items to print for each column.
#' @keywords dataframe
#' @export
#' @examples
#' print_initial_vals(iris)

remove_cols <- function(df, x) {
  # Remove crap accountnumber columns
  cols_to_remove <- c()
  for (i in 1:ncol(df)) {
    for (j in 1:length(x)) {
      if (grepl(x[j], names(df)[i])) {
        cols_to_remove <- c(cols_to_remove, i)
      }
    }
    
  }
  temp <- df[,-cols_to_remove]
  return(temp)
}