#' Missing Cols Function
#'
#' Given a dataframe and a number x, prints the first x non-na values of each column (though prints first 5 values if no number "x" is specified).
#' @param df A DataFrame
#' @param x Number of non-na items to print for each column.
#' @keywords dataframe
#' @export
#' @examples
#' print_initial_vals(iris)

missing_cols <- function(df, x) {
  columns <- c()
  for (i in 1:ncol(df)) {
    if (((sum(is.na(df[,i])) + sum(df[,i] == "-9999999", na.rm = T))/nrow(df)) > x) {
      columns <- c(columns, i)
    }
  }
  return(columns)
}