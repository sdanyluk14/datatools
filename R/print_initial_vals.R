#' Print Intitial Vals Function
#'
#' Given a dataframe and a number x, prints the first x non-na values of each column (though prints first 5 values if no number "x" is specified).
#' @param df A DataFrame
#' @param x Number of non-na items to print for each column.
#' @keywords dataframe
#' @export
#' @examples
#' print_initial_vals(iris)

# Print the first x non-na values for each column in dataframe "df"
print_initial_vals <- function(df, x = 5) {
  for (i in 1:ncol(df)) {
    cat("First ", x, " non-NA values of ", names(df)[i], ":", df[which(!is.na(df[,i]))[1:x],i], "\n")
  }
}