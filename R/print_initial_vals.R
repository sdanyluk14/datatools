#' A Cat Function
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' cat_function()

# Print the first x non-na values for each column in dataframe "df"
print_initial_vals <- function(df, x) {
  for (i in 1:ncol(df)) {
    cat("First ", x, " non-NA values of ", names(df)[i], ":", df[which(!is.na(df[,i]))[1:x],i], "\n")
  }
}