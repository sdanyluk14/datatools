#' Paste Function
#'
#' Given a dataframe, a vector of column numbers, and a string, this adds the string onto each defined column name.
#' Returns dataframe with pasted names.
#' 
#' @param df A DataFrame
#' @param cols A vector of column numbers to paste the string onto
#' @param string The string to paste onto the beginning of each specified column name
#' @keywords dataframe
#' @export
#' @examples
#' paste_func(iris, c(1:ncol(iris)), "PASTETHIS_")

paste_func <- function(df, cols, string) {
  for (i in cols) {
    names(df)[i] <- paste0(string, names(df)[i])
  }
  return(df)
}