#' cleanNAs Function
#'
#' Given a dataframe, this changes all NA, NULL, and empty string values to -9s.
#' 
#' @param df A DataFrame
#' @keywords dataframe
#' @export
#' @examples
#' test <- data.frame(x = c(NA, NA, 1))
#' cleanNAs(test)

cleanNAs <- function(df) {
  for (i in 1:ncol(df)) {
    if (is.factor(df[,i])) {
      df[,i] <- as.character(df[,i])
      df[,i][which(is.na(df[,i]))] <- "-9999999"
      df[,i][which(df[,i] == "NULL")] <- "-9999999"
      df[,i][which(df[,i] == " ")] <- "-9999999"
    }
    else {
      df[,i][which(is.na(df[,i]))] <- -9999999
      df[,i][which(is.null(df[,i]))] <- -9999999
      df[,i][which(df[,i] == " ")] <- "-9999999"
    }
  }
  return(df)
}