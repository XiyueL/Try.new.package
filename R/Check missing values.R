#' Check missing values function
#'
#' This function help check missing values in certain columns in a dataset.
#' @author Xiyue Li
#' @keywords Missing values
#' @import dplyr
#' @param df input dataset
#' @param columns a list of columns to check
#' @return a dataframe with number of missing values for each column
#' @export



check_missing_values <- function(df, columns) {

  missing_cols <- setdiff(columns, colnames(df))
  if (length(missing_cols) > 0) {
    stop(paste("The following columns do not exist in the dataframe:",
               paste(missing_cols, collapse = ", ")))
  }

  na_counts <- sapply(df[columns], function(col) sum(is.na(col)))

  result <- data.frame(Column = columns, Missing_Values = na_counts)

  return(result)
}
