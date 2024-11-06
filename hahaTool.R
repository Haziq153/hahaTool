
#####################
#Detect missing value
#####################

#' This function detects missing values in each column of a dataset.
#' It returns a named vector with counts of missing values for each column that has missing data.
#'
#' @param data A data frame or tibble.
#' @return A named vector with counts of missing values for each column.
#' @export
#' @examples

detect_missing_values <- function(data){
  missing_counts <- numeric(ncol(data))
  names(missing_counts) <- colnames(data)

  for(i in 1:ncol(data)){
    missing_counts[i] <- sum(is.na(data[[i]]))
  }
  missing_counts <- missing_counts[missing_counts > 0]
  return(missing_counts)
}


############################
#Calculate median value
############################
#' This function calculates the median of each numeric variable in a dataset.
#'
#' @param dataset A data frame or tibble.
#' @return A data frame with medians of each numeric variable.
#' @export
#' @examples
#' stats_airquality(airquality)

stats_airquality <- function(dataset){
  calculate_iqr <- function(v) {
    IQR(v, na.rm = TRUE)
  }


  numeric_data <- dataset[sapply(dataset ,is.numeric)]
  medians <- sapply(numeric_data,median,na.rm = TRUE)
  iqrs <- sapply(numeric_data , calculate_iqr)
  results_df <- data.frame(Medians= medians)
  return(results_df)

}

stats_results_df <- stats_airquality(airquality)
print(stats_results_df)



########################################
#Replace missing value with median value
########################################
#' This function replaces missing values in numeric columns with the median of each column.
#'
#' @param dataset A data frame or tibble.
#' @return A data frame with missing values replaced by column medians.
#' @export
#' @examples
#' airquality_filled <- replace_missing_with_median(airquality)


replace_missing_with_median <- function(dataset) {

  numeric_data <- dataset[sapply(dataset, is.numeric)]

  for (col_name in colnames(numeric_data)) {
    median_value <- median(dataset[[col_name]], na.rm = TRUE)
    dataset[[col_name]][is.na(dataset[[col_name]])] <- median_value
  }

  return(dataset)
}


sample_data <- data.frame(airquality)
sample_data_filled <- replace_missing_with_median(sample_data)


print(sample_data_filled)
