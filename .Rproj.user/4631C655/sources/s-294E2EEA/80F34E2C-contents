#' @title Data Quality Report
#'
#' @description Provides a tool to assess the quality and various parameters of the dataframes.
#'
#' @param df Input dataframe
#' @param DQR Name of the function
#'
#' @return NULL
#'
#' @examples DQR(mtcars)
#'
#' @export DQR

DQR <- function(df) {

  #To ignore the warnings during usage
  options(warn=-1)
  options("getSymbols.warning4.0"=FALSE)

  data.frame(
  Names = names(df),
  DataType = sapply(df, class),
  NoofRecordes = nrow(df),
  UniqueRecords = apply(df, 2, function(x) length(unique(x))),
  DataAvailable = nrow(df) - colSums(is.na(df)),
  AvailablePercent = round( (nrow(df)-colSums(is.na(df)))/nrow(df), 3),
  Missing = colSums(is.na(df)),
  MissingPercent = round(colSums(is.na(df))/nrow(df), 3),
  Minimum = apply(dplyr::select_if(df, is.numeric), 2, function(x) min(x, na.rm = T)),
  Maximum = apply(dplyr::select_if(df, is.numeric), 2, function(x) max(x, na.rm = T)),
  Mean = apply(dplyr::select_if(df, is.numeric), 2, function(x) mean(x, na.rm = T)),
  fifthpercentile = apply(dplyr::select_if(df, is.numeric), 2, function(x) quantile(x, p = 0.05, na.rm = T)),
  tenthpercentile = apply(dplyr::select_if(df, is.numeric), 2, function(x) quantile(x, p = 0.10, na.rm = T)),
  twentyfifthpercentile = apply(dplyr::select_if(df, is.numeric), 2, function(x) quantile(x, p = 0.25, na.rm = T)),
  fiftythpercentile = apply(dplyr::select_if(df, is.numeric), 2, function(x) quantile(x, p = 0.50, na.rm = T)),
  sevenyfifthpercentile = apply(dplyr::select_if(df, is.numeric), 2, function(x) quantile(x, p = 0.75, na.rm = T)),
  ninethpercentile = apply(dplyr::select_if(df, is.numeric), 2, function(x) quantile(x, p = 0.90, na.rm = T)),
  ninetyfifthpercentile = apply(dplyr::select_if(df, is.numeric), 2, function(x) quantile(x, p = 0.95, na.rm = T))

)
}
