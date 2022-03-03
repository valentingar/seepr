#' @title trend
#' @description Extract the trend component from a seepr_dhr regression
#' result.
#'
#' @param x A seepr_dhr regression result.
#'
#' @export

trend <- function(x){
  UseMethod("trend")
}

#' @exportS3Method
trend.seepr_dhr <- function(x){
  trend_out <- x$m[,1]
  trend_out
}


