#' @title seepr_dhr
#' @description The central class of the dhr model.
#'
#' @param x The result of a call to \code{\link{dhr()}}
#'
#' @details The class is a subclass of 'dlmFiltered' from the package "dlm"
#' with special methods implemented for this particular usecase.
#'
#' @export

new_seepr_dhr <- function(x,
                          s){

  #stopifnot(inherits(x,"dlmFiltered"))

  x <-
  structure(x,
            class = c("seepr_dhr",class(x)),
            s = s)
  x
}




####### METHODS ######## -----------------

#' @export
dhr_trend <- function(x){
  UseMethod("dhr_trend")
}
#' @exportS3Method
dhr_trend.seepr_dhr <- function(x){
  x$m[,1]
}


#' @export
dhr_amp_sin <- function(x){
  UseMethod("dhr_amp_sin")
}
#' @exportS3Method
dhr_amp_sin.seepr_dhr <- function(x){
  x$m[,2]
}


#' @export
dhr_amp_cos <- function(x){
  UseMethod("dhr_amp_cos")
}
#' @exportS3Method
dhr_amp_cos.seepr_dhr <- function(x){
  x$m[,4]
}
