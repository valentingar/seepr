#' @title seepr_dat
#'
#' @description The central data input class for temperature probes data.
#'
#' @param x A matrix or data frame with the following columns
#' \describe{
#'  \item{datetime}{A vector of length n with the datetime of the measurement.}
#'  \item{T_1}{A vector of length n with the temperature at the first depth in °C.}
#'  \item{T_2}{A vector of length n with the temperature at the second depth in °C}
#'  \item{T_...}{Further temperature measurements}
#'}
#' @param depths A vector with the same length as the number of temperature probes in
#' x. Depth in m from sediement/water interface (down is positive). The depths correspond to the depth of the temperature probes in x read from left
#' to right.
#'
#'
#' @export


# helper
seepr_dat <- function(x,
                      depths){

  # coercing to data.frame
  x <- as.data.frame(x)

  # setting names
  names(x) <- c("datetime", paste0("T_",1:(length(x)-1)))

  stopifnot("For each temperature column a depth must be supplied!" =
              length(x) == length(depths) + 1)
  stopifnot("NAs in depths!" = !anyNA(depths))
  stopifnot("Duplicate depths!" = (length(depths)) == length(unique(depths)))

  stopifnot("Duplicates in datetime column!" =
              length(unique(x[,1])) == length(x[,1]))
  stopifnot("datetime cannot contain NAs" =
              !anyNA(x[,1]))

  if (is.unsorted(x[,1])){
    message("datetime not in increasing order! Trying to sort things out.")
    x <- x %>%
      dplyr::arrange(datetime)
  }

  stopifnot("Data must be a regular time series and cannot contain NAs!" =
              length(unique(diff(x$datetime))) == 1)

  if (is.unsorted(depths)){
    message("rearranging data from top to bottom (left to right)")
    dsi <- sort(depths, index.return = TRUE)
    x <- x[,c(1, dsi+1)]
    depths <- depths[dsi]
  }

  x <- new_seepr_dat(x,
                     depths)

  x
}


# constructor
new_seepr_dat <- function(x,
                          depths){

  stopifnot("At least two depths required!" = length(x) > 2)

  x <- structure(x,
                 class = c("seepr_dat", "data.frame"),
                 depths = depths)
  x
}
