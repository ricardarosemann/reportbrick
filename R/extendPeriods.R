#' Crop MagPIE object to given periods and fill missing periods
#'
#' @param x MagPIE object
#' @param periods numeric vector with target periods
#' @param fill value to fill missing period values
#' @returns MagPIE object with given periods in temporal dimension
#'
#' @author Robin Hasse
#'
#' @importFrom magclass getYears new.magpie getItems getNames

extendPeriods <- function(x, periods, fill = NA) {
  r <- getItems(x, dim = 1)
  n <- getNames(x)
  t <- intersect(getYears(x, as.integer = TRUE), periods)
  out <- new.magpie(cells_and_regions = r, years = periods, names = n, fill = fill)
  out[r, t, ] <- x[r, t, ]
  return(out)
}
