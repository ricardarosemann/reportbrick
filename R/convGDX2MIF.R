#' Read in GDX from BRICK and write *.mif reporting
#'
#' Read in all information from GDX file that was generated with BRICK and
#' create the *.mif reporting
#'
#' @param gdx file path to a BRICK gdx
#' @param tmpl character, BRICK reporting template. There has to be a brickSets
#'   mapping named with the same suffix: \code{brickSets_<tmpl>.yaml}
#' @param file name of the mif file which will be written, if no name is
#'   provided a magpie object containing all the reporting information is
#'   returned
#' @param scenario scenario name that is used in the *.mif reporting
#' @param t numeric vector of reporting periods (years)
#'
#' @author Robin Hasse
#'
#' @importFrom magclass mbind add_dimension write.report getSets<-
#' @importFrom utils capture.output
#' @export

convGDX2MIF <- function(gdx,
                        tmpl = NULL,
                        file = NULL,
                        scenario = "default",
                        t = NULL) {

  # PREPARE --------------------------------------------------------------------

  # common time steps
  if (is.null(t)) {
    t <- as.numeric(as.character(readGdxSymbol(gdx, "ttot", asMagpie = FALSE)[[1]]))
  }

  brickSets <- readBrickSets(tmpl)
  setElements <- .findInconsistenSetElements(brickSets, gdx)
  if (!is.null(setElements[["surplus"]])) {
    warning("The following set elements are mapped in the reporting template ",
            "but not found in the gdx:\n  ",
            paste(capture.output(setElements[["surplus"]]), collapse = "\n  "))
  }
  if (!is.null(setElements[["missing"]])) {
    stop("The following set elements are found in the gdx but not mapped in ",
         "the reporting template:\n  ",
         paste(capture.output(setElements[["missing"]]), collapse = "\n  "))
  }

  # central object containing all output data
  output <- NULL



  # REPORT VARIABLES -----------------------------------------------------------

  ## Stock ====
  message("running reportBuildingStock ...")
  output <- mbind(output, reportBuildingStock(gdx, brickSets)[, t, ])

  ## Construction ====
  message("running reportConstruction ...")
  output <- mbind(output, reportConstruction(gdx, brickSets)[, t, ])

  ## Demolition ====
  message("running reportDemolition ...")
  output <- mbind(output, reportDemolition(gdx, brickSets)[, t, ])

  ## Energy ====
  message("running reportEnergy ...")
  output <- mbind(output, reportEnergy(gdx, brickSets)[, t, ])


  # FINISH ---------------------------------------------------------------------

  if (length(output) == 0) {
    stop("Unable to report any variable.")
  }

  # Add dimension names "scenario.model.variable"
  getSets(output)[3] <- "variable"
  output <- add_dimension(output, dim = 3.1, add = "model", nm = "BRICK")
  output <- add_dimension(output, dim = 3.1, add = "scenario", nm = scenario)

  # either write the *.mif or return the magpie object
  if (!is.null(file)) {
    write.report(output, file = file, ndigit = 7)
    message("MIF file written: ", file)
  } else {
    return(output)
  }

}



#' Find inconsistencies in set elements between reporting template and gdx
#'
#' @param brickSets character, BRICK reporting template
#' @param gdx file path to a BRICK gdx
#' @returns named list of missing and surplus sets elements

.findInconsistenSetElements <- function(brickSets, gdx) {
  out <- do.call(rbind, lapply(names(brickSets), function(s) {
    elementsGdx <- as.character(readGdxSymbol(gdx, s, asMagpie = FALSE)[[1]])
    elementsMap <- names(brickSets[[s]][["elements"]])
    inconsistencies <- list(missing = setdiff(elementsGdx, elementsMap),
                            surplus = setdiff(elementsMap, elementsGdx))
    do.call(rbind, lapply(names(inconsistencies), function(i) {
      if (length(inconsistencies[[i]]) > 0) {
        data.frame(set = s, inconsistency = i, element = inconsistencies[[i]])
      } else {
        NULL
      }
    }))
  }))
  if (is.null(out)) {
    return(NULL)
  }
  split(out[c("set", "element")], out[["inconsistency"]])
}
