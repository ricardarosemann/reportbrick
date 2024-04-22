#' Report aggregated quantities
#'
#' @param x MagPIE object, BRICK object
#' @param name character, name of reporting variable. reported dimensions passed
#'   with \code{rprt} have to be escaped with curly brackets.
#' @param tmpl character, BRICK reporting template
#' @param agg named vector of dimensions to aggregate. Names are dimension names
#'   of \code{x} and values are either set elements or subsets of set elements
#'   to consider.
#' @param rprt named vector of dimensions to report individually. Names are
#'   dimension names of \code{x} and values are either set elements or subsets
#'   of set elements to report.
#' @param silent boolean, suppress warnings and printing of dimension mapping
#'
#' @author Robin Hasse
#'
#' @importFrom magclass getSets
#' @importFrom utils tail

reportAgg <- function(x,
                      name,
                      tmpl = NULL,
                      agg = NULL,
                      rprt = NULL,
                      silent = TRUE) {

  # CHECK INPUT ----------------------------------------------------------------

  dims <- unname(tail(getSets(x, 3), -2))

  # check that each dimension is either aggregated or reported
  missingDims <- setdiff(dims, c(names(agg), names(rprt)))
  if (length(missingDims) > 0) {
    stop("The following dimensions are neither aggregated nor reported ",
         "individually: ", paste(missingDims, collapse = ", "))
  }
  if (!setequal(dims, c(names(agg), names(rprt)))) {
    stop("Each of the following dimension has to be either aggregated or ",
         "reported individually: ", paste(dims, collapse = ", "),
         " You want to aggegate ", paste(names(agg), collapse = ", "),
         " and report ", paste(names(rprt), collapse = ", "), ".")
  }

  # find all tags escaped in curly brackets
  tagsInName <- gregexpr("\\{[a-z]+\\}", name)[[1]]
  tagsInName <- if (all(tagsInName == -1)) {
    NULL
  } else {
    unlist(lapply(seq_along(tagsInName), function(i) {
      substr(name,
             tagsInName[i] + 1,
             tagsInName[i] + attr(tagsInName, "match.length")[i] - 2)
    }))
  }

  # check if all reported dimensions are tagged in name
  if (is.null(rprt)) {
    if (!is.null(tagsInName)) {
      stop("Found the following dimension tags in 'name', though 'rprt' is NULL: ",
           paste(tagsInName, collapse = ", "))
    }
  } else {
    if (!setequal(names(rprt), tagsInName)) {
      stop("Inconsistency between name tags (", paste(tagsInName, collapse = ", "),
           ") and reported dimensions (", paste(names(rprt)), ").")
    }
  }



  # PREPARE --------------------------------------------------------------------

  brickSets <- .readBrickSets(tmpl)

  # list with dimension elements to consider for aggregation and reporting
  map <- .constructDimMapping(agg, rprt, brickSets, silent)
  if (isFALSE(silent)) {
    message(map)
  }



  # AGGREGATE TO REPORTING VARS ------------------------------------------------

  if (is.null(rprt)) {
    out <- .agg(x, map$agg) %>%
      .setNames(name)
  } else {
    # combination of entries of reporting dimensions
    rprtCombinations <- do.call(expand.grid, map$rprt)

    # loop over reporting combinations
    out <- do.call(mbind, apply(rprtCombinations, 1, function(comb) {

      # replace dimension tags to get final variable name
      outName <- name
      for (r in names(comb)) {
        outName <- sub(.tag(r),
                       brickSets[[r]][["elements"]][[comb[[r]]]],
                       outName, fixed = TRUE)
      }

      # select combination of reporting values and aggregate to final variable
      do.call(mselect, c(list(x = x), comb)) %>%
        .agg(map$agg) %>%
        .setNames(outName)
    }, simplify = FALSE))
  }


  return(out)
}





#' Construct dimension mapping (either aggregation or reporting) with explicit
#' set elements for each dimension
#'
#' @param agg named vector of dimensions to aggregate
#' @param rprt named vector of dimensions to report individually
#' @param brickSets named list with definition of common set elements
#' @param silent boolean, suppress warnings and printing of dimension mapping
#' @returns nested named list with dimension mapping

.constructDimMapping <- function(agg, rprt, brickSets, silent) {
  lapply(list(agg = agg, rprt = rprt), function(m) {

    mElements <- lapply(names(m), function(d) {
      # check if dim is defined in brickSets
      if (!(d %in% names(brickSets))) {
        stop("The brick sets file ", attr(brickSets, "file"), "has no mapping ",
             "for the dimension '", d, "'.")
      }

      # explicit list of dimension elements
      unlist(lapply(m[[d]], function(val) {

        if (val %in% names(brickSets[[d]][["elements"]])) {
          return(val)
        }
        if (val %in% names(brickSets[[d]][["subsets"]])) {
          return(brickSets[[d]][["subsets"]][[val]])
        } else {
          if (isFALSE(silent)) {
            warning("No correspondant for '", val, "' as an element of'"
                    , d, " in brick sets file: ", attr(brickSets, "file"))
          }
          return(list(NULL))
        }
      }))
    })
    names(mElements) <- names(m)
    return(mElements)
  })
}





#' Read brickSets mapping
#'
#' @param tmpl character, BRICK reporting template
#' @returns named list with definition of common set elements
#'
#' @importFrom madrat toolGetMapping
#' @importFrom yaml read_yaml

.readBrickSets <- function(tmpl) {

  readIt <- function(file) {
    toolGetMapping(name = file,
                   type  = "sectoral",
                   where = "reportbrick",
                   returnPathOnly = TRUE) %>%
      read_yaml()
  }

  file <- "brickSets.yaml"
  brickSets <- readIt(file)

  # replace default sets with custom sets where defined
  if (!is.null(tmpl)) {
    file <- paste0("brickSets_", tmpl, ".yaml")
    customBrickSets <- readIt(file)
    brickSets[names(customBrickSets)] <- customBrickSets
  }

  # duplicate aliases
  brickSetsExplicit <- list()
  for (dimName in names(brickSets)) {
    dim <- brickSets[dimName]
    aliases <- dim[[1]][["alias"]]
    dim[[1]][["alias"]] <- NULL
    aliasDims <- rep(dim, length(aliases))
    names(aliasDims) <- aliases
    brickSetsExplicit <- c(brickSetsExplicit, c(as.list(dim), aliasDims))
  }

  attr(brickSetsExplicit, "file") <- file
  return(brickSetsExplicit)
}





#' Create tag
#'
#' Escape dimension name in curly brackets
#'
#' @param dim character dimension name
#' @returns character, dimension tag

.tag <- function(dim) {
  paste0("{", dim, "}")
}





#' Aggregate
#'
#' @param x MagPIE object, BRICK object
#' @param agg named vector of dimensions to aggregate.
#' @returns aggregated MagPIE objects without sub dimensions in dim 3
#'
#' @importFrom magclass dimSums mselect

.agg <- function(x, agg) {

  # return NULL if any element in any dimension is missing
  missingElements <- .missingElements(x, agg)
  if (length(missingElements) > 0) {
    return(NULL)
  }

  do.call(mselect, c(list(x = x, agg))) %>%
    dimSums(na.rm = TRUE)
}





#' List any missing elements in any given dimension
#'
#' @param x MagPIE object, BRICK object
#' @param dimLst named vector, dimension ans names and elements as values
#' @returns vector of missing dimension elements in x
#'
#' @importFrom magclass getItems

.missingElements <- function(x, dimLst) {
  missingDims <- setdiff(names(dimLst), getSets(x))
  if (length(missingDims) > 0) {
    stop("The following dimensions are listed in 'dimLst' but missing in 'x': ",
         paste(missingDims, collapse = ", "))
  }
  unlist(lapply(names(dimLst), function(dim) {
    if (!dim %in% getSets(x)) {
      stop("x has no dimension call")
    }
    setdiff(dimLst[[dim]], getItems(x, dim = dim))
  }))
}





#' Set names is not NULL
#'
#' @param object MAgPIE object
#' @param nm a vector of names current names should be replaced with. If only
#'   one data element exists you can also set the name to NULL.
#' @returns NULL if object is NULL else object with manipulated names
#'
#' @importFrom magclass setNames

.setNames <- function(object, nm) {
  if (is.null(object)) {
    return(NULL)
  }
  setNames(object, nm)
}
