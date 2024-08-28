#' Read brickSets mapping
#'
#' This function creates an explicit named list with the elements of all BRICK
#' dimensions and corresponding reporting names.
#'
#' @param tmpl character, BRICK reporting template
#' @returns named list with definition of common set elements
#'
#' @author Robin Hasse
#'
#' @importFrom madrat toolGetMapping
#' @importFrom yaml read_yaml
#' @export

readBrickSets <- function(tmpl = NULL) {



  # use default file as basis --------------------------------------------------

  file <- "brickSets.yaml"
  brickSets <- .readMapping(file)



  # replace default sets with custom sets where defined ------------------------

  if (!is.null(tmpl)) {
    if (file.exists(tmpl)) {
      file <- tmpl
      customBrickSets <- read_yaml(file)
    } else {
      file <- paste0("brickSets_", tmpl, ".yaml")
      customBrickSets <- .readMapping(file)
    }
    brickSets[names(customBrickSets)] <- customBrickSets
  }
  message("Read reporting template: ", file)



  # duplicate aliases and childs -----------------------------------------------

  brickSetsExplicit <- list()
  for (dimName in names(brickSets)) {
    dim <- brickSets[dimName]
    aliases <- dim[[1]][["alias"]]
    childs <- dim[[1]][["child"]]
    dim[[1]][c("alias", "child")] <- NULL
    aliasDims <- stats::setNames(rep(dim, length(aliases)), aliases)
    childDims <- list()
    for (childName in names(childs)) {
      childDef <- childs[[childName]]
      childDims[[childName]] <- list()
      for (key in names(childDef)) {
        if (!key %in% names(dim[[1]])) {
          stop("Problem with reporting template ", file,
               " in dimension '", dimName, "': ",
               "The child '", childName, "' uses the unkown key '", key, "'.")
        }
        childDims[[childName]][[key]] <- dim[[1]][[key]][childDef[[key]]]
      }
    }
    brickSetsExplicit <- c(brickSetsExplicit, c(as.list(dim),
                                                aliasDims,
                                                childDims))
  }



  # auto-fill combined dimensions ----------------------------------------------

  # Combined dimensions can be defined without explicit elements.
  # - If elements are missing entirely, all combinations of the primary
  #   dimensions are filled automatically.
  # - If elements are given as a list without reporting names (which requires a
  #   named list) or missing entirely, reporting names are filled automatically
  #   separating the primary reporting names with |
  #   - If all elements of a combined dimension element are identical, the
  #     reporting name is not repeated but used once.

  combinedDims <- grep("\\.", names(brickSetsExplicit), value = TRUE)
  for (cd in combinedDims) {
    dims <- .split(cd)
    if (is.null(brickSetsExplicit[[cd]][["elements"]])) {
      # add all combinatinos of primary dimensions as elements
      brickSetsExplicit[[cd]][["elements"]] <-
        .combinations(lapply(dims, function(dim) {
          names(brickSetsExplicit[[dim]][["elements"]])
        }))
    }
    if (!is.list(brickSetsExplicit[[cd]][["elements"]])) {
      # paste reporting names of primary dimensions
      brickSetsExplicit[[cd]][["elements"]] <- stats::setNames(
        lapply(brickSetsExplicit[[cd]][["elements"]], function(elem) {
          elemVec <- .split(elem)
          if (all(elemVec == elemVec[1])) {
            # no repetition of reporting names if all elements are identical
            return(brickSetsExplicit[[dims[1]]][["elements"]][[elemVec[1]]])
          }
          paste(lapply(seq_along(dims), function(i) {
            brickSetsExplicit[[dims[i]]][["elements"]][[elemVec[i]]]
          }), collapse = "|")
        }),
        brickSetsExplicit[[cd]][["elements"]]
      )
    }
  }


  attr(brickSetsExplicit, "file") <- file
  return(brickSetsExplicit)
}





.readMapping <- function(file) {
  read_yaml(toolGetMapping(name = file,
                           type  = "sectoral",
                           where = "reportbrick",
                           returnPathOnly = TRUE))
}
