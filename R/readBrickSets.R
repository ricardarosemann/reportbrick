#' Read brickSets mapping
#'
#' @param tmpl character, BRICK reporting template
#' @returns named list with definition of common set elements
#'
#' @importFrom madrat toolGetMapping
#' @importFrom yaml read_yaml

readBrickSets <- function(tmpl) {
  # use default file as basis
  file <- "brickSets.yaml"
  brickSets <- .readMapping(file)

  # replace default sets with custom sets where defined
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

  # duplicate aliases and childs
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


  attr(brickSetsExplicit, "file") <- file
  return(brickSetsExplicit)
}





.readMapping <- function(file) {
  toolGetMapping(name = file,
                 type  = "sectoral",
                 where = "reportbrick",
                 returnPathOnly = TRUE) %>%
    read_yaml()
}
