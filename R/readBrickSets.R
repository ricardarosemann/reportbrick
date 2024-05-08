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





.readMapping <- function(file) {
  toolGetMapping(name = file,
                 type  = "sectoral",
                 where = "reportbrick",
                 returnPathOnly = TRUE) %>%
    read_yaml()
}
