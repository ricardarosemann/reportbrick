#' report results from matching aggregation
#'
#' @param path character, path to the matching folder
#' @param pathAgg character, path to the matching aggregation.
#'   Can be a relative path with respect to the matching folder.
#'   If \code{NULL}: Use the newest aggregation in the matching folder
#'
reportMatchingAggregation <- function(path, pathAgg = NULL) {



  # Functions ------------------------------------------------------------------

  .readMatchAndAgg <- function(gdxes, symbol) {
    lapply(gdxes, function(g) {
      readGdxSymbol(g, symbol = symbol, asMagpie = FALSE)
    })
  }



  # Prepare --------------------------------------------------------------------

  gdx <- file.path(path, "output.gdx")
  gdxInput <- file.path(path, "input.gdx")
  gdxAgg <- file.path(pathAgg, "output.gdx")

  gdxes <- c(gdx, gdxAgg)
  names(gdxes) <- c("matching", "aggregation")

  # Read config
  cfg <- read_yaml(file = file.path(path, "config", "config_COMPILED.yaml"))

  # Determine relevant variables
  if (isTRUE(cfg[["switches"]][["SEQUENTIALREN"]])) {
    varAll <- c("stock", "construction", "demolition", "renovationBS", "renovationHS")
  } else {
    varAll <- c("stock", "construction", "demolition", "renovation")
  }

  namingMap <- c(
    stock = "stock",
    construction = "con",
    demolition = "dem",
    renovation = "ren",
    renovationBS = "renBS",
    renovationHS = "renHS"
  )



  # Read -----------------------------------------------------------------------

  v_stock <- .readMatchAndAgg(gdxes, "v_stock")

  v_construction <- .readMatchAndAgg(gdxes, "v_construction")

  v_demolition <- .readMatchAndAgg(gdxes, "v_demolition")

  v_renovationHS <- .readMatchAndAgg(gdxes, "v_renovationHS")

  v_renovationHSEff <- lapply(v_renovationHS, function(ren) {
    filter(ren, .data$hsr != 0)
  })

  out <- c(
    stats::setNames(v_renovationHSEff, c("renHSEffMatch", "renHSEffAgg"))
  )



  # Share computation ----------------------------------------------------------

  renovationShare <- lapply(v_renovationHS, computeBrickShare, variable = "renovation")

  renovationShareTot <- lapply(v_renovationHS, function(ren) {
    ren %>%
      group_by(across(-all_of(c("hsr", "value")))) %>%
      summarise(value = sum(.data$value), .groups = "drop") %>%
      computeBrickShare("other")
  })

  out <- c(
    out,
    stats::setNames(renovationShare, c("renShareMatch", "renShareAgg")),
    stats::setNames(renovationShareTot, c("renShareTotMatch", "renShareTotAgg"))
  )



  # Renovation rate computation ------------------------------------------------

  renovationRate <- lapply(stats::setNames(nm = names(gdxes)), function(nm) {
    commonDims <- setdiff(intersect(colnames(v_renovationHSEff[[nm]]), colnames(v_stock[[nm]])), "value")

    previousStock <- v_stock[[nm]] %>%
      mutate(ttot = lead(.data$ttot))

    v_renovationHSEff[[nm]] %>%
      group_by(across(-all_of(c("hsr", "value")))) %>%
      summarise(value = sum(.data$value), .groups = "drop") %>%
      left_join(previousStock, by = commonDims, suffix = c("Ren", "StockPrev")) %>%
      mutate(value = .data$valueRen / .data$valueStockPrev, .keep = "unused")
  })

  out <- c(
    out,
    stats::setNames(renovationRate, paste0("renRate", c("Match", "Agg")))
  )



  # Write Gams variables to output ---------------------------------------------

  outGamsVars <- list(
    stock = v_stock,
    construction = v_construction,
    demolition = v_demolition,
    renovationHS = v_renovationHS
  )

  out <- c(
    out,
    do.call(c, lapply(names(outGamsVars), function(nm) {
      stats::setNames(outGamsVars[[nm]], paste0(namingMap[[nm]], c("Match", "Agg")))
    }))
  )



  # EXPAND DIMENSIONS AND COMBINE IN ONE DATA FRAME ----------------------------

  # Determine all dimensions present in output data
  allSets <- unique(unlist(lapply(out, colnames)))

  out <- do.call(rbind, lapply(names(out), function(varName) {
    .expandDims(out[[varName]], varName, allSets)
  }))



  # WRITE OUTPUT FILE ----------------------------------------------------------

  outName <- "BRICK_matching_aggregation_report.csv"
  write.csv(out, file.path(pathAgg, outName), row.names = FALSE)

}
