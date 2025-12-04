#' report results from matching aggregation
#'
#' @param pathAgg character, path to the matching aggregation.
#'   Can be a relative path with respect to the matching folder if pathMatch is given
#' @param pathMatch character, path to the matching folder
#'   If \code{NULL}: Use the respective parent directory of the aggregation folder
#'
reportMatchingAggregation <- function(pathAgg, pathMatch = NULL) {



  # Assemble the input paths ---------------------------------------------------

  if (is.null(pathMatch)) {
    if (!file.exists(pathAgg)) {
      stop("If 'pathMatch' is not specified, 'pathAgg' has to provide a full path.")
    }
    pathMatch <- dirname(dirname(pathAgg))
  } else {
    if (!file.exists(pathAgg)) {
      pathAgg <- file.path(pathMatch, "aggregationForCalibration", pathAgg)
      if (!file.exists(pathAgg)) {
        stop("'pathAgg' is invalid: ",
             "It is neither an existing path nor a subdirectory of '<pathmatch>/aggregationForCalibration'")
      }
    }
  }



  # Functions ------------------------------------------------------------------

  .aggregateRegions <- function(df, regionMap) {
    df %>%
      left_join(regionMap, by = "region") %>%
      group_by(across(-all_of(c("region", "value")))) %>%
      summarise(value = sum(.data$value), .groups = "drop") %>%
      rename(region = "regionAgg") %>%
      select(all_of(names(df))) # reorder columns
  }

  .readMatchAndAgg <- function(gdxes, symbol, regionMap) {
    lapply(stats::setNames(nm = names(gdxes)), function(nm) {
      v <- readGdxSymbol(gdxes[[nm]], symbol = symbol, asMagpie = FALSE)
      if (identical(nm, "matching")) {
        v <- .aggregateRegions(v, regionMap)
      }
      v
    })
  }



  # Prepare --------------------------------------------------------------------

  gdx <- file.path(pathMatch, "output.gdx")
  gdxInput <- file.path(pathMatch, "input.gdx")
  gdxAgg <- file.path(pathAgg, "output.gdx")

  gdxes <- c(gdx, gdxAgg)
  names(gdxes) <- c("matching", "aggregation")

  # Read config
  cfg <- read_yaml(file = file.path(pathMatch, "config", "config_COMPILED.yaml"))
  cfgAgg <- read_yaml(file = file.path(pathAgg, "config", "config_COMPILED.yaml"))

  regionMap <- toolGetMapping(cfgAgg$regionmapping[1], "regional", cfgAgg$regionmapping[2]) %>%
    select(region = "CountryCode", regionAgg = "RegionCode")

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

  v_stock <- .readMatchAndAgg(gdxes, "v_stock", regionMap)

  v_construction <- .readMatchAndAgg(gdxes, "v_construction", regionMap)

  v_demolition <- .readMatchAndAgg(gdxes, "v_demolition", regionMap)

  v_renovationHS <- .readMatchAndAgg(gdxes, "v_renovationHS", regionMap)

  v_renovationHSEff <- lapply(v_renovationHS, function(ren) {
    filter(ren, .data$hsr != 0)
  })

  out <- c(
    stats::setNames(v_renovationHSEff, c("renHSEffMatch", "renHSEffAgg"))
  )



  # Share computation ----------------------------------------------------------

  renovationShare <- lapply(v_renovationHSEff, computeBrickShare, variable = "renovation")

  renovationShareTot <- lapply(v_renovationHSEff, function(ren) {
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
