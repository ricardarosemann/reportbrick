#' Read in model results from calibration for each iteration, calculate deviation
#'
#'
#' @param gdx path to a gdx; it is assumed that for each iteration a gdx is present
#'  with this path and the iteration number inserted at the end.
#' @param regionSubsetList a list containing regions to create report variables region
#' aggregations. If NULL (default value) only the global region aggregation "GLO" will
#' be created.
#' @param t temporal resolution of the reporting, default:
#' t=c(seq(2005,2060,5),seq(2070,2110,10),2130,2150)
#'
#' @author Ricarda Rosemann
#'
#' @importFrom dplyr %>% filter full_join group_by left_join mutate rename
#'     select semi_join summarise ungroup
#' @importFrom readr write_csv
#' @export

# TODO: Temporary, to be removed in package version
library(dplyr)
library(readr)
library(stringr)

reportCalibration <- function(gdx) {

  # READ -----------------------------------------------------------------------

  # Determine last iteration
  allFiles <- list.files(path = dirname(gdx),
                         pattern = paste0(gsub("\\.gdx$", "", basename(gdx)),
                                          "_\\d{1,3}.gdx"))
  maxIter <- max(as.numeric(gsub("\\.gdx$", "", str_extract(allFiles, "\\d{1,3}.gdx$"))))

  # Read gdx files from all iterations
  # TODO: Could potentially replace this call by mip::getPlotData (but that uses gdxrrw and can only start counting at 1)
  p_f <- .readGdxIter(gdx, "p_f", maxIter)

  p_alpha <- .readGdxIter(gdx, "p_alpha", maxIter)

  p_stock <- .readGdxIter(gdx, "p_stock", maxIter)

  p_construction <- .readGdxIter(gdx, "p_construction", maxIter)

  p_renovation <- .readGdxIter(gdx, "p_renovation", maxIter)

  # Read historical gdx files
  p_stockHist <- readGdxSymbol(gdx, "p_stockHist")

  p_constructionHist <- readGdxSymbol(gdx, "p_constructionHist")

  p_renovationHist <- readGdxSymbol(gdx, "p_renovationHist")

  # COMPUTE DEVIATIONS ---------------------------------------------------------

  #TODO: fix restore zeros to avoid NAs
  p_stockDev <- .computeDeviation(p_stock, p_stockHist)

  p_constructionDev <- .computeDeviation(p_construction, p_constructionHist)

  p_renovationDev <- .computeDeviation(p_renovation, p_renovationHist)

  # AGGREGATE QUANTITIES -------------------------------------------------------

  p_stockDev <- reportAgg(p_stockDev,
                          "Stock Iterations | Buildings (mio m2) {iteration}", tmpl,
                          agg = c(bs = "all", hs = "all", vin = "all", loc = "all", typ = "resCom", inc = "all"),
                          rprt = c(iteration = "all"))

  # # Load historical values
  # namesHist <- c("p_stockHist", "p_constructionHist", "p_renovationHist")
  # hist <- lapply(namesHist, function(v) {
  #   readGdxSymbol(gdx, v, asMagpie = FALSE)
  # })
  # names(hist) <- namesHist
  # if (is.null(hist[["p_constructionHist"]]) || is.null(hist[["p_renovationHist"]])) {
  #   gdxHist <- "C:\\Users\\ricardar\\Documents\\Results\\BRICK\\calibration-tests\\stocks-extrElec-vin\\inputHist2010.gdx"
  #   hist[["p_constructionHist"]] <- readGdxSymbol(gdxHist, "v_construction", asMagpie = FALSE)
  #   hist[["p_renovationHist"]] <- readGdxSymbol(gdxHist, "v_renovation", asMagpie = FALSE)
  # }
  # nameVarMap <- c(p_stock = "p_stockHist", p_construction = "p_constructionHist",
  #                 p_renovation = "p_renovationHist")
  # # nameVarMap <- c(p_stock = "p_stockHist")
  #
  # # Loop through iterations, store relevant parameter results. Include p_specCostCon? But this is p_x?
  # # namesParam <- c("p_f", "p_stock")
  # namesParam <- c("p_alpha", "p_d", "p_f", "p_x", "p_stock", "p_construction", "p_renovation")
  # resParam <- lapply(namesParam, function(v) {
  #   res <- data.frame() # see above
  #   browser()
  #   for (i in seq(0, maxIter)) {
  #     fileName <- file.path(dirname(gdx), paste0(gsub("\\.gdx$", "", basename(gdx)), "_", i, ".gdx"))
  #     if (file.exists(fileName)) {
  #       if (v %in% c("p_stockIter", "p_constructionIter", "p_renovationIter")) {
  #         res <- bind_rows(res,
  #                          readGdxSymbol(fileName, v, asMagpie = FALSE) %>%
  #                            filter(iterationAll == i) %>%
  #                            rename(iteration = "iterationAll"))
  #                            # mutate(iteration = i))
  #       } else {
  #         res <- bind_rows(res,
  #                          readGdxSymbol(fileName, v, asMagpie = FALSE) %>%
  #                          mutate(iteration = i))
  #       }
  #     } else {
  #       warning(paste("Data for iteration", i, "is missing. Skipping this iteration"))
  #     }
  #   }
  #   return(res)
  # })
  # names(resParam) <- namesParam
  #
  # # Compute deviation from historical values
  # for (var in names(nameVarMap)) {
  #   resParam[[var]] <- resParam[[var]] %>%
  #     left_join(hist[[nameVarMap[[var]]]] %>%
  #                 rename(hist = "value"),
  #               by = colnames(.data)[colnames(.data) != "value"]) %>% # Rather unsure about this ...
  #     mutate(dev = .data[["value"]] - .data[["hist"]])
  # }
  #
  # # Create flow variable? -> only makes sense for aggregated variable! -> skip for the moment, potentially move to plotting
  #
  # # Compute relative values -> skip for the moment, potentially move to plotting
  #
  # # Save files
  # for (var in names(resParam)) {
  #   write_csv(resParam[[var]], file.path(dirname(gdx), paste0(var, "Iter.csv")))
  # }
}

.readGdxIter <- function(gdx, symbol, maxIter) {
  res <- data.frame() # see above
  for (i in seq(0, maxIter)) {
    fileName <- file.path(dirname(gdx), paste0(gsub("\\.gdx$", "", basename(gdx)), "_", i, ".gdx"))
    if (file.exists(fileName)) {
      tmp <- readGdxSymbol(fileName, symbol)
      tmp <- add_dimension(tmp, dim = 3.1, add = "iteration", nm = i)
      res <- mbind(res, tmp)
    } else {
      warning(paste("Data for iteration", i, "is missing. Skipping this iteration"))
    }
  }
  return(res)
}

#TODO: Figure out how to do this in magpie environement - or transform to quitte or similar
.computeDeviation <- function(mp, mpHist) {
  dfHist <- as_tibble(mpHist)

  df <- as_tibble(mp) %>%
    left_join(dfHist %>%
                rename(hist = "value"),
              by = colnames(.data)[colnames(.data) != "value"])# %>% # Rather unsure about this ...
  browser()
  df <- df %>%
    mutate(value = .data[["value"]] - .data[["hist"]]) %>%
    select(-"hist")

  mpOut <- as.magpie(df)
}

.computeSumSq <- function(df, varName = "value", agg = "") {
  sumSq <- df %>%
    group_by(across(any_of(agg))) %>%
    summarise(value = sum(.data[[varName]] ^ 2, na.rm = TRUE), .groups = "keep") %>%
    ungroup()
  return(sumSq)
}
