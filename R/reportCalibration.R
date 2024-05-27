#' Read in model results from calibration for each iteration, calculate deviation
#'
#'
#' @param gdx path to a gdx; it is assumed that for each iteration a gdx is present
#'  with this path and the iteration number inserted at the end.
#'
#' @author Ricarda Rosemann
#'
#' @importFrom readr write_csv
#' @importFrom stringr str_extract
#' @importFrom tidyr crossing replace_na
#' @export

reportCalibration <- function(gdx) {

  # READ -----------------------------------------------------------------------

  # Determine last iteration
  path <- dirname(gdx)

  allFiles <- list.files(path = path,
                         pattern = paste0(gsub("\\.gdx$", "", basename(gdx)),
                                          "_\\d{1,3}.gdx"))
  maxIter <- max(as.numeric(gsub("\\.gdx$", "", str_extract(allFiles, "\\d{1,3}.gdx$"))))

  # Read relevant time periods
  tCalib <- readGdxSymbol(gdx, "tCalib", asMagpie = FALSE)[["ttot"]]

  # Read different parameters from gdx files for all iterations
  # TODO: Could potentially replace this call by mip::getPlotData (but that uses gdxrrw and can only start counting at 1) # nolint
  targetFunction <- .readGdxIter(gdx, "p_f", maxIter, asMagpie = FALSE)

  stepSize <- .readGdxIter(gdx, "p_alpha", maxIter, asMagpie = FALSE)

  # Potentially shift the time filter to a later stage if I want to save and plot pure stock/flow data
  p_stock <- .readGdxIter(gdx, "p_stock", maxIter, asMagpie = FALSE, ttotFilter = tCalib, replaceVar = TRUE)

  p_construction <- .readGdxIter(gdx, "p_construction", maxIter,
                                 asMagpie = FALSE, ttotFilter = tCalib, replaceVar = TRUE)

  p_renovation <- .readGdxIter(gdx, "p_renovation", maxIter,
                               asMagpie = FALSE, ttotFilter = tCalib)

  # Read historical gdx files
  p_stockHist <- .replaceVarName(readGdxSymbol(gdx, "p_stockHist", asMagpie = FALSE))

  p_constructionHist <- .replaceVarName(readGdxSymbol(gdx, "p_constructionHist", asMagpie = FALSE))

  p_renovationHist <- readGdxSymbol(gdx, "p_renovationHist", asMagpie = FALSE)


  # COMPUTE DEVIATIONS ---------------------------------------------------------

  p_stockDev <- .computeDeviation(p_stock, p_stockHist)

  p_constructionDev <- .computeDeviation(p_construction, p_constructionHist)

  p_renovationDev <- .computeDeviation(p_renovation, p_renovationHist)


  # AGGREGATE QUANTITIES -------------------------------------------------------

  # Aggregate across all dimensions
  stockDevAgg <- .computeSumSq(p_stockDev, rprt = c("iteration", "reg", "typ", "loc", "inc"))

  conDevAgg <- .computeSumSq(p_constructionDev, rprt = c("iteration", "reg", "typ", "loc", "inc"))

  renDevAgg <- .computeSumSq(p_renovationDev, rprt = c("iteration", "reg", "typ", "loc", "inc"))

  flowDevAgg <- .computeFlowSum(conDevAgg, renDevAgg)

  # Aggregate by heating system (hs)
  stockDevHs <- .computeSumSq(p_stockDev, rprt = c("iteration", "reg", "typ", "loc", "inc", "hsr"))

  conDevHs <- .computeSumSq(p_constructionDev, rprt = c("iteration", "reg", "typ", "loc", "inc", "hsr"))

  renDevHs <- .computeSumSq(p_renovationDev, rprt = c("iteration", "reg", "typ", "loc", "inc", "hsr"))

  flowDevHs <- .computeFlowSum(conDevHs, renDevHs)


  # COMPUTE RELATIVE AGGREGATE QUANTITIES --------------------------------------

  # Across all dimensions
  stockDevRel <- .computeRelDev(stockDevAgg, p_stockHist, tCalib)

  conDevRel <- .computeRelDev(conDevAgg, p_constructionHist, tCalib)

  renDevRel <- .computeRelDev(renDevAgg, p_renovationHist, tCalib)

  flowDevRel <- .computeRelDev(flowDevAgg, list(p_constructionHist, p_renovationHist), tCalib)

  # Separately for all heating systems (hs)
  stockDevHsRel <- .computeRelDev(stockDevHs, p_stockHist, tCalib, notInHist = "hsr")

  conDevHsRel <- .computeRelDev(conDevHs, p_constructionHist, tCalib, notInHist = "hsr")

  renDevHsRel <- .computeRelDev(renDevHs, p_renovationHist, tCalib, notInHist = "hsr")

  flowDevHsRel <- .computeRelDev(flowDevHs, list(p_constructionHist, p_renovationHist),
                                  tCalib, notInHist = "hsr")

  # EXPAND DIMENSIONS AND COMBINE IN ONE DATA FRAME ----------------------------
  outList <- list(targetFunction = targetFunction, stepSize = stepSize,
                  stockDevAgg = stockDevAgg, conDevAgg = conDevAgg,
                  renDevAgg = renDevAgg, flowDevAgg = flowDevAgg,
                  stockDevHs = stockDevHs, conDevHs = conDevHs,
                  renDevHs = renDevHs, flowDevHs = flowDevHs,
                  stockDevRel = stockDevRel, conDevRel = conDevRel,
                  renDevRel = renDevRel, flowDevRel = flowDevRel,
                  stockDevHsRel = stockDevHsRel, conDevHsRel = conDevHsRel,
                  renDevHsRel = renDevHsRel, flowDevHsRel = flowDevHsRel)

  # Determine all dimensions present in output data
  allSets <- unique(unlist(lapply(outList, colnames)))

  out <- data.frame()
  for (varName in names(outList)) {
    out <- rbind(out, .expandDims(outList[[varName]], varName, allSets))
  }


  # WRITE OUTPUT FILE ----------------------------------------------------------

  write_csv(out, file.path(path, "BRICK_calibration_report.csv"))

}

#' Read a symbol from several gdx files and combine in one data frame
#'
#' @param gdx Path to gdx files; all gdx file names are assumed to be numbered and
#'  adhere to the structure: If \code{gdx = <path/to/file/name.gdx}, the function looks for
#'  \code{<path/to/file/name_0.gdx>}, \code{<path/to/file/name_1.gdx>}, ...
#' @param symbol Symbol to be read from the gdxes
#' @param maxIter Last iteration to be read
#' @param asMagpie logical, convert to magpie object?
#' @param ttotFilter numeric/factor, time periods to filter for
#' @param replaceVar logical, replace column names \code{bs} and \code{hs} by \code{bsr} and \code{hsr}?
#' @returns data frame of all results read in
#'
#' @importFrom dplyr %>% .data filter mutate

.readGdxIter <- function(gdx, symbol, maxIter, asMagpie = TRUE, ttotFilter = NULL, replaceVar = FALSE) {

  # Loop over all iterations and read in gdx files
  res <- data.frame()
  for (i in seq(0, maxIter)) {
    fileName <- file.path(dirname(gdx), paste0(gsub("\\.gdx$", "", basename(gdx)), "_", i, ".gdx"))
    if (file.exists(fileName)) {
      res <- rbind(res, readGdxSymbol(fileName, symbol, asMagpie = asMagpie) %>%
                     mutate(iteration = i))
    } else {
      warning(paste("Data for iteration", i, "is missing. Skipping this iteration"))
    }
  }

  # Might move this elsewhere, e.g. to a separate function
  # Filter for given time periods
  if (!is.null(ttotFilter)) {
    res <- res %>%
      filter(.data[["ttot"]] %in% ttotFilter)
  }

  # If specified, replace bs and hs column names
  if (isTRUE(replaceVar)) {
    res <- .replaceVarName(res)
  }

  return(res)
}

#' Replace column names \code{bs} and \code{hs} by \code{bsr} and \code{hsr}
#'
#' @param df data frame
#' @returns data frame where column names have been replaced
#'
#'@importFrom dplyr %>% all_of rename_with

.replaceVarName <- function(df) {
  for (var in c("hs", "bs")) {
    if (var %in% colnames(df)) {
      df <- df %>%
        rename_with(~ paste0(.x, "r"), all_of(var))
    }
  }
  return(df)
}

#' Compute the deviation to historical data
#'
#' @param df data frame, containing data from calibration run
#' @param dfHist data frame, containing historical data
#' @returns data frame where the value column contains the deviation from historical data
#'
#' @importFrom dplyr %>% .data filter full_join mutate rename select
#' @importFrom tidyr crossing replace_na

.computeDeviation <- function(df, dfHist) {

  # Add iteration column to historical data and filter for time periods of calibration data in df
  dfHist <- dfHist %>%
    crossing(iteration = df[["iteration"]]) %>%
    filter(.data[["ttot"]] %in% unique(df[["ttot"]])) %>%
    rename(hist = "value")

  # Replace all data points not present in either data frames by zero and compute the difference
  df <- df %>%
    full_join(dfHist,
              by = intersect(colnames(df), colnames(dfHist))) %>% # Rather unsure about this ...
    replace_na(list(value = 0, hist = 0)) %>%
    mutate(value = .data[["value"]] - .data[["hist"]]) %>%
    select(-"hist")

}

#' Compute the sum of the squares in a data frame
#'
#' @param df data frame, containing the data to be evaluated
#' @param rprt character, column names for which the sum of the squares should be reported separately
#' @returns data frame sum of squares as value column
#'
#' @importFrom dplyr %>% across all_of any_of .data group_by rename_with summarise ungroup

.computeSumSq <- function(df, rprt = "") {

  df %>%
    group_by(across(any_of(rprt))) %>%
    summarise(value = sum(.data[["value"]] ^ 2, na.rm = TRUE), .groups = "keep") %>%
    ungroup()

}

#' Compute the relative deviation to historical data as the relative euclidean distance
#'
#' @param dfDev data frame, containing deviation between calibration and historical data
#' @param dfHist data frame or a list of one or two data frames, containing historical data.
#'  The option to pass two data frames is used to combine construction and renovation flows.
#'  If two data frames are passed, the first one is assumed to be construction, the second renovation
#' @param tCalib numerical/factor, calibration time periods to filter historical data
#' @returns data frame with relative deviation in value column
#'
#' @importFrom dplyr %>% .data filter left_join mutate rename select

.computeRelDev <- function(dfDev, dfHist, tCalib, notInHist = NULL) {

  # Determine the reported columns in the calibration data
  rprt <- setdiff(colnames(dfDev), c("iteration", "value", notInHist))

  # Convert historical data to list if necessary
  if (is.data.frame(dfHist)) dfHist <- list(dfHist)

  # Compute the sum of the squares for historical data
  dfHistSumList <- lapply(dfHist, function(df) {
    .computeSumSq(df %>%
                    filter(.data[["ttot"]] %in% tCalib),
                  rprt = rprt)
  })

  if (length(dfHistSumList) == 1) {
    dfHistSum <- dfHistSumList[[1]] %>%
      rename(hist = "value")
  } else if (length(dfHistSumList) >= 2) {

    # If two data frames with historical data have been passed:
    # Combine them by treating the first as construction flows and the second as renovation flows
    dfHistSum <- .computeFlowSum(dfHistSumList[[1]], dfHistSumList[[2]]) %>%
      rename(hist = "value")
  }

  if (length(dfHistSumList) > 2) {
    warning(paste("Only two data frames of historical data can be combined.",
                  "The remaining data frames are ignored."))
  }

  # Compute the relative deviation as the square root of the deviation divided
  # by the square root of the historical data
  dfDev <- dfDev %>%
    left_join(dfHistSum, by = rprt) %>%
    mutate(value = sqrt(.data[["value"]]) / sqrt(.data[["hist"]])) %>%
    select(-"hist")
}

#' Compute the sum of construction and renovation flow values
#'
#' @param con data frame, contains construction flow quantities
#' @param ren data frame, contains renovation flow quantities
#' @returns data frame with the sum in the value column
#'
#' @importFrom dplyr %>% across all_of .data full_join mutate rename select

.computeFlowSum <- function(con, ren) {

  # If columns bsr and hsr are present in the data:
  # Convert the construction data to the factor levels of the renovation data
  for (var in c("hsr", "bsr")) {

    if (var %in% colnames(ren)) {
      if (var %in% colnames(con)) {
        con <- con %>%
          mutate(across(all_of(var), ~ factor(.data[[var]], levels(ren[[var]]))))
      } else {
        stop("Construction and renovation flow data do not match.")
      }
    }
  }

  # Compute the combined flow data as the sum of construction and renovation;
  # NA values in the construction flows are replaced by zeros, thus for "0" flows only renovation is reflected.
  full_join(con %>%
              rename(con = "value"),
            ren %>%
              rename(ren = "value"),
            by = setdiff(intersect(colnames(con), colnames(ren)), "value")) %>%
    replace_na(list(con = 0)) %>%
    mutate(value = con + ren) %>%
    select(-"con", -"ren")
}

#' Extend dimensions of a data frame by adding NA entries, add variable name
#'
#' @param df data frame to be extended
#' @param varName character, variable name to be added
#' @param allSets character, sets that need to be included as column names
#' @returns data frame
#'
#' @importFrom dplyr %>% mutate last_col relocate

.expandDims <- function(df, varName, allSets) {

  # Add missing columns with NA entries
  df[setdiff(allSets, colnames(df))] <- NA

  # Add variable name as first column
  df <- df %>%
    mutate(variable = varName, .before = 1) %>%
    relocate("value", .after = last_col())
}
