#' Plot aggregate calibration results
#'
#' Plot calibration results in aggregated manner: Compute sum of squares.
#'
#' @author Ricarda Rosemann
#'
#' @importFrom dplyr %>% filter full_join group_by left_join mutate rename
#'     select
#' @export

#TODO: Remove for package version
library(tidyverse)
library(ggplot2)

plotAggregateCalibRes <- function(data, path, outname, groupingVars = "", varName = "value",
                                  dataRen = NULL, groupingVarsRen = "",
                                  filterVars = NULL,
                                  facets = c("loc", "typ"), color = "period") {
  # Parameter groupingVarsRen deactivated - don't see why I should need this
  # Activated again - if I want to group by hs resp. hsr

  computeSumSq <- function(df, varName = "value", groupingVars = "") {
    sumSq <- df %>%
      group_by(across(any_of(groupingVars))) %>%
      summarise(value = sum(.data[[varName]] ^ 2, na.rm = TRUE), .groups = "keep") %>%
      ungroup()
    return(sumSq)
  }

  dataSum <- computeSumSq(data, varName = varName, groupingVars = groupingVars)

  if ("bs" %in% colnames(dataSum)) {
    dataSum <- dataSum %>%
      rename(bsr = "bs")
  }
  if ("hs" %in% colnames(dataSum)) {
    dataSum <- dataSum %>%
      rename(hsr = "hs")
  }

  # adjust the column name of value for the function call
  plotGranularCalibRes(dataSum, path, outname, filterVars = filterVars,
                       facets = facets, color = color)

  if (!is.null(dataRen)) {
    dataSumRen <- computeSumSq(dataRen, varName = varName, groupingVars = groupingVarsRen)

    if ("bs" %in% colnames(dataSum)) {
      dataSum <- dataSum %>%
        rename(bsr = "bs")
    }
    if ("hs" %in% colnames(dataSum)) {
      dataSum <- dataSum %>%
        rename(hsr = "hs")
    }

    dataSumFlows <- full_join(dataSum %>%
                                rename(con = "value") ,
                              dataSumRen %>%
                                rename(ren = "value"),
                              by = groupingVarsRen) %>%
      replace_na(list(con = 0)) %>%
      mutate(value = .data[["con"]] + .data[["ren"]])
    plotGranularCalibRes(dataSumRen, path, gsub("Con", "Ren", outname),
                         filterVars = filterVars, facets = facets, color = color)
    plotGranularCalibRes(dataSumFlows, path, gsub("Con", "Flow", outname),
                         filterVars = filterVars, facets = facets, color = color)
  }

}
