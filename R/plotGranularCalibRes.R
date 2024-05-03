#' Plot granular calibration results
#'
#' Plot calibration results in highly granular manner, i.e. without aggregation.
#'
#' @author Ricarda Rosemann
#'
#' @importFrom dplyr %>% filter full_join group_by left_join mutate rename
#'     select
#' @export

#TODO: Remove for package version
library(tidyverse)
library(ggplot2)

plotGranularCalibRes <- function(data, path, outName, dataHist = NULL, varName = "value",
                                 filterVars = NULL,
                                 facets = c("loc", "typ"), color = "period") {
  # Check what happens if I pass NULL here or any other default that might signal zero filtering
  filterList <- lapply(filterVars, function(var) {
    unique(data[[var]])
  })
  names(filterList) <- filterVars

  # for (varName in names(filterVars)) {
  #   filterList[[varName]] <- unique(data[[varName]])
  # }

  filterAll <- expand.grid(filterList)

  if (!is.null(dataHist)) {
    dataAll <- full_join(data %>%
                           mutate(class = "calib"),
                         dataHist %>%
                           mutate(class = "hist"))
  } else {
    dataAll <- data %>%
      mutate(class = "calib") # This will probably lead to an unnecessary legend entry.
  }

  N <- if (nrow(filterAll) == 0) 1 else nrow(filterAll)

  for (i in seq(nrow(filterAll))) {
    nameAdd <- ""
    dataPlot <- dataAll
    for (var in filterVars) {
      dataPlot <- dataPlot %>%
        filter(.data[[var]] == filterAll[[var]][i])
      nameAdd <- paste(nameAdd, var, filterAll[[var]][i], sep = "_")
    }
    if (is.null(color)) {
      plotOut <- dataPlot %>%
        ggplot(mapping = aes(x = .data[["iteration"]], y = .data[[varName]]))
    } else {
      dataPlot[[color]] = as.factor(dataPlot[[color]])
      plotOut <- dataPlot %>%
        ggplot(mapping = aes(x = .data[["iteration"]], y = .data[[varName]],
                             color = .data[[color]]))
    }
    plotOut <- plotOut +
      geom_line() +
      geom_point() +
      facet_grid(rows = vars(.data[[facets[1]]]), cols = vars(.data[[facets[2]]]), scales = "free")
    ggsave(file.path(path, paste0(outName, nameAdd, ".png")))
  }

}
