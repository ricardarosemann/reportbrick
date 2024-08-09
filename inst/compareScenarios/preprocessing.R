# Load additional libraries ----------------------------------------------------

# nolint start: undesirable_function_linter.
library(dplyr, include.only = "%>%")
library(mip, include.only = c("showAreaAndBarPlots", "showLinePlots"))
library(purrr, include.only = c("walk"))
# nolint end


# BRICK sets -------------------------------------------------------------------

# Sets are used in Rmd files to select variables

subsec <- c(
  "Residential",
  "Commercial"
)
type <- c(
  "SFH",
  "MFH"
)
location <- c(
  "Urban",
  "Rural"
)
heating <- c(
  "Biomass heater",
  "District heating",
  "Heat pump",
  "Resistive electric",
  "Hydrogen heater",
  "Gas heater",
  "Liquids heater",
  "Coal heater"
)
carrier <- c(
  "Biomass",
  "Heat",
  "Electricity",
  "Hydrogen",
  "Gases",
  "Liquids",
  "Coal"
)

# automatic identification of vintages to allow for different model resolutions
vintageRegex <- "(Before|After) \\d{4}|\\d{4} - \\d{4}"
vintage <- grep(vintageRegex, unique(data[["variable"]]), value = TRUE) %>%
  unique() %>%
  sub(pattern = paste0("^.*(", vintageRegex, ").*$"), replacement = "\\1") %>%
  unique()
vintage <- c(grep("^Before", vintage, value = TRUE),
             sort(grep("^\\d{4}", vintage, value = TRUE)),
             grep("^After", vintage, value = TRUE))
