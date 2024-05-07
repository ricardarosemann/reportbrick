# Load additional libraries ----------------------------------------------------

library(dplyr) # nolint: undesirable_function_linter.



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
  "Biomass",
  "District heating",
  "Heat pump",
  "Resistive electric",
  "Gas",
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
