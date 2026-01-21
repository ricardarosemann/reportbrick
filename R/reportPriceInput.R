#' Read in input price data and heat pump share
#'
#' @param path character, path to the desired input gdx
#' @param gdxName character, file name of the desired input gdx
#'
#' @importFrom dplyr %>% .data filter mutate pick rename right_join rowwise select ungroup
#' @importFrom tidyr pivot_wider
#'
reportPriceInput <- function(path, gdxName = "input.gdx", gdxNameReferences = "references.gdx") {

  inputFile <- file.path(path, gdxName)
  refFile <- file.path(path, gdxNameReferences)



  # Read in price data from input gdx ------------------------------------------

  carrier <- readGdxSymbol(inputFile, "carrier", asMagpie = FALSE)[[1]]

  p_carrierPrice <- readGdxSymbol(inputFile, "p_carrierPrice", asMagpie = FALSE)
  carrierPriceWide <- pivot_wider(p_carrierPrice, names_from = "carrier")

  p_carbonPrice <- readGdxSymbol(inputFile, "p_carbonPrice", asMagpie = FALSE)



  # Compute price ratios of electricity to other carriers ----------------------

  #TODO: Move this to external map or integrate with general region map
  ratioRegionMap <- expand.grid(
    region = as.character(unique(p_carrierPrice$region)),
    carrierDenom = c("gases", "liquids", "heat", "biomod")
  )

  priceRatios <- p_carrierPrice %>%
    filter(.data$carrier == "elec") %>%
    rename(price = "value") %>%
    right_join(ratioRegionMap, by = "region") %>%
    left_join(p_carrierPrice %>%
                 rename(price = "value"), by = c("region", "ttot", carrierDenom = "carrier"), suffix = c("", "Denom")) %>%
    mutate(value = .data$price / .data$priceDenom) %>%
    select("region", "ttot", carrier = "carrierDenom", "value")



  # Read heat pump sales share--------------------------------------------------

  heatpumpSalesShare <- readGdxSymbol(refFile, "p_refVals", asMagpie = FALSE) %>%
    filter(.data$reference %in% c("VHK", "HeatingSystemSales"), .data$refVar == "ehp1", !is.na(.data$value)) %>%
    select("region", "ttot", "value")



  # EXPAND DIMENSIONS AND COMBINE IN ONE DATA FRAME ----------------------------

  out <- list(
    carrierPrice = p_carrierPrice,
    ratioElec = priceRatios,
    hpsalesShare = heatpumpSalesShare
  )

  # Determine all dimensions present in output data
  allSets <- unique(unlist(lapply(out, colnames)))

  out <- do.call(rbind, lapply(names(out), function(varName) {
    .expandDims(out[[varName]], varName, allSets)
  }))



  # WRITE OUTPUT FILE ----------------------------------------------------------

  outName <- "BRICK_input_price_report.csv"
  write.csv(out, file.path(path, outName), row.names = FALSE)

}
