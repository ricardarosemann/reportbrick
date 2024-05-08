#' Report building Stock
#'
#' Report quantities describing the stock of buildings
#'
#' @param gdx gams transfer container of the BRICK GDX
#' @param brickSets character, BRICK reporting template
#'
#' @author Robin Hasse
#'
#' @importFrom magclass mbind setNames dimSums mselect collapseDim

reportBuildingStock <- function(gdx, brickSets = NULL) {

  # READ -----------------------------------------------------------------------

  # stock variable
  v_stock <- readGdxSymbol(gdx, "v_stock")

  # unit conversion: million m2 -> billion m2
  v_stock <- (v_stock / 1000) %>%
    mselect(qty = "area") %>%
    collapseDim(dim = "qty")




  # REPORT ---------------------------------------------------------------------

  out <- mbind(

    ## Total ====
    reportAgg(v_stock,
              "Stock|Buildings (bn m2)", brickSets,
              agg = c(bs = "all", hs = "all", vin = "all", loc = "all", typ = "resCom", inc = "all")),
    reportAgg(v_stock,
              "Stock|Residential (bn m2)", brickSets,
              agg = c(bs = "all", hs = "all", vin = "all", loc = "all", typ = "res", inc = "all")),
    reportAgg(v_stock,
              "Stock|Commercial (bn m2)", brickSets,
              agg = c(bs = "all", hs = "all", vin = "all", loc = "all", typ = "com", inc = "all")),


    ## by building type ====
    reportAgg(v_stock,
              "Stock|Residential|{typ} (bn m2)", brickSets,
              agg = c(bs = "all", hs = "all", vin = "all", loc = "all", inc = "all"),
              rprt = c(typ = "res")),


    ## by location ====
    reportAgg(v_stock,
              "Stock|Residential|{loc} (bn m2)", brickSets,
              agg = c(bs = "all", hs = "all", vin = "all", typ = "res", inc = "all"),
              rprt = c(loc = "all")),


    ## by vintage ====
    reportAgg(v_stock,
              "Stock|Residential|{vin} (bn m2)", brickSets,
              agg = c(bs = "all", hs = "all", loc = "all", typ = "res", inc = "all"),
              rprt = c(vin = "all")),


    ## by heating system ====
    reportAgg(v_stock,
              "Stock|Residential|{hs} (bn m2)", brickSets,
              agg = c(bs = "all", vin = "all", loc = "all", typ = "res", inc = "all"),
              rprt = c(hs = "all")),


    ## by building type + heating system ====
    reportAgg(v_stock,
              "Stock|Residential|{typ}|{hs} (bn m2)", brickSets,
              agg = c(bs = "all", vin = "all", loc = "all", inc = "all"),
              rprt = c(hs = "all", typ = "res"))

  )

  return(out)
}
