#' Report demolition
#'
#' Report quantities describing the demolition of buildings
#'
#' @param gdx gams transfer container of the BRICK GDX
#' @param brickSets character, BRICK reporting template
#'
#' @author Robin Hasse
#'
#' @importFrom magclass mbind setNames dimSums mselect collapseDim

reportDemolition <- function(gdx, brickSets = NULL) {

  # READ -----------------------------------------------------------------------

  # demolition variable
  v_demolition <- readGdxSymbol(gdx, "v_demolition")

  # unit conversion: million m2/yr -> billion m2/yr
  v_demolition <- (v_demolition / 1000) %>%
    mselect(qty = "area") %>%
    collapseDim(dim = "qty")




  # REPORT ---------------------------------------------------------------------

  out <- mbind(

    ## Total ====
    reportAgg(v_demolition,
              "Demolition|Buildings (bn m2/yr)", brickSets,
              agg = c(bs = "all", hs = "all", vin = "all", loc = "all", typ = "resCom", inc = "all")),
    reportAgg(v_demolition,
              "Demolition|Residential (bn m2/yr)", brickSets,
              agg = c(bs = "all", hs = "all", vin = "all", loc = "all", typ = "res", inc = "all")),
    reportAgg(v_demolition,
              "Demolition|Commercial (bn m2/yr)", brickSets,
              agg = c(bs = "all", hs = "all", vin = "all", loc = "all", typ = "com", inc = "all")),


    ## by building type ====
    reportAgg(v_demolition,
              "Demolition|Residential|{typ} (bn m2/yr)", brickSets,
              agg = c(bs = "all", hs = "all", vin = "all", loc = "all", inc = "all"),
              rprt = c(typ = "res")),


    ## by location ====
    reportAgg(v_demolition,
              "Demolition|Residential|{loc} (bn m2/yr)", brickSets,
              agg = c(bs = "all", hs = "all", vin = "all", typ = "res", inc = "all"),
              rprt = c(loc = "all")),


    ## by vintage ====
    reportAgg(v_demolition,
              "Demolition|Residential|{vin} (bn m2/yr)", brickSets,
              agg = c(bs = "all", hs = "all", loc = "all", typ = "res", inc = "all"),
              rprt = c(vin = "all")),


    ## by heating system ====
    reportAgg(v_demolition,
              "Demolition|Residential|{hs} (bn m2/yr)", brickSets,
              agg = c(bs = "all", vin = "all", loc = "all", typ = "res", inc = "all"),
              rprt = c(hs = "all")),


    ## by building type + heating system ====
    reportAgg(v_demolition,
              "Demolition|Residential|{typ}|{hs} (bn m2/yr)", brickSets,
              agg = c(bs = "all", vin = "all", loc = "all", inc = "all"),
              rprt = c(hs = "all", typ = "res"))

  )

  return(out)
}
