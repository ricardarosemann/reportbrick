#' Report renovation
#'
#' Report quantities describing the renovation of new buildings
#'
#' @param gdx gams transfer container of the BRICK GDX
#' @param brickSets character, BRICK reporting template
#' @param silent boolean, suppress warnings and printing of dimension mapping
#'
#' @author Robin Hasse
#'
#' @importFrom magclass mbind setNames dimSums mselect collapseDim

reportRenovation <- function(gdx, brickSets = NULL, silent = TRUE) {

  # READ -----------------------------------------------------------------------

  # renovation variable
  v_renovation <- readGdxSymbol(gdx, "v_renovation")

  # unit conversion: million m2 / yr-> billion m2 / yr
  v_renovation <- (v_renovation / 1000) %>%
    mselect(qty = "area") %>%
    collapseDim(dim = "qty")




  # REPORT ---------------------------------------------------------------------

  out <- mbind(

    ## Total ====
    reportAgg(v_renovation,
              "Renovation|Buildings (bn m2/yr)", brickSets,
              agg = c(bs = "all", hs = "all", bsr = "all0", hsr = "all", vin = "all", loc = "all", typ = "resCom", inc = "all"),
              silent = silent),
    reportAgg(v_renovation,
              "Renovation|Residential (bn m2/yr)", brickSets,
              agg = c(bs = "all", hs = "all", bsr = "all0", hsr = "all", vin = "all", loc = "all", typ = "res", inc = "all"),
              silent = silent),
    reportAgg(v_renovation,
              "Renovation|Commercial (bn m2/yr)", brickSets,
              agg = c(bs = "all", hs = "all", bsr = "all0", hsr = "all", vin = "all", loc = "all", typ = "com", inc = "all"),
              silent = silent),


    ## by building type ====
    reportAgg(v_renovation,
              "Renovation|Residential|{typ} (bn m2/yr)", brickSets,
              agg = c(bs = "all", hs = "all", bsr = "all0", hsr = "all", vin = "all", loc = "all", inc = "all"),
              rprt = c(typ = "res"),
              silent = silent),


    ## by location ====
    reportAgg(v_renovation,
              "Renovation|Residential|{loc} (bn m2/yr)", brickSets,
              agg = c(bs = "all", hs = "all", bsr = "all0", hsr = "all", vin = "all", typ = "res", inc = "all"),
              rprt = c(loc = "all"),
              silent = silent),


    ## by initial heating system ====
    reportAgg(v_renovation,
              "Renovation|Residential|{hs} (bn m2/yr)", brickSets,
              agg = c(bs = "all", bsr = "all0", hsr = "all", vin = "all", loc = "all", typ = "res", inc = "all"),
              rprt = c(hs = "all"),
              silent = silent),


    ## by building type + initial heating system ====
    reportAgg(v_renovation,
              "Renovation|Residential|{typ}|{hs} (bn m2/yr)", brickSets,
              agg = c(bs = "all", bsr = "all0", hsr = "all", vin = "all", loc = "all", inc = "all"),
              rprt = c(hs = "all", typ = "res"),
              silent = silent),


    ## by final heating system ====
    reportAgg(v_renovation,
              "Renovation|Residential|all|{hsr} (bn m2/yr)", brickSets,
              agg = c(bs = "all", hs = "all", bsr = "all0", vin = "all", loc = "all", typ = "res", inc = "all"),
              rprt = c(hsr = "all"),
              silent = silent),


    ## by final heating system, without zero state ====
    reportAgg(v_renovation,
              "Renovation|Residential|all|With zero|{hsr} (bn m2/yr)", brickSets,
              agg = c(bs = "all", hs = "all", bsr = "all0", vin = "all", loc = "all", typ = "res", inc = "all"),
              rprt = c(hsr = "all0"),
              silent = silent),


    ## by initial and final heating system ====
    reportAgg(v_renovation,
              "Renovation|Residential|{hs}|{hsr} (bn m2/yr)", brickSets,
              agg = c(bs = "all", bsr = "all0", vin = "all", loc = "all", typ = "res", inc = "all"),
              rprt = c(hs = "all", hsr = "all0"),
              silent = silent)

  )

  return(out)
}
