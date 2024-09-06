#' Report renovation
#'
#' Report quantities describing the renovation of new buildings
#'
#' @param gdx gams transfer container of the BRICK GDX
#' @param brickSets character, BRICK reporting template
#' @param silent boolean, suppress warnings and printing of dimension mapping
#'
#' @author Ricarda Rosemann
#'
#' @importFrom magclass mbind setNames mselect collapseDim

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
              agg = c(bs = "all", hs = "all", bsr.hsr = "all", vin = "all", loc = "all", typ = "resCom", inc = "all"),
              silent = silent),
    reportAgg(v_renovation,
              "Renovation|Residential (bn m2/yr)", brickSets,
              agg = c(bs = "all", hs = "all", bsr.hsr = "all", vin = "all", loc = "all", typ = "res", inc = "all"),
              silent = silent),
    reportAgg(v_renovation,
              "Renovation|Residential|Heating (bn m2/yr)", brickSets,
              agg = c(bs = "all", hs = "all", bsr = "all0", hsr = "all", vin = "all",
                      loc = "all", typ = "res", inc = "all"),
              silent = silent),
    reportAgg(v_renovation,
              "Renovation|Commercial (bn m2/yr)", brickSets,
              agg = c(bs = "all", hs = "all", bsr.hsr = "all", vin = "all", loc = "all", typ = "com", inc = "all"),
              silent = silent),


    ## by building type ====
    reportAgg(v_renovation,
              "Renovation|Residential|{typ} (bn m2/yr)", brickSets,
              agg = c(bs = "all", hs = "all", bsr.hsr = "all", vin = "all", loc = "all", inc = "all"),
              rprt = c(typ = "res"),
              silent = silent),


    ## by location ====
    reportAgg(v_renovation,
              "Renovation|Residential|{loc} (bn m2/yr)", brickSets,
              agg = c(bs = "all", hs = "all", bsr.hsr = "all", vin = "all", typ = "res", inc = "all"),
              rprt = c(loc = "all"),
              silent = silent),

    ## by final building shell ====
    reportAgg(v_renovation,
              "Renovation|Residential|{bsr} (bn m2/yr)", brickSets,
              agg = c(bs = "all", hs = "all", hsr = "all0", vin = "all", loc = "all", typ = "res", inc = "all"),
              rprt = c(bsr = "all"),
              silent = silent),


    ## by initial heating system ====
    reportAgg(v_renovation,
              "Renovation|Residential|Initial|{hs} (bn m2/yr)", brickSets,
              agg = c(bs = "all", bsr = "all0", hsr = "all", vin = "all", loc = "all", typ = "res", inc = "all"),
              rprt = c(hs = "all"),
              silent = silent),


    ## by building type + initial heating system ====
    reportAgg(v_renovation,
              "Renovation|Residential|{typ}|Initial|{hs} (bn m2/yr)", brickSets,
              agg = c(bs = "all", bsr = "all0", hsr = "all", vin = "all", loc = "all", inc = "all"),
              rprt = c(hs = "all", typ = "res"),
              silent = silent),


    ## by final heating system ====
    reportAgg(v_renovation,
              "Renovation|Residential|Final|{hsr} (bn m2/yr)", brickSets,
              agg = c(bs = "all", hs = "all", bsr = "all0", vin = "all", loc = "all", typ = "res", inc = "all"),
              rprt = c(hsr = "all"),
              silent = silent),


    ## by final heating system, with zero state ====
    reportAgg(v_renovation,
              "Renovation|Residential|Final with zero|{hsr} (bn m2/yr)", brickSets,
              agg = c(bs = "all", hs = "all", bsr = "all0", vin = "all", loc = "all", typ = "res", inc = "all"),
              rprt = c(hsr = "all0"),
              silent = silent),

    ## only identical replacement ====
    reportAgg(v_renovation,
              "Renovation|Residential|Identical replacement (bn m2/yr)", brickSets,
              agg = c(bs = "all", hs.hsr = "identRepl", bsr = "all0", vin = "all", loc = "all",
                      typ = "res", inc = "all"),
              silent = silent),

    ## only changes of heating systems ====
    reportAgg(v_renovation,
              "Renovation|Residential|Change of heating system (bn m2/yr)", brickSets,
              agg = c(bs = "all", hs.hsr = "trueRenov", bsr = "all0", vin = "all", loc = "all", typ = "res", inc = "all"),
              silent = silent),

    ## only identical replacement by heating system ====
    reportAgg(v_renovation,
              "Renovation|Residential|Identical replacement|{hs.hsr} (bn m2/yr)", brickSets,
              agg = c(bs = "all", bsr = "all0", vin = "all", loc = "all", typ = "res", inc = "all"),
              rprt = c(hs.hsr = "identRepl"),
              silent = silent),

    ## only changes of heating systems by heating system ====
    reportAgg(v_renovation,
              "Renovation|Residential|Change of heating system|{hs.hsr} (bn m2/yr)", brickSets,
              agg = c(bs = "all", bsr = "all0", vin = "all", loc = "all", typ = "res", inc = "all"),
              rprt = c(hs.hsr = "trueRenov"),
              silent = silent)

  )

  return(out)
}
