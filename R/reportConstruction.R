#' Report construction
#'
#' Report quantities describing the construction of new buildings
#'
#' @param gdx gams transfer container of the BRICK GDX
#' @param tmpl character, BRICK reporting template
#'
#' @author Robin Hasse
#'
#' @importFrom magclass mbind setNames dimSums mselect collapseDim

reportConstruction <- function(gdx, tmpl = NULL) {

  # READ -----------------------------------------------------------------------

  # construction variable
  v_construction <- readGdxSymbol(gdx, "v_construction")

  # unit conversion: million m2 / yr-> billion m2 / yr
  v_construction <- (v_construction / 1000) %>%
    mselect(qty = "area") %>%
    collapseDim(dim = "qty")




  # REPORT ---------------------------------------------------------------------

  out <- mbind(

    ## Total ====
    reportAgg(v_construction,
              "Construction|Buildings (bn m2/yr)", tmpl,
              agg = c(bs = "all", hs = "all", loc = "all", typ = "resCom", inc = "all")),
    reportAgg(v_construction,
              "Construction|Residential (bn m2/yr)", tmpl,
              agg = c(bs = "all", hs = "all", loc = "all", typ = "res", inc = "all")),
    reportAgg(v_construction,
              "Construction|Commercial (bn m2/yr)", tmpl,
              agg = c(bs = "all", hs = "all", loc = "all", typ = "com", inc = "all")),


    ## by building type ====
    reportAgg(v_construction,
              "Construction|Residential|{typ} (bn m2/yr)", tmpl,
              agg = c(bs = "all", hs = "all", loc = "all", inc = "all"),
              rprt = c(typ = "res")),


    ## by location ====
    reportAgg(v_construction,
              "Construction|Residential|{loc} (bn m2/yr)", tmpl,
              agg = c(bs = "all", hs = "all", typ = "res", inc = "all"),
              rprt = c(loc = "all")),


    ## by heating system ====
    reportAgg(v_construction,
              "Construction|Residential|{hs} (bn m2/yr)", tmpl,
              agg = c(bs = "all", loc = "all", typ = "res", inc = "all"),
              rprt = c(hs = "all")),


    ## by building type + heating system ====
    reportAgg(v_construction,
              "Construction|Residential|{typ}|{hs} (bn m2/yr)", tmpl,
              agg = c(bs = "all", loc = "all", inc = "all"),
              rprt = c(hs = "all", typ = "res"))

  )

  return(out)
}
