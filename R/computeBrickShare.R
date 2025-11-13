#' Estimate Brick model heating system shares
#'
#' @author Ricarda Rosemann
#'
#' @param data data frame, Brick stock and flow data
#' @param variable character, Brick variable that is evaluated
#'
#' @importFrom dplyr %>% across all_of .data filter group_by mutate
#'   rename select ungroup
#'
computeBrickShare <- function(data, variable) {

  # If this is renovation data: Need to sum over hsr and remove zero renovation
  if (variable == "renovation") {
    hsName <- "hsr"
    data <- filter(data, .data[["hsr"]] != "0")
  } else {
    hsName <- "hs"
  }

  # Compute the share of each hs/hsr entry
  data %>%
    group_by(across(-all_of(c(hsName, "value")))) %>%
    mutate(totVal = sum(.data[["value"]]),
           shareVal = .data[["value"]] / .data[["totVal"]]) %>%
    ungroup() %>%
    select(-"totVal", -"value") %>%
    rename(value = "shareVal")
}
