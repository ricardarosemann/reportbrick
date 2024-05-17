#' Render the BRICK calibration plotting routine
#'
#' Renders the file plotCalibration.Rmd to create the plots for the BRICK calibration
#'
#' @param outputDir (named) character vector, path(s) to output directories.
#'  If several paths are given, the names can be used to pass short scenario names.
#' @param cal character vector, name(s) of file(s) with calibration reporting results
#' @param outName character, string added to the pdf file name and names of additionally saved plots
#' @param scenNames character vector, scenario names for different paths.
#'  Needs to be specified if \code{outputDir} is unnamed and contains more than one element.
#' @param savePlots logical, whether all plots should additionally be saved as png
#'
#' @author Ricarda Rosemann
#'
#' @importFrom piamutils getSystemFile
#' @importFrom rmarkdown render
#' @export

plotBRICKCalib <- function(outputDir = ".", cal = "BRICK_calibration_report.csv",
                           outName = "", scenNames = NULL,
                           savePlots = FALSE) {

  # Extract the scenario name from the output directory
  scenario <- sub("_\\d{4}-\\d{2}-\\d{2}_\\d{2}.\\d{2}.\\d{2}", "", basename(outputDir))

  # Extract scenario names if more than one output directory was passed
  if (length(outputDir) > 1 && is.null(scenNames)) {
    if (!is.null(names(outputDir))) {
      scenNames <- names(outputDir)
    } else {
      stop("You passed more than one output directory, but did not provide the scenario names.",
           "Please either specify scenNames or pass outputDir as a named vector.")
    }
  }

  # Assemble the parameters to be passed to the markdown file
  yamlParams <- list(
    outputDir = normalizePath(outputDir),
    cal = cal,
    docTitle = paste("BRICK Calibration Report", paste(scenario, collapse = " - ")),
    scenNames = scenNames,
    name = outName,
    savePlots = savePlots
  )

  # All output will be stored in the first directory passed
  finalOutputDir <- unname(outputDir[1])

  # Copy markdown file to the final output directory
  file.copy(
            piamutils::getSystemFile(file.path("plotsCalibrationReporting", "plotsCalibration.Rmd"),
                                     package = "reportbrick"),
            finalOutputDir, overwrite = TRUE)


  # Call the Rmd file
  rmarkdown::render(
    file.path(finalOutputDir, "plotsCalibration.Rmd"),
    output_dir = finalOutputDir,
    output_file = paste0("CES_calibration_report_", outName, ".pdf"),
    output_format = "pdf_document",
    params = yamlParams
  )
}
