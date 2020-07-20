#' Check NAs in species data
#'
#' Check for NAs in species data by extracting data from environmental raster
#' layers. If argument remove is TRUE the function can automatically remove
#' NAs from the species dataset.
#'
#' @param species short species name (character) used in project. For example:
#' satmon for Satureja montana, aursax for Aurinia saxatilis etc
#' @param species_column_name column name (character) as in .gpkg file from
#' where species data will be extracted. Default value is "spec"
#' @param env_layers environmental layers (raster stack) that is the result
#' of running create_env_stack function
#' @param remove optional value (boolean) to choose if removal of NAs will
#' also be performed
#'
#' @return Data table object
#' @export
#' @importFrom magrittr "%>%"
#' @import data.table

check_na_data = function(
  species,
  species_column_name = "spec",
  species_gpkg = "data/species.gpkg",
  env_layers,
  remove = FALSE
) {
  cat("\nChecking for NAs ... \n")

  # set magrittr dot variable to NULL
  . = NULL

  # get the species points from .gpkg geopackage file
  species_points = sf::st_read(
    dsn = species_gpkg,
    layer = species,
    quiet = TRUE
  )

  # extract values from raster object at the locations of species points
  species_points_extracted = suppressWarnings(
    raster::extract(
      x = env_layers,
      y = species_points,
      sp = TRUE
    ) %>%
      data.table::as.data.table()
  )

  # perform check for NAs/removal of NAs
  if (remove == TRUE) {
    cat("\nRemoving NAs from species data ... \n")
    species_points_extracted[
      complete.cases(species_points_extracted[
        , .SD, .SDcols = names(env_layers)])] %>%
      .[, .(
        spec = get(species_column_name),
        X = coords.x1,
        Y = coords.x2
      )
    ] %>%
      return()
  } else {
    species_points_extracted[
      !complete.cases(species_points_extracted[
        , .SD, .SDcols = names(env_layers)])] %>%
      return()
  }
}
