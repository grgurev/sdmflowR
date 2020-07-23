#' Prepare species data
#'
#' Prepare species data by creating presence and/or pseudoabsence datasets for
#' modeling. In that process, first the NAs are removed, then presence points
#' are thinned to remove points that are too close together (less than 1.5km
#' when) modeling with ~1km pixel size. It is assumed that in wd the directory
#' containing Geopackage file with species data is present.
#'
#' @param species name (character) used in project. For example:
#' satmon for Satureja montana, aursax for Aurinia saxatilis etc.
#' @param species_column_name column name (character) as in .gpkg file from
#' where species data will be extracted. Default value is "spec"
#' @param mask sf polygon object with specific areas from where pseudoabsences
#' will be extracted
#' @param env_layers environmental layers (raster stack) that is the result
#' of running create_env_stack function
#'
#' @return Data table object
#' @export
#' @importFrom magrittr "%>%"
#' @import data.table

prepare_species_data = function(
  species,
  species_column_name = "spec",
  mask,
  env_layers
) {
  # set magrittr dot variable to NULL
  . = NULL

  # remove NA rows if exists
  species_points = check_na_data(
    species,
    species_column_name,
    env_layers = env_layers,
    remove = TRUE
  )

  # perform dataset thinning
  cat("\nThinning species data ... \n")

  suppressWarnings(
    species_points %>%
      spThin::thin(
        loc.data = .,
        lat.col = "Y",
        long.col = "X",
        spec.col = "spec",
        thin.par = 1,
        reps = 100,
        locs.thinned.list.return = FALSE,
        write.files = TRUE,
        max.files = 1,
        out.dir = tempdir(),
        out.base = species,
        write.log.file = FALSE,
        verbose = FALSE
      )
  )

  # import thinned species data
  cat("\nImporting thinned species data ... \n")
  species_points = sf::st_read(
    dsn = paste0(
      tempdir(),
      "/",
      species,
      "_thin1.csv"
    ),
    options = c(
      "X_POSSIBLE_NAMES=X",
      "Y_POSSIBLE_NAMES=Y"
    ),
    quiet = TRUE
  )

  # prepare species presence points
  cat("\nPreparing species presence points ... \n")
  species_presence = sf::st_coordinates(species_points) %>%
    raster::extract(
      x = env_layers,
      y = .,
      sp = TRUE
    ) %>%
    as.data.table() %>%
    within(
      .,
      assign(
        species,
        rep(
          x = 1,
          nrow(.)
        )
      )
    ) %>%
    cbind(sf::st_coordinates(species_points))

  # using extent prepare pseudoabsence points
  cat("\nPreparing species pseudoabsence points ... \n")
  pseudobasence_points = sf::st_as_sfc(
    x = sf::st_bbox(
      mask,
      crs = 4326
    )
  ) %>%
    {
      suppressMessages(
        sf::st_intersection(
          .,
          mask
        )
      )
    } %>%
    {
      suppressMessages(
        sf::st_sample(
          x = .,
          size = 10000,
          type = "random"
        )
      )
    }

  species_pseudoabsence = sf::st_coordinates(pseudobasence_points) %>%
    raster::extract(
      x = env_layers,
      y = .
    ) %>%
    as.data.table() %>%
    within(., assign(species, rep(NA, nrow(.)))) %>%
    cbind(sf::st_coordinates(pseudobasence_points))

  # create data.table from both presence and pseudoabsence objects
  cat("\nCreating DT object from presence and pseudoabsence objects\n\n")
  species_presence_absence = rbind(species_presence, species_pseudoabsence) %>%
    data.table()

  return(species_presence_absence)
}





