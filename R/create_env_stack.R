#' Create raster stack from environmental data
#' 
#' Create raster stack from environmental data so that raster stack by
#' providing folder where raster are stored and optionally subset of those
#' raster that needs processing. It is mostly used when checking for NAs and in
#' value extraction in generating absences and psudoabsences
#' 
#' @param folder_in filename (character) or Raster object. Supported file types
#' are the "native" raster package format and those that can be read via rgdal
#' @param env_subset vector of filenames without extension (character) that
#' will be processed and stacked
#'
#' @return Raster stack
#' @export
#' @importFrom magrittr "%>%"

create_env_stack = function(folder_in, env_subset = c()) {
  # set magrittr dot variable to NULL
  . = NULL

  # message for the user
  cat("\nCreating stack from environmental data ... \n")

  # define pattern based on most often used formats
  pattern = "\\.bil$|\\.tif$|\\.asc$"

  # get the rasters and create raster stack
  stack = list.files(gsub("/$", "", folder_in), pattern, full.names = TRUE) %>%
    raster::stack()

  # check if subset is defined to return only subset rasters
  if (length(env_subset) != 0) {
    stack = raster::subset(stack, env_subset)
  }

  # set the crs of raster stack
  raster::crs(stack) = "+proj=longlat +datum=WGS84"
  return(stack)
}
