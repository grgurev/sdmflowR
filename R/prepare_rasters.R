#' Prepare rasters
#'
#' Prepare environmental raster layers by loading them from the folder and
#' cropping to the project extent. In that process rasters are aligned to
#' the same grid and extent and converted to ASCII rasters.
#'
#' @param folder filename (character) or Raster object. Supported file types
#' are the ’native’ raster package format and those that can be read via rgdal
#' @param extent Extent object, or any object from which an Extent object can
#' be extracted
#'
#' @return RasterStack
#' @export
prepare_rasters = function(folder, extent) {
  list.files(folder)
}
