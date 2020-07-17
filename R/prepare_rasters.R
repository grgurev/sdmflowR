#' Prepare rasters
#'
#' Prepare environmental raster layers by loading them from the folder and
#' cropping to the project extent. In that process rasters are aligned to
#' the same grid and extent and converted to ASCII rasters. This function works 
#' only for continuous rasters.
#'
#' @param folder_in filename (character) or Raster object. Supported file types
#' are the "native" raster package format and those that can be read via rgdal
#' @param folder_out filename (character). Folder where prepared data will be
#' saved. Default is to save them in the same folder where original rasters are
#' stored.
#' @param extent Extent object, or any object from which an Extent object can
#' be extracted such as: a 2x2 matrix (first row: xmin, xmax; second row: ymin,
#' ymax), vector (length=4; order= xmin, xmax, ymin, ymax) or list (with at
#' least two elements, with names 'x' and 'y')
#' @param ref_raster filename (character) or Raster object. Supported file types
#' are the "native" raster package format and those that can be read via rgdal
#'
#' @return New rasters prepared for analysis
#' @export
#' @importFrom magrittr "%>%"
#' @importFrom sp "spTransform"

prepare_rasters = function(folder_in, folder_out = folder_in, extent, ref_raster) {
  # set magrittr dot variable to NULL
  . = NULL

  # create folder (if provided) for storing rasters
  ifelse(!dir.exists(folder_out), dir.create(folder_out), FALSE)

  # default projection is WGS84
  default_proj = "+proj=longlat +datum=WGS84"

  # create sf object from the project extent
  extent_sp = methods::as(raster::extent(extent), 'SpatialPolygons')
  raster::crs(extent_sp) <- raster::crs(default_proj)

  # define pattern based on most often used formats
  pattern = "\\.bil$|\\.tif$"

  # read the files from folder where original rasters are stored
  files_in = list.files(gsub("/$", "", folder_in), pattern = pattern, full.names = TRUE)

  # prepare filenames for prepared rasters
  files_out = gsub(folder_in, "", files_in) %>%
    gsub(pattern, ".asc", .) %>%
    paste0(ifelse(!endsWith(folder_out, "/"), paste0(folder_out, "/"), folder_out), .)

  # load reference raster
  ref_raster = raster::raster(ref_raster) %>%
    raster::crop(., raster::extent(extent_sp))

  # read, crop and save rasters
  for (i in 1:length(files_in)) {
    # get the raster projection
    raster_proj = raster::crs(raster::raster(files_in[i]))@projargs

    # prepare raster by raster - load, project and resample
    raster::raster(files_in[i]) %>%
      {
        if(default_proj == raster_proj) {
          raster::crop(., raster::extent(extent_sp)) %>%
            raster::resample(., ref_raster, method = "ngb")
        } else {
          raster::crop(., raster::extent(sp::spTransform(extent_sp, raster_proj))) %>%
            raster::projectRaster(., ref_raster, method = "bilinear") %>%
            raster::resample(., ref_raster, method = "ngb")
        }
      } %>%
      raster::writeRaster(., paste0(files_out[i]), overwrite=TRUE)
  }
}
