test_that("create_env_stack returns raster stack", {
  f_in = paste0(tempdir(), "/test_rasters")
  ifelse(!dir.exists(f_in), dir.create(f_in), FALSE)

  r1 = raster::raster(nrows = 10, ncols = 10, xmn = 0, xmx = 10, ymn = 0, ymx = 10, vals = 1)
  raster::writeRaster(r1, file.path(f_in, "test_raster1.tif"), format="GTiff", overwrite=TRUE)

  r2 = raster::raster(nrows = 10, ncols = 10, xmn = 0, xmx = 10, ymn = 0, ymx = 10, vals = 2)
  raster::writeRaster(r2, file.path(f_in, "test_raster2.tif"), format="GTiff", overwrite=TRUE)

  r3 = raster::raster(nrows = 10, ncols = 10, xmn = 0, xmx = 10, ymn = 0, ymx = 10, vals = 3)
  raster::writeRaster(r3, file.path(f_in, "test_raster3.tif"), format="GTiff", overwrite=TRUE)

  s = c("test_raster1", "test_raster2")
  es = create_env_stack(f_in, s)

  expect_true(class(es)[1] == "RasterStack" & length(es@layers) == length(s))
  rm(f_in, r1, r2, r3, s, es)
})
