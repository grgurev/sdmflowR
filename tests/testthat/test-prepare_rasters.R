test_that("prepare_rasters returns croppped asci raster", {
  f_in = tempdir()
  f_out = f_in

  r = raster::raster(nrows=10, ncols=10, xmn=0, xmx=10, ymn=0, ymx=10, vals=1)
  raster::writeRaster(r, file.path(f_in, "test_raster.tif"), format="GTiff", overwrite=TRUE)

  e = raster::extent(c(0, 2, 0, 2))
  prepare_rasters(f_in, f_out, extent = e, ref_raster = r)
  res = raster::raster(file.path(f_in, "test_raster.asc"))

  expect_equal(res@extent, e)
  rm(f_in, f_out, r, e, res)
})
