test_that("check_na_data returns NAs clean data.table object", {
  f_in = paste0(tempdir(), "/data")
  gpkg_in = file.path(f_in, "species.gpkg")
  ifelse(!dir.exists(f_in), dir.create(f_in), FALSE)

  sf = sf::st_sf(spec = c("tesspe"), geom = sf::st_sfc(sf::st_point(c(0.5, 0.5)), sf::st_point(c(1.5, 1.5)), sf::st_point(c(2.5, 2.5))))

  rgdal::writeOGR(obj = as(sf, "Spatial"), dsn = gpkg_in, layer = "spec", driver = "GPKG")

  r1 = raster::raster(nrows=3, ncols=3, xmn=0, xmx=3, ymn=0, ymx=3, vals=c(1,2,NA,4,5,NA,7,8,NA))
  r2 = raster::raster(nrows=3, ncols=3, xmn=0, xmx=3, ymn=0, ymx=3, vals=c(1,NA,3,4,NA,6,7,8,NA))
  rs = raster::stack(r1, r2)

  cn_remove_false = check_na_data(species = "spec", species_gpkg = gpkg_in, env_layers = rs)
  cn_remove_true = check_na_data(species = "spec", species_gpkg = gpkg_in, env_layers = rs, remove = TRUE)

  expect_equal(length(cn_remove_false[, spec]), 2)
  expect_equal(length(cn_remove_true[, spec]), 1)
})
