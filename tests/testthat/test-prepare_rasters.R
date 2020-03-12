test_that("prepare_rasters return list of files", {
  expect_gt(length(prepare_rasters("./")), 0)
})
