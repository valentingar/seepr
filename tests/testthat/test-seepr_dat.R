test_that("seepr_dhr works", {

  df <- data.frame(datetime = c(1:10),
                   T_1 = c(1:10),
                   T_2 = c(1:10))
  mat <- matrix(c(rep(1:10, times = 3)), ncol = 3)

  expect_equal(class(seepr_dat(df, c(1,2))),
               c("seepr_dat", "data.frame"))
  expect_equal(class(seepr_dat(mat, c(1,2))),
               c("seepr_dat", "data.frame"))
  expect_equal(seepr_dat(mat, c(1,2)), seepr_dat(df, c(1,2)))
  expect_error(seepr_dat(df, c(1)))
  expect_error(seepr_dat(df, c(1,2,3)))
  expect_error(seepr_dat(df[, (1:2)], c(1)))


})
