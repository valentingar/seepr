test_that("fit_dhr works", {

  s <- 10
  t <- 0:99 / s * 2 * pi

  input_dat <- data.frame(datetime = 1:100,
                          sin(t),
                          sin(t+pi))

  dat_fit <-
    input_dat %>%
    seepr_dat(c(0, 10)) %>%
    fit_dhr(s)

})
