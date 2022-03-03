test_that("dhr works", {

  t <- 1:100
  s <- 10
  x <- cos(t/s*2*pi)

  a <- dhr(x, s = s)

})
