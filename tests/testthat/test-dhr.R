test_that("dhr works", {

  t <- 0:99
  s <- 10
  x1 <- sin((t / s * 2 * pi) + 0)
  x2 <- sin((t / s * 2 * pi) + pi / 2)
  x3 <- sin((t / s * 2 * pi) + pi)
  x4 <- sin((t / s * 2 * pi) + 1.5 * pi)

  a <- dhr(x1, s = s)

  p1 <- phase(dhr(x1, s = s))[50]
  p2 <- phase(dhr(x2, s = s))[50]
  p3 <- phase(dhr(x3, s = s))[50]
  p4 <- phase(dhr(x4, s = s))[50]

  t1 <- trend(dhr(x1+10, s= s))[50]

  expect_true(abs(p1 - 0) < 0.0001 |  abs(p1 - 2*pi) < 0.0001 )
  expect_true(abs(p2 - pi/2) < 0.0001 )
  expect_true(abs(p3 - pi) < 0.0001 )
  expect_true(abs(p4 - 1.5 * pi) < 0.0001 )

  expect_equal(round(t1),10)

})
