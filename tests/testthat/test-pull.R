context("Pulling a single column as a vector")

gr0 <- GRanges()
ir0 <- IRanges()

mdata <- DataFrame(score = as.integer(c(0,3,4,5,10,14,0, 0, 4,8)),
                   gc = runif(10),
                   counts = rpois(10, 2),
                   name = letters[1:10])
ir1 <- IRanges(start = 1:10, width = 5)
mcols(ir1) <- mdata
gr1 <- GRanges(seqnames = "seq1",
               strand = c(rep("+", 4), rep("-", 3), rep("*", 3)),
               ranges = ir1)

test_that("Pull returns a vector even if empty", {
  expect_identical(pull(ir0) %>% length(), ir0 %>% length())
  expect_identical(pull(gr0) %>% length(), gr0 %>% length())
  expect_identical(pull(ir1) %>% length(), ir1 %>% length())
  expect_identical(pull(gr1) %>% length(), gr1 %>% length())
})

test_that("Pull returns the correct column", {
  expect_equal(pull(ir1,score), mdata$score)
  expect_equal(pull(ir1,gc), mdata$gc)
  expect_equal(pull(ir1,counts), mdata$counts)
  expect_equal(pull(ir1, name), letters[1:10])
  expect_equal(pull(ir1, start), 1:10)
  expect_equal(pull(ir1, width),rep(5,10))
  
  expect_equal(pull(gr1,score), mdata$score)
  expect_equal(pull(gr1,gc), mdata$gc)
  expect_equal(pull(gr1,counts), mdata$counts)
  expect_equal(pull(gr1, name), letters[1:10])
  expect_equal(pull(gr1, start), 1:10)
  expect_equal(pull(gr1, seqnames), as.factor(rep("seq1", 10)))
  expect_equal(pull(gr1, width),rep(5,10))
})

test_that("Pull returns a named vector", {

  out <- pull(ir1,score, name)
  expect_equal(unname(out), mdata$score)
  expect_equal(names(out), mdata$name)
  
  out <- pull(gr1,score, name)
  expect_equal(unname(out), mdata$score)
  expect_equal(names(out), mdata$name)
})
