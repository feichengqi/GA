library(assertthat)
library(testthat)
library(prodlim)
library(data.table)
library(rlist)
library(data.table)
library(testthat)
library(devtools)

# Test main function select and ga_fitness_score

context('test 1')

test_that('ga_fitness_score works', {
  y = c(1,3,5,7,9)
  data2 = cbind(c(10,9,5,7,6),  c(7,6,5,4,3), c(1,2,3,4,5))
  data3 = data.frame(x1 = c(10,9,5,7,6), x2 = c(7,6,5,4,3), x3 = c(1,2,3,4,5))
  gene = list()
  gene[[1]] = c(TRUE, TRUE, TRUE)
  gene[[2]] = c(TRUE, TRUE, FALSE)
  gene[[3]] = c(TRUE, FALSE, TRUE)
  gene[[4]] = c(FALSE, TRUE, TRUE)
  gene[[5]] = c(FALSE, FALSE, TRUE)
  gene[[6]] = c(FALSE, TRUE, FALSE)
  gene[[7]] = c(TRUE, FALSE, FALSE)

  fitness = numeric()
  for(i in 1:7){
    mod = lm(y~data2[,gene[[i]]])
    fitness = c(fitness, AIC(mod))
  }

  fitness_score = ga_fitness_score(list_of_gene = gene, response = y, data = data3)
  expect_equal(fitness, fitness_score)
})


# Test fitness_score again

context('test 2')

test_that("ga_fitness_score works", {
  x = 1:100
  data = cbind(x, cos(1:100), sin(1:100))
  y = x + rnorm(n = 100)

  gene = list()
  gene[[1]] = c(TRUE, TRUE, TRUE)
  gene[[2]] = c(TRUE, TRUE, FALSE)
  gene[[3]] = c(TRUE, FALSE, TRUE)
  gene[[4]] = c(FALSE, TRUE, TRUE)
  gene[[5]] = c(FALSE, FALSE, TRUE)
  gene[[6]] = c(FALSE, TRUE, FALSE)
  gene[[7]] = c(TRUE, FALSE, FALSE)

  fitness = numeric()
  for(i in 1:7){
    mod = lm(y~data[,gene[[i]]])
    fitness = c(fitness, AIC(mod))
  }

  data = as.data.frame(data)
  fitness_scores = ga_fitness_score(list_of_gene = gene, data = data, fitness = AIC, func = lm, response = y, min = FALSE)
  expect_identical(fitness,fitness_scores)
})



# Tests for ga_mutate and ga_crossover

context('test 3')

test_that("ga_mutate works",{
  all_combos = unique(expand.grid(c(TRUE,FALSE,FALSE),
                                  c(TRUE,FALSE,FALSE),
                                  c(TRUE,FALSE,FALSE)))
  result = ga_mutate(c(TRUE,FALSE,FALSE))
  check = sum(row.match(all_combos, matrix(result, nrow = 1)), na.rm = T)
  expect_equal(check,1L)
})

context('test 4')

test_that("ga_crossover works",{
  parentA = c(TRUE, FALSE, FALSE)
  parentB = c(FALSE, TRUE, TRUE)
  child1 = as.data.frame(matrix(
    c(TRUE,TRUE,TRUE,
      TRUE,FALSE,TRUE,
      TRUE,FALSE,FALSE), byrow = T, nrow = 3))
  child2 = as.data.frame(matrix(
    c(FALSE,FALSE,FALSE,
      FALSE,TRUE,FALSE,
      FALSE,TRUE,TRUE), byrow = T, nrow = 3))
  result = ga_crossover(parentA, parentB)
  check1 = row.match(child1, matrix(result[[1]], nrow = 1))
  check2 = row.match(child2, matrix(result[[2]], nrow = 1))
  expect_identical(check1,check2)
})

