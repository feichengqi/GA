## Test for fitness
library(testthat)
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
fitness_scores

assert_that(identical(fitness, fitness_scores))

pop_result = select(dim = 3, p = 25, t = 50, m_prob = 0.01, data = data, fitness = AIC, func = lm, response = y, min = TRUE)
names(pop_result[[3]][[1]]) = NULL
assert_that(identical(unlist(pop_result[[3]][[1]]), c(TRUE, FALSE, FALSE)))

y = c(1,3,5,7,9)
data2 <- cbind(c(10,9,5,7,6),  c(7,6,5,4,3), c(1,2,3,4,5))
data3 <- data.frame(x1 = c(10,9,5,7,6), x2 = c(7,6,5,4,3), x3 = c(1,2,3,4,5))
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
test_that('the fitness_socre result is wrong',
          {
            expect_equal(fitness, fitness_score)
          })

