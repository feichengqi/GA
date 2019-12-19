## Test for fitness
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

