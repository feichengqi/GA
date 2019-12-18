library(assertthat)
y = c(1,3,5,7,9)

data <- data.frame(x1 = c(10,9,5,7,6), x2 = c(7,6,5,4,3), x3 = c(1,2,3,4,5))
data2 <- data.frame(x1 = c(10,9,5,7), x2 = c(7,6,5,4), x3 = c(1,2,3,4))
cool <- c(7,6,5,4,7)

list_gene <- list(c(TRUE,FALSE,FALSE), c(TRUE,FALSE,TRUE), c(TRUE,FALSE,TRUE))

# The new fitness function.
#list_of_gene is a list of gene likes 
#list(c(TRUE,FALSE,FALSE), c(TRUE,FALSE,TRUE), c(TRUE,FALSE,TRUE)).
#data is a dataframe containing several x columus.
#fitness is the fitness function, default in AIC.
#func is the regression method, likes lm or glm, default in lm.
#response is the response value (y).
#min: TRUE is for those fitness function the smaller the fitness value the better the model
#like AIC,while FALSE is for those the larger fitness value the better the model.
fitness_score <- function(list_of_gene, data, fitness = AIC, func = lm, response, min = TRUE) {
  
  assert_that(is.data.frame(data), msg = "the data must be a dataframe.")
  assert_that(length(response) == nrow(data), 
              msg = "the dimension of observed vectors should be the same as that of response vector.")
  fitness_value <- vector()
  
  for (i in 1:length(list_of_gene)){
    gene <- list_of_gene[[i]]
    
    regression_data <- data[,gene]
    regression_data <- data.frame(y,regression_data)
    model <- func(y~., data = regression_data) 
    fitness_value <- c(fitness_value,fitness(model))
  }
  if (min == TRUE){return (-fitness_value)} else{return (fitness_value)}
}
# test
fitness_score(list_gene, data = data)
fitness_score(list_gene, data = data, func = glm, response = y)
fitness_score(list_gene, data = cool, response = y)
fitness_score(list_gene, data = data2, response = y)


