
y = c(1,3,5,7,9)

data <- data.frame(x1 = c(10,9,5,7,6), x2 = c(7,6,5,4,3), x3 = c(1,2,3,4,5))

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
fitness2 <- function(list_of_gene, data, fitness = AIC, func = lm, response, min = TRUE) {
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
fitness2(list_gene, data = data, response = y)
fitness2(list_gene, data = data, func = glm, response = y)



mod1 <- lm(data$y~data$x1)

mod2 <- lm(data$y~data$x1+data$x2)

mod3 <- lm(data$y~data$x3+data$x1)

glmmod1 <- glm(data$y~data$x1)
AIC(mod1)
AIC(mod2)
AIC(mod3)
AIC(glmmod1)
mod <- list(mod1,mod2,mod3) # a list of mod

#the smaller the AIC value the better the model
#the function's argument is a lisr of lm or glm mod and a objective function(defalut:AIC)
fitness <- function(mod ,func = AIC){
  fitvec <- vector()
for (i in 1:length(mod)) {
  fitvec <- c(fitvec, func(mod[[i]]))
}
  vec <- order(fitvec)
  rank_mod <- list()
  for (i in order(fitvec)) {
    rank_mod <-c(rank_mod,mod[i])
  }
  return (rank_mod)
}
#the function return a list of mod ranked by their AIC value.
rank_mod <- fitness(mod)
AIC(rank_mod[[1]])
AIC(rank_mod[[2]])
AIC(rank_mod[[3]])
