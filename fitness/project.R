# I expect to get a lIst of lm or glm mod.
data <- data.frame(y = c(1,2,3,4,5), x1 = c(10,9,5,7,6), x2 = c(7,6,5,4,3), x3 = c(1,2,3,4,5))

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
