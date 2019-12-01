data <- data.frame(y = c(1,2,3,4,5), x1 = c(10,9,5,7,6), x2 = c(7,6,5,4,3), x3 = c(1,2,3,4,5))
mod1 <- lm(data$y~data$x1)
mod2 <- lm(data$y~data$x1+data$x2)
mod3 <- lm(data$y~data$x3+data$x1)
glmmod1 <- glm(data$y~data$x1)
AIC(mod1)
AIC(mod2)
AIC(mod3)
AIC(glmmod1)
mod <- list(mod1,mod2,mod3)
#the smaller the AIC value the better the model
fitness <- function(mod ,func = AIC){
  fitvec <- vector()
for (i in 1:length(mod)) {
  fitvec <- c(fitvec, func(mod[[i]]))
}
  return (fitvec)
}

fitness(mod)
order(fitness(mod))
rank_mod <- list()
for (i in order(fitness(mod))) {
  rank_mod <-c(rank_mod,mod[i])
}
