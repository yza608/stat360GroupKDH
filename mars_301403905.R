library(rpart)
library(earth)

fwd_stepwise <- function(){
  return (list())
}

bwd_stepwise <- function(input_model){
  return (list())
}

mars.control <- function(){
  return (list())
}

mars <- function(formula, data, control){
  tmp_model = fwd_stepwise()
  return (bwd_stepwise(tmp_model))
}

mars.control()
mars()
