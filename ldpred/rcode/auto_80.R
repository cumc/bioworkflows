if(response==1){
  auto_model <- lm(reg.formula, dat=reg.dat)
}
if(response==2){
  auto_model <- glm(reg.formula, dat=reg.dat,family=binomial)
}