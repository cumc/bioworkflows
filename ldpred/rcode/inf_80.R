if(response==1){
  inf_model = lm(reg.formula, dat=reg.dat)
}
if(response==2){
  inf_model = glm(reg.formula, dat=reg.dat,family=binomial)
}