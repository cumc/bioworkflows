if(response==1){
  inf_model = lm(reg.formula, dat=data[train.ind,])
}
if(response==2){
  inf_model = glm(reg.formula, dat=data[train.ind,],family=binomial)
}