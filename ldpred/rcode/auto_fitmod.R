if(response==1){
  auto_model = lm(reg.formula, dat=data[train.ind,])
}
if(response==2){
  auto_model = glm(reg.formula, dat=data[train.ind,],family=binomial)
}