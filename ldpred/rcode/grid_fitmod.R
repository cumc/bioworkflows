if(response==1){
  grid_model = lm(reg.formula, dat=data[train.ind,])
}
if(response==2){
  grid_model = glm(reg.formula, dat=data[train.ind,],family=binomial)
}