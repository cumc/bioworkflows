for(i in 1:ncol(pred_grid)){
  reg.dat$PRS <- pred_grid[,i]
  if(response==1){
    grid.model <- lm(reg.formula, dat=reg.dat)
  }
  if(response==2){
    grid.model <- glm(reg.formula, dat=reg.dat,family = binomial)
  }
  if(max.r2 < summary(grid.model)$r.squared){
    max.r2 <- summary(grid.model)$r.squared
  }
}