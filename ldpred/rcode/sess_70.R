# Reformat the phenotype file such that y is of the same order as the 
# sample ordering in the genotype file
y <- pheno[fam.order, on = c("FID", "IID")]
if(response==1){
  
  # Calculate the null R2
  
  null.model <- paste("PC", 1:6, sep = "", collapse = "+") %>%
    paste0("Height~Sex+", .) %>%
    as.formula %>%
    lm(., data = y) %>%
    summary()
  null.r2 <- null.model$r.squared
}

if(response==2){
  library(fmsb)
  # Calculate the null R2
  null.model <- paste("PC", 1:6, sep = "", collapse = "+") %>%
    paste0("Height~Sex+", .) %>%
    as.formula %>%
    glm(., data = y, family=binomial) %>%
    summary()
  null.r2 <- NagelkerkeR2(null.model)
}