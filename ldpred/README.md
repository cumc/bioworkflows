# Polygenic risk score using LDpred2

1. Calculate correlation (LD) matrix R from other dataset

2. Perform LD score regression to obtain $h^2$ the (SNP) heritability

3. estimate posterior mean of beta using LD scores and estimated heritability

4. predict polygenic score using genotype, posterior beta