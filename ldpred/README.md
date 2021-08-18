# Polygenic risk score using LDpred2

Author: Mengyu Zhang, Gao Wang

This repo shows the work on ldpred2 pipeline developed by Mengyu Zhang with input from Gao Wang. 
The pipeline is built on R package `bigsnpr` developed by Florian Privé et al. 

The overall workflow shown in `ldpred.ipynb` is 

1. Calculate correlation (LD) matrix

2. Perform LD score regression to obtain $h^2$ the (SNP) heritability

3. estimate posterior mean of beta

4. predict polygenic risk score

`HDL_example.ipynb` shows the commands for analyzing real-world data from Million Veteran Program and UK Biobank.
