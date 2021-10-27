# Polygenic risk score using LDpred2

Author: Mengyu Zhang, Gao Wang

This repo shows the work on ldpred2 pipeline developed by Mengyu Zhang with input from Gao Wang. 
The pipeline is built on R package `bigsnpr` developed by Florian Privé et al. 

The overall workflow shown in `ldpred.ipynb` is 

1. Calculate correlation (LD) matrix

2. Perform LD score regression to obtain $h^2$ the (SNP) heritability

3. estimate posterior mean of beta

4. predict polygenic risk score

`ldpred_example.ipynb` shows the commands for analyzing [an example public data-set](https://drive.google.com/file/d/1x_G0Gxk9jFMY-PMqwtg6-vdEyUPp5p5u/view) from github user [@choishingwan](https://github.com/choishingwan). [`20200605_Urbut_MVP.ipynb`](https://github.com/gaow/random-nbs/blob/master/analysis/20200605_Urbut_MVP.ipynb) shows a real-data analysis application from one of our projects (available to authorized readers only).
