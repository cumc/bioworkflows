# Polygenic risk score using LDpred2

## Data

* Phenotype and covariates data (X and Y): `pheno` (data we collect???)

* Summary statistics: `sumstats` (reference dataset???)

* genotype objects: `obj.bigSNP` (data we collect???)

genotype, fam, map (snp information)

* HapMap3 SNPs: `info` (restricting the analysis to only the HapMap3 SNPs, filter SNPs)

## Workflow?

1. Calculate correlation (LD) matrix R from other dataset

    * Perform SNP matching `snp_match(sumstats, map)` to get `info_snp`

Mathch alleles between summary statistics `sumstats` and SNP information from `obj.bigSNP`.

    * CM information from 1000 Genome `snp_asGeneticPos(CHR, POS, dir = ".")`

Use genetic maps available at https://github.com/joepickrell/1000-genomes-genetic-maps/ to interpolate physical positions (in bp) to genetic positions (in cM).

    * calculate LD using genotype from `obj.bigSNP` and CM information

2. Perform LD score regression to obtain $h^2$ the (SNP) heritability

`snp_ldsc()`

3. estimate posterior mean of beta using LD scores, beta in `sumstats` and estimated heritability

`snp_ldpred2_xxx (corr, beta, h2,...)`

xxx: inf/grid/auto

4. predict polygenic score using genotype, posterior beta

`big_prodVec()`: vector product

`big_prodMat()`: matrix product

`big_univLogReg()`: Slopes of column-wise logistic regressions of each column of a Filebacked Big Matrix, with some other associated statistics. Covariates can be added to correct for confounders.

5. Calculate the null R^2 using pricipal component (???)

6. Get the final performance of the LDpred models

## per chromosome or genome-wide?

difference in obtian LD matrix and model PRS 

(???): Polygenic risk score? GLM? Depend on summary statistics and the type of the traits (quatitative or binary)?


## which model?

infinitesimal/grid/auto model


# Polygenic score using LDpred

[github code and tutorial](https://github.com/bvilhjal/ldpred)

