# [WIP] Polygenic risk score using LDpred2

## Data

* Phenotype and covariates data (X and Y): `pheno` (data we collect???)

* Summary statistics: `sumstats` (data we collect???)

* genotype objects: `obj.bigSNP` (reference dataset???)

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

# Reference Pannel

https://www.biorxiv.org/content/10.1101/642546v1.full.pdf

## Hapmap3 [20]

## 1000G Project Phase 3

identified more than 84.483 million single nucleotide polymorphisms (SNP) from 2,504 individuals which collected from 2684 worldwide populations, each population contains 61~113 individuals.  All of the 26 populations
85 were divided into 5 groups (AFR, African; AMR, Ad Mixed American; EAS, East Asian; EUR,
86 European; SAS, South Asian)

## UK10K Project

## Genome of the Netherlands

## Haplotype Reference Consortium (HRC)

## The China

NARD

