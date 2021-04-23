library(bigsnpr)
library(data.table)
library(bigsparser)
# calculate LD
#load("./res-data/LdInput.RData")
# Get maximum amount of cores
NCORES = nb_cores()
# Initialize variables for storing the LD score and LD matrix
corr = NULL
ld = NULL
# We want to know the ordering of samples in the bed file 
fam.order = NULL
# Open a temporary file
#load("./res-data/LdInput.Rdata")
tmp = tempfile(tmpdir = "./res-data")
on.exit(file.remove(paste0(tmp, ".sbk")), add = TRUE)
for (chr in 1:22) {
  # Extract SNPs that are included in the chromosome
  ind.chr <- which(info_snp$chr == chr)
  ind.chr2 <- info_snp$`_NUM_ID_`[ind.chr]
  # Calculate the LD
  corr0 <- snp_cor(
    genotype,
    ind.col = ind.chr2,
    ncores = NCORES,
    infos.pos = POS2[ind.chr2],
    size = 3 / 1000
  )
  if (chr == 1) {
    ld <- Matrix::colSums(corr0^2)
    corr <- as_SFBM(corr0, tmp)
  } else {
    ld <- c(ld, Matrix::colSums(corr0^2))
    corr$add_columns(corr0, nrow(corr))
  }
}

# We assume the fam order is the same across different chromosomes
fam.order <- as.data.table(obj.bigSNP$fam)
# Rename fam order
setnames(fam.order,
         c("family.ID", "sample.ID"),
         c("FID", "IID"))