FROM gaow/base-notebook:1.0.0

LABEL mantainer="dmc2245@cumc.columbia.edu"

ARG BOOST_IO
ARG LIB_INSTALL

USER root

WORKDIR /tmp

# Download and compile  bgenix needed for regenie to run
#RUN gpg --keyserver pgp.mit.edu --recv-key E084DAB9 && gpg -a --export E084DAB9 |  apt-key add -

RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9 

RUN wget https://enkre.net/cgi-bin/code/bgen/tarball/7aa2c109c6/BGEN-7aa2c109c6.tar.gz

RUN apt-get update && apt-get install -y --no-install-recommends \
      g++ \
      make \
      python3 \
      zlib1g-dev \
      autoconf \
      automake \
      gcc \
      perl \
      zlib1g-dev libbz2-dev liblzma-dev libcurl4-gnutls-dev libssl-dev libncurses5-dev \
      $LIB_INSTALL\ 
      && tar -xzf BGEN-7aa2c109c6.tar.gz \
      && rm BGEN-7aa2c109c6.tar.gz \
      && cd BGEN-7aa2c109c6 \
      && ./waf configure --prefix=/usr/local/ \
      && ./waf install

# Install pre-requisites for region-extraction pipeline

RUN pip install pandas_plink pybgen scipy memory_profiler rpy2

# Download and install PLINK2 version alpha2.3 date:01-24-2020

RUN wget http://s3.amazonaws.com/plink2-assets/alpha2/plink2_linux_x86_64.zip  && \
    unzip plink2_linux_x86_64.zip && \
    cp plink2 /usr/local/bin && \
    rm -rf plink2*

#Download and install  PLINK1.9 beta6.21 date:10-19-2020
RUN wget http://s3.amazonaws.com/plink1-assets/plink_linux_x86_64_20201019.zip && \
    unzip plink_linux_x86_64_20201019.zip && \
    cp plink /usr/local/bin && \
    rm -rf plink

# Install qctool
RUN wget https://www.well.ox.ac.uk/~gav/resources/qctool_v2.0.6-Ubuntu16.04-x86_64.tgz && tar -zxvf  qctool_v2.0.6-Ubuntu16.04-x86_64.tgz && \
    rm -rf qctool_v2.0.6-Ubuntu16.04-x86_64.tgz && \
    cd qctool_v2.0.6-Ubuntu16.04-x86_64 && chmod a+x qctool &&\
    cp qctool /usr/local/bin/

#Download and install R packages
RUN Rscript -e 'p = c("data.table", "ggplot2", "ggrepel", "dplyr", "qqman", "remotes","scales", "stats", "matrixStats", "gridExtra", "igraph", "devtools", "RccpArmadillo", "CompQuadForm", "doMC", "foreach", "Matrix", "BiocManager", "testthat"); install.packages(p, repos="https://cloud.r-project.org")'
RUN Rscript -e 'remotes::install_github("anastasia-lucas/hudson")'
RUN Rscript -e 'remotes::install_github("stephenslab/susieR")'
RUN Rscript -e 'remotes::install_github("gabraham/flashpca/flashpcaR")'
RUN Rscript -e 'BiocManager::install(c("SeqArray","SeqVarTools"))'
RUN Rscript -e 'devtools::install_github("hanchenphd/GMMAT")'

#Download and intall BOLT-LMM

ADD https://storage.googleapis.com/broad-alkesgroup-public/BOLT-LMM/downloads/old/BOLT-LMM_v2.3.4.tar.gz /tmp/BOLT-LMM_v2.3.4.tar.gz

RUN tar -zxvf BOLT-LMM_v2.3.4.tar.gz && \
    rm -rf BOLT-LMM_v2.3.4.tar.gz && \
    cp BOLT-LMM_v2.3.4/bolt /usr/local/bin/ && \
    cp BOLT-LMM_v2.3.4/lib/* /usr/local/lib/

#Download and install FastGWA

RUN wget https://cnsgenomics.com/software/gcta/bin/gcta_1.93.2beta.zip && \
    unzip gcta_1.93.2beta.zip && \
    cp gcta_1.93.2beta/gcta64 /usr/local/bin/ && \
    rm -rf gcta*
    
#Download and install GEMMA

RUN wget https://github.com/genetics-statistics/GEMMA/releases/download/v0.98.4/gemma-0.98.4-linux-static-AMD64.gz && \
    gunzip gemma-0.98.4-linux-static-AMD64.gz && \
    chmod a+x gemma-0.98.4-linux-static-AMD64 && \
    cp gemma-0.98.4-linux-static-AMD64 /usr/local/bin/gemma && \
    rm gemma-0.98.4-linux-static-AMD64*
    
       
# Download and compile regenie from source code

COPY .  /tmp/

WORKDIR /tmp/regenie

RUN  make BGEN_PATH=/tmp/BGEN-7aa2c109c6 HAS_BOOST_IOSTREAM=$BOOST_IO

RUN apt-get update && apt-get install -y --no-install-recommends \
      libgomp1 $LIB_INSTALL \
     &&  cp /tmp/regenie/regenie /usr/local/bin


#Install bcftools
RUN wget https://github.com/samtools/bcftools/releases/download/1.12/bcftools-1.12.tar.bz2 -O bcftools.tar.bz2 && \
  tar -xjvf bcftools.tar.bz2 && \
  cd bcftools-1.12 && \
  make && \
  make prefix=/usr/local/bin install && \
  ln -s /usr/local/bin/bin/bcftools /usr/bin/bcftools

#Install htslib that includes tabix
RUN wget https://github.com/samtools/htslib/releases/download/1.12/htslib-1.12.tar.bz2 -O htslib-1.12.tar.bz2 && \
    tar -xjvf htslib-1.12.tar.bz2 && \
    cd htslib-1.12 && \
    ./configure --prefix=/usr/local/bin && \
    make && \
    make install && \
    cp tabix bgzip htsfile /usr/local/bin

USER jovyan

WORKDIR /home/jovyan
