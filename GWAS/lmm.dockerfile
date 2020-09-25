FROM gaow/base-notebook:1.0.0

LABEL mantainer="dmc2245@cumc.columbia.edu"

ARG BOOST_IO
ARG LIB_INSTALL

USER root

WORKDIR /tmp

# Download and compile  bgenix needed for regenie to run

ADD http://code.enkre.net/bgen/tarball/release/v1.1.7 /tmp/v1.1.7.tgz

RUN apt-get update && apt-get install -y --no-install-recommends \
      g++ \
      make \
      python3 \
      zlib1g-dev \
      $LIB_INSTALL \
      && tar -xzf v1.1.7.tgz \
      && rm v1.1.7.tgz \
      && cd v1.1.7 \
      && python3 waf configure \
      && python3 waf

# Download and install PLINK2

RUN wget http://s3.amazonaws.com/plink2-assets/plink2_linux_x86_64_20200831.zip  && \
    unzip plink2_linux_x86_64_20200831.zip && \
    cp plink2 /usr/local/bin && \
    rm -rf plink2*

#Download and install R packages
RUN Rscript -e 'p = c("ggplot2", "ggrepel", "dplyr", "qqman", "remotes"); install.packages(p, repos="https://cloud.r-project.org")'
RUN Rscript -e 'remotes::install_github("anastasia-lucas/hudson")'

#Download and intall BOLT-LMM

ADD https://data.broadinstitute.org/alkesgroup/BOLT-LMM/downloads/BOLT-LMM_v2.3.4.tar.gz /tmp/BOLT-LMM_v2.3.4.tar.gz

RUN tar -zxvf BOLT-LMM_v2.3.4.tar.gz && \
    rm -rf BOLT-LMM_v2.3.4.tar.gz && \
    cp BOLT-LMM_v2.3.4/bolt /usr/local/bin/ && \
    cp BOLT-LMM_v2.3.4/lib/* /usr/local/lib/

#Download and install FastGWA

RUN wget https://cnsgenomics.com/software/gcta/bin/gcta_1.93.2beta.zip && \
    unzip gcta_1.93.2beta.zip && \
    cp gcta_1.93.2beta/gcta64 /usr/local/bin/ && \
    rm -rf gcta*
    
# Download and compile regenie from source code

COPY .  /tmp/

WORKDIR /tmp/regenie

RUN  make BGEN_PATH=/tmp/v1.1.7 HAS_BOOST_IOSTREAM=$BOOST_IO

RUN apt-get update && apt-get install -y --no-install-recommends \
      libgomp1 $LIB_INSTALL \
     &&  cp /tmp/regenie/regenie /usr/local/bin

RUN wget https://data.broadinstitute.org/alkesgroup/BOLT-LMM/downloads/BOLT-LMM_v2.3.4.tar.gz && \
    tar -zxvf BOLT-LMM_v2.3.4.tar.gz && \
    rm -rf BOLT-LMM_v2.3.4.tar.gz && \
    cp BOLT-LMM_v2.3.4/bolt /usr/local/bin/ && \
    cp BOLT-LMM_v2.3.4/lib/* /usr/local/lib/

USER jovyan

WORKDIR /home/jovyan
