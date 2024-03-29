FROM debian:stable-slim

MAINTAINER Gao Wang, wang.gao@columbia.edu

# Install tools
WORKDIR /tmp

RUN apt-get update -y \
    && apt-get install -qq -y --no-install-recommends \
    curl ca-certificates \
    tabix samtools \
    default-jdk python3 python3-matplotlib r-base \
    build-essential zlib1g-dev libbz2-dev liblzma-dev \
    && apt-get autoclean \
    && rm -rf /var/lib/apt/lists/* /var/log/dpkg.log
ADD https://raw.githubusercontent.com/cumc/bioworkflows/master/variant-calling/install_R_packages.R /opt
RUN Rscript /opt/install_R_packages.R && rm -rf /tmp/*
ENV GATK_VERSION 4.2.6.1
RUN curl -L \
    https://github.com/broadinstitute/gatk/releases/download/${GATK_VERSION}/gatk-${GATK_VERSION}.zip -o gatk.zip \
    && unzip gatk.zip \
    && mv gatk-${GATK_VERSION} /opt \
    && ln -s /opt/gatk-${GATK_VERSION}/gatk /usr/local/bin/gatk \
    && rm -rf /tmp/gatk.zip
RUN ln -s /usr/bin/python3 /usr/local/bin/python
#RUN curl http://www.openbioinformatics.org/annovar/download/0wgxR2rIVP/annovar.latest.tar.gz -o /tmp/annovar.latest.tar.gz 
COPY annovar.latest.tar.gz /tmp
RUN tar zxvf /tmp/annovar.latest.tar.gz \
    && mv /tmp/annovar/* /usr/local/bin/ \
    && rm -f /tmp/annovar.latest.tar.gz \
    && chmod +x /usr/local/bin/*.pl

# Default command
CMD ["bash"]

# To build singularity image out of this:
# spython recipe gatk4-annovar.dockerfile | sed 's/Stage: spython-base//g' &> gatk4-annovar.def
# singularity build --fakeroot gatk4-annovar.sif gatk4-annovar.def
