# Add GATK4 to debian-ngs

FROM gaow/debian-ngs:latest

# :)
MAINTAINER Gao Wang, wang.gao@columbia.edu

# Install tools
WORKDIR /tmp
## https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=863199
RUN mkdir -p /usr/share/man/man1
RUN apt-get update -y \
    && apt-get install -qq -y --no-install-recommends \
    default-jdk python3 python3-matplotlib r-base \
    build-essential zlib1g-dev libbz2-dev liblzma-dev \
    && apt-get autoclean \
    && rm -rf /var/lib/apt/lists/* /var/log/dpkg.log
ENV GATK_VERSION 4.1.6.0
ADD https://raw.githubusercontent.com/broadinstitute/gatk/${GATK_VERSION}/scripts/docker/gatkbase/install_R_packages.R /opt
RUN curl -L \
    https://github.com/broadinstitute/gatk/releases/download/${GATK_VERSION}/gatk-${GATK_VERSION}.zip -o gatk.zip \
    && unzip gatk.zip \
    && mv gatk-${GATK_VERSION} /opt \
    && ln -s /opt/gatk-${GATK_VERSION}/gatk /usr/local/bin/gatk \
    && rm -rf /tmp/*
#RUN Rscript /opt/install_R_packages.R && rm -rf /tmp/*
RUN ln -s /usr/bin/python3 /usr/local/bin/python
COPY Annovar.tar.gz /tmp
RUN tar zxvf Annovar.tar.gz -C /usr/local/bin && rm -f /tmp/*

# Default command
CMD ["bash"]
