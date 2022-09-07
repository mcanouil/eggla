# See here for image contents: https://github.com/microsoft/vscode-dev-containers/tree/main/containers/debian/.devcontainer/base.Dockerfile

ARG VARIANT="bullseye"
FROM mcr.microsoft.com/vscode/devcontainers/base:${VARIANT}

LABEL org.opencontainers.image.title="eggla"
LABEL org.opencontainers.image.description="Docker image for the 'eggla' R package."
LABEL org.opencontainers.image.source="https://github.com/mcanouil/eggla"
LABEL org.opencontainers.image.base.name="mcr.microsoft.com/vscode/devcontainers/base:${VARIANT}"

# ** [Optional] Uncomment this section to install additional packages. **
# RUN apt-get update && export DEBIAN_FRONTEND=noninteractive \
#     && apt-get -y install --no-install-recommends <your-package-list-here>

RUN apt-get update && export DEBIAN_FRONTEND=noninteractive \
    && apt-get -y install --no-install-recommends \
       curl \
       build-essential \
       libcurl4-openssl-dev \
       ca-certificates \
       bash-completion \
       gzip \
       libxt6 \
       bzip2 \
       wget \
       autoconf \
       automake \
       make \
       cmake \
       libbz2-dev \
       liblzma-dev \
       libjpeg-dev \
       libxml2-dev \
       zlib1g-dev \
       libgdal-dev \
       libfontconfig1-dev \
       libharfbuzz-dev \
       libfribidi-dev

ARG BCFTOOLS_VERSION=1.15.1
RUN wget -q -P /tmp/ https://github.com/samtools/bcftools/releases/download/${BCFTOOLS_VERSION}/bcftools-${BCFTOOLS_VERSION}.tar.bz2 \
    && tar -C /tmp/ -xjf /tmp/bcftools-${BCFTOOLS_VERSION}.tar.bz2 \
    && cd /tmp/bcftools-${BCFTOOLS_VERSION} \
    && autoreconf -i \
    && ./configure --prefix=/usr \
    && make \
    && make install \
    && rm -rf /tmp/bcftools-${BCFTOOLS_VERSION}

COPY inst/bin/plink2 /usr/bin/plink2
RUN chmod +x /usr/bin/plink2

ARG RIG_VERSION=latest
ARG R_VERSION="4.2.0"
COPY inst/setup/pkg.lock pkg.lock
RUN wget -q -P /tmp/ "https://github.com/r-lib/rig/releases/download/${RIG_VERSION}/rig-linux-${RIG_VERSION#v}.tar.gz" \
    && tar -C /usr/local -zxvf /tmp/rig-linux-${RIG_VERSION#v}.tar.gz \
    && yes | rig add ${R_VERSION} \
    && Rscript -e "pak::lockfile_install(update = FALSE)" \
    && Rscript -e "pak::pkg_install(c('renv', 'data.table', 'mcanouil/eggla'), upgrade = FALSE, dependencies = TRUE)" \
    && Rscript -e "pak::cache_clean()"

COPY inst/setup/eggla-example.R eggla-example.R
RUN chmod +x eggla-example.R

RUN apt-get autoremove -y \
    && apt-get autoclean -y \
    && rm -rf /var/lib/apt/lists/* \
    && rm -rf /tmp/*
