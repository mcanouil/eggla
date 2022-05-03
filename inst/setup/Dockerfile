FROM mcanouil/r-ver:4.2.0

LABEL org.opencontainers.image.licenses="GPL-3.0" \
      org.opencontainers.image.source="https://github.com/mcanouil/eggla" \
      org.opencontainers.image.authors="Mickaël Canouil <https://mickael.canouil.fr/>"

COPY inst/setup/pkg.lock pkg.lock
RUN Rscript -e 'install.packages("pak", repos = sprintf("https://r-lib.github.io/p/pak/stable/%s/%s/%s", .Platform$pkgType, R.Version()$os, R.Version()$arch))'
RUN Rscript -e "pak::pkg_install('mcanouil/eggla', upgrade = FALSE)"

COPY inst/setup/eggla-example.R eggla-example.R

ENV BCFTOOLS_VERSION=1.15.1
RUN apt-get update && \
  apt-get install -y --no-install-recommends \
    ca-certificates \
    bzip2 \
    libbz2-dev \
    liblzma-dev \
    wget \
    autoconf \
    automake \
    make

RUN wget -q -P /tmp/ https://github.com/samtools/bcftools/releases/download/${BCFTOOLS_VERSION}/bcftools-${BCFTOOLS_VERSION}.tar.bz2 && \
  tar -C /tmp/ -xjf /tmp/bcftools-${BCFTOOLS_VERSION}.tar.bz2 && \
  cd /tmp/bcftools-${BCFTOOLS_VERSION} && \
  autoreconf -i && \
  ./configure --prefix=/usr && \
  make && \
  make install && \
  rm -rf /tmp/bcftools-${BCFTOOLS_VERSION}

RUN wget -q -P /tmp/ https://github.com/samtools/htslib/releases/download/${BCFTOOLS_VERSION}/htslib-${BCFTOOLS_VERSION}.tar.bz2 && \
  tar -C /tmp/ -xjf /tmp/htslib-${BCFTOOLS_VERSION}.tar.bz2 && \
  cd /tmp/htslib-${BCFTOOLS_VERSION} && \
  autoreconf -i && \
  ./configure --prefix=/usr && \
  make && \
  make install && \
  rm -rf /tmp/htslib-${BCFTOOLS_VERSION}

RUN apt-get autoremove -y
RUN apt-get autoclean -y
RUN rm -rf /var/lib/apt/lists/*
RUN rm -rf /tmp/*

CMD ["R"]
