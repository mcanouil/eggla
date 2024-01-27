#!/usr/bin/env bash

BCFTOOLS_VERSION=${VERSION:-"1.19"}

set -e

apt_get_update() {
    if [ "$(find /var/lib/apt/lists/* | wc -l)" = "0" ]; then
        echo "Running apt-get update..."
        apt-get update -y
    fi
}

# Checks if packages are installed and installs them if not
check_packages() {
    if ! dpkg -s "$@" >/dev/null 2>&1; then
        apt_get_update
        apt-get -y install --no-install-recommends "$@"
    fi
}

check_packages curl \
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
    zlib1g-dev

wget -q -P /tmp/ https://github.com/samtools/bcftools/releases/download/${BCFTOOLS_VERSION}/bcftools-${BCFTOOLS_VERSION}.tar.bz2
tar -C /tmp/ -xjf /tmp/bcftools-${BCFTOOLS_VERSION}.tar.bz2
cd /tmp/bcftools-${BCFTOOLS_VERSION}
autoreconf -i
./configure --prefix=/usr
make
make install
rm -rf /tmp/bcftools-${BCFTOOLS_VERSION}

# Clean up
rm -rf /var/lib/apt/lists/*

echo "Done!"
