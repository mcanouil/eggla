#!/usr/bin/env bash
set -e

PLINK2_VERSION=${1:-${PLINK2_VERSION:-20230109}}

mkdir inst/bin

wget -q -P /tmp/ "https://s3.amazonaws.com/plink2-assets/plink2_linux_x86_64_${PLINK2_VERSION}.zip" &&
  unzip -d /tmp /tmp/plink2_linux_x86_64_${PLINK2_VERSION}.zip &&
  mv /tmp/plink2 inst/bin/plink2 &&
  rm /tmp/plink2_linux_x86_64_${PLINK2_VERSION}.zip
