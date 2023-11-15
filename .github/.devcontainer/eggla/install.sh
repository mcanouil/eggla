#!/usr/bin/env bash


USERNAME=${USERNAME:-${_REMOTE_USER:-"automatic"}}
EGGLA_VERSION=${VERSION:-"main"}

FEATURE_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

set -e

# Determine the appropriate non-root user
if [ "${USERNAME}" = "auto" ] || [ "${USERNAME}" = "automatic" ]; then
    USERNAME=""
    POSSIBLE_USERS=("vscode" "node" "codespace" "$(awk -v val=1000 -F ":" '$3==val{print $1}' /etc/passwd)")
    for CURRENT_USER in "${POSSIBLE_USERS[@]}"; do
        if id -u "${CURRENT_USER}" >/dev/null 2>&1; then
            USERNAME=${CURRENT_USER}
            break
        fi
    done
    if [ "${USERNAME}" = "" ]; then
        USERNAME=root
    fi
elif [ "${USERNAME}" = "none" ] || ! id -u "${USERNAME}" >/dev/null 2>&1; then
    USERNAME=root
fi

if [ "${USERNAME}" = "root" ]; then
    user_home="/root"
# Check if user already has a home directory other than /home/${USERNAME}
elif [ "/home/${USERNAME}" != $( getent passwd $USERNAME | cut -d: -f6 ) ]; then
    user_home=$( getent passwd $USERNAME | cut -d: -f6 )
else
    user_home="/home/${USERNAME}"
    if [ ! -d "${user_home}" ]; then
        mkdir -p "${user_home}"
        chown ${USERNAME}:${group_name} "${user_home}"
    fi
fi

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
    zlib1g-dev \
    libgdal-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libv8-dev \
    libudunits2-dev \
    libfftw3-dev \
    libgit2-dev

install_pak() {
    local version=$1

    if [ "${version}" = "auto" ]; then
        if su "${USERNAME}" -c "R -s -e 'packageVersion(\"pak\")'" >/dev/null 2>&1; then
            echo "pak is already installed. Skip pak installation..."
            return
        else
            version="devel"
        fi
    fi

    echo "Installing pak ${version}..."
    # shellcheck disable=SC2016
    su "${USERNAME}" -c 'R -q -e "install.packages(\"pak\", repos = sprintf(\"https://r-lib.github.io/p/pak/'"${version}"'/%s/%s/%s\", .Platform\$pkgType, R.Version()\$os, R.Version()\$arch))"'
}

export DEBIAN_FRONTEND=noninteractive

echo "Install R packages..."
mkdir /tmp/r-packages
pushd /tmp/r-packages


install_pak "auto"

cp ${FEATURE_DIR}/R/pkg.lock pkg.lock
su "${USERNAME}" -c "R -q -e \"n <- 0; while(inherits(try(pak::lockfile_install(update = TRUE), silent = TRUE), 'try-error') && n < 3) n <- n + 1\""

if [ "${EGGLA_VERSION}" = "main" ]; then
    su "${USERNAME}" -c "R -q -e \"pak::pkg_install('mcanouil/eggla', upgrade = FALSE, dependencies = TRUE)\""
else
    su "${USERNAME}" -c "R -q -e \"pak::pkg_install('mcanouil/eggla@${EGGLA_VERSION}', upgrade = FALSE, dependencies = TRUE)\""
fi

su "${USERNAME}" -c "R -q -e \"pak::cache_clean()\""

cp ${FEATURE_DIR}/tests/eggla-example.R /eggla-example.R
chmod +x /eggla-example.R

cp ${FEATURE_DIR}/bin/plink2 /usr/bin/plink2
chmod +x /usr/bin/plink2

popd
rm -rf /tmp/r-packages
rm -rf /tmp/Rtmp*

echo "Done!"
