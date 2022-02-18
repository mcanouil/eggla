FROM mcanouil/r-ver:4.1.2

LABEL org.opencontainers.image.licenses="GPL-3.0" \
      org.opencontainers.image.source="https://github.com/mcanouil/eggla" \
      org.opencontainers.image.authors="Mickaël Canouil <https://mickael.canouil.fr/>"

COPY pkg.lock pkg.lock
RUN R -e "install.packages('pak', repos = 'https://r-lib.github.io/p/pak/devel/')"
RUN R -e "pak::lockfile_install(lockfile = 'pkg.lock')"

CMD ["R"]
