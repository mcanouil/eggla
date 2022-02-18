FROM mcanouil/r:4.1.2

LABEL org.opencontainers.image.licenses="GPL-3.0" \
      org.opencontainers.image.source="https://github.com/mcanouil/eggla" \
      org.opencontainers.image.authors="Mickaël Canouil <https://mickael.canouil.fr/>"

COPY renv.lock renv.lock
ENV RENV_VERSION 0.15.2
RUN R -e "install.packages('pak', repos = 'https://r-lib.github.io/p/pak/devel/')"
RUN R -e "pak::pkg_install('renv@${RENV_VERSION}')"
RUN R -e "pak::lockfile_install(lockfile = 'renv.lock')"

ENV EGGLA_VERSION 0.4.3

CMD ["R"]
