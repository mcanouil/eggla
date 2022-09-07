ARG TAG=devel
FROM --platform=linux/amd64 ghcr.io/mcanouil/eggla:${TAG}
RUN Rscript -e 'pak::pkg_install(c("devtools", "languageserver", "httpgd", "jsonlite"), upgrade = FALSE, dependencies = TRUE)'
