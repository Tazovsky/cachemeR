FROM rocker/rstudio:4.0.0-ubuntu18.04
ARG RPKG=/tmp/pkg
ADD . ${RPKG}
RUN R -e "install.packages('devtools')" \
    && R -e "devtools::install_deps('${RPKG}')" \
    && R CMD INSTALL ${RPKG}
