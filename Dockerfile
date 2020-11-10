FROM rocker/rstudio:4.0.0-ubuntu18.04

RUN sudo apt-get update && apt-get install -y libxml2 pandoc libcairo2-dev

ARG RPKG=/tmp/pkg
ADD . ${RPKG}
RUN R -e "install.packages('devtools')" \
    && R -e "devtools::install_dev_deps('${RPKG}')" \
    && R CMD INSTALL ${RPKG}
