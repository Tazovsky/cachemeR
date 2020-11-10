FROM rocker/rstudio:4.0.0-ubuntu18.04

RUN sudo apt-get install -y libxml2

ARG RPKG=/tmp/pkg
ADD . ${RPKG}
RUN R -e "install.packages('devtools')" \
    && R -e "devtools::install_deps('${RPKG}')" \
    && R CMD INSTALL ${RPKG}
