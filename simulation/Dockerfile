FROM rocker/r-ver:4.0.4
RUN mkdir -p /simulation-files
COPY rpcm_tree_simulation* /simulation-files
RUN install2.r --error \
    partykit \
    lme4 \
    stringr \
    effects \
    merDeriv \
    tictoc \
    doParallel \
    doRNG \
    tidyr \
    dplyr \
    mclust

CMD Rscript /simulation-files/rpcm_tree_simulation.R