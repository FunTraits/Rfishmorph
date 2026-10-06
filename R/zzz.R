# Quiet R CMD check about the ggplot2/rlang `.data` pronoun and the computed
# `level` variable from stat_density_2d(), both used inside aes().
utils::globalVariables(c(".data", "level"))
