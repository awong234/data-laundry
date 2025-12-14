if (! "pak" %in% .packages(all=TRUE)) install.packages("pak")

pak::pkg_install(c(
  "mirai",
  "dplyr",
  "plotly",
  "dmurdoch/rgl2gltf"
))