ggrandomforests.news <- function(...) {
  newsfile <- file.path(system.file(package="ggRandomForests"), "NEWS")
  file.show(newsfile)
}