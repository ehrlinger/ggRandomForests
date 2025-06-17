.onAttach <- function(libname, pkgname) {
  ggRandomForests.version <- read.dcf(file=system.file("DESCRIPTION", package=pkgname), 
                            fields="Version")
  packageStartupMessage(paste("\n",
                              pkgname,
                              ggRandomForests.version,
                              "\n",
                              "\n",
                              "Type ggrandomforests.news() to see new features, changes, and bug fixes.",
                              "\n",
                              "\n"))
}