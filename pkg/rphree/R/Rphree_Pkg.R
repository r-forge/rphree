### Rphree library: interface R/PHREEQC (Parkhurst & Appelo)
### packaging-related functions
### 
### Marco De Lucia, delucia@gfz-potsdam.de, 2009-2014
### Time-stamp: "Last modified 2014-05-30 17:26:33 delucia"

.onAttach <- function(libname,pkgname) {
  ## figuring version number, adapted from package mgcv/CHNOSZ
  pkghelp <- library(help=Rphree)$info[[1]]
  ## things are different for older versions of R
  if(length(pkghelp)==1)
      pkghelp <- library(help=Rphree)$info[[2]]

  version <- pkghelp[pmatch("Version:", pkghelp)]
  um <- strsplit(version, " ")[[1]]
  version <- um[nchar(um)>0][2]
  date <- pkghelp[pmatch("Date:", pkghelp)]
  um <- strsplit(date, " ")[[1]]
  date <- um[nchar(um)>0][2]
  ## identify the program and version
  packageStartupMessage(paste(":: This is [rforge/dev] Rphree version ", version, " (", date, ")", sep=""))
}
