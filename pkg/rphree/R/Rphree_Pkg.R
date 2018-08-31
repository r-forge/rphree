### Rphree library: interface R/PHREEQC (Parkhurst & Appelo)
### packaging-related functions
### 
### Marco De Lucia, delucia@gfz-potsdam.de, 2009-2018
### Time-stamp: "Last modified 2018-08-31 11:14:13 delucia"

.onAttach <- function(libname,pkgname) {
    ## figuring version number, adapted from package mgcv/CHNOSZ
    ## pkghelp <- library(help="Rphree")$info[[1]]
    ## ## things are different for older versions of R
    ## if(length(pkghelp)==1)
    ##     pkghelp <- library(help="Rphree")$info[[2]]
    
    ## version <- pkghelp[pmatch("Version:", pkghelp)]
    ## um <- strsplit(version, " ")[[1]]
    ## version <- um[nchar(um)>0][2]
    ## date <- pkghelp[pmatch("Date:", pkghelp)]
    ## um <- strsplit(date, " ")[[1]]
    ## date <- um[nchar(um)>0][2]
    ## ## identify the program and version
    ## packageStartupMessage(paste(":: This is [rforge/dev] Rphree version ", version, " (", date, ")", sep=""))
    packageStartupMessage(":: This is [rforge/dev] Rphree version 0.1-7 (2018-08-31)")
}
