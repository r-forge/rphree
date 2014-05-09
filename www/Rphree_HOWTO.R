### Rphree-0.1-3, 2014 05 09
### Marco De Lucia, delucia at gfz-potsdam.de


### This should install the current version from rforge directly from
### R:

install.packages("Rphree", repos="http://R-Forge.R-project.org")


### If this does not work, download manually the source code or the
### compiled package from the rforge web site.

### To build and install from source under windows:
## 1. install R (32 & 64 bit) - probably 3.1.0 as of today 
## 2. install appropriate Rtools from http://cran.r-project.org/bin/windows/Rtools/
##    (I installed both on a remote server under "X:")
## 3. Open a DOS cmdline, integrate the PATH (of course depending on where you installed these tools).
##    Just copy and paste this line - after opportune modifications - in the dos windows:

PATH=X:\Rtools\bin;X:"R-3.1.0\bin";X:\Rtools\gcc-4.6.3\bin;%PATH%

## 4. In the cmdline, navigate where you did save the Rphree_0.1-x.tar.gz
##    The following should compile and install both 32 and 64 bit version of the package 
##    (depending on what is installed on your machine)

cd PATH\TO\YOUR\WORKING\DIR

R CMD INSTALL --build Rphree_0.1-x.tar.gz

## 5. Sit back and relax while compiling and installing!


############ Basic usage example ############

## Consider that I only did limited testing on windows!!

## start R and load Rphree
library(Rphree)

## Look at the included files: databases and one simple example PHREEQC script
dir(system.file("extdata", package="Rphree"))

## Use the db directly or, better, copy them in a more suitable location in your working directory
llndb <- RPhreeFile(system.file("extdata", "llnl_rphree.dat", package="Rphree"), is.db=TRUE)
pqcdb <- RPhreeFile(system.file("extdata", "phreeqc.dat", package="Rphree"), is.db=TRUE)

## Load a normal phreeqc script
ex1 <- RPhreeFile(system.file("extdata", "ex1.phrq", package="Rphree"), is.db=FALSE)

## First of all understand how the "selection" for the outputs is coded
## One needs to define a named logical vector exactly like this:
mysel <- c(kin=FALSE, tot=TRUE, desc=TRUE, species = TRUE, pphases = TRUE, SI=TRUE, punch =FALSE)

## NB: EQUILIBRIUM_PHASES is coded as "pphases" in Rphree, while the
## simulations blocks in the input buffer use the keyword "PURE"!!
## There is an helper function for that, for checking and for
## generating valid selections:
sel <- !RPhreeCheckSel()

## To visualize the difference:
mysel
sel

## Documentation...
?RPhreeCheckSel

## Your first Rphree simulation:
equilln <- Rphree(ex1, db=llndb, sel=mysel, write=TRUE, out="ex1")
equipqc <- Rphree(ex1, db=pqcdb, sel=mysel, write=FALSE)

## understanding the output
str(equilln)

equilln$species
equilln$pphases

## From a solution, create a new script (for restart):
eq2 <- RInputFromList(equilln)

#####################
## Distribute: assign specific values to a property in input
## Only one value:
ext <- Distribute(input=ex1, prop="Cl", values=3) 
ext <- Distribute(input=ext, prop="Na", values=3) 
equi3pqc <- Rphree(ext, db=pqcdb, sel=mysel)
equi3lln <- Rphree(ext, db=llndb, sel=mysel)


## RPinfo: extract informations from calculated solutions
RPinfo(equi3lln,"tot","C")
RPinfo(equi3pqc,"tot","C")

RPinfo(equi3lln,"pphases","CO2(g)")
RPinfo(equi3pqc,"pphases","CO2(g)")

## Graphically compare the 2 solutions
VizSol(list(equi3lln,equi3pqc),names=c("llnl.dat","phreeqc.dat"),log="y",col=c("light blue","light green"))


#####################
## Distribute more than 1 value
## The argument "values" in function "Distribute" must be of length 1
## OR the same length as the solutions already in the input!
ext_range <- Distribute(ext,"Cl",seq(1,4,0.1))
ext_range <- Distribute(ext_range,"Na",seq(1,4,0.1))

## ext_range now is an input containing 31 solutions. This can be run at once:
equi_extrange_llnl <- Rphree(ext_range, db=llndb,sel=mysel)
equi_extrange_phrq <- Rphree(ext_range, db=pqcdb,sel=mysel)

## the result is a list of lists!
str(equi_extrange_llnl)

## extracting results: RPinfo automagically gets all properties
RPinfo(equi_extrange_llnl,"desc","pH") ## 31 values!

## standard R plot
plot(type="l",seq(1,4,0.1), RPinfo(equi_extrange_llnl,"desc","pH"), 
     main="pH vs salinity under constant CO2(g) pressure", 
     xlab="Na, Cl concentration [molal]", ylab="pH", ylim=c(5.44, 5.61))
lines(seq(1,4,0.1), RPinfo(equi_extrange_phrq,"desc","pH"), col="red")
legend("bottomleft",legend=c("phreeqc.dat","llnl.dat"),lwd=2, col=c("red","black"))

## AddProp: add a property to an input!
## remember for "pphases" to "paste" SI and moles!
extmin <- AddProp(ext, name='Calcite', cat="pphases", values="0.0 10")
extmin <- AddProp(extmin, name='Anhydrite', cat="pphases", values="0.0 10")

resextmin_pqc <- Rphree(extmin, db=pqcdb,sel=mysel)
resextmin_lln <- Rphree(extmin, db=llndb,sel=mysel)

VizMinDelta(list(resextmin_pqc,resextmin_lln),phases=c("Anhydrite","Calcite"), names=c("phreeqc.dat","llnl.dat"))


## By default nothing gets written to disc, but you can let Rphree
## write the standard outfile by specifying write=TRUE:
equi_extrange_phrq <- Rphree(ext_range, db=pqcdb, sel=mysel, write=TRUE, out="equi_extrange")
## Now in your current working dir you will have the file "equi_extrange.Rout"
dir(pattern="Rout")
# [1] "equi_extrange.Rout" "ex1.Rout"

## There are helper functions to (partially) parse PHREEQC outfiles:
equi_extrange_fromoutfile <- RReadOut(out="equi_extrange.Rout")
## Currently tot, pphases and partly "desc" are read from outfile.
equi_extrange_phrq[[1]]$pphases
equi_extrange_fromoutfile[[1]]$pphases

