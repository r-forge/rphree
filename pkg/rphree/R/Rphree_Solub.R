## Rphree, functions to model solubility of gases in brine, mainly
## Duan_Sun

### Marco De Lucia, delucia@gfz-potsdam.de, 2009-2014
### Time-stamp: "Last modified 2018-08-31 10:50:00 delucia"

##' Solubility of CO2 in NaCl solutions in mol/kg water Duan & Sun
##' (2003) Chemical Geology 193 (2003) 257-271. Adapted from excel
##' macro by B.M. Krooss, Aachen.
##'
##' TODO 
##' @title Duan and Sun model for solubility of CO2 in NaCl solutions
##' @param Tk Temperature in Kelvin
##' @param Pmpa Partial pressure of CO2 in MPa
##' @param mNaCl Molality (mol/kgw) of NaCl in the aqueous solution
##' @param verbose Logical. If TRUE, additional messages are
##' visualized.
##' @return Numeric value of xCO2 (mass fraction) of CO2 in the
##' aqueous phase at equilibrium
##' @author MDL
##' @export
DS_CO2sol <- function(Tk, Pmpa, mNaCl, verbose = FALSE)
{
  ## Units: T [K], Pmpa [MPa], mNaCl [mol/kg water]
  Pbar <- Pmpa * 10
  yCO2 <- (Pmpa - PH2Ompa(Tk)) / Pmpa ## gas phase molar fraction of
                                      ## CO2

  if (verbose)
    {
      cat(paste("Pressure in bar:",Pbar,"bar\n"))
      cat(paste("yCO2:",yCO2,"\n"))
    }

  par1 <- c(28.9447706, -0.0354581768, -4770.67077, 0.0000102782768,
            33.8126098, 0.0090403714, -0.00114934031, -0.307405726,
            -0.0907301486, 0.000932713393, 0)

  par2 <- c(-0.411370585, 0.000607632013, 97.5347708, 0, 0, 0, 0,
            -0.0237622469, 0.0170656236, 0, 0.0000141335834)

  par3 <- c(0.000336389723, -0.000019829898, 0, 0, 0, 0, 0,
            0.0021222083, -0.00524873303, 0, 0)


  fTP <- c(1, Tk, 1/Tk, Tk^2, 1/(630-Tk),
           Pbar, Pbar*log(Tk), Pbar/Tk, Pbar/(630-Tk),
           (Pbar/(630-Tk))^2, Tk*log(Pbar))

  muCO2     <- sum(par1 * fTP)
  laCO2Na   <- sum(par2 * fTP)
  xiCO2NaCl <- sum(par3 * fTP)

  tmp <- muCO2 - log(DS_PhiCO2(Tk, Pmpa)) + 2*mNaCl*laCO2Na +  xiCO2NaCl * mNaCl^2
  
  out <- Pbar * yCO2 / exp(tmp)
  if (out<0)
    out <- 0
  return(out)
}

## vDS_CO2sol<- Vectorize(DS_CO2sol,c("Tk","Pmpa"))

##' Computes the compressibility Z of CO2 by Duan and Sun, 2003.
##' Internally uses the \code{\link{stats::uniroot}} function.
##'
##' NONE
##' @title Compressibility Z of CO2 by the Duan and Sun (2003) model
##' @param Tk Temperature in Kelvin
##' @param Pmpa Pressure in MPa
##' @param verbose Logical. If TRUE, additional messages are showed
##' @return Numeric value of compressibility Z of CO2
##' @author MDL
##' @export
DS_ZCO2 <- function(Tk, Pmpa, verbose = FALSE)
{
  
  R <- 8.314467    ## J/mol/K
  PcCO2 <- 73.8    ## bar
  TcCO2 <- 304.15  ## K
  ##  VcCO2 <- R * TcCO2 / PcCO2
  
  Pr <- Pmpa * 10 / PcCO2 # bar
  Tr <- Tk / TcCO2

  a1  <-  0.0899288497
  a2  <- -0.494783127
  a3  <-  0.0477922245
  a4  <-  0.0103808883
  a5  <- -0.0282516861
  a6  <-  0.0949887563
  a7  <-  0.00052060088
  a8  <- -0.000293540971
  a9  <- -0.00177265112
  a10 <- -0.0000251101973
  a11 <-  0.0000893353441
  a12 <-  0.0000788998563
  a13 <- -0.0166727022
  a14 <-  1.398
  a15 <-  0.0296

  zfun <- function(Z)
    {
      return(-Z + (1 + (a1 + a2 / Tr ^ 2 + a3 / Tr ^ 3) / Z / Tr * Pr +
                   (a4 + a5 / Tr ^ 2 + a6 / Tr ^ 3) / Z ^ 2 / Tr ^ 2 * Pr ^ 2 +
                   (a7 + a8 / Tr ^ 2 + a9 / Tr ^ 3) / Z ^ 4 / Tr ^ 4 * Pr ^ 4 +
                   (a10 + a11 / Tr ^ 2 + a12 / Tr ^ 3) / Z ^ 5 / Tr ^ 5 * Pr^5 +
                   a13 / Tr ^ 3 / Z ^ 2 / Tr ^ 2 * Pr ^ 2 * (a14 + a15 / Z ^ 2 / Tr ^ 2 * Pr ^ 2)
                   * exp(-a15 / Z ^ 2 / Tr ^ 2 * Pr ^ 2))
             )
    }      
  solz <- uniroot(f=zfun, interval=c(0.001,10),tol=1e-10) ## interval
                                                          ## taken
                                                          ## from vba
                                                          ## script
  z <- solz$root
  if (verbose) {
    cat(paste("Z equation solved after",solz$iter,"iteration, \n"))
    cat(paste("Z=",z,"; zfun(",z,")=",zfun(z)," \n"))
  }
  return(z)
}


##' Computes the virial compressibility Z of CO2 by Duan, alternate
##' solution method using the Anderson-Björck Method
##' 
##' NONE
##' @title Compressibility Z of CO2 by the Duan and Sun (2003) model
##' @param Tk Temperature in Kelvin
##' @param Pmpa Pressure in MPa
##' @param verbose Logical. If TRUE, additional messages are showed
##' @return Numeric value of compressibility Z of CO2
##' @author MDL
##' @export
DS_ZCO2_var <- function(T, Pmpa, verbose=FALSE)
{
  a1  <- 0.0899288497
  a2  <- -0.494783127
  a3  <- 0.0477922245
  a4  <- 0.0103808883
  a5  <- -0.0282516861
  a6  <- 0.0949887563
  a7  <- 0.00052060088
  a8  <- -0.000293540971
  a9  <- -0.00177265112
  a10 <- -0.0000251101973
  a11 <- 0.0000893353441
  a12 <- 0.0000788998563
  a13 <- -0.0166727022
  a14 <- 1.398
  a15 <- 0.0296          

  R <- 8.314467    # J/mol/K
  PcCO2 <- 73.8      # bar
  TcCO2 <- 304.15    # K
  ##  VcCO2 <- R * TcCO2 / PcCO2
  Pr <- Pmpa * 10 / PcCO2
  Tr <- T / TcCO2

  e <- 0.00000001
  Z <- 0.2
  a <- Z
  Va <- -Z + (1 + (a1 + a2 / Tr ^ 2 + a3 / Tr ^ 3) / Z / Tr * Pr + (a4 + a5 / Tr ^ 2 + a6 / Tr ^ 3) / Z ^ 2 / Tr ^ 2 * Pr ^ 2 + (a7 + a8 / Tr ^ 2 + a9 / Tr ^ 3) / Z ^ 4 / Tr ^ 4 * Pr ^ 4 + (a10 + a11 / Tr ^ 2 + a12 / Tr ^ 3) / Z ^ 5 / Tr ^ 5 * Pr ^ 5 + a13 / Tr ^ 3 / Z ^ 2 / Tr ^ 2 * Pr ^ 2 * (a14 + a15 / Z ^ 2 / Tr ^ 2 * Pr ^ 2) * exp(-a15 / Z ^ 2 / Tr ^ 2 * Pr ^ 2))

  Z <- 1
  b <- Z
  Vb <- -Z + (1 + (a1 + a2 / Tr ^ 2 + a3 / Tr ^ 3) / Z / Tr * Pr + (a4 + a5 / Tr ^ 2 + a6 / Tr ^ 3) / Z ^ 2 / Tr ^ 2 * Pr ^ 2 + (a7 + a8 / Tr ^ 2 + a9 / Tr ^ 3) / Z ^ 4 / Tr ^ 4 * Pr ^ 4 + (a10 + a11 / Tr ^ 2 + a12 / Tr ^ 3) / Z ^ 5 / Tr ^ 5 * Pr ^ 5 + a13 / Tr ^ 3 / Z ^ 2 / Tr ^ 2 * Pr ^ 2 * (a14 + a15 / Z ^ 2 / Tr ^ 2 * Pr ^ 2) * exp(-a15 / Z ^ 2 / Tr ^ 2 * Pr ^ 2))

  for (iAB in c(1:100)) {
    cat(paste("Iter: ",iAB,"\n"))
    s12 <- (Vb - Va) / (b - a)
    c <- b - Vb / s12
    Z <- c
    Vc <- -Z + (1 + (a1 + a2 / Tr ^ 2 + a3 / Tr ^ 3) / Z / Tr * Pr + (a4 + a5 / Tr ^ 2 + a6 / Tr ^ 3) / Z ^ 2 / Tr ^ 2 * Pr ^ 2 + (a7 + a8 / Tr ^ 2 + a9 / Tr ^ 3) / Z ^ 4 / Tr ^ 4 * Pr ^ 4 + (a10 + a11 / Tr ^ 2 + a12 / Tr ^ 3) / Z ^ 5 / Tr ^ 5 * Pr ^ 5 + a13 / Tr ^ 3 / Z ^ 2 / Tr ^ 2 * Pr ^ 2 * (a14 + a15 / Z ^ 2 / Tr ^ 2 * Pr ^ 2) * exp(-a15 / Z ^ 2 / Tr ^ 2 * Pr ^ 2))
    if (abs(Vc) <= e) 
      return(c)

    if (Vb * Vc < 0) {
        a <- b
        b <- c
        Va <- Vb
        Vb <- Vc
      } else {
        g <- 1 - Vc / Vb
        if (g <= 0)
          g <- 0.5
        b <- c
        Va <- g * Va
        Vb <- Vc
      }
    if (abs(b - a) <= e)
      if (abs(Vb) <= abs(Va))
        return(b)
      else
        return(a)
  }
}

##' Computes the fugacity coefficient of CO2 by Duan and Sun 2003
##' model.
##'
##' TODO
##' @title Fugacity coefficient of CO2
##' @param Tk Temperature in Kelvin
##' @param Pmpa Pressure in MPa
##' @return Numeric value of the fugacity coefficient of CO2. The
##' molar volume is also computed and given as \emph{attribute} (if
##' \code{out} is the output, it becomes accessible with
##' \code{attr(out,"vol")})
##' @author MDL
##' @export
DS_PhiCO2 <- function(Tk, Pmpa)
{
  ## Parameters
  a1  <-  0.0899288497
  a2  <- -0.494783127
  a3  <-  0.0477922245
  a4  <-  0.0103808883
  a5  <- -0.0282516861
  a6  <-  0.0949887563
  a7  <-  0.00052060088
  a8  <- -0.000293540971
  a9  <- -0.00177265112
  a10 <- -0.0000251101973
  a11 <-  0.0000893353441
  a12 <-  0.0000788998563
  a13 <- -0.0166727022
  a14 <-  1.398
  a15 <-  0.0296

  R = 8.314467    # J/mol/K == cm3 bar / mol / K
  PcCO2 = 73.8    # bar
  TcCO2 = 304.15  # K

  Z <- DS_ZCO2(Tk, Pmpa)

  Pr <- Pmpa * 10 / PcCO2
  Tr <- Tk / TcCO2
  Vr <- Z * Tr / Pr

  tmp <- Z - 1 - log(Z) + (a1 + a2 / (Tr ^ 2) + a3 / (Tr ^ 3)) / Vr +
    (a4 + a5 / (Tr ^ 2) + a6 / (Tr ^ 3)) / (2 * Vr ^ 2) + ((a7 + a8 /
    (Tr ^ 2) + a9 / (Tr ^ 3)) / (4 * Vr ^ 4) + (a10 + a11 / (Tr ^ 2) +
    a12 / (Tr ^ 3)) / (5 * Vr ^ 5) + a13 / (2 * Tr ^ 3 * a15) * (a14 +
    1 - (a14 + 1 + a15 / Vr ^ 2) * exp(-a15 / Vr ^ 2)))

  Vol <- Z*Tr/Pr*R*TcCO2/PcCO2 *10 ## cm3 / mol

  out <- exp(tmp)
  attr(out,"vol") <- Vol 
  return(out)
}

## vDS_PhiCO2 <- Vectorize(DS_PhiCO2,c("Tk", "Pmpa"))


##' Calculate the Saturation Index SI of CO2(g) given T, P and using
##' the Duan equation of state. A Poynting correction factor is also
##' applied.
##'
##' @title Saturation Index for solubility of CO2(g) using Duan
##' equation of State
##' @param Tk Temperature in Kelvin
##' @param Pmpa Pressure in MPa
##' @param poy Logical. If TRUE, the Poynting correction term is also
##' computed and applied to the result
##' @param V Optional average specific molar volume of CO2. If
##' unspecified, the standard value of 32.0E-3 l/mol is used
##' @param verbose Logical. If TRUE, edditional messages are given.
##' @return The corrected SI to include in PHREEQC simulations of
##' CO2(g)
##' @author MDL
##' @export
DS_SI_CO2 <- function(Tk, Pmpa, poy=TRUE, V=NULL,verbose=FALSE)
## Values in are in K and MPa, SI is given in terms of atm!!
{
   Patm <- Pmpa*10.1325
   Pb <- Pmpa*10
   dsphi <- DS_PhiCO2(Tk=Tk, Pmpa=Pmpa)
   Poy <- 1
   if (poy) {
      R <-   8.205746E-2 ## l atm /K/mol
      if (is.null(V))
        Vm <-  32.0E-3      ## l / mol
      else
        Vm <- V
      Poy <- exp(-(Patm-1)*Vm/R/Tk)
      if (verbose)
        cat(paste("Poynting correction factor: ",Poy,"\n"))
   }
   SI <- log10(dsphi*Patm*Poy)
   return(SI)
}

## vDS_SI_CO2 <- Vectorize(DS_SI_CO2,c("Tk", "Pmpa"))


##' Calculate SI of CO2(g) given T, P and con using the
##' RedlichKwongSoave eos and applying a Poynting correction factor ..
##' 
##'
##' @title Calculate the Saturation Index of CO2(g) by
##' Redlich-Kwong-Soave EOS
##' @param Tk Temperature in Kelvin
##' @param Pmpa Pressure in MPa
##' @param poy Logical. If TRUE, a Poynting correction term is applied
##' @param V Numeric value of partial molar volume of aqueous CO2. If
##' left unspecified, 32 l/mol is assumed.
##' @return The corrected SI to include in PHREEQC simulations of
##' CO2(g)
##' @author MDL
##' @export
RKS_SI_CO2 <- function(Tk, Pmpa, poy=TRUE, V=NULL)
{
  R = 8.314467    # J/mol/K
  PcCO2 = 7.38    # MPa
  TcCO2 = 304.15  # K
  
  Tr <- Tk/TcCO2
  a  <- 0.42747*R*R*TcCO2*TcCO2/PcCO2
  b  <- 0.08664*R*TcCO2/PcCO2
  
  alpha <- (1+(0.48508+1.55171*omega-0.17613*omega*omega2)*(1-sqrt(Tr)))^2
  
  P <- R*Tk/(Vm-b) - a*alpha/(Vm*(Vm-b))
  
  dsphi <- DS_PhiCO2(Tk=Tk, Pmpa=Pmpa)
  Poy <- 1
  if (poy) {
    R <-   8.205746E-2 ## l atm /K/mol
    if (is.null(V))
      Vm <-  32.0E-3      ## l / mol
    else
      Vm <- V
    Poy <- exp(-(Patm-1)*Vm/R/Tk)
    cat(paste("Poynting correction factor: ",Poy,"\n"))
  }
  SI <- log10(dsphi*Patm*Poy)
  return(SI)
}


##' Duan & Sun model to calculate vapor pressure of water
##' 
##' @title Vapor pressure of water 
##' @param Tk Temperature in K
##' @return Vapor pressure of water in MPa
##' @author MDL
##' @export
PH2Ompa <- function(Tk)
{
  c1 = -38.640844
  c2 = 5.894842
  c3 = 59.876516
  c4 = 26.654627
  c5 = 10.637097
  PcH2O = 220.85     ## bar
  TcH2O = 647.29     ## K

  T = (Tk - TcH2O) / TcH2O

  return(0.1 * PcH2O * Tk/TcH2O*(1+c1*(-T)^1.9 + c2*T +c3*T^2 + c4*T^3 + c5*T^4))
}

