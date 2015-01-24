### Marco De Lucia, delucia@gfz-potsdam.de, 2009-2015
### Time-stamp: "Last modified 2015-01-24 22:13:26 delucia"


##' Reads a normal PHREEQC input file and prepares it for
##' \code{\link{Rphree}}
##'
##' content for DETAILS: todo!
##' @title Read a normal PHREEQC input file and prepare it for Rphree
##' @param filein PHREEQC input script to read.
##' @param is.db Logical. Set to TRUE if reading a database, for
##' performing additional checks.
##' @param tabs Logical, defaults to TRUE. Should we replace
##' tabulation with spaces?
##' @return A buffer (character vector, one line per element)
##' containing a synthctically valid PHREEQC input
##' @author MDL
##' @examples
##' \dontrun{inp <- RPhreeFile("test.phrq")
##' db <- RPhreeFile("db/phreeqc.dat",is.db=TRUE)}
##' @export
RPhreeFile <- function(filein, is.db=FALSE, tabs=TRUE)
{
    ## open connection, read, close file
    fbuff <- file(filein, "r")
    buff <- readLines(fbuff)
    close.connection(fbuff)
    ## regexps to clean up
    buff <- sub(' +$', '', buff)  ## remove spaces at the end of the lines
    buff <- sub("#.*$","",buff)   ## remove everything after the #'s
    
    if (tabs)
        buff <- gsub('\t', '  ', buff)  ## substitute tabs with spaces
    
    if (!is.db) {
        buff <- sub('^ +', '', buff)  ## remove spaces at the beginning
        
        db <- grep("^DATABASE",buff)
        if (length(db) != 0) {
            buff <- buff[-db]
        }
    }
    multilines <- grep("\\\\$",buff)
    if (length(multilines) != 0) {
        for (i in multilines)
            buff[i] <- paste(sub("\\\\$", "", buff[i]), buff[i+1])
    }
    buff[multilines+1] <- ""
    buff <- buff[buff!=""]        ## remove empty elements
    return(buff)
}

##' Replicates an input buffer containing only one SOLUTION, taking
##' care of SOLUTION/KINETICS/PURE identifiers. Eventually insert a
##' block (e.g: PUNCH) under the first SOLUTION.
##'
##' Analogous to standard \code{rep} function.
##' @title Replicate a SOLUTION 
##' @param sol The initial phreeqc buffer that needs to be replicated.
##' @param n Number of replicates to generate.
##' @param first A new block which needs to be inserted only in the
##' first solution (typically, PUNCH and alike).
##' @return A buffer upon which Rphree can be called.
##' @author MDL
##' @export
RepSol <- function(sol, n, first=NULL)
{
    if (length(dum <- grep("^TITLE|^DATABASE",sol)) > 0 )
        sol <- sol[-dum]
    
    if (is.character(first))
        {
            firstend <- grep("^END",sol)[1]
            newsol <- c(sol[-firstend],first,"END",rep(sol,n-1))
        } else {
            newsol <- rep(sol,n)
        }
    
    if (length(linesol <- grep("^SOLUTION",newsol)) != n)
        {
            stop("too many or no SOLUTION defined\n")
        } else {
            newsol[linesol] <- paste("SOLUTION",1:n)
        }
    
    linepure <- grep("^PURE|^EQUIL",newsol)
    if (length(linepure) > 0 )
        {
            if (length(linepure) != n)
                {
                    stop("too many or no PURE defined\n")
                } else {
                    newsol[linepure] <- paste("PURE",1:n)
                }
        }
    
    linekin <- grep("^KINET",newsol)
    if (length(linekin) > 0)
        {
            if (length(linekin) != n)
                {
                    stop("too many or no KINET defined\n")
                } else {
                    newsol[linekin] <- paste("KINETICS",1:n)
                }
        }
    return(newsol)  
}


##' Transform a (formatted) solution in a new input buffer.
##'
##' Details TODO
##' @title Transform a (formatted) solution in a new input buffer.
##' @param lin Output list from a previous Rphree simulation
##' @param rem Optional string vector containing. If missing, pH, pe,
##' temp and water are reported by default.
##' @param title optional TITLE to appear in the new input buffer
##' @param signif integer specifying how many significant digits will
##' be written in the buffer
##' @return An input buffer ready for further manipulation and/or a
##' new Rphree call.
##' @author MDL
##' @export
RInputFromList <- function(lin, rem, title=NULL, signif=10L)
{
    ## check that totals, phases, pH and pe are in the list
    if (missing(rem))
        rem <- c("pH","pe","temp","water")
    if (!is.null(title))
        title <- paste("TITLE",title)
    ## Desc
    ind_desk <- match(rem,rownames(lin$desc))
    desc <- c(paste(rownames(lin$desc)[ind_desk], signif(lin$desc[ind_desk,1],signif), sep="  " ), "units mol/kgw")
    
    ## totals
    tot <- paste(rownames(lin$tot), signif(lin$tot[,"molal"],signif))

    ## Equilibrium phases
    if ("pphases" %in% names(lin))
        phases <- paste(rownames(lin$pphases), 0.0, signif(lin$pphases[,1]))
    else
        phases <- NULL
  
    ## All SOLUTION
    buff <- c(title, "SOLUTION 1", desc, tot,"PURE 1", phases, "END")
    ##  print(buff)
    return(buff)
}

##' Function to distribute different values of one property
##' (concentration, temperature, mineral phase,\dots) properties in an
##' input buffer.
##'
##' The number of SOLUTIONs in the input buffer must be 1 or equal to
##' length(values). If the input buffer contains only one SOLUTION,
##' and length(values)>1, then the new input gets automatically
##' replicated length(values) times by RepSol. For KINETICS use
##' \code{DistributeKin}.
##' 
##' @title Distribute properties in an input buffer.
##' @param input The initial input buffer.
##' @param prop The property whose values need to be distributed.
##' @param values The numerical value(s) to distribute across the SOLUTIONS.
##' @param newname Optional name of a property which is not initially
##' present in the input buffer.
##' @param first The \code{first} block which will be passed to
##' \code{\link{RepSol}} if needed
##' @param wholeline logical. if TRUE, comments after the properties (i.e. "as HCO3") get also distributed.
##' @return A new input buffer upon which Rphree can be run.
##' @author MDL
##' @export
Distribute <- function(input, prop, values, newname=NULL, first=NULL, wholeline=TRUE)
{
    ## correct quoting of parenthesis
    reg <- gsub("\\(","\\\\(",prop)
    reg <- gsub("\\)","\\\\)",reg)

    ## coerce values to character
    n <- length(val <- as.character(values))
    nsim <- length(proplines <- grep(paste("^", reg," ", sep=""),input))
    
    if (n != nsim && nsim != 1) {
        ##  cat("Lengths of simulation and values do not agree, n=",n,", nsim=",nsim,"!\n")
        stop("Distribute :: Lengths of simulation and values do not agree")
    }
    linesave <- NULL

    if (nsim == 1)
        {
            ## deal with comments after the properties (i.e. "as HCO3")
            if (!wholeline) {
                dum <- unlist(strsplit(gsub(' +',' ',grep(paste("^", reg," ",sep=""),input,value=TRUE))," "))
                if (length(dum)>2)
                    linesave <- paste(dum[3:length(dum)],collapse=" ")
            }
            ## if the simulation is just 1, then repeat it n times (adding "first")
            ## grep(paste("^", reg, " ", sep=""), input, value=TRUE)
            newinp <- RepSol(input, n, first)
            nsim <- length(proplines <- grep(paste("^", reg, " ", sep=""), newinp))
        }
    else
        {
            newinp <- input
        }
    if (is.null(newname))
        newname <- prop
    newinp[proplines] <- paste(newname, val, linesave)
    
    return(newinp)  
}


##' Distribute kinetics parameters in an input buffer containing one
##' single SOLUTION and KINETIC block.
##'
##' 
##' @title DistributeKin
##' @param input The initial buffer. It must contain one single
##' SOLUTION.
##' @param prop The property (in this case, the mineral with KINETICS)
##' @param values The numeric value to distribute. Will be coerced to
##' char.
##' @param ident Identifier of the property (e.g., -m, -m0, -parm)
##' @return A new input buffer upon which Rphree can be run.
##' @author MDL
##' @export
DistributeKin <- function(input, prop, values, ident="-m0")
{
    ## correct quoting of parenthesis
    reg <- gsub("\\(","\\\\(",prop)
    reg <- gsub("\\)","\\\\)",reg)

    ## coerce values to character
    n <- length(val <- as.character(values))
    if (n > 1) stop(":: DistributeKin can only deal with a single input solution")

    newinp <- input
    initline <- grep("^KINETIC", newinp)
    kinblock <- newinp[initline:length(input)]
    ## find the block defining the parameters for the given mineral
    i <- grep(prop,kinblock)
    ## find the line of next occurrence of identifier (m0)
    m0 <- grep(ident, kinblock[c((i+1):(i+5))], fixed=TRUE)[1] + i + initline-1
    newinp[m0] <- paste(ident,values,sep="  ")
    return(newinp)  
}


##' Function that adds a new property (species, pure phase, ...) to an
##' input buffer.
##'
##' .. content for DETAILS ..
##' @title Add a new property (species, pure phase, ...) to an input
##' buffer
##' @param input The input buffer.
##' @param name Name of the new property to add (element, PURE
##' mineral,...)
##' @param values The value(s) of the property.
##' @param cat The category of the property. Must be one of
##' \code{c("tot","pphases","kin")}
##' @param kinpar Kinetics parameters for the KINETICS case.
##' @param first A block to be added in the first SOLUTION, as in
##' \code{RepSol} and \code{Distribute}.
##' @return The new input buffer upon which Rphree can be called.
##' @author MDL
##' @export
AddProp <- function(input, name, values, cat, kinpar=NULL, first=NULL)
{
    cat <- match.arg(cat,c("tot","pphases","kin"))
    
    ## correct quoting of parenthesis
    reg <- gsub("\\(","\\\\(",name)
    reg <- gsub("\\)","\\\\)",reg)
    
    ## coerce values to character and find lengths
    n <- length(val <- as.character(values))
    nsim <- length(grep("^SOLUTION",input))

    if (n != nsim && nsim != 1)
        stop("AddProp :: Lengths of simulation and values to add do not agree!\n")
    
    if (cat == "kin") {
        if (nsim != 1)
            stop(":: AddProp with KINETICS is able to handle only one simulation at a time!")

        markkin <- max(grep("^KINETIC|step",input))

        ## add the lines
        tmp <- c(input[1:markkin],name,paste("-m0", val), paste("-parms", kinpar), input[(markkin+1):length(input)])
        if (n!=1)
            ## use Distribute to create the new input
            newinp <- Distribute(tmp, reg, val, first)
        else
            newinp <- tmp
    } else { ## from here pphases and tot
        ## use "PURE*" or "EQUILIBR*" as delimiter for the pphases block
        markpure <- grep("^PURE|^EQUIL",input)

        ## one line before if is a "tot" component, one line after if is
        ## a "pphases"
        if (cat == "tot") 
            markpure <- markpure - 1

        if (nsim == 1)
            ## if input is just 1 simulation, then first add the line and then
            ## repeat it n times (adding "first")
            {
                ## add the line
                tmp <- c(input[1:markpure],paste(name,val),input[(markpure+1):length(input)])
                if (n!=1)
                    ## use Distribute to create the new input
                    newinp <- Distribute(tmp, reg, val, first)
                else
                    newinp <- tmp
            }
        else
            {
                ## beautiful indexes arithmetic :)
                markpure <- markpure+seq_along(markpure) - ifelse(cat=="tot",1,0)
                tmp <- character(length(input)+n)
                tmp[markpure] <- paste(reg,val)
                rest <- seq_along(tmp)[-markpure]
                tmp[rest] <- input
                ## use Distribute to create the new input
                newinp <- Distribute(tmp, reg, values, first)      
            }
    }
    
    
    return(newinp)  
}

##' Write an input buffer to a file on disk.
##'
##' It really just opens a file and uses "writeLines"" for writing the
##' input to it.
##' @title RPhreeWriteInp
##' @param ofile designated file the input will be written to.
##' @param input the input buffer.
##' @return NULL
##' @author MDL
##' @export
RPhreeWriteInp <- function(ofile,input)
{
    oconn <- file(ofile,"w")
    writeLines(input, con = oconn, sep = "\n")
    close(oconn)
}



##' Reads a phreeqc output file and forms a results list as if the
##' calculation were made with \code{\link{Rphree}}.
##'
##' Currently all blocks are read. For simulations with kinetics the
##' function to use is \code{\link{RReadOutKin}}.
##' @title RReadOut
##' @param out The PHREEQC output file.
##' @return An output list, as if the simulation would have being run
##' through Rphree (the same blocks and the same names are returned)
##' @author MDL
##' @export
##' @examples
##' \dontrun{
##' out <- RReadOut("ex.out")
##' }
RReadOut <- function(out)
{
    if (length(out)==1) {## is it a buffer or a file?
        ## note that empty lines are removed!
        cat(paste("RReadOut:: opening file",out,"\n"))
        tot <- RPhreeFile(out, is.db=FALSE, tabs=TRUE) 
    } else {
        ## remove empty lines and tabs as in RPhreeFile, store
        ## everything in "tot"
        cat("RReadOut:: scanning the buffer... \n")
        tot <- sub(' +$', '', out)  ## remove spaces at the end of the lines
        tot <- sub("#.*$","", tot)  ## remove everything after the #'s
        tot <- gsub('\t', '  ', tot)  ## substitute tabs with spaces
        tot <- tot[tot!=""]        ## remove empty elements
    }

    ## some extra lines in PHREEQC version 3 we need to take care of
    toremove <- grep("^\\*\\*For", tot)
    if (length(toremove)>0) {
        toremove <- sort(c(toremove, toremove+1))
        tot <- tot[-toremove]
    }
   
    solutions <- grep("Beginning of initial solution calculations", tot)
    ntot <- length(solutions)

    ## This is unique (only for calculated solution)
    endsim <- grep('End of simulation.',tot,fixed=TRUE)

    cat(paste("RReadOut::",ntot," simulation in the given output"))

    ## "phase assemblage" is unique, but could not be there
    has_pphases <- TRUE
    endkin  <- grep('-Phase assemblage-',tot,fixed=TRUE)
    if (length(endkin)==0) {
        ## This means we don't have any pure phase in the simulations,
        ## and that the only solutions to read are the initial ones!
        has_pphases <- FALSE
        initials <- seq(1,ntot)
    } else {
        ## this index stores the lines marking initial solutions
        initials <- -(seq(1,ntot)*2 -1)
       
    }
    ## for these, don't consider the "initial solution" instead
    endpure <- grep('-Solution composition-',tot,fixed=TRUE)[initials]
    endcomp <- grep('-Description of solution-',tot,fixed=TRUE)[initials]
    enddesc <- grep('-Distribution of species-',tot,fixed=TRUE)[initials]
    endspec <- grep('-Saturation indices-',tot,fixed=TRUE)[initials]

    ## create the container
    res <- vector(mode="list",length=ntot)

    ## loop over all steps
    for (n in seq_along(solutions)) {
        ## cat(paste(":: Reading solution n. ",n,"\n"))
        
        ## check if there is a fatal error
        error <- grep("^ERROR",tot[ solutions[n] : endsim[n] ])
        if (length(error) > 0) {
            res[[n]] <- "error"
            cat(" ERROR!!\n")
            next
        }

        ## next block in output file is the "optional"
        ## EQUILIBRIUM_PHASES
        if (has_pphases) {
            startpure <- endkin[n]+3
            npure <- endpure[n] - startpure - 1
        
            pure_conn <- textConnection( tot[startpure:(startpure+npure)])
            pure <- read.table( pure_conn, row.names=1, fill=TRUE, as.is=TRUE)
            close(pure_conn)
            indreactants <- which(pure[,2]=="reactant")
            if (length(indreactants) > 0)
                {
                    for (ir in indreactants) {
                        rownames(pure)[ir-1] <- paste(rownames(pure)[ir-1], rownames(pure)[ir],sep="_")
                        pure[(ir-1),c(4,5,6)] <- pure[ir,c(3,4,5)]
                    }
                    pure <- pure[-indreactants,]
                }
            pure <- pure[,c(5,6)]
            
            colnames(pure) <- c("moles","delta")
            ##    if (verbose)
            ##      cat(paste(":: Read pphases block ", n, "of length",npure+1," \n"))
        } else {
            pure <- NA
        }
        
        ## now total solutes  
        startcomp <- endpure[n] + 2
        ncomp <- endcomp[n]-startcomp
        comp_conn <- textConnection(tot[startcomp:(startcomp+ncomp-1)])
        comp <- read.table(comp_conn,row.names=1,fill=TRUE)
        close(comp_conn)
        colnames(comp) <- c("molal","moles")
        ##    if (verbose)
        ##      cat(paste(":: Read total solutes block ", n, "\n"))
        
        ## desc: pH, pe, ecc
        block <- tot[(endcomp[n]+1):(enddesc[n]-1)]

        pH <- as.numeric(unlist(strsplit(grep('^pH',block, value=TRUE)," +"))[3])
        pe <- as.numeric(unlist(strsplit(grep('^pe',block, value=TRUE)," +"))[3])
        temp <- as.numeric(unlist(strsplit(grep('^Temperature ',block,value=TRUE)," = "))[2])
        water <- as.numeric(unlist(strsplit(grep('Mass of water',block,fixed=TRUE,value=TRUE)," = "))[2])
        WaterActivity <- as.numeric(unlist(strsplit(grep('Activity of water',block,fixed=TRUE,value=TRUE)," = "))[2])
        IonStr <- as.numeric(unlist(strsplit(grep('Ionic strength',block,fixed=TRUE,value=TRUE)," = "))[2])
        TotAlk <- as.numeric(unlist(strsplit(grep('Total alkalinity',block,fixed=TRUE,value=TRUE)," = "))[2])
        Tot_C  <- as.numeric(unlist(strsplit(grep('Total carbon',block,fixed=TRUE,value=TRUE)," = "))[2])
        Tot_CO2  <- as.numeric(unlist(strsplit(grep('Total CO2',block,fixed=TRUE,value=TRUE)," = "))[2])
        ElBal <- as.numeric(unlist(strsplit(grep('Electrical balance',block,fixed=TRUE,value=TRUE)," = "))[2])
        Per_Error <- as.numeric(unlist(strsplit(grep('Percent error',block,fixed=TRUE,value=TRUE)," = "))[2])
        Iter <- as.numeric(unlist(strsplit(grep('^Iterations',block,value=TRUE)," = "))[2])
        Tot_H <- as.numeric(unlist(strsplit(grep('Total H',block,fixed=TRUE,value=TRUE)," = "))[2])
        Tot_O <- as.numeric(unlist(strsplit(grep('Total O',block,fixed=TRUE,value=TRUE)," = "))[2])
                block <- tot[(endcomp[n]+1):(enddesc[n]-1)]

        desc <- data.frame(val=c(pH=pH,pe=pe,temp=temp,water=water,WaterActivity=WaterActivity,
                               Tot_Alk=TotAlk, IonicStr=IonStr, ElectrBal=ElBal,
                               Per_Error=Per_Error, iterations=Iter, Tot_CO2=Tot_CO2, Tot_H=Tot_H, Tot_O=Tot_O))
            
        ## next block in output is the speciation
        block <- tot[(enddesc[n] + 4) :( endspec[n] - 1)]
        ## find out the short lines
        shorts <- nchar(block) 
        excl <- which(shorts < mean(shorts))
        block <- block[-excl]

        block_conn <- textConnection(block)
        ## Now we can read.table it
        tmp <- read.table( block_conn, fill=TRUE, as.is=TRUE)[,c(1,2,3)]
        close(block_conn)
        ## remove duplicated
        rnames <- tmp$V1
        dup <- which(!duplicated(rnames))
        species <- tmp[dup,c(2,3)]
        rownames(species) <- rnames[dup]
        colnames(species) <- c("molal","act")

        ## Now saturation indexes
        block <- tot[(endspec[n] + 2) :( endsim[n] - 4)]
        block_conn <- textConnection(block)
        SI <- read.table(block_conn, fill=TRUE, row.names=1, as.is=TRUE)
        close(block_conn)
        names(SI) <- c("SI","IAP","logK","formula")
        
        ## finally pack all together
        res[[n]] <- list(desc=desc,pphases=pure,tot=comp, SI=SI, species=species)
        
    }  ## end of loop over simulations

    cat(" OK\n")
    
    return(res)
    
}

##' Kinetics simulation: reads a phreeqc output file and forms an
##' output list - as if the calculation was made through
##' Rphree.
##'
##' 
##' @title RReadOutKin, import the output file of a kinetic simulation
##' into R
##' @param out The PHREEQC output file.
##' @param strip logical. If TRUE, the "ListInfo" element will be
##' appended to the list.
##' @param verbose logical. If TRUE more output is given to the
##' console (for debugging).
##' @return  An output list as per Rphree call.
##' @author MDL
##' @export
RReadOutKin <- function(out, strip=TRUE, verbose=FALSE)
{
    if (length(out)==1) {## is it a buffer or a file?
        ## note that empty lines are removed!
        cat(paste("RReadOutKin:: opening file",out,"\n"))
        tot <- RPhreeFile(out, is.db=FALSE, tabs=TRUE) 
    } else {
        ## remove empty lines and tabs as in RPhreeFile, store
        ## everything in "tot"
        cat("RReadOutKin:: scanning the buffer... \n")
        tot <- sub(' +$', '', out)  ## remove spaces at the end of the lines
        tot <- sub("#.*$","", tot)  ## remove everything after the #'s
        tot <- gsub('\t', '  ', tot)  ## substitute tabs with spaces
        tot <- tot[tot!=""]        ## remove empty elements
     }

    ## some extra lines in PHREEQC version 3 we need to take care of
    toremove <- grep("^\\*\\*For", tot)
    if (length(toremove)>0) {
        toremove <- sort(c(toremove, toremove+1))
        tot <- tot[-toremove]
        if (verbose) cat("RReadOutKin:: Appears to be PHREEQC version 3\n")
    }
    
    times <- grep("Time step:", tot, fixed=TRUE)
    ntot <- length(times)

    years <- round(as.numeric(sub('\ .*$','',gsub('.*:\ ','',tot[times]))),1)
    if (verbose) {
       cat(out,":\n")
       cat(paste("RReadOutKin:: Found ",ntot,"time steps, the last at time",years[ntot],"\n"))
    }
    endkin  <- grep('-Phase assemblage-',tot,fixed=TRUE)
    endpure <- grep('-Solution composition-',tot,fixed=TRUE)[-1]
    endcomp <- grep('-Description of solution-',tot,fixed=TRUE)[-1]
    enddesc <- grep('-Distribution of species-',tot,fixed=TRUE)[-1]
    endspec <- grep('-Saturation indices-',tot,fixed=TRUE)[-1]

    ## This is unique (only for calculated solution)
    endsim <- grep('Reaction step',tot,fixed=TRUE)[-1]
    endtot <- grep('End of simulation',tot,fixed=TRUE)
    endsim <- c(endsim, endtot-1)

    if (strip) ## do you want "ListInfo"?
        reslen <- ntot
    else
        reslen <- ntot + 1
    
    ## create the container
    res <- vector(mode="list",length=reslen)

    ## loop over all steps
    for (n in seq_along(times)) {
    	if (verbose)
	    cat(paste("RReadOutKin:: Reading", n, "solution, time ",years[n],"\n"))
        ## find the last solution
        start <- times[n]+2

        ## how many kinetics??  
        nkin <- endkin[n] - start

        kconn <- textConnection(tot[(start):(start+nkin-1)])
        kin <- read.table(kconn, row.names=1,fill=TRUE)[,c(2,1)]
        close(kconn)
        
        colnames(kin) <- c("moles","delta")
        if (verbose)
            cat(paste("RReadOutKin:: Read kinetic block ", n, "\n"))
        
        ## next block in output file is EQUILIBRIUM_PHASES
        startpure <- endkin[n]+3
        npure <- endpure[n] - startpure - 1
        pconn <- textConnection(tot[startpure:(startpure+npure)])
        pure <- read.table(pconn, row.names=1,fill=TRUE)[,c(5,6)]
        close(pconn)
        
        colnames(pure) <- c("moles","delta")
        if (verbose)
            cat(paste("RReadOutKin:: Read pphases block ", n, "of length",npure+1," \n"))
        
        ## now solutes  
        startcomp <- endpure[n] + 2
        ncomp <- endcomp[n]-startcomp

        sconn <- textConnection(tot[startcomp:(startcomp+ncomp-1)])
        comp <- read.table(sconn, row.names=1,fill=TRUE)
        close(sconn)
        
        colnames(comp) <- c("molal","moles")
        if (verbose)
            cat(paste("RReadOutKin:: Read total solutes block ", n, "\n"))

        ## desc: pH, pe, ecc
        block <- tot[(endcomp[n]+1):(enddesc[n]-1)]
        pH <- as.numeric(unlist(strsplit(grep('^pH',block, value=TRUE)," +"))[3])
        pe <- as.numeric(unlist(strsplit(grep('^pe',block, value=TRUE)," +"))[3])
        temp <- as.numeric(unlist(strsplit(grep('^Temperature ',block,value=TRUE)," = "))[2])
        water <- as.numeric(unlist(strsplit(grep('Mass of water',block,fixed=TRUE,value=TRUE)," = "))[2])
        WaterActivity <- as.numeric(unlist(strsplit(grep('Activity of water',block,fixed=TRUE,value=TRUE)," = "))[2])
        IonStr <- as.numeric(unlist(strsplit(grep('Ionic strength',block,fixed=TRUE,value=TRUE)," = "))[2])
        TotAlk <- as.numeric(unlist(strsplit(grep('Total alkalinity',block,fixed=TRUE,value=TRUE)," = "))[2])
        Tot_C  <- as.numeric(unlist(strsplit(grep('Total carbon',block,fixed=TRUE,value=TRUE)," = "))[2])
        Tot_CO2  <- as.numeric(unlist(strsplit(grep('Total CO2',block,fixed=TRUE,value=TRUE)," = "))[2])
        ElBal <- as.numeric(unlist(strsplit(grep('Electrical balance',block,fixed=TRUE,value=TRUE)," = "))[2])
        Per_Error <- as.numeric(unlist(strsplit(grep('Percent error',block,fixed=TRUE,value=TRUE)," = "))[2])
        Iter <- as.numeric(unlist(strsplit(grep('^Iterations',block,value=TRUE)," = "))[2])
        Tot_H <- as.numeric(unlist(strsplit(grep('Total H',block,fixed=TRUE,value=TRUE)," = "))[2])
        Tot_O <- as.numeric(unlist(strsplit(grep('Total O',block,fixed=TRUE,value=TRUE)," = "))[2])
                block <- tot[(endcomp[n]+1):(enddesc[n]-1)]

        desc <- data.frame(val=c(pH=pH,pe=pe,temp=temp,water=water,WaterActivity=WaterActivity,
                               Tot_Alk=TotAlk, IonicStr=IonStr, ElectrBal=ElBal,
                               Per_Error=Per_Error, iterations=Iter, Tot_CO2=Tot_CO2, Tot_H=Tot_H, Tot_O=Tot_O))
            

        ## next block in output is the speciation
        block <- tot[(enddesc[n] + 4) :( endspec[n] - 1)]
        ## find out the short lines
        shorts <- nchar(block) 
        excl <- which(shorts < mean(shorts))
        block <- block[-excl]

        block_conn <- textConnection(block)
        ## Now we can read.table it
        tmp <- read.table( block_conn, fill=TRUE, as.is=TRUE)[,c(1,2,3)]
        close(block_conn)
        ## remove duplicated
        rnames <- tmp$V1
        dup <- which(!duplicated(rnames))
        species <- tmp[dup,c(2,3)]
        rownames(species) <- rnames[dup]
        colnames(species) <- c("molal","act")
        
        if (verbose)
            cat(paste("RReadOutKin:: Read speciation ", n, "\n"))

        ## Now saturation indexes
        block <- tot[(endspec[n] + 2) :( endsim[n]-1)]
        block_conn <- textConnection(block)
        SI <- read.table(block_conn, fill=TRUE, row.names=1, as.is=TRUE)
        close(block_conn)
        names(SI) <- c("SI","IAP","logK","formula")
        
        ## finally pack all together
        res[[n]] <- list(desc=desc, pphases=pure, tot=comp, SI=SI, species=species, kin=kin)

        ## end loop over time steps
    }

    if (!strip) {
        res[[reslen]] <- list(n=ntot,format=TRUE,years=years)
        names(res) <- c(paste("z",1:ntot,sep=""),"ListInfo")
    } else {
        names(res) <- c(paste("z",1:ntot,sep=""))
        attr(res,"time") <- years
    }
    return(res)
}



##' Erases the SOLUTION number n (label, not position) from an input
##' buffer containing many.
##'
##' @title Suppress a simulation from an input buffer
##' @param biginp The input buffer
##' @param n The number of the simulation to erase
##' @return The new input buffer 
##' @author MDL
##' @export
SuppressSim <- function(biginp, n=1L)
{
    startsols <- grep("^SOLUTION",biginp)
    endsols   <- grep("^END",biginp)
    newinp <- biginp[ -c(startsols[n]:endsols[n]) ]
    return(newinp)
}

##' Utility function to check if the selected output vector,
##' specifying the blocks included in the output of Rphree, is well
##' formed. The selection vector has the form: \code{ c(kin=FALSE,
##' tot=TRUE, desc=TRUE, species = TRUE, pphases = TRUE, SI=TRUE,
##' punch =FALSE)}, where the names of each vector component refer to
##' corresponding blocks in PHREEQC output.
##'
##' This utility function is quite needed since \bold{the precise
##' order of this vector is in c-level hard-coded!}
##' @title Generate and/or check the selection of output from Rphree
##' @param sel A named logical vector of length 7. If missing, the
##' function returns a valid selection vector with all components set
##' to FALSE.
##' @return A well formed selection vector 
##' @author MDL
##' @export
##' @examples
##' \dontrun{
##' ## A valid example
##' mysel <- c(kin=FALSE, tot=TRUE, desc=TRUE, species = TRUE, pphases = TRUE, SI=TRUE, punch =FALSE)
##'
##' ## Let RPhreeCheckSel generate a selection:
##' sel <- RPhreeCheckSel()
##' sel <- !RPhreeCheckSel()
##' }
RPhreeCheckSel <- function(sel)
{
    ## Our prototype. NB: this precise order is in c-level hard-coded!!
    selok <- c(kin=FALSE, tot=FALSE, desc=FALSE, species=FALSE, pphases=FALSE, SI=FALSE, punch=FALSE)
    if (missing(sel))
        return(selok)
    
    if (length(sel) != 7  && length(names(sel)) != length(sel))
        stop("\"selection\" is not appropriate")

    ## case 1 : unnamed vector, assuming ordered input
    if (length(sel) == 7 && length(names(sel)) == 0) {
        cat("Assuming ordered selection c(kin, tot, desc, species, phases, SI, punch\n")
        names(sel) <- names(selok)
        return(sel)
    }
    
    ## case 2, named vector
    indexes <- match(names(sel), names(selok))
    selok[indexes] <- sel
    return(selok)
}

##' This functions extracts informations from calculated Rphree
##' formatted solutions.
##'
##' @title Extract specific values from an Rphree output
##' @param lin The list containg the Rphree solution(s)
##' @param cat A string indicating the category (or output block) to
##' search in, must be one of: "tot, desc, pphases, master, species,
##' kin, SI"
##' @param prop The name of the inquired property (element, species,
##' mineral phase,\dots) whose value(s)
##' @param force Logical. If TRUE (default if left unspecified), a
##' valid numeric value (0) is returned even if the inquired property
##' is not found in the solution
##' @param flex Logical. If TRUE, do not rely on "ListInfo" in the
##' formatted solution list and perform heuristics to circumvent this
##' absence
##' @return A numeric vector containing the inquired properties
##' @author MDL
##' @examples
##' \dontrun{
##'   RPinfo(SOL,"tot","Fe")
##'   RPinfo(SOL,"pphases","Anhydrite")
##' }
##' @export
RPinfo <- function(lin, cat=c("tot","desc","pphases","master","species","kin","SI"),
                   prop, force=TRUE, flex=FALSE)
{
    cat <- match.arg(cat)
    
    if ("ListInfo" %in% names(lin))
        nsim <- lin$ListInfo$n
    else {
        if (!flex)
            stop("RPinfo:: no ListInfo! Perhaps not a valid list produced by Rphree?
  Specify flex=TRUE if you still want to try")
        else
            nsim <- length(lin)
    }
    
    if ( nsim > 1 )
        ## we have a list of solutions, check if the name exists
        {
            if ( cat %in% names(lin[[1]])) {
                ## everything seems fine, let's (s)apply
                lout <- sapply(lin[1:nsim], RPhreeExt, cat=cat, prop=prop)
            } else {
                if (force)
                    {
                        lout <- rep(0,length(lin))
                    } else {
                        stop("RPinfo:: Sure that the right output was selected in the simulation?")
                    }
            }
        } else {
            ## just 1 solution
            if ( cat %in% names(lin)) {
                lout <- RPhreeExt(lin, cat, prop)
            } else {
                if (force)
                    {
                        lout <- 0
                    } else {
                        stop("RPinfo:: Sure that the right output was selected in the simulation?")
                    }
            } 
        }
    return(lout)
}

##' Workhorse function for extraction of results from a solution,
##' primarily intended to be used by \code{\link{RPinfo}}, where it is
##' called inside a \code{sapply} statement.
##'
##' @title Workhorse function for extraction of informations from a
##' solution
##' @param sol The solution outputted by Rphree
##' @param cat String: the category or output block where to look for
##' the property \code{prop}
##' @param prop The name of the property to look for
##' @return Numeric of length 1, the inquired value for the property
##' @author MDL
##' @export
RPhreeExt <- function(sol, cat, prop)
{
    if (is.data.frame(sol[[cat]]))
        return(sol[[cat]][prop,1])
    else
        return(0)
}

##' Workhorse function for extraction of results from a solution,
##' specifically equilibrium phases. 
##'
##' @title Workhorse function for extraction of 
##' @param lin The solution outputted by Rphree
##' @param which String containing the name of the specific pphase to
##' look for. If omitted, all pphases are returned.
##' @param delta Logical. If TRUE, delta min are returned, if FALSE,
##' total amount
##' @return A data.frame containing the results of equilibrium phases
##' @author MDL
##' @export
RGetPhases <- function(lin, which=NULL, delta=TRUE)
{
    i <- 1
    if (delta) i <- 2
    
    tab <- lin$pphases
    if (!is.character(which)) {
        return(tab[,i])
    } else {
        inde <- match(which,rownames(tab))
        return(tab[inde,i])
    }
}


##' Workhorse function for extraction of results from a solution,
##' specifically equilibrium phases. Variant.
##'
##' @title extract the phases from a Rphree solution
##' @param lin The solution outputted by Rphree
##' @return A data.frame containing the results of equilibrium phases
##' @author MDL
##' @export
RReadPhases <- function(lin)
{
    ns <- length( phas <- unique(unlist(lapply(lin,function (x) return(rownames(x$pphases))))) )
    if (ns==0)
        return(NULL)
    
    mat <- matrix(0,ncol=ns,nrow=n)
    for (i in seq_along(phas))
        {
            mat[,i] <- RPinfo(lin,"pphases",phas[i])
        }
    colnames(mat) <- phas
    mat[is.na(mat)] <- 0
    return(mat)
}
