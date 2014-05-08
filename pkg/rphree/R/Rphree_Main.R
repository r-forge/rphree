### Rphree library: interface R/PHREEQC (Parkhurst & Appelo)
### 
### Marco De Lucia, delucia@gfz-potsdam.de, 2009-2014
### Time-stamp: "Last modified 2014-01-29 16:57:33 delucia"

##' This function performs the actual PHREEQC calculations. 
##'
##' TODO
##' @title Run PHREEQC
##' @param input The input buffer
##' @param sel Selection of output blocks to be included in the
##' formatted list. If missing, everything is included in the output
##' @param write Logical. If TRUE, PHREEQC will write its usual output
##' on file
##' @param out If \code{write=TRUE}, the name of the output file to write to 
##' @param db The database
##' @param n Optional integer parameter containing the number of
##' simulations included in the input buffer
##' @param punch number of parameters included in the PUNCH block
##' @param format Logical. If TRUE, the output blocks are nicely formatted
##' @param verbose Logical. If TRUE, some additional checkpoint
##' message is outputted to the console
##' @return A list (or a list of lists) containing the output(s) of
##' the calculation
##' @author MDL
##' @useDynLib Rphree
##' @export
Rphree <- function(input = stop("No input specified.\n"), sel,
                   write = FALSE, out = "Rphree", db = "phreeqc.dat",
                   n, punch = 0, format = TRUE, verbose = FALSE)
{  
    if (length(db)==1) {
        dbfile <- paste("db/",db,sep="")
        if (!file.exists(dbfile))
            stop(paste("Cannot find the database \"",dbfile,"\""))
        db_inp<- RPhreeFile(dbfile)
        lendb <- length(db_inp)
    } else {
        db_inp<- db
        lendb <- length(db_inp)
    }

  ## check the number of simulations if n is missing
    if (missing(n))
        {
            n <- length(grep("^SOLUTION",input))
            ##  ntime <- system.time(n <- length(grep("^SOLUTION",input)))
            ##  cat(paste(" -> ",ntime[3],"seconds lost searching for",n,"simulations\n"))
        }
    
    files <- c("Rphree", "RBuffInput", paste(out,".Rout",sep=""),
               "db_inp", ifelse(write,"T","F"), ifelse(verbose,"T","F") )
    
    ## check and format the output selection
    if (missing(sel))
        ## everything is selected
        selok <- !RPhreeCheckSel()
    else
        ## actual check
        selok <- RPhreeCheckSel(sel)
    
    ## additional check on selection based on input
    if (length(grep("KINETICS",input) ) == 0 ) 
        selok["kin"] <- FALSE
    
    if (length(grep("PURE|EQUILIBRIUM_PHASES|EQUILIBRIUM",input))==0)
        selok["pphases"] <- FALSE
    
    ## Form ListInfo
    ListInfo <- list(n=n, format=format)
    
    ## check on USER_PUNCH
    if (selok["punch"])
        {
            if (length(grep("PUNCH",input))==0)
                selok["punch"] <- FALSE
            else
                {
                    punch_lines <- grep('SELECTED|PUNCH',input)
                    rem_punch <- input[min(punch_lines):max(punch_lines)]
                    head <- grep("head", rem_punch, value=TRUE)
                    head <- gsub("^.*head", "", head)
                    head <- gsub("^\ +","", head)
                    head <- gsub("\ +"," ", head)
                    names <- unlist(strsplit(head," ",fixed=TRUE))
                    if (length(names)!=punch)
                        stop("Wrong USER_PUNCH specification!\n")
                    ListInfo$punch <- list(npunch=punch, names=names, lines=rem_punch)
                }
        }
    
    ## call the RPhreemain
    out_list <- .Call("RPhreemain",
                      args=as.character(files), 
                      inp_length=as.integer(c(length(input), n, punch, lendb)),
                      buff = as.character(input),
                      db_buff = as.character(db_inp),
                      sel = as.integer(c(sum(selok),selok)) )
    
    if (format)
        {
            if (n==1)
                newlist <- RPhreeFormatOut(out_list, sel=selok)
            else
                {
                    if (length(out_list)!=n) 
                        stop("Rphree:: length(list)!=n\n")
                    newlist <- lapply(out_list, RPhreeFormatOut, sel=selok)
                }
        }
    else
        newlist <- out_list
    
    newlist$ListInfo <- ListInfo
    return(newlist)  
}



##' This is the workhorse function used for formatting the raw output
##' returned by PHREEQC at c-level. Not intended for direct use.
##'
##' Bla bla bla
##' @title Format the output returned by PHREEQC
##' @param out_list The list formed by the \code{.Call} to PQCLIB
##' @param sel The selection of output blocks
##' @return A list (or a list of lists) in which all blocks are
##' formatted properly in data.frames or named matrices.
##' @author MDL
##' @export

RPhreeFormatOut <- function(out_list, sel)
{
    ## First of all, check the presence of errors
    if ( length(out_list)==2 && "error" %in% names(out_list)) {
        cat(paste("Error",out_list$error," in ",out_list$token,"\n"))
        return(out_list)
    }
    
    routputdim <- sum(sel)
    
    ## "species" gives elements "master" and "species"
    if (sel["species"]) routputdim = routputdim +1
    
    new_list=vector("list")
    
    ## 1: Kinetics (remove voids, format as data.frame)
    if (sel["kin"]) {
        ##     ind_kin <- ( out_list$kin$names != "" )
        ##     if (TRUE %in% ind_kin) 
        ##       {
        tmp1 <- data.frame (moles  = out_list$kin$moles,
                            delta  = out_list$kin$delta)
        rownames(tmp1)  = out_list$kin$name 
        new_list$kin <- tmp1
    }
    
    ## 2: total composition
    if (sel["tot"]) {
        if (length(out_list$elements$elem) > 0) {
            tmp2 <- data.frame (molal = out_list$elements$molal,
                                ann   = out_list$elements$ann)
            rownames(tmp2)  = out_list$elements$elem
            new_list$tot <- tmp2
        }
        else
            new_list$tot <- FALSE
    }
    
    ## 3: desc (no voids, just format as data.frame)
    if (sel["desc"]) {
        descdim <- out_list$desc$dim
        tmp3 <- data.frame (val   = out_list$desc$val[1:descdim],
                            ann   = out_list$desc$ann[1:descdim])
        rownames(tmp3)= out_list$desc$name[1:descdim]
        new_list$desc <- tmp3
    }
    
    ## 4: species (remove duplicated, sort)
    if (sel["species"]) {
        ind41 <- duplicated(out_list$species$name)
        ind41 <- as.logical(1-ind41)
        tmp41 <- data.frame(molal = out_list$species$molal[ind41],
                            act = out_list$species$act[ind41])
        rownames(tmp41) <- out_list$species$name[ind41]
        
        ind42 <- ( out_list$species$Mname != "" )
        tmp42 <- data.frame( molal = out_list$species$Mmolal[ind42])
        rownames(tmp42) <- out_list$species$Mname[ind42]
        
        new_list$species <- tmp41
        new_list$master <- tmp42
    }
    
    ## 5: pure phases (empty removed at c-level)
    if (sel["pphases"]) {
        if (length(out_list$pphases$moles) > 0) {
            tmp5 <- data.frame(moles = out_list$pphases$moles, delta = out_list$pphases$delta)
            rownames(tmp5)  = out_list$pphases$names
            new_list$pphases <- tmp5
        }
        else
            new_list$pphases <- FALSE
    }
    
    ## 6: Saturation Indices of phases (empty values removed, format
    ## as data.frame)
    if (sel["SI"]) {
        if (length(out_list$SI$names) > 0) {
            tmp6 <- data.frame (SI     = out_list$SI$SI,
                                IAP    = out_list$SI$IAP,
                                logK   = out_list$SI$logK,
                                formula= out_list$SI$formula)
            rownames(tmp6)  = out_list$SI$names
            new_list$SI <- tmp6
        }
        else
            new_list$SI <- FALSE
    }
    
    ## 7: punch: don't format
    if (sel["punch"]) {
        if (length(out_list$punch) > 0) {
            new_list$punch <- out_list$punch
        }
        ##     else
        ##       new_list$punch <- FALSE
    }
    
    return(new_list)
}



