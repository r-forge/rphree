## Rphree, functions to visualize Rphree simulations

### Marco De Lucia, delucia@gfz-potsdam.de, 2009-2014
### Time-stamp: "Last modified 2014-02-13 11:51:22 delucia"


##' Utility function to visualize and compare the concentrations of
##' total soluted elements using barplot.
##'
##' @title Visualize barplot of total solute concentrations of
##' solution(s)
##' @param sol A list of solution (output of Rphree). Must be in the
##' form \code{list(sol1,sol2)}
##' @param names Char vector containing the names of the solutions to
##' visualize e.g., in the legend
##' @param vlim inferior bound for visualizations (as in ylim)
##' @param sel Char vector containing the names of the elements to
##' plot
##' @param mult named numeric vector containing multiplicative factors
##' for the elements who need to be magnified in the plot
##' @param leg Logical. If TRUE, a legend is plotted
##' @param flex Logical. If TRUE, ListInfo is not required and a
##' specific heuristic ist applied instead
##' @param ... Further parameters passed to
##' \code{\link{graphics::barplot}}
##' @return invisible return of \code{\link{graphics::barplot}}
##' @author MDL
##' @export
VizSol <- function(sol, names, vlim, sel, mult, leg=TRUE, flex=TRUE, ...)
{
    ## sol must be of the form list(sol)
    n <- length(sol)
    if (missing(names))
        names <- as.character(seq(n))
    tot <- lapply(sol,function(x) rownames(x$tot))
    allnames <- unique(sort(unlist(tot)))
    matches <- lapply(sol, function(x,nam) match(rownames(x$tot),nam),allnames)
    
    ## extract the total concentrations
    tab <- lapply(sol,function(x) x$tot[,1])
  
    ## create a matrix for the concentratons
    res <- matrix(NA,length(allnames),n)
    for (i in seq(n)) { ## distribute
        res[matches[[i]],i] <- tab[[i]]
    }

    ## extract pH
    pH <- RPinfo(sol,cat="desc",prop="pH",flex=flex)
    ## add it to tab 
    res <- rbind(pH,res)
    ## take care of names
    rownames(res) <- c("pH",allnames)
    colnames(res) <- names

    if (!missing(sel)) {
      newallnames <- intersect(sel,allnames)
      res <- res[newallnames,]
    }

    if (!missing(mult)) {
      namesmult <- intersect(names(mult),newallnames)
      vecmult <- mult[namesmult]
      res[namesmult,] <- res[namesmult,]*vecmult
    }
        
    ## apply vlim (not visualize too small concentrations)
    if (!missing(vlim))
      res[res < vlim] <- NA
    out <- barplot(t(res),beside=TRUE, legend.text=leg, ylab="mol/kgw", ...)
    invisible(out)
}

##' Graphically compares - through barplots - the activity
##' coefficients for given species of a set of Rphree solutions. The input
##'
##' @title Plots activities or activity coefficients of a set of
##' solutions
##' @param sol A list containing the output solutions. Must be of the
##' form \code{list(sol1,sol2)}
##' @param spec Char vector containing the species to be visualized
##' @param names Char vector of the names of the solutions
##' @param coef Logical. If TRUE, activity coefficients are
##' visualized, if FALSE activities.
##' @param cutoff Numeric. Lower bound for visualizing an activity
##' @param ... Further optional parameters passed to
##' \code{\link{graphics::barplot}}
##' @return Optional (invisible). A data.frame containing the
##' activities/act.coefficients (rows) for the given solutions
##' (columns)
##' @author MDL
##' @export
VizAct <- function(sol, spec, names, coef=TRUE, cutoff=1e-12, ...)
{
  if (missing(spec))
    stop(":: VizAct: explicit selection of species ('spec') is mandatory\n")
  ## sol must be of the form list(sol)
  n <- length(sol)
  if (missing(names))
    names <- as.character(seq(n))
  
  acts <- sapply(sol,RGetAct,species=spec,coef=coef)
  acts[ acts<cutoff ] <- NA

  colnames(acts) <- names
  rownames(acts) <- spec
  barplot(t(acts),beside=TRUE, ...)
  invisible(acts)
}

##' Graphically compares - through barplots - the mineral deltas of a
##' set of Rphree solutions
##'
##' @title Plots mineral deltas of a set of solutions
##' @param sol A list containing the output solutions. Must be of the
##' form \code{list(sol1,sol2)}
##' @param names Char vector of the names of the solutions
##' @param ... Further optional parameters passed to
##' \code{\link{graphics::barplot}}
##' @return Optional (invisible). A data.frame containing the
##' activities/act.coefficients (rows) for the given solutions
##' (columns)
##' @author MDL
##' @export
VizMinDelta <- function(sol, phases=NULL, names, ...)
{
    ## sol must be of the form list(sol)
    n <- length(sol)
    if (missing(names))
        names <- as.character(seq(n))

    deltas <- sapply(sol, RGetPhases, which=phases)
    if (is.null(phases))
      rownames(deltas) <- rownames(sol[[1]]$pphases)
    else
      rownames(deltas) <- phases
    
    colnames(deltas) <- names
    barplot(t(deltas),beside=TRUE, legend.text=TRUE, ...)
    invisible(deltas)
}

##' Plots mineral deltas of an Rphree solution
##'
##' @title Barplot of mineral deltas of an Rphree solution
##' @param sol The Rphree solution to visualize
##' @param lim Bound for the mindeltas to plot (-lim < mindelta < lim)
##' @param names Logical (if TRUE, all phases) or character vector of
##' the phases to plot
##' @param line Logical. If TRUE, a vertical line discriminating
##' dissolved from precipitated mineral phases gets plotted
##' @return the output of \code{\link{graphics::barplot}}
##' @author MDL
##' @export
PlotMinDelta <- function(sol,lim=1e-4,names=TRUE,line=TRUE)
{
    val <- sol$pphases[,2]
    all <- rownames(sol$pphases)
    ind <- which(all=='CO2(g)')
    val <- val[-ind]
    all <- all[-ind]
    positive <- which(val>lim)
    negative <- which(val< -lim)
    a <- order(val[positive], decreasing=TRUE)
    b <- order(val[negative], decreasing=TRUE)

    ## tot <- c(log10(val[positive][a]),-log10(-(val[negative][b])))
    tot <- c(val[positive][a],val[negative][b])
    nam <- c(all[positive][a],all[negative][b])

    if (is.logical(names)) {
        if (names) {
            names.arg <- nam
        } else names.arg <- NULL
    } else {
        names.arg <- names
    }

    tt <- barplot(tot, names.arg=names.arg, col=c(rep("light blue",length(positive)),rep("red",length(negative))),
                  legend.text=FALSE,  ylab="moles/ liter rock")
    if (line) {
        abline(v=tt[length(a)]+(tt[2]-tt[1])/2)
    }
    invisible(tt)
}
