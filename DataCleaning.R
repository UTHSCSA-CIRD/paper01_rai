#' ---
#' title: "RAI Data Quality Checks"
#' author: "Wilson, Bokov, Shireman"
#' date: "02/05/2018"
#' ---
#'
#+ echo=FALSE, message=FALSE
knitr::opts_chunk$set(echo=F,warning = F,cache=T,message=F);
#+ cache=FALSE
source('global.R');
#' Report date: `r date()`.
#'
#' Revision: `r gitstamp()`.
#'
#' Data file: `r inputdata`.
#' 
#' Cost data file: `r inputdata_cost`.
#' 
#' 
#' 
#+ cache=TRUE
source('run.R');

#' 
#' 
#' Here is, for every subset currently of interest, a table showing which columns
#' have missing data, but not all of it missing, along with their original names
# First we filter our subsets down down to their respective years of interest
dq_missing <- lapply(dat1subs, subset,eval(subs_criteria$y2016)) %>%
  # then for each of the filtered subsets
  lapply(function(xx) {
    # we toss out any that have no visits in the year of interest (there's one)
    # filter might work here too, doesn't really matter which way to do it, as
    # long as the error gets trapped
    if(nrow(xx)==0) return(NULL);
    # now, for each of these datasets, (xx) iterate over each row (yy).
    # oo is my shorthand for "variable that will eventually be the output" just
    # like xx, yy, aa, bb, ii, are often shorthand for cycling input variables
    oo <- apply(xx,1
                # and return TRUE if it's either missing or equal in value to
                # the string 'unknown' when coerced to character and converted
                # to lowercase
                ,function(yy) ifelse(is.na(yy),T
                                     ,tolower(as.character(yy))=='unknown')) %>%
      # rowSums here is still part of the above line!! The output from apply is
      # a matrix of T/F values, so the rowSums give us the total number of missing
      # for each variable (the apply function transposes the output, so that's 
      # why the variables are now represented by columns
      rowSums;
    # ... but the output from rowSums is a numeric vector whose names are the 
    # original column names of xx. So we store those names in the 'cnames variable'
    # ...after dropping all the variables that are never missing (0) or always
    # missing (nrow(xx))
    cnames <- names(oo<-oo[oo>0&oo<nrow(xx)]);
    # now we build the output-- we could pipe this through setNames to give it
    # pretty column names, but since there are only two column names, we can do
    # it within data.frame by escaping the names with backquotes. Note that at 
    # the same time we also call mapnames() on our output object-- when creating
    # a data.frame, those names will become user-friendly rownames 
    oo <- data.frame(`N Missing/Unknown`=mapnames(oo,dat1namelookup)
                     ,`Column Names`=cnames
                     # the below argument prevents R from replacing spaces with
                     # dots
                     ,check.names=F);
    });

#' ## Missing data
#' 
#+ results='asis'
knitr::kable(dq_missing$all_colon_all);

