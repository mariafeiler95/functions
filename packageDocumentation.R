# Get R package documentation

# Given one or more package names and a date, extract the package version and 
# authors.

# 12/06/2022
# M.E.F. 
# mariaefeiler@gmail.com

# @param p      Character string or a vector of character strings denoting 
#               package name(s).
# @param day    Character string, the date of package use to inform package 
#               version. 
#               Format: YYYY-MM-DD
#               Default: Current system date as given by Sys.Date()
# @param first  Character string denoting if given ("given") or family 
#               ("family") should come first. 
#               Default: "given"
# @param sys    Location to look for package version. When "local", local 
#               library is referenced. When "CRAN", CRAN's website is 
#               referenced. As a result, unless "CRAN" is specified, the 
#               package version currently installed in the local system will be
#               returned.
#               Default = "local"
# @param no     Numeric, number of package authors to be returned. If NULL, all
#               author names are returned. 
#               Default: NULL
# @param df     Logical, whether to return a list (FALSE) or a dataframe (TRUE).
#               Default: FALSE

# @return       If df = FALSE, a list of length(p). Each list item contains the 
#               following: 
#               $Version        Package version on given date, system date, or 
#                               as currently installed on system.
#               $Authors        Vector of author names. 
#                               If short = TRUE, a maximum of three authors will
#                               be returned. 
#                               
#               If df = TRUE, a dataframe with the package names, versions, and 
#               authors as column values. Authors will be concatenated into one 
#               character string. 

# @examples
# One package, local system version, full author list, given name first.
# packageDocumentation("geomorph")

# One package, current CRAN version, shortened author list, family name first.
# packageDocumentation("geomorph", sys = "CRAN", no = 3, first = "family")

# One package, CRAN version on given date, as dataframe.
# packageDocumentation("geomorph", day = "2022-09-19", sys = "CRAN", df = TRUE)

# Multiple packages.
# p <- c("geomorph", "dplyr")
# packageDocumentation(p)

packageDocumentation <- function(p, day = Sys.Date(), first = "given",
                                 sys = "local", no = NULL, df = FALSE){
   # Require libraries
        require(rvest)
    
   # GET PACKAGE VERSIONS (params: p, day, sys)     
        # List to capture package versions
        v <- vector(mode = "list", length = length(p))
        names(v) <- p
        
        # Get package versions on given or system date...
        if(sys == "CRAN"){
                for(i in p){
                        x <- paste0("https://cran.microsoft.com/snapshot/", 
                                    as.character(day),
                                    "/web/packages/", 
                                    i, 
                                    "/index.html"
                                    )
                        x <- read_html(x)
                        x <- html_nodes(x, xpath = "//td")
                        v[[i]] <- gsub("<td>", "", gsub("</td>", "", x[2]))
                }
        }
        # ...or from local library   
        if(sys == "local"){
                for(i in p){v[[i]] <- as.character(packageVersion(i))}
        }

    # GET AUTHORS (params: p, first, no)    
        # Get and clean authors
        u <- lapply(p, packageDescription, fields = "Authors@R")
        
        # List of author names as class "person"
        a <- list()
        for(i in u){
                x <- textConnection(i)
                on.exit(close(x))
                a[[i]] <- dget(x)
        }
        names(a) <- p
        
        # Pull family and given names from hidden "person" objects within object a        
        giv <- list()
        fam <- list()
        for(i in p){
                giv[[i]] <- a[[i]]$given
                fam[[i]] <- a[[i]]$family
        }
    
        # Clean any names that have more than one element    
        for(i in p){
                for(j in 1:length(giv[[i]])){
                        giv[[i]][j] <- ifelse(length(giv[[i]][[j]]) == 1,
                                              as.character(giv[[i]][[j]]),
                                              paste(giv[[i]][[j]], 
                                                    collapse = " ")
                                              )
                }
        }
        for(i in p){
                for(j in 1:length(fam[[i]])){
                        fam[[i]][j] <- ifelse(length(fam[[i]][[j]]) == 1,
                                              as.character(fam[[i]][[j]]),
                                              paste(fam[[i]][[j]], 
                                                    collapse = " ")
                                              )
                }
        }
    
        # Save all author names...         
        if(is.null(no)){
                for(i in p){
                        a[[i]] <- data.frame("Given" = unlist(giv[[i]]),
                                             "Family" = unlist(fam[[i]]))
                }
        }
        # ... or only as many as defined in param no.    
        if(is.integer(no)){
                for(i in p){
                        a[[i]] <- data.frame("Given" = unlist(head(giv[[i]], no)),
                                             "Family" = unlist(head(fam[[i]], no)))
                }
        }
        
        # Create author list with family name first...
        if(first == "family"){
                for(i in p){
                        a[[i]] <- unlist(apply(a[[i]][,1:2],1, 
                                               function(x) paste(x[x!=""], 
                                                                 collapse = " "))
                                         )
                }
        }
        # ... or given name first.     
        if(first == "given"){
                for(i in p){
                        a[[i]] <- unlist(apply(a[[i]][,1:2], 
                                               1, 
                                               function(x) paste(x[x!=""], 
                                                                 collapse = ", "))
                                         )
                }
        }

   # PRINT RESULTS (params: p, df)
        # Create results list     
        result <- vector(mode = "list", length = length(p))
        names(result) <- p
        for(i in p){
                result[[i]] <- list("Version" = as.character(v[[i]]),  # Import versions.
                                    "Authors" = a[[i]])                # Import full author list.
        }

        # Return list...
        if(df == FALSE){return(result)}
        
        # ... or dataframe
        if(df == TRUE){
                for(i in p){a[[i]] <- paste(result[[i]]$Authors, collapse = ", ")}
                result <- data.frame(version = unlist(v),
                                     authors = unlist(a)
                                     ) 
                return(result)
        }

        # Close all connections
        closeAllConnections()
}
