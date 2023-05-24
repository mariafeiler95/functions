# mrkJsonToArray()
# Read .mrk.json landmark files into array

# Given one or more file names, read the 2D or 3D landmark data into an array
# parsable by geomorph. Coerces all landmarks into the same order. User recieves
# a message if any landmarks are excluded from specimens (because they are not
# part of the landmark set provided by the user or the first file) or if 
# specimens have missing landmark values. 

# 12/07/2022
# M.E.F. 
# mariaefeiler@gmail.com

# R4.1.3, RStudio 2022.7.0.548
# Windows 10 x64

# SOURCES:
        # Original inspiration for reading .mrk.json files
        # read.markups.json() by Murat Maga
        # https://github.com/muratmaga/SlicerMorph_Rexamples/blob/main/read.markups.json.R

# @param files  Character string or a vector of character strings denoting
#               names. Must be .mrk.json files. 
#               Default: NULL
# @param path   Character string denoting the path to the files.
#               Default: NULL
# @param lmset  Character vector denoting names for landmarks.
#               Default: NULL
# @param spec   Character vector denoting names for the specimens. 
#               Default: NULL
# @param k      Numeric denoting number od landmark dimensions. 
#               Default: NULL             

# @return       An array of [l,k,n], where l is the number of distinct 
#               landmarks, k is the number of landmark dimensions, and n is the 
#               number of specimens. If the parameters lmset and spec are 
#               provided, they will be applied to the dimnames of the l and n 
#               dimensions. If they are not provided, landmark names will be 
#               taken from the labels of the first file and specimens will be 
#               numbered. k is always determined by the first file

# @examples
# Single file, no other params
# mrkJsonToArray(files = "test.mrk.json")
 
# Multiple files, defined path one level above working directory
# f <- c(files = "test.mrk.json", "test1.mrk.json")
# 
# Both will work! Function will provide the / if necessary.
# mrkJsonToArray(files = f, path = "../")
# mrkJsonToArray(files = f, path = "..")

# Single file, define specimen name and landmark set
# lms <- c("a", "b", "c")
# mrkJsonToArray(files = "test.mrk.json", spec = "0001", lmset = lms)

# Multiple files, define specimen names, landmark set, and k.
# ids <- c("0001", "0002")
# mrkJsonToArray(files = f, spec = ids, lmset = lms, k = 2)

mrkJsonToArray <- function(files, path = NULL, lmset = NULL, spec = NULL, k = NULL){
    # Require jsonlite and geomorph    
        require(jsonlite)
    # Check that path works if param provided, otherwise fill with value from 
    # getwd()
        if(is.character(path)){
                if(dir.exists(path) == "FALSE"){
                        stop("Provided file directory does not exist.")
                }
                else{
                        # Ensure that there is a "/" at the end of the path param
                        path <- ifelse(endsWith(x = path, suffix = "/") == FALSE, 
                                          paste0(path, "/"),
                                          path
                                          )
                        fileLoc <- paste0(path,files)
                }
        }
        if(is.null(path)){
                fileLoc <- files
        }

    # Check that all files exist
        if(sum(!file.exists(fileLoc)) > 0){
                stop(paste0("The following file(s) do not exist: ",
                            paste(files[which(file.exists(fileLoc) == FALSE)],
                                  collapse = ", ")
                            )
                )
        }
        
    # Check that there are the correct number of specimen IDs (if param spec 
    # provided), otherwise assign integers from one to length(fileLoc) to spec.    
        if(is.null(spec)){
                spec <- 1:length(fileLoc)
        }
        if(length(fileLoc) != length(spec)){
                stop("Number of files and number of specimen IDs are not equal.")
        }
        
    # Define landmark names (if null) and dimensions (k)
        if(is.null(lmset) | is.null(k)){
                dat <- fromJSON(fileLoc[[1]], flatten = TRUE)$markups
        }
        
        if(is.null(lmset)){lmset <- dat$controlPoints[[1]]$label}
        if(is.null(k)){k <- length(dat$controlPoints[[1]]$position[[1]])}
        if(k == 0 | k == 1){stop("Not enough dimensions. Must be 2 or 3.")}
        if(k > 3){stop("Too many dimensions. Must be 2 or 3.")}

    # Two dimensional landmarks
        if(k == 2){
                ar <- array(data = 0,
                            dim = c(length(lmset), k, length(spec)),
                            dimnames = list(lmset, c("X", "Y"), spec)
                            )
                extras <- c()    # Capture specs with extra landmarks not included.
                missings <- c()  # Capture specs with missing data.
                
                for(i in 1:length(files)){
                        x <- fromJSON(fileLoc[[i]], flatten = TRUE)$markups
                        x <- matrix(data = unlist(x$controlPoints[[1]]$position),
                                    nrow = length(x$controlPoints[[1]]$label),
                                    ncol = k,
                                    byrow = TRUE,
                                    dimnames = list(x$controlPoints[[1]]$label, NULL)
                                    )
                        # Fill missing data with NA and log
                        if(dim(ar)[1] > dim(x)[1]){
                                dif <- setdiff(lmset, dimnames(x)[[1]])
                                m <- matrix(data = NA,
                                            nrow = length(dif),
                                            ncol = k,
                                            dimnames = list(dif, NULL)
                                            )
                                x <- rbind(x,m)
                                missings <- c(missings, paste(dif))
                                names(missings) <- c(names(missings),files[i])
                        }
                        # Log specimens who had landmarks excluded
                        if(dim(ar)[1] < dim(x)[1]){
                                dif <- setdiff(dimnames(x)[[1]], lmset)
                                extras <- c(x, paste(dif))
                                names(extras) <- c(names(extras), files[i])
                        }
                        # Stop if wrong dimensions
                        if(dim(ar)[2] != dim(x)[2]){
                                stop(paste0("The following file has the wrong dimensions: ",
                                            files[i], ". Should be", k, "."))
                        }
                        ar[,,i] <- x[lmset,]
                }
        }

    # Three dimensional landmarks
        if(k == 3){
                ar <- array(data = 0,
                            dim = c(length(lmset), k, length(spec)),
                            dimnames = list(lmset, c("X", "Y", "Z"), spec)
                            )
                extras <- c()    # Capture specs with extra landmarks not included.
                missings <- c()  # Capture specs with missing data.
                
                for(i in 1:length(files)){
                        x <- fromJSON(fileLoc[[i]], flatten = TRUE)$markups
                        x <- matrix(data = unlist(x$controlPoints[[1]]$position),
                                    nrow = length(x$controlPoints[[1]]$label),
                                    ncol = k,
                                    byrow = TRUE,
                                    dimnames = list(x$controlPoints[[1]]$label, NULL)
                                    )
                        # Fill missing data with NA and log
                        if(dim(ar)[1] > dim(x)[1]){
                                dif <- setdiff(lmset, dimnames(x)[[1]])
                                m <- matrix(data = NA,
                                            nrow = length(dif),
                                            ncol = k,
                                            dimnames = list(dif, NULL)
                                )
                                x <- rbind(x,m)
                                missings <- c(missings, paste(dif))
                                names(missings) <- c(names(missings),files[i])
                        }
                        # Log specimens who had landmarks excluded
                        if(dim(ar)[1] < dim(x)[1]){
                                dif <- setdiff(dimnames(x)[[1]], lmset)
                                extras <- c(x, paste(dif))
                                names(extras) <- c(names(extras), files[i])
                        }
                        # Stop if wrong dimensions
                        if(dim(ar)[2] != dim(x)[2]){
                                stop(paste0("The following file has the wrong dimensions: ",
                                            files[i], ". Should be", k, "."))
                        }
                        ar[,,i] <- x[lmset,]
                }
        }
        
    # Produce warnings if necessary    
        if(!is.null(extras)){
                message("The following landmarks were excluded from the following files: ",
                        paste0(extras, " from ", names(extras)))
        }
        if(!is.null(missings)){
                message("The following landmarks were missing from the following files:",
                        paste0(missings, " from ", names(missings)))
        }
        
    # Results    
        return(ar)
}

