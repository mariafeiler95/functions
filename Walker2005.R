# Walker2005()
# Estimate sex based on greater sciatic notch scores using Walker 2005.

# Given a vector of greater sciatic notch ordinal data scored using Walker's 
# 2005 method, produces sex estimations for the individuals. 

# 05/23/2023
# M.E.F.
# mariaefeiler@gmail.com

# R4.1.3, RStudio 2022.7.0.548
# Windows 10 x64

# SOURCES: Walker, P. L. (2005). Greater sciatic notch morhology: Sex, age, and
#          population differences. American Journal of Physical Anthropology, 
#          127(4), 385-391.

# @param data           Vector of ordinal data.
# @param skip           A numeric or vector indicating if any individuals should 
#                       be omitted from analysis.
#                       Default: NULL

# @return               A vector of sex estimations for each individual.

# @examples
# gsn <- sample(1:5, 5, replace = TRUE)
# 
# # Skip last individual
# Walker2005(gsn, skip = 5)


Walker2005 <- function(data, skip = NULL){
  
  # Remove individuals if called for
  if(!is.null(skip)){data <- data[-skip]}
  
  # Sex estimation based on probability of being female
  sexest <- vector(mode = "character", length = length(data))
  names(sexest) <- names(data)
  
  for(i in 1:length(data)){
    if(is.na(data[i])) {sexest[i] <- NA}
    else if(data[i] >= 4) {sexest[i] <- "Male"}
    else if(data[i] <= 2) {sexest[i] <- "Female"}
    else {sexest[i] <- "Indeterminate"}
  }
  
  # Convert sexest to a factor, levels = Female, Male, Indeterminate
  sexest <- factor(sexest, levels = c("Female", "Male", "Indeterminate"))
  
  # Return results
  sexest
}
