# Klales2012
# Estimate sex based on pubic trait scores using Klales et al. 2012.

# Given a matrix of pubic trait ordinal data scored using Klales et al.'s 2012 
# method, produces sex estimations for the individuals. 

# 05/23/2023
# M.E.F.
# mariaefeiler@gmail.com

# R4.1.3, RStudio 2022.7.0.548
# Windows 10 x64

# SOURCES: Klales, A. R., Ousley, S. D., & Vollner, J. M. (2012). A revised 
#          method of sexing the human innominate using Phenice's nonmetric 
#          traits and statistical methods. American Journal of Physical 
#          Anthropology, 149, 104-114.

# @param data           Matrix of ordinal data.
# @param ven            Numeric or character indicating column position or name
#                       for ventral arch scores. 
# @param ipr            Numeric or character indicating column position or name
#                       for ischiopubic ramus scores. 
# @param spc            Numeric or character indicating column position or name
#                       for subpubic concavity scores. 
# @param skip           A numeric or vector indicating if any individuals should 
#                       be omitted from analysis.
#                       Default: NULL

# @return               A data frame of pubic score ($Pubic.Score), probability 
#                       of being female ($Prob.Female), and sex estimation 
#                       ($Sex.Estimation) for each individual.

# @examples
# df <- matrix(data = sample(1:5, 15, replace = TRUE),
#              nrow = 5,
#              ncol = 3,
#              dimnames = list(c("Max", "Joe", "Jenny", "Tyler", "Layla"),
#                              c("Ventral.Arch", "Ischiopubic.Ramus",
#                                "Subpubic.Concavity")
#                              )
#             )
#             
# # Using default column positions
# Klales2012(df)
# 
# # Define column names or locations and skip 4th and 5th individual
# Klales2012(df, ven = "Ventral.Arch", ipr = 2, spc = 3, skip = 4:5)

Klales2012 <- function(data, ven = 1, ipr = 2, spc = 3, skip = NULL){
  
  # Remove individuals if called for.
  if(!is.null(skip)){data <- data[-skip,]}
  
  # Scores vector with specimen names
  scores <- vector(mode = "numeric", length = nrow(data))
  names(scores) <- rownames(data)
  
  # Probability of being a female vector with specimen names
  pfem <- vector(mode = "numeric", length = nrow(data))
  names(pfem) <- rownames(data)
  
  # Sex estimation based on probability of being female
  sexest <- vector(mode = "numeric", length = nrow(data))
  names(sexest) <- rownames(sexest)
  
  for(i in 1:nrow(data)){
    scores[i] <- 2.726*data[i,ven] + 1.214*data[i,ipr] + 1.073*data[i,spc] - 16.312
    pfem[i] <- 1/(1 + exp(scores[i]))
    sexest[i] <- ifelse(pfem[i] > 0.5, "Female", "Male")
  }
  
  # Convert sexest to a factor, levels = Female, Male
  sexest <- factor(sexest, levels = c("Female", "Male"))
  
  # Print results
  cbind("Pubis.Score" = scores, 
        "Prob.Female" = pfem, 
        "Sex.Estimation" = sexest
        )
}
