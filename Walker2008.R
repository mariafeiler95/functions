# Walker2008()
# Estimate sex based on cranial nonmetric trait scores using Walker 2008.

# Given a matrix of ordinal data, where the rows are individuals and columns
# are variables, estimates sex based on Walker 2008 protocol using cranial 
# nonmetric trait scores.

# 05/23/2023
# M.E.F.
# mariaefeiler@gmail.com

# R4.1.3, RStudio 2022.7.0.548
# Windows 10 x64

# SOURCES: Walker, P. L. (2008). Sexing skulls using discriminant function 
#          analysis of visually assessed traits. American Journal of Physical 
#          Anthropology, 136, 39-50.

# @param data           Matrix of ordinal data.
# @param gla            Numeric or character indicating column position or name
#                       for glabella scores. 
# @param mas            Numeric or character indicating column position or name
#                       for mastoid process scores. 
# @param som            Numeric or character indicating column position or name
#                       for supraorbital margin scores. 
# @param nuc            Numeric or character indicating column position or name
#                       for nuchal crest scores. 
# @param men            Numeric or character indicating column position or name
#                       for mental eminence scores. 
# @param skip           A numeric or vector indicating if any individuals (rows) 
#                       should be omitted from analysis.
#                       Default: NULL

# @return               A data frame of cranial score ($Cranial.Score), 
#                       probability of being female ($Prob.Female), and sex
#                       estimation ($Sex.Estimation) for each individual.

# @examples
# df <- matrix(data = sample(1:5, 25, replace = TRUE),
#              nrow = 5,
#              ncol = 5,
#              dimnames = list(c("Max", "Joe", "Jenny", "Tyler", "Layla"),
#                              c("Glabella", "Mastoid.Process", "Supraorbital.Margin",
#                              "Nuchal.Crest", "Mental.Eminence")
#                              )
#             )
# 
# # Using default column positions
# Walker2008(df)
# 
# # Define column names or locations and skip 4th and 5th individual
# Walker2008(df, gla = "Glabella", mas = 2, som = 3, nuc = "Nuchal.Crest", 
#            men = 5, skip = 4:5)

Walker2008 <- function(data, gla = 1, mas = 2, som = 3, nuc = 4, men = 5, 
                       skip = NULL){
  
  # Remove specimens if called for
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
    # Vector to catch calculations
    res <- vector(mode = "numeric", length = 6)
    names(res) <- c("GlaMasMen", "GlaMas", "GlaMen", "MenMas", "SomMen",
                    "NucMen")
    
    # Calculate values based on residuals from Walker 2008
    res["GlaMasMen"] <- -1.375*data[i,gla] - 1.185*data[i,mas] - 1.15*data[i,men] + 9.128
    res["GlaMas"] <- -1.558*data[i,gla] - 1.459*data[i,mas] + 7.434
    res["GlaMen"] <- -1.525*data[i,gla] - 1.485*data[i,men] + 7.372
    res["MenMas"] <- -1.415*data[i,men] - 1.629*data[i,mas] + 7.382
    res["SomMen"] <- -1.007*data[i,som] - 1.85*data[i,men] + 6.018
    res["NucMen"] <- -0.7*data[i,nuc] - 1.559*data[i,men] + 5.329
    
    # Retain score and probability of being female in results vectors
    scores[i] <- res[which(is.na(res) == FALSE)[1]]
    pfem[i] <- 1/(1 + exp(-scores[i]))
    sexest[i] <- ifelse(pfem[i] > 0.5, "Female", "Male")
  }
  
  # Convert sexest to a factor, levels = Female, Male
  sexest <- factor(sexest, levels = c("Female", "Male"))
  
  # Print results
  cbind("Cranial.Score" = scores, 
        "Prob.Female" = pfem, 
        "Sex.Estimation" = sexest)
}
