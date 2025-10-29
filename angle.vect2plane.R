angle.vect2plane <- function(vect1, vect2, planeA, planeB, planeC, angle = "radians",
                             robust = FALSE){
    # Define plane equation as d = Ax + By + Cz to get the normal vector of the
    # plane in form (A,B,C).
    # source: https://rdrr.io/cran/pcds/src/R/AuxGeometry.R function: Plane
    # Find two vectors on the line
    AB = planeA - planeB
    AC = planeA - planeC
    
    # Get normal of plane, which is equal to the cross product of the two vectors
    pNorm <- c(AB[2] * AC[3] - AB[3] * AC[2],
               AB[3] * AC[1] - AB[1] * AC[3],
               AB[1] * AC[2] - AB[2] * AC[1])
    
    names(pNorm) = c("A", "B", "C")
    
    # Define vector between two landmarks provided (assumes x,y,z)
    vect = vect1-vect2
    
    # Get dot product between vector and plane normal
    dot = vect%*%pNorm
    
    # Calculate magnitude of vector and plane normal
    mag_vect = sqrt(sum(vect^2))
    mag_pNorm = sqrt(sum(pNorm^2))
    
    # Calculate sin(theta)
    sin_theta = abs(dot) / (mag_vect * mag_pNorm)
    
    # Calculate angle as either degrees or radians
    if(angle == "radians" | angle == "r"){
        theta = as.numeric(asin(sin_theta))
        angle = "radians"}
    if(angle == "degrees" | angle == "d"){
        theta = as.numeric(asin(sin_theta) * (180 / pi))
        angle = "degrees"}
    
    # Return 
    if(robust == TRUE){
        return(list("pNorm" = pNorm,
                    "vector" = vect,
                    "theta" = theta,
                    "angle" = angle))
    }
    if(robust == FALSE){return(theta)}
}
