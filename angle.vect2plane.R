# angle.vect2plane()
# Calculate absolute angle between vector and a plane in 3D space.

# Given 3D coordinates of points that define the vector and plane in question,
# produces angle in desired units.

# 10/29/2025
# M.E.F.
# mariaefeiler@gmail.com

# R4.5.0, RStudio 2025.09.0.387
# Windows 10 x64

# @param vect1     Vector in (x,y,z) format, first point on the vector.
# @param vect2     Vector in (x,y,z) format, second point on the vector.
# @param planeA    Vector in (x,y,z) format, first point on the plane.
# @param planeB    Vector in (x,y,z) format, second point on the plane.
# @param planeC    Vector in (x,y,z) format, third point on the plane.
# @param unit      Character, "radians" or "r" for radians, "degrees" or
#                  "d" for degrees, determines units of output.
#                  Default: "degrees"
# @param robust    If TRUE: returns list (see below). 
#                  If FALSE, returns only angle.
#                  Default: FALSE

# @return         List:
#                 $pNorm      Normal vector of the calculated plane.
#                 $vector     Vector calculated between the two points of
#                             interest.
#                 $theta      The angle calculated.
#                 $unit       The unit of the angle calculated.

angle.vect2plane <- function(vect1, vect2, planeA, planeB, planeC, unit = "degrees",
                             robust = FALSE){
    # Define plane equation as d = Ax + By + Cz to get the normal vector of the
    # plane in form (A,B,C).
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
    if(unit == "radians" | unit == "r"){
        theta = as.numeric(asin(sin_theta))
        unit = "radians"}
    if(unit == "degrees" | unit == "d"){
        theta = as.numeric(asin(sin_theta) * (180 / pi))
        unit = "degrees"}
    
    # Return 
    if(robust == TRUE){
        return(list("pNorm" = pNorm,
                    "vector" = vect,
                    "theta" = theta,
                    "unit" = angle))
    }
    if(robust == FALSE){return(theta)}
}
