point2plane <- function(p,a,b,c){
  if(dim(p) != 3 | dim(a) != 3 | dim(b) != 3 | dim(c) != 3){
    stop("all arguments must be numeric points of dimension 3")
  }

  p1<-p[1]; p2<-p[2]; p3<-p[3]
  a1<-a[1]; a2<-a[2]; a3<-a[3]
  b1<-b[1]; b2<-b[2]; b3<-b[3]
  c1<-c[1]; c2<-c[2]; c3<-c[3]

  Cx <-(-a2*b3+a2*c3+a3*b2-a3*c2-b2*c3+b3*c2)/(a1*b2-a1*c2-a2*b1+a2*c1+b1*c2-b2*c1)
  Cy <-(a1*b3-a1*c3-a3*b1+a3*c1+b1*c3-b3*c1)/(a1*b2-a1*c2-a2*b1+a2*c1+b1*c2-b2*c1)
  Cz <- -1
  
  C0 <-(a1*b2*c3-a1*b3*c2-a2*b1*c3+a2*b3*c1+a3*b1*c2-a3*b2*c1)/(a1*b2-a1*c2-a2*b1+a2*c1+b1*c2-b2*c1)

  num.dis<-Cx*p1+Cy*p2+Cz*p3+C0 #numerator of the distance equation

  t<-(Cx*a1-Cx*p1+Cy*a2-Cy*p2+Cz*a3-Cz*p3)/(Cx^2+Cy^2+Cz^2)
  prj<-p+t*c(Cx,Cy,Cz)

  dis<-num.dis/sqrt(Cx^2+Cy^2+Cz^2)

  return(list(distance=dis, #distance
              prj.pt2plane=prj #c(pr.x,pr.y,pr.z) #point of orthogonal projection (i.e. closest point) on the plane
  )
}
