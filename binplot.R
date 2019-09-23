binplot <- function(x,y,nr=20,nc=20, scale="raw") {
  zx = c(1:nr,rep(1,nc),1+trunc( nr*(x- min(x))/(max(x)-min(x)) ))
  zx[zx>nr] = nr
  zy = c(rep(1,nr),1:nc,1+trunc( nc*(y- min(y))/(max(y)-min(y)) ))
  zy[zy>nc] = nc
  z = table(zx,zy); z[,1]=z[,1]-1; z[1,]=z[1,]-1;
  if (scale=="l") z= log(1+z)
  image(z=t(z),x=seq(length=nr+1,from=min(x),to=max(x)),
        y= seq(length=nc+1,from=min(y),to=max(y)),
        xlab="",ylab="", col=topo.colors(100))
}
# Run this code line by line
x = rnorm(10000) ; y = rnorm(10000)
plot(x,y)
binplot(x,y,10,10)
binplot(x,y,50,50)
binplot(x,y,100,100)
binplot(x,y,100,100,'l')
binplot(x,y,500,500,'l')
ux = rnorm(5000)/3;ux
uy = ux^2 -0.5;uy
par(mar=c(4,4,1,1)) ## set the margin sizes in the following order:bottom, left, top and right
plot(x=c(y,uy), y=c(x,ux),pch=20, col=blues9, xlab="X", ylab="Y",cex=0.7) #pch controls the shape of points
ues9binplot(x=c(y,uy)+20,y=c(x,ux),100,100)
binplot(c(y,uy)+20,c(x,ux),100,100,"l")

