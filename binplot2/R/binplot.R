binplot <-
  function(x,y,nr=20,nc=20, scale="raw") {
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

scatterhist = function(x, y, xlab="Column 1 for histogram", ylab="Column 2 for historgram"){
  zones=matrix(c(2,0,1,3), ncol=2, byrow=TRUE)
  layout(zones, widths=c(4/5,1/5), heights=c(1/5,4/5))
  xhist = hist(x, plot=FALSE)
  yhist = hist(y, plot=FALSE)
  top = max(c(xhist$counts, yhist$counts))
  par(mar=c(3,3,1,1))
  plot(x,y)
  par(mar=c(0,3,1,1))
  barplot(xhist$counts, axes=FALSE, ylim=c(0, top), space=0)
  par(mar=c(3,0,1,1))
  barplot(yhist$counts, axes=FALSE, xlim=c(0, top), space=0, horiz=TRUE)
  par(oma=c(3,3,0,0))
  mtext(xlab, side=1, line=1, outer=TRUE, adj=0, 
        at=.8 * (mean(x) - min(x))/(max(x)-min(x)))
  mtext(ylab, side=2, line=1, outer=TRUE, adj=0, 
        at=(.8 * (mean(y) - min(y))/(max(y) - min(y))))
}

