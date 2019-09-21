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
