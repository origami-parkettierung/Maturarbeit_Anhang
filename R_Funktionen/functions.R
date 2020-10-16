#source('/Users/anouk/Documents/Maturarbeit/Graphiken/R/g_functions.R')
#pdf(file="~/Documents/Maturarbeit/LaTeX/parkettierungen/Abbildungen/b_vGleitspiegel.pdf", width=6, height=3)
par(pty = "m")
par(mar=c(3,3,3,3))
plot(0,0, type="n", xlim=c(0, 10), ylim=c(0, 10), xlab="", ylab="", frame=TRUE, axes=TRUE, asp=1)

#dev.off()
require(plotrix)

a = sqrt(3)/2
pi = 3.14159265358979323846264338327950288419716939937510582097494459230781640628620899862803482534211
achse = "chartreuse3"
tal = "firebrick2"
berg = "dodgerblue4"
dunkel = "cornsilk2"
hell = "cornsilk1"

MVM = c(berg,berg, tal,tal,   berg,berg)
VMM = c(tal,tal,   berg,berg, berg,berg)
MVV = c(berg,berg, tal,tal,   tal,tal)
VMV = c(tal,tal,   berg,berg, tal,tal)

MVMn = c(berg,tal, tal,berg, berg,tal)
VMMn = c(tal,berg, berg,tal, berg,tal)
MVVn = c(berg,tal, tal,berg, tal,berg)
VMVn = c(tal,berg, berg,tal, tal,berg)

ub = c("gray40", "gray40", "gray40", "gray40", "gray40", "gray40")

#eigene Funktionen
rad = function(deg=70){
  return(pi * deg / 180)
}

zeichenLeer = function(midX = 4, midY = 4, pch = "2.1", cex = 0.2){
  switch(pch,
         "2.1"= polygon(x=c(midX, midX+0.5*cex, midX, midX-0.5*cex),
                        y=c(midY-cex, midY, midY+cex, midY)),
         "2.2"= polygon(x=c(midY-cex, midY, midY+cex, midY),
                        y=c(midX, midX+0.5*cex, midX, midX-0.5*cex)),
         "3.1"= points(midX, midY, pch=24, cex=7*cex),
         "3.2"= points(midX, midY, pch=25, cex=7*cex),
         "4.1"= points(midX, midY, pch=22, cex=8*cex),
         "4.2"= points(midX, midY, pch=23, cex=8*cex),
         "6"  = polygon(x=c(midX-0.5*cex, midX+0.5*cex, midX+cex, midX+0.5*cex, midX-0.5*cex, midX-cex),
                        y=c(midY-a*cex, midY-a*cex, midY, midY+a*cex, midY+a*cex, midY))
  )
}

zeichenVoll = function(midX = 4, midY = 4, pch = "2.1", cex = 0.2){
  switch(pch,
         "2.1"= polygon(x=c(midX, midX+0.5*cex, midX, midX-0.5*cex),
                        y=c(midY-cex, midY, midY+cex, midY), col=1),
         "2.2"= polygon(x=c(midX-cex, midX, midX+cex, midX),
                        y=c(midY, midY+0.5*cex, midY, midY-0.5*cex), col=1),
         "3.1"= points(midX, midY, pch=24, cex=7*cex, lwd=0, bg=1),
         "3.2"= points(midX, midY, pch=25, cex=7*cex, lwd=0, bg=1),
         "4.1"= points(midX, midY, pch=22, cex=8*cex, lwd=0, bg=1),
         "4.2"= points(midX, midY, pch=23, cex=8*cex, lwd=0, bg=1),
         "6"  = polygon(x=c(midX-0.5*cex, midX+0.5*cex, midX+cex, midX+0.5*cex, midX-0.5*cex, midX-cex),
                        y=c(midY-a*cex, midY-a*cex, midY, midY+a*cex, midY+a*cex, midY), col=1)
  )
}

liniewinkel = function(x1=5, y1=1, beta = 0, x2=NULL, y2=NULL, l=NULL, plot=TRUE, ...){
  if (!is.null(x2)){# d.h. if x2 ungleich null
    y2 = y1 + tan(beta)*(x2-x1)
  }
  else{
    if(!is.null(y2)){
      x2 = x1 + (y2-y1)/tan(beta)
    }
    else{
      x2 = x1 + cos(beta)*l
      y2 = y1 + sin(beta)*l
    }
  }
  print(paste(x1, y1, x2, y2))
  if(plot){
    segments(x1, y1, x2, y2, ...)
  }
  invisible(return(c("x"=x2, "y"=y2)))# gibt nur zurück, wenn verlangt
}

centeredPolygon = function(n=6, x1=6, y1=6, l=3, wRel=0.2, alpha=pi/7, beta=0, frame=TRUE, mv=MVM){
  iWin = pi*(1-2/n)
  aWin = (pi-pi*(1-2/n))
  w = wRel*l
  delta = atan(tan(alpha)/wRel)
  
  sh = (l-w)/2
  s = sh*sqrt(2-2*cos(iWin))
  l2 = sin(pi - delta - asin(sh/s*sin(iWin)) ) / sin(aWin) * s
  l1 = sin(delta - asin(sh/s*sin(iWin)) ) / sin(aWin) * s
  pOld = c(x1, y1)
  
  for(i in 0:(n-1)){
    p1  = liniewinkel(pOld[1], pOld[2], beta+i*aWin, l=(l-w)/2, plot=FALSE)
    p2  = liniewinkel(pOld[1], pOld[2], beta+i*aWin, l=(l+w)/2, plot=FALSE)
    pm1 = liniewinkel(p1[1], p1[2], beta+i*aWin + delta, l=l1, col=mv[1+i%%2])
    pm2 = liniewinkel(p2[1], p2[2], beta+i*aWin + delta, l=l2, col=mv[3+i%%2])
    
    pNew = liniewinkel(pOld[1], pOld[2], beta+i*aWin, l=l, plot=frame, col="gray")
    segments(pm1[1], pm1[2], pm2[1], pm2[2], col=mv[5+i%%2])
    pOld = pNew
  }
}

offsetPolygon = function(n=6, x1=1, y1=1, l=3, wRel=0.2, dRel=0.1, beta=0, frame=TRUE, mv=MVM){
  w = wRel*l
  d = dRel*l
  iWin = pi*(1-2/n)
  aWin = (pi-pi*(1-2/n))
  ah = l/2 + d - w/2
  bh = l/2 - d - w/2
  s = sqrt(ah*ah + bh*bh - 2*ah*bh*cos(iWin))
  l2 = sin(pi/2 - asin(bh/s * sin(iWin)) ) / sin(aWin) * s
  l1 = sin(pi/2 - asin(ah/s * sin(iWin)) ) / sin(aWin) * s
  pOld = c(x1, y1)
  
  for(i in 0:(n-1)){
    p1  = liniewinkel(pOld[1], pOld[2], beta+i*aWin, l=(l-w)/2+d, plot=FALSE)
    p2  = liniewinkel(pOld[1], pOld[2], beta+i*aWin, l=(l+w)/2+d, plot=FALSE)
    pm1 = liniewinkel(p1[1], p1[2], beta+i*aWin + pi/2, l=l1, col=mv[1+i%%2])
    pm2 = liniewinkel(p2[1], p2[2], beta+i*aWin + pi/2, l=l2, col=mv[3+i%%2])
    
    pNew = liniewinkel(pOld[1], pOld[2], beta+i*aWin, l=l, plot=frame, col="gray")
    segments(pm1[1], pm1[2], pm2[1], pm2[2], col=mv[5+i%%2])
    pOld = pNew
  }
}

regpolygon = function(n=6, x1=3, y1=3, l=4, beta=0, leer=TRUE, ...){
  aWin = (pi-pi*(1-2/n))
  pOld = c(x1, y1)
  xFarb=c()
  yFarb=c()
  for(i in 1:n){
    xFarb[i] = pOld[1]
    yFarb[i] = pOld[2]
    pNew = liniewinkel(pOld[1], pOld[2], beta+(i-1)*aWin, l=l, plot=leer, ...)
    pOld = pNew
  }
  if(isFALSE(leer)){
    polygon(x=xFarb, y=yFarb, col="gray70")
  }
}
