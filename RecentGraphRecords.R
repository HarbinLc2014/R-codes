> x<-rnorm(100)
> hist(x)
> y<-rnorm(100)
> plot(x,y)
> z<-rnorm(100)
> plot(x,z)
> plot(x,y)
> par(mar=c(2,2,2,2))
> plot(x,y)
> par(mar=c(4,4,2,2))
> plot(x,y)
> plot(x,y,pch=20)
> plot(x,y,pch=19)
> plot(x,y,pch=19)
> plot(x,y,pch=2)
> plot(x,y,pch=3)
> plot(x,y,pch=4)
> example(plots)
Warning message:
In example(plots) : no help found for ¡®plots¡¯
> example(points)

points> require(stats) # for rnorm

points> plot(-4:4, -4:4, type = "n")  # setting up coord. system
Waiting to confirm page change...

points> points(rnorm(200), rnorm(200), col = "red")

points> points(rnorm(100)/2, rnorm(100)/2, col = "blue", cex = 1.5)

points> op <- par(bg = "light blue")

points> x <- seq(0, 2*pi, len = 51)

points> ## something "between type='b' and type='o'":
points> plot(x, sin(x), type = "o", pch = 21, bg = par("bg"), col = "blue", cex = .6,
points+  main = 'plot(..., type="o", pch=21, bg=par("bg"))')
Waiting to confirm page change...

points> par(op)

points> ## Not run: 
points> ##D ## The figure was produced by calls like
points> ##D png("pch.png", height = 0.7, width = 7, res = 100, units = "in")
points> ##D par(mar = rep(0,4))
points> ##D plot(c(-1, 26), 0:1, type = "n", axes = FALSE)
points> ##D text(0:25, 0.6, 0:25, cex = 0.5)
points> ##D points(0:25, rep(0.3, 26), pch = 0:25, bg = "grey")
points> ## End(Not run)
points> 
points> ##-------- Showing all the extra & some char graphics symbols ---------
points> pchShow <-
points+   function(extras = c("*",".", "o","O","0","+","-","|","%","#"),
points+            cex = 3, ## good for both .Device=="postscript" and "x11"
points+            col = "red3", bg = "gold", coltext = "brown", cextext = 1.2,
points+            main = paste("plot symbols :  points (...  pch = *, cex =",
points+                         cex,")"))
points+   {
points+     nex <- length(extras)
points+     np  <- 26 + nex
points+     ipch <- 0:(np-1)
points+     k <- floor(sqrt(np))
points+     dd <- c(-1,1)/2
points+     rx <- dd + range(ix <- ipch %/% k)
points+     ry <- dd + range(iy <- 3 + (k-1)- ipch %% k)
points+     pch <- as.list(ipch) # list with integers & strings
points+     if(nex > 0) pch[26+ 1:nex] <- as.list(extras)
points+     plot(rx, ry, type = "n", axes  =  FALSE, xlab = "", ylab = "", main = main)
points+     abline(v = ix, h = iy, col = "lightgray", lty = "dotted")
points+     for(i in 1:np) {
points+       pc <- pch[[i]]
points+       ## 'col' symbols with a 'bg'-colored interior (where available) :
points+       points(ix[i], iy[i], pch = pc, col = col, bg = bg, cex = cex)
points+       if(cextext > 0)
points+           text(ix[i] - 0.3, iy[i], pc, col = coltext, cex = cextext)
points+     }
points+   }

points> pchShow()
Waiting to confirm page change...

points> pchShow(c("o","O","0"), cex = 2.5)
Waiting to confirm page change...

points> pchShow(NULL, cex = 4, cextext = 0, main = NULL)
Waiting to confirm page change...

points> ## No test: 
points> ## ------------ test code for various pch specifications -------------
points> # Try this in various font families (including Hershey)
points> # and locales.  Use sign = -1 asserts we want Latin-1.
points> # Standard cases in a MBCS locale will not plot the top half.
points> TestChars <- function(sign = 1, font = 1, ...)
points+ {
points+    MB <- l10n_info()$MBCS
points+    r <- if(font == 5) { sign <- 1; c(32:126, 160:254)
points+        } else if(MB) 32:126 else 32:255
points+    if (sign == -1) r <- c(32:126, 160:255)
points+    par(pty = "s")
points+    plot(c(-1,16), c(-1,16), type = "n", xlab = "", ylab = "",
points+         xaxs = "i", yaxs = "i",
points+         main = sprintf("sign = %d, font = %d", sign, font))
points+    grid(17, 17, lty = 1) ; mtext(paste("MBCS:", MB))
points+    for(i in r) try(points(i%%16, i%/%16, pch = sign*i, font = font,...))
points+ }

points> TestChars()
Waiting to confirm page change...

points> try(TestChars(sign = -1))
Waiting to confirm page change...

points> TestChars(font = 5)  # Euro might be at 160 (0+10*16).
Waiting to confirm page change...

points>                      # Mac OS has apple at 240 (0+15*16).
points> try(TestChars(-1, font = 2))  # bold
Waiting to confirm page change...

points> ## End(No test)
points> 
points> 
> plot(x,y,pch=20)
Error in xy.coords(x, y, xlabel, ylabel, log) : 
  'x' and 'y' lengths differ
> z<-rnorm(100)
> x<-rnorm(100)
> y<-rnorm(100)
> plot(x,y,pch=20)
> title("Scatterplot")
> text(-2,-2,"label")
> legend("topleft",legend="Data")
> legend("topleft",legend="Data",pch=20)
> fit<-lm(y-x)
Error in formula.default(object, env = baseenv()) : invalid formula
> fit<-lm(y~x)
> abline(fit)
> abline(fit,lmd=3)
Warning message:
In int_abline(a = a, b = b, h = h, v = v, untf = untf, ...) :
  "lmd" is not a graphical parameter
> abline(fit,lwd=3)
> abline(fit,lwd=3,col="blue")
> plot(x,y,xlab="Weight",ylab="Height",main="Scatterplot",pch=20)
> legend("topright",legend="Data",pch=20)
> fit<-lm(y~x)
> abline(fit,lmd=3,col="red")
Warning message:
In int_abline(a = a, b = b, h = h, v = v, untf = untf, ...) :
  "lmd" is not a graphical parameter
> abline(fit,lwd=3,col="red")
> z<-rpois(100,2)
> par(mfrow=c(2,1))
> plot(x,y,pch=20)
> plot(x,z,pch=19)
> par("mar")
[1] 4 4 2 2
> par(mar=c(2,2,1,1))
> plot(x,y,pch=20)
> plot(x,z,pch=19)
> par(mfrow=c(1,2))
> plot(x,y,pch=20)
> plot(x,z,pch=19)
> par(mfrow=c(1,1))
> y<-x+rnorm(100)
> g<-gl(2,50)
> g<-gl(2,50,labels=c("Male","Female"))
> str(g)
 Factor w/ 2 levels "Male","Female": 1 1 1 1 1 1 1 1 1 1 ...
> plot(x,y)
> plot(x,y,type="n")
> points(x[g=="Male"])
> points(x[g=="Male"],y[g=="Male"],col=green)
Error in plot.xy(xy.coords(x, y), type = type, ...) : 
  object 'green' not found
> points(x[g=="Male"],y[g=="Male"],col="green")
> points(x[g=="Female"],y[g=="Female"],col="blue")
> points(x[g=="Female"],y[g=="Female"],col="blue",pch=19)
> 