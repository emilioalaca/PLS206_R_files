##------------------------------------------------------------##
##                  Script for R Graphics                     ##
##                         John Fox                           ##
##          The R Statistical Computing Environment           ##
##             ICPSR Summer Program Berkeley                  ##
##                         2012                               ##
##------------------------------------------------------------##

# Graphics Basics (traditional S/R graphics)

args(plot.default)  # default plot method

?plot

    # points, lines, axes, frames

plot(c(0, 1), c(0, 1), type="n", xlab="", ylab="")  # simply creates a "canvas" with a coordinate system onto which we can add graphics elements
# type="n" means "plot nothing"
par("col")  # graphical parameters are called or set with par()

names(par())

?par

plot(1:25, pch=1:25, xlab="Symbol Number", ylab="")  # symbols
lines(1:25, type="h", lty="dashed")

plot(26:1, xlab="letters", ylab="",
    pch=letters, axes=FALSE, frame.plot=TRUE)

plot(c(1, 7), c(0, 1), type="n", axes=FALSE,  # lines
    xlab="Line Type (lty)", ylab="")
box() # add frame
axis(1, at=1:6)  # x-axis
for (lty in 1:6) 
    lines(c(lty, lty, lty + 1), c(0, 0.5, 1), lty=lty)
    
plot(c(0, 1), c(0, 1), type="n", xlab="", ylab="")
abline(0, 1) # intercept and slope
abline(c(1, -1), lty="dashed")
        # horizontal and vertical lines: 
abline(h=seq(0, 1, by=0.1), v=seq(0, 1, by=0.1), col="gray")

    # text

par(mfrow=c(1, 2))  # array of plots

plot(c(0, 1), c(0, 1), axes=FALSE, type="n", xlab="", ylab="",
    frame.plot=TRUE, main="(a)")
text(x=c(0.2, 0.5), y=c(0.2, 0.7),
    c("example text", "another string")) # by default the text is centered at the coordinates specified

plot(c(0, 1), c(0, 1), axes=FALSE, type="n", xlab="", ylab="",
    frame.plot=TRUE, main="(b)")
text(locator(3), c("one", "two", "three"))


locator() # returns mouse coordinates, remember to exit

    # legend

plot(c(1,5), c(0,1), axes=FALSE, type="n", xlab="", ylab="",
    frame.plot=TRUE)
legend(locator(1), legend=c("group A", "group B", "group C"),
    lty=1:3, pch=1:3, col=c("blue", "green", "red"))
legend("topright", legend=c("group A", "group B", "group C"),
       lty=1:3, pch=1:3, col=c("blue", "green", "red"))
    
    # curve
    
curve(x*cos(25/x), 0.01, pi, n=1000)

curve(sin, 0, 2*pi, ann=FALSE, axes=FALSE, lwd=2)
axis(1, pos=0, at=c(0, pi/2, pi, 3*pi/2, 2*pi),
    labels=c(0, expression(pi/2), expression(pi),
        expression(3*pi/2), expression(2*pi)))
axis(2, pos=0)
curve(cos, add=TRUE, lty="dashed", lwd=2)
legend(pi, 1, lty=1:2, lwd=2, legend=c("sine", "cosine"), bty="n")

?plotmath  # mathematical annotation

    # colors

pie(rep(1, length(palette())), col=palette())

palette()

colors()[1:10] # first ten named colors
length(colors())


        # for palettes based on psychophysical principles, see the colorspace package

# Putting it together (as time permits)

    # diagrams of the standard normal density function

        # showing the area above 1.96

oldpar <- par(mar = c(5, 6, 4, 2) + 0.1)    # leave room on the left
oldpar  # old parameter saved

z <- seq(-4, 4, length=1000)
p <- dnorm(z)
plot(z, p, type="l", lwd=2,
    main=expression("The Standard Normal Density Function" ~~ phi(z)),
    ylab=expression(phi(z) ==
        frac(1, sqrt(2*pi)) * ~~ e^- ~~ frac(z^2, 2)))
abline(h=0, col="gray")
abline(v=0, col="gray")
z0 <- z[z >= 1.96]    # define region to fill
z0 <- c(z0[1], z0)
p0 <- p[z >= 1.96]
p0 <- c(0, p0)
polygon(z0, p0, col="gray")
coords <- locator(2)    # locate head and tail of arrow
arrows(coords$x[1], coords$y[1], coords$x[2], coords$y[2], code=1,
    length=0.125)
text(coords$x[2], coords$y[2], pos=3,   # text above tail of arrow
    expression(integral(phi(z)*dz, 1.96, infinity) == .025))

        # with lines at z = -3:3

par(oldpar)  # restore graphics parameters

plot(z, p, type="n", xlab="", ylab="", axes=FALSE,
    main=expression("The Standard Normal Density Function" ~~ phi(z)))
axis(1, pos=0, at=-3:3)
abline(h=0)
axis(2, pos=0, at=.1*1:3)
abline(v=0)
curve(dnorm, -4, 4, n=1000, add=TRUE, lwd=2)
text(locator(2), c("z", expression(phi(z))), xpd=TRUE)
for (z0 in -3:3) lines(c(z0, z0), c(0, dnorm(z0)), lty=2)


 