



Arrow <- function(x0, y0, x1, y1, col=par("bg"), border = par("fg"), head=1, cex=1, lwd=1, lty=1){

  ArrowHead <- function(x=0, y=0, type=2, cex=1, theta=0){

    # choose a default
    rx <- par("pin")[1] / 100  * cex

    # get aspect ratio for not allowing the arrowhead to lose form
    asp <- Asp()

    head <- DrawRegPolygon(x, y, radius.x = rx, radius.y = rx * asp, plot=FALSE)

    if(type==3){
      head$x <- append(head$x, head$x[1] - rx, 2)
      head$y <- append(head$y, y, 2)
    }

    # Rotate the head
    head <- Rotate(head, theta=theta, mx=x, my=y, asp = asp)

    head$x <- head$x - rx * cos(theta)
    head$y <- head$y - rx * sin(theta)

    return(head)

  }


  if(head > 1){
    segments(x0 = x0, y0 = y0, x1 = x1, y1 = y1, lty=lty, lwd=lwd)
    head <- ArrowHead(x=x1, y=y1, type=head, cex=cex,
                      theta= (atan((y0-y1) / Asp() /(x0-x1)) + (x0 > x1) * pi))

    polygon(head, col=col, border=border)

  } else {
    arrows(x0 = x0, y0 = y0, x1 = x1, y1 = y1, lty=lty, lwd=lwd)
  }

  invisible()

}


