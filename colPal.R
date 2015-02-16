#Color-blind friendly palette, from R-cookbook
# n = number of colors (1-6)
# a = alpha value (1-255)

colPal <- function(n,a=255) {
  colours = data.frame(CC98=c(213,94,0),CW03=c(0,158,115),M10=c(230,159,0),
                       R12b=c(0,114,178),VP13=c(204,121,167),H14=c(86,180,233))
  cols =NA
  for (i in 1:n) {
    cols[i] = rgb(colours[1,i],colours[2,i],colours[3,i],a,maxColorValue = 255)
  }
  
  return (cols)
}
