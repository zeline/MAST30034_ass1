'
start = c(rep(NA, AV[1]))
period = c(rep(1, DUR_ONES[1]), rep(-1, DUR_NEG[1]))
n = length(c(start, period))
branch = rep_len(period, N-n)
tc1 = c(start, period, branch)
tc1
'


#Q1.2
# Function to plot color bar by https://stackoverflow.com/questions/9314658/colorbar-from-custom-colorramppalette
color.bar <- function(lut, min, max=-min, nticks=11, ticks=seq(min, max, len=nticks), title='') {
  scale = (length(lut)-1)/(max-min)
  
  dev.new(width=1.75, height=5)
  plot(c(0,10), c(min,max), type='n', bty='n', xaxt='n', xlab='', yaxt='n', ylab='', main=title)
  axis(2, ticks, las=1)
  for (i in 1:(length(lut)-1)) {
    y = (i-1)/scale + min
    rect(0,y,10,y+1/scale, col=lut[i], border=NA)
  }
}

scale = color.bar(colorRampPalette(c("light green", "yellow", "orange", "red"))(100), -1)