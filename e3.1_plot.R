plotsegraph <- function(loc, value, sterr, wiskwidth, color = "grey", linewidth = 2) {
  
  w <- wiskwidth/2
  segments(x0 = loc, x1 = loc, y0 = value - sterr, y1 = value + sterr, col = color, 
           lwd = linewidth)
  segments(x0 = loc - w, x1 = loc + w, y0 = value + sterr, y1 = value + sterr, 
           col = color, lwd = linewidth)  # upper whiskers
  segments(x0 = loc - w, x1 = loc + w, y0 = value - sterr, y1 = value - sterr, 
           col = color, lwd = linewidth)  # lower whiskers
}

# Means over subjects
min.tarskip.ss <- with(min, aggregate(list(skip=pskipR3),list(Preview=preview,Length=factor(len),subj=subj), mean, na.rm=T))
min.tarskip.ss2 <- summarySE(min.tarskip.ss, measurevar="skip", groupvars=c("Preview", "Length"), na.rm=T)
min.tarskip.ss2$Preview <- with(min.tarskip.ss2, ifelse(Preview=="Higher-Frequency", "Function Word", "Target"))

# Replace "sp" points with "Target" and "ac" with "Function Word"
# hf = 3-letter
# lf = 4-letter
# vlf = 5-letter
RT.hf.sp <- min.tarskip.ss2[min.tarskip.ss2$Preview=="Target" & min.tarskip.ss2$Length==3,]$skip
RT.lf.sp <- min.tarskip.ss2[min.tarskip.ss2$Preview=="Target" & min.tarskip.ss2$Length==4,]$skip
RT.vlf.sp <- min.tarskip.ss2[min.tarskip.ss2$Preview=="Target" & min.tarskip.ss2$Length==5,]$skip
se.RT.hf.sp <- min.tarskip.ss2[min.tarskip.ss2$Preview=="Target" & min.tarskip.ss2$Length==3,]$se
se.RT.lf.sp <- min.tarskip.ss2[min.tarskip.ss2$Preview=="Target" & min.tarskip.ss2$Length==4,]$se
se.RT.vlf.sp <- min.tarskip.ss2[min.tarskip.ss2$Preview=="Target" & min.tarskip.ss2$Length==5,]$se
RT.hf.ac <- min.tarskip.ss2[min.tarskip.ss2$Preview=="Function Word" & min.tarskip.ss2$Length==3,]$skip
RT.lf.ac <- min.tarskip.ss2[min.tarskip.ss2$Preview=="Function Word" & min.tarskip.ss2$Length==4,]$skip
RT.vlf.ac <- min.tarskip.ss2[min.tarskip.ss2$Preview=="Function Word" & min.tarskip.ss2$Length==5,]$skip
se.RT.hf.ac <- min.tarskip.ss2[min.tarskip.ss2$Preview=="Function Word" & min.tarskip.ss2$Length==3,]$se
se.RT.lf.ac <- min.tarskip.ss2[min.tarskip.ss2$Preview=="Function Word" & min.tarskip.ss2$Length==4,]$se
se.RT.vlf.ac <- min.tarskip.ss2[min.tarskip.ss2$Preview=="Function Word" & min.tarskip.ss2$Length==5,]$se

pdf("~/Dropbox/Dissertation/Defense/experiment3.1.pdf", width = 6.5, height = 4.5)

par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5, 
    font.lab = 2, cex.axis = 1.3, bty = "n", las = 1)
x <- c(1, 2, 3, 4)
plot(x, c(-10, -10, -10, -10), type = "p", ylab = "", xlab = " ", cex = 1.5, 
     ylim = c(0.15, 0.8), xlim = c(1, 4), lwd = 2, pch = 5, axes = F, main = " ")
axis(1, at = c(1.5, 2.5, 3.5), labels = c("3", "4", "5"))
mtext("Word Length (Letters)", side = 1, line = 3, cex = 1.5, font = 2)
axis(2, pos = 1.2, )
par(las = 0)
mtext("Skipping Rate", side = 2, line = 2, cex = 1.5, font = 2)
x <- c(1.5, 2.5, 3.5)
points(x, c(RT.hf.sp, RT.lf.sp, RT.vlf.sp), cex = 1.5, lwd = 2, pch = 19)
plot.errbars <- plotsegraph(x, c(RT.hf.sp, RT.lf.sp, RT.vlf.sp), c(se.RT.hf.sp, 
                                                                   se.RT.lf.sp, se.RT.vlf.sp), 0.1, color = "black")  #0.1 = wiskwidth
lines(c(1.5, 2.5, 3.5), c(RT.hf.sp, RT.lf.sp, RT.vlf.sp), lwd = 2, type = "c")
points(x, c(RT.hf.ac, RT.lf.ac, RT.vlf.ac), cex = 1.5, lwd = 2, pch = 21)
plot.errbars <- plotsegraph(x, c(RT.hf.ac, RT.lf.ac, RT.vlf.ac), c(se.RT.hf.ac, 
                                                                   se.RT.lf.ac, se.RT.vlf.ac), 0.1, color = "black")  #0.1 = wiskwidth
lines(c(1.5, 2.5, 3.5), c(RT.hf.ac, RT.lf.ac, RT.vlf.ac), lwd = 2, type = "c")
points(1.5, 0.3, pch = 21, lwd = 2, cex = 1.5)
text(1.7, 0.3, "Invalid HF", cex = 1.2, font = 1, adj = 0)
points(1.5, 0.22, pch = 19, lwd = 2, cex = 1.5)
text(1.7, 0.22, "Target", cex = 1.2, font = 1, adj = 0)

dev.off()