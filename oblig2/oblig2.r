#12.2.16

## Definer vektorene
Midparent <- c(66.0, 65.5, 71.5, 68.0, 70.0, 65.5, 67.0, 70.5, 69.5, 64.5, 67.5)
Daughter <- c(64.0, 63.0, 69.0, 69.0, 69.0, 65.0, 63.0, 68.5, 69.0, 64.0, 67.0)

# a) "Make a scatter plot of daughters height against the midparent height and comment on the strength of the relationship
# Skriv en omtale av delspørsmålet, oppdater koden for plottet, og fyll inn din konklusjon. 
## Lag figur
.x <- Midparent
.y <- Daughter
.main <- "Scatterplot daugher vs midparent height (y/x)"
.xlab <- "Midparent x-aksen"
.ylab <- "Daughter y-aksen"
plot(x = .x, y = .y, type = "p", , las = 1, main = .main, xlab = .xlab, ylab = .ylab)

# b) Is the daughters height completely and uniquely determined by the midparent height? Explain.
# c) Use the the accompanying MINITAB output to obtain the equation of the least squares line
# for prediting daugher height from midparent height, and then predict the height of daughter whose midparent heigtht is 70 in. 
# Would you feel comfortable, using the least squares line to predict daughter height when midparent height is 74in? Explain.
# d) 
# e)
