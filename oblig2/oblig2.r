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

# 1.a) Svar:
# Viss der er en relasjon så bør der finnes en (linear) regression ax+b som følger trenden av punktene i scatter-plottet.
# Fra figuren ser vi en klar trend, men det ser også ut som der er mye støy. Dvs variansen - epsilon fra y= ax+b+epsilon er stor.
# punktene ligger ikke på en rett linje som man hadde forventet om epsilon var liten.

# b) Is the daughters height completely and uniquely determined by the midparent height? Explain.
# 1.b) Svar: 
# som beskrevet i b) epsilon er ikke liten. Der man forventer at epsilon er en rv fra en normalfordelt fordeling.

# c) Use the the accompanying MINITAB output to obtain the equation of the least squares line
# for prediting daugher height from midparent height, and then predict the height of daughter whose midparent heigtht is 70 in. 
# Would you feel comfortable, using the least squares line to predict daughter height when midparent height is 74in? Explain.

# 1.c svar)

#least squares line:
regline <- lm(Daughter ~ Midparent)
abline(regline)

b0 = regline$coef[1]
b1 = regline$coef[2]

y = b1*70 + b0
y
#Midparent 
# 68.53733 

y = b1*74 + b0
y
#Midparent 
# 72.35948 

#Viss datter er 70 in => midparent 68.5
#Viss datter er 74 in => midparent 72.36


# d) 
# e)
