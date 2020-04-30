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
# Usikkerheten i prediksjonen kan beskrives med r^2.
# Prediksjonen har høy usikkerhet r^2=0.723

# c) Use the the accompanying MINITAB output to obtain the equation of the least squares line
# for prediting daugher height from midparent height, and then predict the height of daughter whose midparent heigtht is 70 in. 
# Would you feel comfortable, using the least squares line to predict daughter height when midparent height is 74in? Explain.

# 1.c svar)

#least squares line:
regline <- lm(Daughter ~ Midparent)
abline(regline)

b0 = regline$coef[1]
b1 = regline$coef[2]
#> b0
#(Intercept) 
#   1.649748 
#> b1
#Midparent 
#0.9555369 

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

# Prediksjonen har høy usikkerhet fordi variasjonen er høy siden r^2=0.723
# dvs at error sum of squares er 72.3% mindre med b1x+b0 enn en horisontal linje.

# d) What are the values of SSE, SST, and the coefficient of determination? How well does
#the midparent height account for dauthers height? 

# 1 d)Svar: Man kan komfortabelt bruke midparent høyde for å estimere
# datters høyde. Fra boken ser man:
############################
#SSE=18.938
#SST=68.409
#coefficient of determination is r^2=0.723
############################

# fra r finner man:
###########################
SSE = sum(regline$resid^2 )
SST = sum((Daughter-mean(Daughter))^2)
#18.93834
#68.40909

r_squared = 1-(SSE/SST)
#0.7231605
regline$df.residual # 9

# e) Notice that for most of the families the midparent height exceeds the daughter height. 
# Is this what is meant by regression to the mean? Explain

# 1 e) svar:
# mu(Y|X=x) - viss X, Y har samme std - vil datters betingete høyde være nærme mu(X)
# enn mu(Y) - midparent.
# Fra boken side 260: 
#"In general, the conditional expected Y is closer when it is measured in terms of
#standard deviations. One can think of the conditional expectation as being pulled
#back toward the mean, and that is why Galton called this regression to the mean."
# 
> mean(Daughter)
[1] 66.40909
> mean(Midparent)
[1] 67.77273

#Siden mean midparent er høyere enn Daughter stemmer utsagnet.

###################################################################

# Oppg 71 fra kap 12.6 - side 680
# 1. Skriv en omtale av oppgaven

# Svar 1: En linear regression forutsetter at residue elementet er normalfordelt.
# Dette er en forutsetning som maa undersokes.
# Ikke-normalfordelt viss: ikke-linear relasjon, ikke konstant varianse, obervasjoner uten forklaring,
# store utliggere, avhengighet i feil/residue, der er flere variabler
# 2. a) 
## Definer vektorene
x <- c(0, 7, 17, 114, 133, 142, 190, 218, 237, 285)
y <- c(20.3, 19.8, 19.5, 15.9, 15.1, 14.7, 11.9, 11.5, 8.3, 6.6)
.x <- x
.y <- y
.lin_mod_y_vs_x <- lm(formula = .y ~ .x)
.residuals <- .lin_mod_y_vs_x$residuals
## Et mer direkte alternativ få å hente ut residualene er:
residuals(.lin_mod_y_vs_x)
## 1 2 3 4 5 6
## -4.9553265 1.9673540 5.3264605 0.4037801 -2.3917526 -2.0326460
## 7 8 9 10 11
## -3.0326460 -0.4690722 5.9673540 -1.1099656 0.3264605
## Lag et plott som viser '.x' versus '.residuals'
# dette er: 3. ei on the vertical axis versus xi on the horizontal axis
.main <- "plot 3: (x, e_i) x mot residuals"
.xlab <- "oxide-layer thickness (nm)"
.ylab <- "Residual"
plot(x = .x, y = .residuals, type = "p", , las = 1, main = .main, xlab = .xlab, ylab = .ylab)
## Legg til horisontal linje med skjaeringspunkt '0'
abline(h = 0, col = "red", lty = 2)

# "The residuals should be randomly distributed about
# 0 according to a normal distribution, so all but a very few standardized residuals
# should lie between -2 and +2" (side 676)

# Svar: der er 5 punkter over linjen og 5 punkter under. Alle punktene er mellom ca -1 og 1.
# Der er 2 klustere, men ellers så ser punktene tilfeldige.
# Alle punkt utenom ett ligger mellom -1 og 1. 
# Siden boker sier at punktene skal ligge mellom -2 og 2 så er dette punktet ikke en utligger.
#
# y = 20.6 + .047x som er least squares fit for linear regression - ser ut som en god
# tilnærming til problemet.

# b)
## Bruk 'rstandard' til å regne ut de standardiserte residualene.
.standard_res <- rstandard(model = .lin_mod_y_vs_x)
## Lag et plott som viser '.x' versus '.standard_res'
# dette er: 4. ei*  on the vertical axis versus xi on the horizontal axis
.main <- "plot 3: (x, e_i*) x mot residuals"
.xlab <- "oxide-layer thickness (nm)"
.ylab <- "Standardized Residual"
plot(x = .x, y = .standard_res, type = "p", , las = 1, main = .main, xlab = .xlab, ylab = .ylab)
## Legg til horisontal linje med skjæringspunkt '0'
abline(h = 0, col = "red", lty = 2)
## Lag et normal-sannsynsplott for de standardiserte residualene.
.main <- "quantil vs quantil - test for normal-plot"
.xlab <- "quantil x-aksen"
.ylab <- "quantil y-aksen"
qqnorm(.standard_res, las = 1,main = .main,xlab = .xlab,ylab = .ylab)
## Legg til linje gjennom første og tredje kvartil.
qqline(.standard_res, col = "red", lty = 2)
library("car")
qqPlot(.standard_res$)

#fortsatt er punktene mellom -2 og 2. Fra normalplottet forventer man at punktene
#ligger på en rett linje y=x som man ser fra plottet at de gjør.
#man kan derfor forutsette at residualene kommer fra en normalfordeling og
#en linaer regression vil passe.

###################################################################
# Oppg 4 fra kap 14.1 - side 764
# 1. Skriv omtale av oppgaven:
# Svar: Vi skal bruke uparameterisert prosedyre til å test en hypotese. 
# Metoden er wilcoxon signed-rank og brukes til paa en distribusjon:
# som er: 1. kontinuerlig 2. H_0 forutsetter at mean=meadian dvs H_0: mu=mu0 => symmetrisk distribusjon

## Skriv inn observasjonene:
observasjoner <- c(30.6, 30.1, 15.6, 26.7, 27.1, 25.4, 35.0, 30.8,31.9, 53.2, 12.5, 23.2, 8.8, 24.9, 30.2)
## Gjør relevante endringer av koden under, så du får svart på spørsmålet.
.x <- observasjoner
## Definer mu som skal brukes i testen, og regn ut test-observatoren:
.mu0 <- 30
.centered <- .x - .mu0
.signed_ranks <- sign(.centered) * rank(abs(.centered))
s_pluss <- sum(.signed_ranks[.signed_ranks > 0])
## Finc den kritiske verdien 'c1' for dette tilfellet.
## (Formelen under gjennskaper tabell A.12 på side 809.)
.level <- 0.1
.c1 <- 1 + qsignrank(p = .level, n = length(.x), lower.tail = FALSE)

# H_0: mu=mu0, H_a: mu0 < mu 
# mu0 = 30 => H_0: mu=30, H_a: mu < 30 => test for s_+ <= c2 som er ensidig test
# c2 = n(n+1)/2 - c1
n= 15
.c2 = (15*16)/2 - .c1
# Resultat 39=s_pluss < c2=36 == False  => vi beholder H_0, og 
#konkluderer mu er ikke mindre enn 30 ved alpha 0.1

###################################################################
# Oppg 12 fra kap 14.2 - side 764
# 1. Skriv omtale av oppgaven:
# Vi skal bruke uparameterisert prosedyre til å test en hypotese. 
# Denne gangen er det wilcoxon rank-sum og brukes til aa sammenligne 
# mean paa 2  distribusjon som er: 
#1. kontinuerlig 2. fra identisk distribusjon 
#H_0 forutsetter at mean=meadian dvs H_0: mu=mu0 => symmetrisk distribusjon

## Definer vektorene
Unexposed <- c( 8, 11, 12, 14, 20, 43, 111)
Exposed <- c(35, 56, 83, 92, 128, 150, 176, 208)
## Registrer lengdene
m <- min(length(Unexposed), length(Exposed)) ## Se merknad før eksempel 14.3
n <- max(length(Unexposed), length(Exposed)) ## (side 768) mht valg av m og n.
## Gjør relevante endringer av koden under, så du får svart på spørsmålet.
.x <- runif(n = m) ## Test-vektor for å vise koden.
.y <- runif(n = n) ## Test-vektor for å vise koden.
Delta <- -0.3 ## Dette er verdien 'mu1 - mu2' har under H0.
## Regn ut test-observatoren.
w <- sum(rank(c(.x - Delta, .y))[1:m])
## Kritisk verdi: Her hentet fra tabell A.13, side 810.
.c1 <- 71
## Merk: I R ville vi normalt brukt 'wilcox.test' for denne oppgaven, og det er ingen
## enkel formel som direkte reproduserer tabell A.13.




