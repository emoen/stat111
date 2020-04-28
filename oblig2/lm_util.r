Midparent <- c(66.0, 65.5, 71.5, 68.0, 70.0, 65.5, 67.0, 70.5, 69.5, 64.5, 67.5)
Daughter <- c(64.0, 63.0, 69.0, 69.0, 69.0, 65.0, 63.0, 68.5, 69.0, 64.0, 67.0)


## Lag figur
.x <- sample(x = 1:10, size = 11, replace = TRUE)
.y <- sample(x = 1:10, size = 11, replace = TRUE)
.main <- "Hovedtittel"
.xlab <- "Tekst langs x-aksen"
.ylab <- "Tekst langs y-aksen"
plot(x = .x, y = .y, type = "p", , las = 1,
main = .main, xlab = .xlab, ylab = .ylab)


## Rekn ut en lineær modell basert på observasjonene.
.linear_model_y_explained_by_x <- lm(formula = .y ~ .x)
## Ser på utskrift av
.linear_model_y_explained_by_x
##
1
STAT111
## Call:
## lm(formula = .y ~ .x)
##
## Coefficients:
## (Intercept) .x
## 3.8215 0.4057
## NB: Det er ikke opplagt fra dette at '.linear_model_y_explained_by_x'
## er en liste med følgende 12 komponenter.
names(.linear_model_y_explained_by_x)
## [1] "coefficients" "residuals" "effects" "rank"
## [5] "fitted.values" "assign" "qr" "df.residual"
## [9] "xlevels" "call" "terms" "model"
## Dette skyldes at R bruker en spesiell print-metode for å vise resultatet.
## Bruk 'str(.linear_model_y_explained_by_x)' for å se alle detaljene, og for
## å se hvorfor følgende kode gir stigningstall 'a' og konstantledd 'b'.
a <- .linear_model_y_explained_by_x$coefficients[2]
b <- .linear_model_y_explained_by_x$coefficients[1]
## Et mer direkte alternativ for å hente ut koeffisientene er:
coef(.linear_model_y_explained_by_x)
## (Intercept) .x
## 3.8215010 0.4056795
## Fra dette kan vi definere følgende funksjon
.regresjonslinje <- function(x) {a * x + b}
## Som for en gitt innverdi (oppdater til den veriden oppgaven spør om) gir oss
.inn_verdi <- 5
.verdi <- .regresjonslinje(.inn_verdi)
