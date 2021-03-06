library(ergmito)
library(texreg)
library(sna)
library(network)

data("fivenets")

# Plot of the networks
graphics.off()
pdf("figures/fivenets_graphs.pdf", width = 6, height = 4)
op <- par(mfrow = c(2, 3), mai=rep(.1, 4), oma = rep(0, 4))
# USCCARDINAL <- rgb(153, 0, 0, maxColorValue = 255)
ans <- lapply(fivenets, function(f) {
  gplot(
    f,
    vertex.cex = 2,
    vertex.col = c("black", "gray")[
      get.vertex.attribute(f, "female") + 1
      ]
  )
  box(lwd = 2, col="gray")
})
plot.new()
plot.window(xlim = c(0, 1), ylim = c(0, 1))
legend( 
  "center", fill = c("black", "gray"), legend = c("Male", "Female"),
  cex=1.5, bty="n", title = "Gender")
par(op)
dev.off()

set.seed(1)
model_10 <- ergmito(fivenets ~ edges)
model_01 <- ergmito(fivenets ~ nodematch("female"))
model_11 <- ergmito(fivenets ~ edges + nodematch("female"))

# Plot of diagnostics
graphics.off()
pdf("figures/fivenets_loglike.pdf", width = 6, height = 6)
plot(model_11, main = "", breaks = 100, extension = 10,
     xlab = 1, ylab = 2,
     params_labs = c(
       "nodematch.female" = expression(theta[Homophily~on~gender]),
       "edges"            = expression(theta[edges])
       ))
dev.off()

graphics.off()
pdf("figures/fivenets_gof.pdf", width = 6, height = 6)
plot(gof_ergmito(model_11), main = "")
dev.off()

# Need to generate the confint for printing
ci_11 <- confint(model_11)

library(lmtest)

TESTS <- list(
  lrtest(model_10, model_11),
  lrtest(model_01, model_11)
)

vals <- sapply(TESTS, "[[", "Pr(>Chisq)")[2, ]

TESTS <- sprintf(
  "%.2f %s",
  sapply(TESTS, "[[", "Chisq")[2, ],
  ifelse(vals < .001, "^{***}", ifelse(vals < .01, "^{**}", ifelse(vals < .05, "^*", "")))
  )

texreg(
  l  = list(
    Homopholy    = model_01,
    Edgecount    = model_10,
    `Full model` = model_11
    ),
  custom.coef.map = list(edges = "Edgecount", "nodematch.female" = "Homophily (on Gender)"),
  # ci.force = TRUE,
  caption  = paste(
    "Fitted ERGMitos using the fivenets dataset. Looking at AICs and LR-test",
    "statistics, the full model (last column of the table) is the one with the",
    "best fit to the observed data.",
    "More over, the 95\\% level CI of each covers the true parameters:",
    sprintf(
      "$\\hat\\theta_{edges} \\in [%.2f, %.2f]$; $\\hat\\theta_{Homophily} \\in [%.2f, %.2f]$.",
      ci_11["edges", "2.5 %"], ci_11["edges", "97.5 %"],
      ci_11["nodematch.female", "2.5 %"], ci_11["nodematch.female", "97.5 %"]
      )
  ),
  custom.gof.rows = list(
    `LR-test statistic ($\\chi^2$)` = c(TESTS, "")
  ),
  file = "fivenets_fit.tex"
  )




