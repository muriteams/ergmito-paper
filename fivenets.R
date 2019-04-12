library(ergmito)
library(texreg)
library(sna)
library(network)

data("fivenets")

# Plot of the networks
graphics.off()
pdf("figures/fivenets_graphs.pdf", width = 6, height = 4)
op <- par(mfrow = c(2, 3), mai=rep(.1, 4), oma = rep(0, 4))
USCCARDINAL <- rgb(153, 0, 0, maxColorValue = 255)
ans <- lapply(fivenets, function(f) {
  gplot(
    f,
    vertex.cex = 2,
    vertex.col = c("white", USCCARDINAL)[
      get.vertex.attribute(f, "female") + 1
      ]
  )
  box(lwd = 2, col="gray")
})
plot.new()
plot.window(xlim = c(0, 1), ylim = c(0, 1))
legend(
  "center", fill = c("white", USCCARDINAL), legend = c("Male", "Female"),
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
plot(model_11, main = "", breaks = 100, extension = 10)
dev.off()

graphics.off()
pdf("figures/fivenets_gof.pdf", width = 6, height = 6)
plot(gof_ergmito(model_11), main = "")
dev.off()
