library(ergmito)
library(texreg)

data("fivenets")

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
