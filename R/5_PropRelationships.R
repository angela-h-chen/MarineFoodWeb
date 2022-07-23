
library(ggplot2)

# Load data ----

load(file="Data/all_igraph.rda")
load(file="Results/comp_str_stab.rda")

# Exploratory plots ----

# Stability (QSS) vs Connectance
ggplot(fw_results, aes(x = Connectance, y = MEing)) +
  geom_point() +
  labs(x = "Connectance", y = "QSS (mean MaxEigen)") +
  theme_classic()

# Stability (QSS) vs Latitude
ggplot(fw_results, aes(x = Latitude, y = MEing)) +
  geom_point() +
  geom_smooth(method="lm") +
  labs(x = "Latitude", y = "QSS (mean MaxEigen)") +
  theme_classic()


# Quantile regression ----

library(SparseM)
library(quantreg)

summary(fw_results)

attach(fw_results)
datatable <- data.frame(Size, Omnivory, Links, Connectance, PathLength, Clustering, 
                        TLmean, Vulnerability, Generality, Modularity, MEing, Latitude)

cor(datatable)
pairs(datatable, col="blue", main="Scatterplots")

Y=cbind(MEing)
X=cbind(Latitude)  # Connectance, PathLength, TLmean, Modularity, 

hist(Y, prob=TRUE, col = "blue", border = "black")
lines(density(Y))

OLSreg=lm(Y~X)
summary(OLSreg)

Qreg25=rq(Y~X, tau=0.25)
summary(Qreg25)

Qreg75=rq(Y~X, tau=0.75)
summary(Qreg75)

Qreg90=rq(Y~X, tau=0.9)
summary(Qreg90)

anova(Qreg25, Qreg75)

QR_2575 <- rq(Y~X, tau=c(0.25, 0.75))

sumQR <- summary(QR)
sumQR_2575_boot <- summary(QR_2575, se = "boot")

plot(sumQR_2575_boot)


# Create scatterplot with quantile regression line

q10 <- seq(0.05, 0.95, by = 0.05)
ggplot(fw_results, aes(Latitude,MEing)) +
  geom_point() + 
  geom_quantile(colour = "red", size = 2, alpha = 0.5) +
  geom_smooth(method="lm", se=F)


# GLM model ----

library(performance)

model <- glm(MEing ~ Latitude, data = fw_results)
check_model(model)
model_performance(model)
r2(model)