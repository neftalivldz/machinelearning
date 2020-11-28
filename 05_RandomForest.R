

##################
## randomForest
library(bitops)
library(randomForest)
library(RCurl)

x <- getURL("http://people.hbs.edu/mtoffel/datasets/rsei212/rsei212_chemical.txt")
raw.orig  <- read.csv(text = x, header=T, sep="\t")

# Keep the dataset small and tidy
# The Dataverse: hdl:1902.1/21235

raw = subset(raw.orig, select=c("Metal","OTW","AirDecay","Koc"))
head(raw, n=10)
row.names(raw) = raw.orig$CASNumber
raw = na.omit(raw);

frmla = Metal ~ OTW + AirDecay + Koc
 
# Metal: Core Metal (CM); Metal (M); Non-Metal (NM); Core Non-Metal (CNM)
#Air Decay (1/hr) The rate at which a chemical degrades in air, due 
#primarily to photooxidation by radicals (hr-1). 
#Oral Tox Weight
#The TRI Indicator toxicity weight for a chemical for the oral pathway. 
#Koc (mL/g) The organic carbon-water partition coefficient, used in estimates of
#chemical sorption to soil (mL/g).

summary(raw)


library(randomForest)
fit.rf = randomForest(frmla, data=raw)
print(fit.rf)
importance(fit.rf)
plot(fit.rf)
plot( importance(fit.rf), lty=2, pch=16)
lines(importance(fit.rf))
imp = importance(fit.rf)
impvar = rownames(imp)[order(imp[, 1], decreasing=TRUE)]
op = par(mfrow=c(1, 3))
for (i in seq_along(impvar)) {
partialPlot(fit.rf, raw, impvar[i], xlab=impvar[i],
main=paste("Partial Dependence on", impvar[i]),
ylim=c(0, 1))
}

