#Corre bamm con la siguiente linea de comando "./bamm -c traitcontrol.txt" en tu teminal de linux, mac o msdos.
#Luego copia pega todos los archivos de salida y pegalos en la carpeta de este proyecto de R.
#Ahora vamos a procesar los datos con el paquete de R BAMMtools.


#phenotype evolution
mytree <- read.tree("whaletree.tre")
mcmcout <- read.csv("./pheno/mcmc_out.txt")
## create edata
phenoedata <- getEventData(mytree, eventdata = "./pheno/event_data.txt", burnin=0.15, type = "trait")
phenoedata
#### Check convergence
plot(mcmcout$logLik ~ mcmcout$generation)

burnstart <- floor(0.15 * nrow(mcmcout))
phenopostburn <- mcmcout[burnstart:nrow(mcmcout), ]

effectiveSize(phenopostburn$N_shifts)
effectiveSize(phenopostburn$logLik)



### Shift probabilities
phenoshift_probs <- summary(phenoedata)
phenoshift_probs

### Bayes factors
phenobfmat <- computeBayesFactors(phenopostburn, expectedNumberOfShifts=10, burnin=0.15)
phenobfmat

#### PLOT CREDIBLE SHIFTS
phenocss <- credibleShiftSet(phenoedata, expectedNumberOfShifts=10, threshold=5, set.limit = 0.95)
phenocss

plot.credibleshiftset(phenocss)


### PLOT BEST SHIFT
par(mfrow=c(1,1))
phenobest <- getBestShiftConfiguration(phenoedata, expectedNumberOfShifts=10)
phenobest
plot.bammdata(phenobest, lwd = 2,spex = "netdiv",label=T,cex=0.5)
axisPhylo()
addBAMMshifts(phenobest, cex=3)
phenobest$eventData

phenophylorates <- plot(phenoedata, breaksmethod='jenks', show = FALSE)
ratesHistogram(phenophylorates, plotBrks = FALSE, xlab = 'size evolution rates')

