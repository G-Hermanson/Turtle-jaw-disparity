#Analysis using discrete characters

library(ape)
library(Claddis)
library(cluster)

turtle_mat <- read_nexus_matrix('Matrix_extant_prune.nex')
turtle_mat <- data.frame(apply( turtle_mat$matrix_1$matrix , 2,as.numeric),
                         row.names = rownames(turtle_mat$matrix_1$matrix))
colnames(turtle_mat) <- 1:ncol(turtle_mat)
head(turtle_mat)

#Distance matrix to use as input in PCoA
turtle_dist <- as.matrix( daisy(turtle_mat , metric='gower'))

#Run PCoA function
turtle_pcoa <- pcoa(turtle_dist)
scree.data <- apply(turtle_pcoa$vectors, 2, var) / sum(apply(turtle_pcoa$vectors, 2, var)) * 100

biplot.pcoa(turtle_pcoa,Y = turtle_mat,cex=0.5)


#A few plots
par(mfrow=c(2,2))

#Clade
plot(turtle_pcoa$vectors[rownames(dat),1:2], 
     pch=ifelse(dat$Clade=='Chelidae' | dat$Clade=='Pelomedusoides',24,21), cex=1.5,
     bg=c('#aa4499','darkblue','#66ccee','lightgreen','#999933',
          '#ee6677','forestgreen','#eecc66') [as.numeric(as.factor(dat$Clade))]
)
title('Clade')
text(turtle_pcoa$vectors[rownames(dat),1:2], cex=0.35 
     #,lab=rownames(dat)
     ,col=ifelse(dat$Clade=='Chelonioidea','white','black'))
legend('topright',pch = c(24,21),bty='n',legend=c('pleurodires','cryptodires'),
       cex=0.75, ncol=1,pt.cex=1.2,pt.bg='black')

#Habitat
plot(turtle_pcoa$vectors[rownames(dat),1:2], 
     pch=ifelse(dat$Terrestrial_feeding=='yes',22,21), cex=1.5,
     bg=c('royalblue','lawngreen','lightsalmon') [as.numeric(as.factor(dat$habitat))]
)
title('Habitat')
text(turtle_pcoa$vectors[rownames(dat),1:2], cex=0.35
     #,lab=rownames(dat)
     ,col=ifelse(dat$habitat=='freshwater','white','black'))
legend('topright',pt.bg=c('lightsalmon','royalblue','lawngreen'),pch=21,pt.cex=1.2,
       bty='n',legend=c('terr','freshw','marine') )
legend('topleft',pch = c(21,22),bty='n',legend=c('no','yes'),
       title='terrestrial feeding',cex=0.75,
       ncol=2,pt.cex=1.2)

#Primary diet
plot(turtle_pcoa$vectors[rownames(dat),1:2], 
     pch=ifelse(dat$Terrestrial_feeding=='yes',22,21), cex=1.5,
     bg=c('hotpink','black','lemonchiffon') [as.numeric(as.factor(dat$primary_diet))]
)
title('Primary diet')
text(turtle_pcoa$vectors[rownames(dat),1:2], cex=0.35
     #,lab=rownames(dat)
     ,col=ifelse(dat$primary_diet=='herbivorous','white','black'))
legend('topright',pt.bg=c('black','lemonchiffon','hotpink'),pch=21,pt.cex=1.2,
       bty='n',legend=c('herb','omniv','carniv') )
legend('topleft',pch = c(21,22),bty='n',legend=c('no','yes'),
       title='terrestrial feeding',cex=0.75,
       ncol=2,pt.cex=1.2)

#Specialized diet
plot(turtle_pcoa$vectors[rownames(dat),1:2], 
     pch=ifelse(dat$Terrestrial_feeding=='yes',22,21), cex=1.5,
     bg=c('red','lemonchiffon','black','dodgerblue') [as.numeric(as.factor(dat$specialized_diet))]
)
title('Specialized diet')
text(turtle_pcoa$vectors[rownames(dat),1:2], cex=0.35
     #,lab=rownames(dat)
     ,col=ifelse(dat$specialized_diet=='high_fiber' | dat$specialized_diet=='suction','white','black'))
legend('topright',pt.bg=c('lemonchiffon','black','red','dodgerblue'),pch=21,pt.cex=1.2,
       bty='n',legend=c('gen','high_f','durop','suct') )
legend('topleft',pch = c(21,22),bty='n',legend=c('no','yes'),
       title='terrestrial feeding',cex=0.75,
       ncol=2,pt.cex=1.2)

dev.off()

#Disparity analysis - Clades

clade_disparity_disc <- custom.subsets(turtle_pcoa$vectors,group = clades)
set.seed(1)
clade_disparity_disc <- boot.matrix(clade_disparity_disc,bootstraps = 1000)
clade_disparity_disc <- dispRity(clade_disparity_disc,metric = c(sum,variances))

plot(clade_disparity_disc, col = c('blue','red','#aa4499','#ee6677','#eecc66','darkblue','#66ccee',
                              'lightgreen','forestgreen','#999933'),cex.axis=0.5)

test.dispRity(clade_disparity_disc,test = wilcox.test,correction = 'bonferroni')

#Disparity analyses - primary diets
prim_diet_disparity_disc <- custom.subsets(turtle_pcoa$vectors,group = prim_diets)
set.seed(1)
prim_diet_disparity_disc <- boot.matrix(prim_diet_disparity_disc,bootstraps = 1000)
prim_diet_disparity_disc <- dispRity(prim_diet_disparity_disc,metric = c(sum,variances))

plot(prim_diet_disparity_disc, col = c('hotpink','lemonchiffon','grey20'))

test.dispRity(prim_diet_disparity_disc,test = wilcox.test,correction = 'bonferroni')

#Disparity analyses - specialized diets
spec_diet_disparity_disc <- custom.subsets(turtle_pcoa$vectors,group = spec_diets)
set.seed(1)
spec_diet_disparity_disc <- boot.matrix(spec_diet_disparity_disc,bootstraps = 1000)
spec_diet_disparity_disc <- dispRity(spec_diet_disparity_disc,metric = c(sum,variances))

plot(spec_diet_disparity_disc, col = c('lemonchiffon','grey20','red','dodgerblue'))

test.dispRity(spec_diet_disparity_disc,test = wilcox.test,correction = 'bonferroni',
              comparisons = 'referential')

#Disparity analyses - terrestrial feeding
terr_feed_disparity_disc <- custom.subsets(turtle_pcoa$vectors,group = terr_feed)
set.seed(1)
terr_feed_disparity_disc <- boot.matrix(terr_feed_disparity_disc,bootstraps = 1000)
terr_feed_disparity_disc <- dispRity(terr_feed_disparity_disc,metric = c(sum,variances))

plot(terr_feed_disparity_disc, col = c('lemonchiffon','grey20'))

test.dispRity(terr_feed_disparity_disc,test = wilcox.test,correction = 'bonferroni')
