##
set.seed(1)

#load packages
library(dispRity)

#Read data
dat <- read.csv('data_clean.csv',header = T, row.names = 1, sep=',')
head(dat)

#Calculate mandible length vs mandible width ratio (Ch 1), and store it in the dataset
dat$Ch1_LWratio <- dat$Mandible_length/dat$Mandible_width

#Reorder clades so that chelids and pelomedusoids are plotted next to one another in the clades boxplot below

dat$Clade <- as.factor(dat$Clade)
  dat$Clade <- factor(dat$Clade, 
                      levels= c('Chelidae','Pelomedusoides','Trionychia','Chelonioidea','Chelydroidea',
                                'Emysternia','Testudinidae','Geoemydidae')  )


#A few boxplots
#Anterior and posterior mechanical advantages
par(mfrow=c(2,3))
boxplot(dat$Ch2_AMA~dat$primary_diet, col =c('hotpink','darkgrey','lemonchiffon') ,
        cex.axis=0.5)
legend('topright',fill=c('hotpink','black','lemonchiffon'),bty='n',
       legend=c('carn','herb','omni'))
boxplot(dat$Ch2_AMA~dat$specialized_diet,col=c('red','lemonchiffon','darkgrey','dodgerblue'),
        cex.axis=0.5)
legend('topleft',fill=c('red','lemonchiffon','black','dodgerblue'),bty='n',
       legend=c('duroph','gen','high_fib','suct'))
boxplot(dat$Ch2_AMA~dat$habitat,col=c('royalblue','lawngreen','lightsalmon'),
        cex.axis=0.5)
legend('topright',fill=c('royalblue','lawngreen','lightsalmon'),bty='n',
       legend=c('freshw','marine','terr'))

boxplot(dat$Ch3_PMA~dat$primary_diet, col =c('hotpink','darkgrey','lemonchiffon') ,
        cex.axis=0.5)
legend('topright',fill=c('hotpink','black','lemonchiffon'),bty='n',
       legend=c('carn','herb','omni'))
boxplot(dat$Ch3_PMA~dat$specialized_diet,col=c('red','lemonchiffon','darkgrey','dodgerblue'),
        cex.axis=0.5)
legend('topleft',fill=c('red','lemonchiffon','black','dodgerblue'),bty='n',
       legend=c('duroph','gen','high_fib','suct'))
boxplot(dat$Ch3_PMA~dat$habitat,col=c('royalblue','lawngreen','lightsalmon'),
        cex.axis=0.5)
legend('topright',fill=c('royalblue','lawngreen','lightsalmon'),bty='n',
       legend=c('freshw','marine','terr'))

#Opening mechanical advatnage and second moment of inertia
par(mfrow=c(2,3))
boxplot(dat$Ch4_OMA~dat$primary_diet, col =c('hotpink','darkgrey','lemonchiffon') ,
        cex.axis=0.5)
legend('topright',fill=c('hotpink','black','lemonchiffon'),bty='n',
       legend=c('carn','herb','omni'))
boxplot(dat$Ch4_OMA~dat$specialized_diet,col=c('red','lemonchiffon','darkgrey','dodgerblue'),
        cex.axis=0.5)
legend('topleft',fill=c('red','lemonchiffon','black','dodgerblue'),bty='n',
       legend=c('duroph','gen','high_fib','suct'))
boxplot(dat$Ch4_OMA~dat$habitat,col=c('royalblue','lawngreen','lightsalmon'),
        cex.axis=0.5)
legend('topright',fill=c('royalblue','lawngreen','lightsalmon'),bty='n',
       legend=c('freshw','marine','terr'))

boxplot(dat$Ch5_SMOI~dat$primary_diet, col =c('hotpink','darkgrey','lemonchiffon') ,
        cex.axis=0.5)
boxplot(dat$Ch5_SMOI~dat$specialized_diet,col=c('red','lemonchiffon','darkgrey','dodgerblue'),
        cex.axis=0.5)
boxplot(dat$Ch5_SMOI~dat$habitat,col=c('royalblue','lawngreen','lightsalmon'),
        cex.axis=0.5)
legend('bottomright',fill=c('royalblue','lawngreen','lightsalmon'),bty='n',
       legend=c('freshw','marine','terr'))


#Mandible length/width ratio and relative width of the triturating surface
par(mfrow=c(2,3))
boxplot(dat$Ch1_LWratio~dat$primary_diet, col =c('hotpink','darkgrey','lemonchiffon') ,
        cex.axis=0.5)
legend('topright',fill=c('hotpink','black','lemonchiffon'),bty='n',
       legend=c('carn','herb','omni'))
boxplot(dat$Ch1_LWratio~dat$specialized_diet,col=c('red','lemonchiffon','darkgrey','dodgerblue'),
        cex.axis=0.5)
legend('top',fill=c('red','lemonchiffon','black','dodgerblue'),bty='n',
       legend=c('duroph','gen','high_fib','suct'))
boxplot(dat$Ch1_LWratio~dat$habitat,col=c('royalblue','lawngreen','lightsalmon'),
        cex.axis=0.5)
legend('topright',fill=c('royalblue','lawngreen','lightsalmon'),bty='n',
       legend=c('freshw','marine','terr'))

boxplot(dat$Ch6_TRIT~dat$primary_diet, col =c('hotpink','darkgrey','lemonchiffon') ,
        cex.axis=0.5)
legend('topright',fill=c('hotpink','black','lemonchiffon'),bty='n',
       legend=c('carn','herb','omni'))
boxplot(dat$Ch6_TRIT~dat$specialized_diet,col=c('red','lemonchiffon','darkgrey','dodgerblue'),
        cex.axis=0.5)
legend('topright',fill=c('red','lemonchiffon','black','dodgerblue'),bty='n',
       legend=c('duroph','gen','high_fib','suct'))
boxplot(dat$Ch6_TRIT~dat$habitat,col=c('royalblue','lawngreen','lightsalmon'),
        cex.axis=0.5)
legend('topright',fill=c('royalblue','lawngreen','lightsalmon'),bty='n',
       legend=c('freshw','marine','terr'))


#Test differences between measurements and categories
#Anterior mechanical advantage (AMA)
#primary diet
pairwise.t.test(x=dat$Ch2_AMA,g = dat$primary_diet,p.adjust.method = 'bonferroni')
write.table(pairwise.t.test(x=dat$Ch2_AMA,g = dat$primary_diet,p.adjust.method = 'bonferroni')$p.value,
            file='AMA_ttest_primarydiet.txt')
#specialized diet
pairwise.t.test(x=dat$Ch2_AMA,g = dat$specialized_diet,p.adjust.method = 'bonferroni')
write.table(pairwise.t.test(x=dat$Ch2_AMA,g = dat$specialized_diet,p.adjust.method = 'bonferroni')$p.value,
            file='AMA_ttest_specializeddiet.txt')
#habitat
pairwise.t.test(x=dat$Ch2_AMA,g = dat$habitat,p.adjust.method = 'bonferroni')
write.table(pairwise.t.test(x=dat$Ch2_AMA,g = dat$habitat,p.adjust.method = 'bonferroni')$p.value,
            file='AMA_ttest_habitat.txt')
#clade-wise
pairwise.t.test(x=dat$Ch2_AMA,g = dat$Clade,p.adjust.method = 'bonferroni')
write.table(pairwise.t.test(x=dat$Ch2_AMA,g = dat$Clade,p.adjust.method = 'bonferroni')$p.value,
            file='AMA_ttest_taxonomy.txt')


#Posterior mechanical advantage (PMA)
#primary diet
pairwise.t.test(x=dat$Ch3_PMA,g = dat$primary_diet,p.adjust.method = 'bonferroni')
write.table(pairwise.t.test(x=dat$Ch3_PMA,g = dat$primary_diet,p.adjust.method = 'bonferroni')$p.value,
            file='PMA_ttest_primarydiet.txt')
#specialized diet
pairwise.t.test(x=dat$Ch3_PMA,g = dat$specialized_diet,p.adjust.method = 'bonferroni')
write.table(pairwise.t.test(x=dat$Ch3_PMA,g = dat$specialized_diet,p.adjust.method = 'bonferroni')$p.value,
            file='PMA_ttest_specializeddiet.txt')
#habitat
pairwise.t.test(x=dat$Ch3_PMA,g = dat$habitat,p.adjust.method = 'bonferroni')
write.table(pairwise.t.test(x=dat$Ch3_PMA,g = dat$habitat,p.adjust.method = 'bonferroni')$p.value,
            file='PMA_ttest_habitat.txt')
#clade-wise
pairwise.t.test(x=dat$Ch3_PMA,g = dat$Clade,p.adjust.method = 'bonferroni')
write.table(pairwise.t.test(x=dat$Ch3_PMA,g = dat$Clade,p.adjust.method = 'bonferroni')$p.value,
            file='PMA_ttest_taxonomy.txt')


#Opening mechanical advantage (OMA)
#primary diet
pairwise.t.test(x=dat$Ch4_OMA,g = dat$primary_diet,p.adjust.method = 'bonferroni')
write.table(pairwise.t.test(x=dat$Ch4_OMA,g = dat$primary_diet,p.adjust.method = 'bonferroni')$p.value,
            file='OMA_ttest_primarydiet.txt')
#specialized diet
pairwise.t.test(x=dat$Ch4_OMA,g = dat$specialized_diet,p.adjust.method = 'bonferroni')
write.table(pairwise.t.test(x=dat$Ch4_OMA,g = dat$specialized_diet,p.adjust.method = 'bonferroni')$p.value,
            file='OMA_ttest_specializeddiet.txt')
#habitat
pairwise.t.test(x=dat$Ch4_OMA,g = dat$habitat,p.adjust.method = 'bonferroni')
write.table(pairwise.t.test(x=dat$Ch4_OMA,g = dat$habitat,p.adjust.method = 'bonferroni')$p.value,
            file='OMA_ttest_habitat.txt')
#clade-wise
pairwise.t.test(x=dat$Ch4_OMA,g = dat$Clade,p.adjust.method = 'bonferroni')
write.table(pairwise.t.test(x=dat$Ch4_OMA,g = dat$Clade,p.adjust.method = 'bonferroni')$p.value,
            file='OMA_ttest_taxonomy.txt')

#Second moment of inertia (SMOI)
#primary diet
pairwise.t.test(x=dat$Ch5_SMOI,g = dat$primary_diet,p.adjust.method = 'bonferroni')
write.table(pairwise.t.test(x=dat$Ch5_SMOI,g = dat$primary_diet,p.adjust.method = 'bonferroni')$p.value,
            file='SMOI_ttest_primarydiet.txt')
#specialized diet
pairwise.t.test(x=dat$Ch5_SMOI,g = dat$specialized_diet,p.adjust.method = 'bonferroni')
write.table(pairwise.t.test(x=dat$Ch5_SMOI,g = dat$specialized_diet,p.adjust.method = 'bonferroni')$p.value,
            file='SMOI_ttest_specializeddiet.txt')
#habitat
pairwise.t.test(x=dat$Ch5_SMOI,g = dat$habitat,p.adjust.method = 'bonferroni')
write.table(pairwise.t.test(x=dat$Ch5_SMOI,g = dat$habitat,p.adjust.method = 'bonferroni')$p.value,
            file='SMOI_ttest_habitat.txt')
#clade-wise
pairwise.t.test(x=dat$Ch5_SMOI,g = dat$Clade,p.adjust.method = 'bonferroni')
write.table(pairwise.t.test(x=dat$Ch5_SMOI,g = dat$Clade,p.adjust.method = 'bonferroni')$p.value,
            file='SMOI_ttest_taxonomy.txt')

#Mandible length-width ratio
#primary diet
pairwise.t.test(x=dat$Ch1_LWratio,g = dat$primary_diet,p.adjust.method = 'bonferroni')
write.table(pairwise.t.test(x=dat$Ch1_LWratio,g = dat$primary_diet,p.adjust.method = 'bonferroni')$p.value,
            file='LWRatio_ttest_primarydiet.txt')
#specialized diet
pairwise.t.test(x=dat$Ch1_LWratio,g = dat$specialized_diet,p.adjust.method = 'bonferroni')
write.table(pairwise.t.test(x=dat$Ch1_LWratio,g = dat$specialized_diet,p.adjust.method = 'bonferroni')$p.value,
            file='LWRatio_ttest_specializeddiet.txt')
#habitat
pairwise.t.test(x=dat$Ch1_LWratio,g = dat$habitat,p.adjust.method = 'bonferroni')
write.table(pairwise.t.test(x=dat$Ch1_LWratio,g = dat$habitat,p.adjust.method = 'bonferroni')$p.value,
            file='LWRatio_ttest_habitat.txt')
#clade-wise
pairwise.t.test(x=dat$Ch1_LWratio,g = dat$Clade,p.adjust.method = 'bonferroni')
write.table(pairwise.t.test(x=dat$Ch1_LWratio,g = dat$Clade,p.adjust.method = 'bonferroni')$p.value,
            file='LWRatio_ttest_taxonomy.txt')

#Relative width of triturating surface
#primary diet
pairwise.t.test(x=dat$Ch6_TRIT,g = dat$primary_diet,p.adjust.method = 'bonferroni')
write.table(pairwise.t.test(x=dat$Ch6_TRIT,g = dat$primary_diet,p.adjust.method = 'bonferroni')$p.value,
            file='TritSurfWidth_ttest_primarydiet.txt')
#specialized diet
pairwise.t.test(x=dat$Ch6_TRIT,g = dat$specialized_diet,p.adjust.method = 'bonferroni')
write.table(pairwise.t.test(x=dat$Ch6_TRIT,g = dat$specialized_diet,p.adjust.method = 'bonferroni')$p.value,
            file='TritSurfWidth_ttest_specializeddiet.txt')
#habitat
pairwise.t.test(x=dat$Ch6_TRIT,g = dat$habitat,p.adjust.method = 'bonferroni')
write.table(pairwise.t.test(x=dat$Ch6_TRIT,g = dat$habitat,p.adjust.method = 'bonferroni')$p.value,
            file='TritSurfWidth_ttest_habitat.txt')
#clade-wise
pairwise.t.test(x=dat$Ch6_TRIT,g = dat$Clade,p.adjust.method = 'bonferroni')
write.table(pairwise.t.test(x=dat$Ch6_TRIT,g = dat$Clade,p.adjust.method = 'bonferroni')$p.value,
            file='TritSurfWidth_ttest_taxonomy.txt')


#Boxplot of measurements by clades
par(mfrow=c(3,2))
for ( i in c(5,6,7,8,4,9) ) {
  boxplot(dat[,i]~dat$Clade,
          ylab=colnames(dat)[i],xlab='Clades',
          col = c('#aa4499','#ee6677','#eecc66','darkblue','#66ccee',
                  'lightgreen','forestgreen','#999933'),
          xaxt='n')
  axis(1,at=1:length(unique(dat$Clade)),
       labels = c('Chel','Pelom','Trion','Chelon','Chelyd','Emyst','Test','Geoem'),
        # c('Chel','Chelon','Chelyd','Emyst','Geoem','Pelom','Test','Trion'),
       cex.axis=0.6)
  
}


# z-transformation of measurements
dat$Ch1_LWratio <- sapply(dat$Ch1_LWratio , function(x) (x-mean(dat$Ch1_LWratio))/sd(dat$Ch1_LWratio) )
dat$Ch2_AMA <- sapply(dat$Ch2_AMA , function(x) (x-mean(dat$Ch2_AMA))/sd(dat$Ch2_AMA) )
dat$Ch3_PMA <- sapply(dat$Ch3_PMA , function(x) (x-mean(dat$Ch3_PMA))/sd(dat$Ch3_PMA) )
dat$Ch4_OMA <- sapply(dat$Ch4_OMA , function(x) (x-mean(dat$Ch4_OMA))/sd(dat$Ch4_OMA) )
dat$Ch5_SMOI <- sapply(dat$Ch5_SMOI , function(x) (x-mean(dat$Ch5_SMOI))/sd(dat$Ch5_SMOI) )
dat$Ch6_TRIT <-  sapply(dat$Ch6_TRIT , function(x) (x-mean(dat$Ch6_TRIT))/sd(dat$Ch6_TRIT) )

#PCA command
pca <- prcomp(dat[,4:9],scale. = T)
summary(pca)
biplot(pca,cex=c(0.6,0.8))


#Cretae pairs plot to show clades/ecologies/etc

#Exploratory plots
par(mfrow=c(2,2))

#Clade
plot(pca$x[,1:2], 
     pch=ifelse(dat$Clade=='Chelidae' | dat$Clade=='Pelomedusoides',24,21), cex=1.5,
     bg=c('#aa4499','#ee6677','#eecc66','darkblue','#66ccee',
          'lightgreen','forestgreen','#999933') [as.numeric(as.factor(dat$Clade))])
title('Clade')
text(pca$x[,1:2], cex=0.35 
     ,col=ifelse(dat$Clade=='Chelonioidea','white','black'))
#legend('topleft',pt.bg=c('#aa4499','#ee6677','#eecc66','darkblue','#66ccee',
 #                        'lightgreen','forestgreen','#999933'),pch=21,pt.cex=0.8,bty='n',
  #     legend=levels(as.factor(dat$Clade)),cex=0.8, ncol=1 )
legend('bottomleft',pch = c(24,21),bty='n',legend=c('pleurodires','cryptodires'),
cex=0.75, ncol=1,pt.cex=1.2,pt.bg='black')

#Habitat
plot(pca$x[,1:2], 
     pch=ifelse(dat$Terrestrial_feeding=='yes',22,21), cex=1.5,
     bg=c('royalblue','lawngreen','lightsalmon') [as.numeric(as.factor(dat$habitat))]
)
title('Habitat')
text(pca$x[,1:2], cex=0.35
     ,col=ifelse(dat$habitat=='freshwater','white','black'))
legend('topleft',pt.bg=c('lightsalmon','royalblue','lawngreen'),pch=21,pt.cex=1.2,
       bty='n',legend=c('terrestrial','freshwater','marine') ,cex=0.7)
legend('bottomleft',pch = c(21,22),bty='n',legend=c('no','yes'),
       title='capable of\nterrestrial feeding',cex=0.75,
       ncol=2,pt.cex=1.2)

#Primary diet
plot(pca$x[,1:2], 
     pch=ifelse(dat$Terrestrial_feeding=='yes',22,21), cex=1.5,
     bg=c('hotpink','black','lemonchiffon') [as.numeric(as.factor(dat$primary_diet))]
     )
title('Primary diet')
text(pca$x[,1:2], cex=0.35
     ,col=ifelse(dat$primary_diet=='herbivorous','white','black'))
legend('topleft',pt.bg=c('black','lemonchiffon','hotpink'),pch=21,pt.cex=1.2,
       bty='n',legend=c('herbivorous','omnivorous','carnivorous') ,cex=0.7)
legend('bottomleft',pch = c(21,22),bty='n',legend=c('no','yes'),
       title='capable of\nterrestrial feeding',cex=0.75,
       ncol=2,pt.cex=1.2)

#Specialized diet
plot(pca$x[,1:2], 
     pch=ifelse(dat$Terrestrial_feeding=='yes',22,21), cex=1.5,
     bg=c('red','lemonchiffon','black','dodgerblue') [as.numeric(as.factor(dat$specialized_diet))]
)
title('Specialized diet')
text(pca$x[,1:2], cex=0.35
     ,col=ifelse(dat$specialized_diet=='high_fiber' | dat$specialized_diet=='suction','white','black'))
legend('topleft',pt.bg=c('lemonchiffon','black','red','dodgerblue'),pch=21,pt.cex=1.2,
       bty='n',legend=c('generalist','high fiber','durophagous','suction'), cex=0.7 )
legend('bottomleft',pch = c(21,22),bty='n',legend=c('no','yes'),
       title='capable of\nterrestrial feeding',cex=0.75,
       ncol=2,pt.cex=1.2)

dev.off()


#Disparity analyses - Cladewise
#Create clades object
clades <- lapply ( sort(unique(dat$Clade)), function(x) rownames(dat)[dat$Clade==x]  )
  names(clades) <- sort(unique(dat$Clade))
clades$Pleurodira <- rownames(dat)[dat$Clade=='Chelidae' | dat$Clade=='Pelomedusoides' ]
clades$Cryptodira  <- setdiff(rownames(dat),clades$Pleurodira)

clades <- clades[c('Pleurodira','Cryptodira','Chelidae','Pelomedusoides','Trionychia','Chelonioidea',
                   'Chelydroidea','Emysternia','Testudinidae','Geoemydidae')]


clade_disparity <- custom.subsets(pca$x,group = clades)
set.seed(1)
clade_disparity <- boot.matrix(clade_disparity,bootstraps = 1000)
clade_disparity <- dispRity(clade_disparity,metric = c(sum,variances))

plot(clade_disparity, col = c('blue','red','#aa4499','#ee6677','#eecc66','darkblue','#66ccee',
                              'lightgreen','forestgreen','#999933'),cex.axis=0.5)

test.dispRity(clade_disparity,test = wilcox.test,correction = 'bonferroni')

#Disparity analyses - primary diets
prim_diets <- lapply ( sort(unique(dat$primary_diet)), function(x) rownames(dat)[dat$primary_diet==x]  )
names(prim_diets) <- sort(unique(dat$primary_diet))

prim_diets <- prim_diets[c('carnivorous','omnivorous','herbivorous')]

prim_diet_disparity <- custom.subsets(pca$x,group = prim_diets)
set.seed(1)
prim_diet_disparity <- boot.matrix(prim_diet_disparity,bootstraps = 1000)
prim_diet_disparity <- dispRity(prim_diet_disparity,metric = c(sum,variances))

plot(prim_diet_disparity, col = c('hotpink','lemonchiffon','grey20'))

test.dispRity(prim_diet_disparity,test = wilcox.test,correction = 'bonferroni')

#Disparity analyses - specialized diets
spec_diets <- lapply ( sort(unique(dat$specialized_diet)), function(x) rownames(dat)[dat$specialized_diet==x]  )
names(spec_diets) <- sort(unique(dat$specialized_diet))

spec_diets <- spec_diets[c('generalist','high_fiber','durophagous','suction')]

spec_diet_disparity <- custom.subsets(pca$x,group = spec_diets)
set.seed(1)
spec_diet_disparity <- boot.matrix(spec_diet_disparity,bootstraps = 1000)
spec_diet_disparity <- dispRity(spec_diet_disparity,metric = c(sum,variances))

plot(spec_diet_disparity, col = c('lemonchiffon','grey20','red','dodgerblue'))

test.dispRity(spec_diet_disparity,test = wilcox.test,correction = 'bonferroni',
              comparisons = 'referential')


#Disparity analyses - terrestrial feeding
terr_feed <- lapply ( sort(unique(dat$Terrestrial_feeding)), function(x) rownames(dat)[dat$Terrestrial_feeding==x]  )
names(terr_feed) <- sort(unique(dat$Terrestrial_feeding))

terr_feed_disparity <- custom.subsets(pca$x,group = terr_feed)
set.seed(1)
terr_feed_disparity <- boot.matrix(terr_feed_disparity,bootstraps = 1000)
terr_feed_disparity <- dispRity(terr_feed_disparity,metric = c(sum,variances))

plot(terr_feed_disparity, col = c('lemonchiffon','grey20'))

test.dispRity(terr_feed_disparity,test = wilcox.test,correction = 'bonferroni')
