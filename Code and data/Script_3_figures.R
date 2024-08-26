#Script for making the figures

#Figure 2- Cladewise boxplots of raw measurements
dat <- read.csv('data_clean.csv',header = T, row.names = 1, sep=',')
  dat$Ch1_LWratio <- dat$Mandible_length/dat$Mandible_width

dat$Clade <- as.factor(dat$Clade)
dat$Clade <- factor(dat$Clade, 
                    levels= c('Chelidae','Pelomedusoides','Trionychia','Chelonioidea','Chelydroidea',
                              'Emysternia','Testudinidae','Geoemydidae')  )


#Boxplot of measurements by clades

pdf('Fig2-Taxonomic_boxplots.pdf',width = 6, height = 7, useDingbats = F)

cols_measurements <- c(5,6,7,8,4,9)

par(mfrow=c(3,2))
for ( i in 1:length(cols_measurements) ) {

      names.tmp <- c('Anterior mechanical advantage (AMA)','Posterior mechanical advantage (PMA)',
                 'Opening mechanical advantage (OMA)','Second moment of inertia (SMOI)',
                 'Mandible length-width ratio','Relative width of triturating surface')
  
  
  boxplot(dat[,cols_measurements[i] ]~dat$Clade,
          ylab=names.tmp[i],xlab='Clades',
          col = c('#aa4499','#ee6677','#eecc66','darkblue','#66ccee',
                  'lightgreen','forestgreen','#999933'),
          xaxt='n',cex.lab=0.9,cex.axis=0.75)
  
  axis(1,at=1:length(unique(dat$Clade)),
       labels = c('Chel','Pelom','Trion','Chelon','Chelyd','Emyst','Test','Geoem'),
       # c('Chel','Chelon','Chelyd','Emyst','Geoem','Pelom','Test','Trion'),
       cex.axis=0.6)
  
}

dev.off()

#Figure 3- Ecology boxplots of AMA and PMA

pdf('Fig3-Ecology_boxplots (AMA-PMA).pdf', width = 6.8, height = 6.8)

#Anterior and posterior mechanical advantages
par(mfrow=c(3,3))
boxplot(dat$Ch2_AMA~dat$primary_diet, col =c('hotpink','darkgrey','lemonchiffon') ,
        cex.axis=0.75, xlab='Primary diet', ylab='Anterior mechanical advantage (AMA)')
#legend('topright',fill=c('hotpink','black','lemonchiffon'),bty='n',
 #      legend=c('carn','herb','omni'))
boxplot(dat$Ch2_AMA~dat$specialized_diet,col=c('red','lemonchiffon','darkgrey','dodgerblue'),
        cex.axis=0.75, ylab='', xlab='Specialized diet')
#legend('topleft',fill=c('red','lemonchiffon','black','dodgerblue'),bty='n',
 #      legend=c('duroph','gen','high_fib','suct'))
boxplot(dat$Ch2_AMA~dat$habitat,col=c('royalblue','lawngreen','lightsalmon'),
        cex.axis=0.75, ylab='', xlab='Habitat')
#legend('topright',fill=c('royalblue','lawngreen','lightsalmon'),bty='n',
 #      legend=c('freshw','marine','terr'))

boxplot(dat$Ch3_PMA~dat$primary_diet, col =c('hotpink','darkgrey','lemonchiffon') ,
        cex.axis=0.75,xlab='Primary diet', ylab='Posterior mechanical advantage (PMA)')
#legend('topright',fill=c('hotpink','black','lemonchiffon'),bty='n',
 #      legend=c('carn','herb','omni'))
boxplot(dat$Ch3_PMA~dat$specialized_diet,col=c('red','lemonchiffon','darkgrey','dodgerblue'),
        cex.axis=0.75,xlab='Specialized diet',ylab='')
#legend('topleft',fill=c('red','lemonchiffon','black','dodgerblue'),bty='n',
 #      legend=c('duroph','gen','high_fib','suct'))
boxplot(dat$Ch3_PMA~dat$habitat,col=c('royalblue','lawngreen','lightsalmon'),
        cex.axis=0.75,xlab='Habitat',ylab='')
#legend('topright',fill=c('royalblue','lawngreen','lightsalmon'),bty='n',
 #      legend=c('freshw','marine','terr'))

dev.off()


#Figure 4- Ecology boxplots of OMA and SMOI

pdf('Fig4-Ecology_boxplots (OMA-SMOI).pdf', width = 6.8, height = 6.8)

#Anterior and posterior mechanical advantages
par(mfrow=c(3,3))
boxplot(dat$Ch4_OMA~dat$primary_diet, col =c('hotpink','darkgrey','lemonchiffon') ,
        cex.axis=0.75, xlab='Primary diet', ylab='Opening mechanical advantage (OMA)')
#legend('topright',fill=c('hotpink','black','lemonchiffon'),bty='n',
#      legend=c('carn','herb','omni'))
boxplot(dat$Ch4_OMA~dat$specialized_diet,col=c('red','lemonchiffon','darkgrey','dodgerblue'),
        cex.axis=0.75, ylab='', xlab='Specialized diet')
#legend('topleft',fill=c('red','lemonchiffon','black','dodgerblue'),bty='n',
#      legend=c('duroph','gen','high_fib','suct'))
boxplot(dat$Ch4_OMA~dat$habitat,col=c('royalblue','lawngreen','lightsalmon'),
        cex.axis=0.75, ylab='', xlab='Habitat')
#legend('topright',fill=c('royalblue','lawngreen','lightsalmon'),bty='n',
#      legend=c('freshw','marine','terr'))

boxplot(dat$Ch5_SMOI~dat$primary_diet, col =c('hotpink','darkgrey','lemonchiffon') ,
        cex.axis=0.75,xlab='Primary diet', ylab='Second moment of inertia (SMOI)')
#legend('topright',fill=c('hotpink','black','lemonchiffon'),bty='n',
#      legend=c('carn','herb','omni'))
boxplot(dat$Ch5_SMOI~dat$specialized_diet,col=c('red','lemonchiffon','darkgrey','dodgerblue'),
        cex.axis=0.75,xlab='Specialized diet',ylab='')
#legend('topleft',fill=c('red','lemonchiffon','black','dodgerblue'),bty='n',
#      legend=c('duroph','gen','high_fib','suct'))
boxplot(dat$Ch5_SMOI~dat$habitat,col=c('royalblue','lawngreen','lightsalmon'),
        cex.axis=0.75,xlab='Habitat',ylab='')
#legend('topright',fill=c('royalblue','lawngreen','lightsalmon'),bty='n',
#      legend=c('freshw','marine','terr'))

dev.off()


#Figure 5- Ecology boxplots of mandible L-W ratio and relative width of triturating surface

pdf('Fig5-Ecology_boxplots (LWRatio-WidthTritSurf).pdf', width = 6.8, height = 6.8)

#Anterior and posterior mechanical advantages
par(mfrow=c(3,3))
boxplot(dat$Ch1_LWratio~dat$primary_diet, col =c('hotpink','darkgrey','lemonchiffon') ,
        cex.axis=0.75, xlab='Primary diet', ylab='Mandible length-width ratio')
#legend('topright',fill=c('hotpink','black','lemonchiffon'),bty='n',
#      legend=c('carn','herb','omni'))
boxplot(dat$Ch1_LWratio~dat$specialized_diet,col=c('red','lemonchiffon','darkgrey','dodgerblue'),
        cex.axis=0.75, ylab='', xlab='Specialized diet')
#legend('topleft',fill=c('red','lemonchiffon','black','dodgerblue'),bty='n',
#      legend=c('duroph','gen','high_fib','suct'))
boxplot(dat$Ch1_LWratio~dat$habitat,col=c('royalblue','lawngreen','lightsalmon'),
        cex.axis=0.75, ylab='', xlab='Habitat')
#legend('topright',fill=c('royalblue','lawngreen','lightsalmon'),bty='n',
#      legend=c('freshw','marine','terr'))

boxplot(dat$Ch6_TRIT~dat$primary_diet, col =c('hotpink','darkgrey','lemonchiffon') ,
        cex.axis=0.75,xlab='Primary diet', ylab='Relative width of triturating surface')
#legend('topright',fill=c('hotpink','black','lemonchiffon'),bty='n',
#      legend=c('carn','herb','omni'))
boxplot(dat$Ch6_TRIT~dat$specialized_diet,col=c('red','lemonchiffon','darkgrey','dodgerblue'),
        cex.axis=0.75,xlab='Specialized diet',ylab='')
#legend('topleft',fill=c('red','lemonchiffon','black','dodgerblue'),bty='n',
#      legend=c('duroph','gen','high_fib','suct'))
boxplot(dat$Ch6_TRIT~dat$habitat,col=c('royalblue','lawngreen','lightsalmon'),
        cex.axis=0.75,xlab='Habitat',ylab='')
#legend('topright',fill=c('royalblue','lawngreen','lightsalmon'),bty='n',
#      legend=c('freshw','marine','terr'))

dev.off()


#Figure 6- PCA

pdf('Fig6-PCA_plot.pdf',width = 6.8, height = 6.8, useDingbats = F)

par(mfrow=c(2,2))

xlabel <- paste0('PC1 (', round(summary(pca)$importance[2,1]*100,1),'%)')
ylabel <- paste0('PC2 (', round(summary(pca)$importance[2,2]*100,1),'%)')

#Clade
plot(pca$x[,1:2], 
     pch=ifelse(dat$Clade=='Chelidae' | dat$Clade=='Pelomedusoides',24,21), cex=1.2,
     bg=c('#aa4499','#ee6677','#eecc66','darkblue','#66ccee',
          'lightgreen','forestgreen','#999933') [as.numeric(as.factor(dat$Clade))],
     xlab=xlabel, ylab=ylabel, cex.axis=0.75)
abline(v=0,h=0,lty=3,col='grey50')
title('Clade')
#text(pca$x[,1:2], cex=0.35 
 #    ,col=ifelse(dat$Clade=='Chelonioidea','white','black'))
#legend('topleft',pt.bg=c('#aa4499','#ee6677','#eecc66','darkblue','#66ccee',
#                        'lightgreen','forestgreen','#999933'),pch=21,pt.cex=0.8,bty='n',
#     legend=levels(as.factor(dat$Clade)),cex=0.8, ncol=1 )
legend('bottomleft',pch = c(24,21),bty='n',legend=c('pleurodires','cryptodires'),
       cex=0.7, ncol=1,pt.cex=0.9,pt.bg='black')

#Habitat
plot(pca$x[,1:2], 
     pch=ifelse(dat$Terrestrial_feeding=='yes',22,21), cex=1.2,
     bg=c('royalblue','lawngreen','lightsalmon') [as.numeric(as.factor(dat$habitat))],
     xlab=xlabel, ylab=ylabel, cex.axis=0.75)
abline(v=0,h=0,lty=3,col='grey50')
title('Habitat')
#text(pca$x[,1:2], cex=0.35
#    ,col=ifelse(dat$habitat=='freshwater','white','black'))
legend('topleft',pt.bg=c('lightsalmon','royalblue','lawngreen'),pch=21,pt.cex=0.9,
       bty='n',legend=c('terrestrial','freshwater','marine') ,cex=0.7)
legend('bottomleft',pch = c(21,22),bty='n',legend=c('no','yes'),
       title='terrestrial feeding',cex=0.7,
       ncol=2,pt.cex=0.9)

#Primary diet
plot(pca$x[,1:2], 
     pch=ifelse(dat$Terrestrial_feeding=='yes',22,21), cex=1.2,
     bg=c('hotpink','black','lemonchiffon') [as.numeric(as.factor(dat$primary_diet))],
     xlab=xlabel, ylab=ylabel, cex.axis=0.75)
abline(v=0,h=0,lty=3,col='grey50')

title('Primary diet')
#text(pca$x[,1:2], cex=0.35
#    ,col=ifelse(dat$primary_diet=='herbivorous','white','black'))
legend('topleft',pt.bg=c('black','lemonchiffon','hotpink'),pch=21,pt.cex=0.9,
       bty='n',legend=c('herbivorous','omnivorous','carnivorous') ,cex=0.7)
legend('bottomleft',pch = c(21,22),bty='n',legend=c('no','yes'),
       title='terrestrial feeding',cex=0.7,
       ncol=2,pt.cex=0.9)

#Specialized diet
plot(pca$x[,1:2], 
     pch=ifelse(dat$Terrestrial_feeding=='yes',22,21), cex=1.2,
     bg=c('red','lemonchiffon','black','dodgerblue') [as.numeric(as.factor(dat$specialized_diet))],
     xlab=xlabel, ylab=ylabel, cex.axis=0.75)
abline(v=0,h=0,lty=3,col='grey50')
title('Specialized diet')
#text(pca$x[,1:2], cex=0.35
#    ,col=ifelse(dat$specialized_diet=='high_fiber' | dat$specialized_diet=='suction','white','black'))
legend('topleft',pt.bg=c('lemonchiffon','black','red','dodgerblue'),pch=21,pt.cex=0.9,
       bty='n',legend=c('generalist','high fiber','durophagous','suction'), cex=0.7 )
legend('bottomleft',pch = c(21,22),bty='n',legend=c('no','yes'),
       title='terrestrial feeding',cex=0.7,
       ncol=2,pt.cex=0.9)

dev.off()


#Figure 7- PCoA

pdf('Fig7-PCoA_plot.pdf',width = 6.8, height = 6.8, useDingbats = F)

par(mfrow=c(2,2))

xlabel <- paste0('PCo1 (', round(scree.data[1],1),'%)')
ylabel <- paste0('PCo2 (', round(scree.data[2],1),'%)')


#Clade
plot(turtle_pcoa$vectors[rownames(dat),1:2],xlab=xlabel,ylab=ylabel,cex.axis=0.75,
     pch=ifelse(dat$Clade=='Chelidae' | dat$Clade=='Pelomedusoides',24,21), cex=1.2,
     bg=c('#aa4499','#ee6677','#eecc66','darkblue','#66ccee',
             'lightgreen','forestgreen','#999933') [as.numeric(as.factor(dat$Clade))] )
abline(v=0,h=0,lty=3,col='grey50')
title('Clade')
#text(turtle_pcoa$vectors[rownames(dat),1:2], cex=0.35 
     #,lab=rownames(dat)
 #    ,col=ifelse(dat$Clade=='Chelonioidea','black','black'))
legend('topleft',pch = c(24,21),bty='n',legend=c('pleurodires','cryptodires'),
       cex=0.7, ncol=1,pt.cex=0.9,pt.bg='black')

#Habitat
plot(turtle_pcoa$vectors[rownames(dat),1:2], xlab=xlabel,ylab=ylabel,cex.axis=0.75,
     pch=ifelse(dat$Terrestrial_feeding=='yes',22,21), cex=1.2,
     bg=c('royalblue','lawngreen','lightsalmon') [as.numeric(as.factor(dat$habitat))]
)
abline(v=0,h=0,lty=3,col='grey50')
title('Habitat')
#text(turtle_pcoa$vectors[rownames(dat),1:2], cex=0.35
     #,lab=rownames(dat)
 #    ,col=ifelse(dat$habitat=='freshwater','white','black'))
legend('topleft',pt.bg=c('lightsalmon','royalblue','lawngreen'),pch=21,pt.cex=0.9,cex=0.7,
       bty='n',legend=c('terr','freshw','marine') )
legend('topright',pch = c(21,22),bty='n',legend=c('no','yes'),
       title='terrestrial feeding',cex=0.7,
       ncol=1,pt.cex=0.9)

#Primary diet
plot(turtle_pcoa$vectors[rownames(dat),1:2], xlab=xlabel,ylab=ylabel,cex.axis=0.75,
     pch=ifelse(dat$Terrestrial_feeding=='yes',22,21), cex=1.2,
     bg=c('hotpink','black','lemonchiffon') [as.numeric(as.factor(dat$primary_diet))]
)
abline(v=0,h=0,lty=3,col='grey50')
title('Primary diet')
#text(turtle_pcoa$vectors[rownames(dat),1:2], cex=0.35
     #,lab=rownames(dat)
 #    ,col=ifelse(dat$primary_diet=='herbivorous','white','black'))
legend('topleft',pt.bg=c('black','lemonchiffon','hotpink'),pch=21,pt.cex=0.9,cex=0.7,
       bty='n',legend=c('herb','omniv','carniv') )
legend('topright',pch = c(21,22),bty='n',legend=c('no','yes'),
       title='terrestrial feeding',cex=0.7,
       ncol=1,pt.cex=0.9)

#Specialized diet
plot(turtle_pcoa$vectors[rownames(dat),1:2], xlab=xlabel,ylab=ylabel,cex.axis=0.75,
     pch=ifelse(dat$Terrestrial_feeding=='yes',22,21), cex=1.2,
     bg=c('red','lemonchiffon','black','dodgerblue') [as.numeric(as.factor(dat$specialized_diet))]
)
abline(v=0,h=0,lty=3,col='grey50')
title('Specialized diet')
#text(turtle_pcoa$vectors[rownames(dat),1:2], cex=0.35
     #,lab=rownames(dat)
 #    ,col=ifelse(dat$specialized_diet=='high_fiber' | dat$specialized_diet=='suction','white','black'))
legend('topleft',pt.bg=c('lemonchiffon','black','red','dodgerblue'),pch=21,pt.cex=0.9,cex=0.7,
       bty='n',legend=c('gen','high_f','durop','suct') )
legend('topright',pch = c(21,22),bty='n',legend=c('no','yes'),
       title='terrestrial feeding',cex=0.7,
       ncol=1,pt.cex=0.9)

dev.off()


#Figure 8- Disparity analyses (continuous and discrete measurements vs. diet ecology)

pdf('Fig8-Disparity_Ecology.pdf', width = 6.8, height = 6.8)

par(mfcol=c(3,3))

#Disparity (continuouos)
#Primary ecology
plot(prim_diet_disparity, col = c('hotpink','lemonchiffon','grey20'),
     ylab='Disparity (sum of variances)', xlab='Primary diet',cex.axis=0.75)

#Specialized ecology
plot(spec_diet_disparity, col = c('lemonchiffon','grey20','red','dodgerblue'),
     ylab='Disparity (sum of variances)', xlab='Specialized diet',cex.axis=0.75)

#Terrestrial feeding
plot(terr_feed_disparity, col = c('lemonchiffon','grey20'),
     ylab='Disparity (sum of variances)', xlab='Terrestrial feeding',cex.axis=0.75)

#Disparity (discrete)
#Primary ecology
plot(prim_diet_disparity_disc, col = c('hotpink','lemonchiffon','grey20'),
     ylab='Disparity (sum of variances)', xlab='Primary diet',cex.axis=0.75)

#Specialized ecology
plot(spec_diet_disparity_disc, col = c('lemonchiffon','grey20','red','dodgerblue'),
     ylab='Disparity (sum of variances)', xlab='Specialized diet',cex.axis=0.75)

#Terrestrial feeding
plot(terr_feed_disparity_disc, col = c('lemonchiffon','grey20'),
     ylab='Disparity (sum of variances)', xlab='Terrestrial feeding',cex.axis=0.75)

dev.off()

#Figure 9- Disparity analyses (continuous and discrete measurements vs. clade [crypto/pleuro])

pdf('Fig9-Disparity_Clades.pdf', width = 6.8, height = 6.8)

par(mfrow=c(2,2))


#Disparity (continuous)

clades.tmp <- c('Pleurodira','Cryptodira')
clades.tmp <- lapply(clade_disparity$disparity[clades.tmp] , function(x) x[[2]] )

boxplot(clades.tmp, xlab='Clades',ylab='Disparity (sum of variances)',
        col = c('#abd1ff','#e54b22'),cex.axis=0.75)  

#Disparity (discrete)

clades.tmp <- c('Pleurodira','Cryptodira')
clades.tmp <- lapply(clade_disparity_disc$disparity[clades.tmp] , function(x) x[[2]] )

boxplot(clades.tmp, xlab='Clades',ylab='Disparity (sum of variances)',
        col = c('#abd1ff','#e54b22'),cex.axis=0.75)  

#All clades
#Continuous
clades.tmp <- lapply(clade_disparity$disparity[-c(1,2)] , function(x) x[[2]] )

boxplot(clades.tmp, xlab='Clades',ylab='Disparity (sum of variances)',
        col = c('#aa4499','#ee6677','#eecc66','darkblue','#66ccee',
                'lightgreen','forestgreen','#999933') ,cex.axis=0.75)  

#All clades
#Discrete
clades.tmp <- lapply(clade_disparity_disc$disparity[-c(1,2)] , function(x) x[[2]] )

boxplot(clades.tmp, xlab='Clades',ylab='Disparity (sum of variances)',
        col = c('#aa4499','#ee6677','#eecc66','darkblue','#66ccee',
                'lightgreen','forestgreen','#999933'),cex.axis=0.75)  



dev.off()



#Supplementary Figures/tables shown in Supplementary File S3

#Correlation between AMA and PMA for turtles

plot(dat$Ch2_AMA, dat$Ch3_PMA, cex=1.8, pch=21, col=adjustcolor('black',alpha.f = 0.8),
     bg=adjustcolor('grey85',alpha.f = 0.6),
     xlab='Anterior mechanical advantage (AMA)', ylab = 'Posterior mechanical advantage (PMA)')
text(dat$Ch2_AMA, dat$Ch3_PMA, cex=0.4)

cor.test(dat$Ch2_AMA, dat$Ch3_PMA) #Pearson's r=0.44, p<0.001


#Correlation between AMA and mandible size for turtles

plot(dat$Ch2_AMA~log10(dat$Mandible_length), cex=1.8, pch=21, col=adjustcolor('black',alpha.f = 0.8),
     bg=adjustcolor('grey85',alpha.f = 0.6),
     ylab='Anterior mechanical advantage (AMA)', xlab = 'log10 Mandible length')
text(dat$Ch2_AMA~log10(dat$Mandible_length), cex=0.4)

cor.test(dat$Ch2_AMA, log10(dat$Mandible_length)) #Pearson's r=-0.33, p=0.03


#Correlation between AMA and PMA for pseudosuchians (Stubbs paper)
stubbs <- read.csv('Stubbs_data.csv',header = T,row.names = 1)

stubbs$PMA[stubbs$PMA=='?'] <- NA
stubbs$PMA <- as.numeric(stubbs$PMA)

plot(stubbs$AMA, stubbs$PMA, cex=1.8, pch=21, col=adjustcolor('black',alpha.f = 0.8),
     bg=adjustcolor('grey85',alpha.f = 0.6),
     xlab='Anterior mechanical advantage (AMA)', ylab = 'Posterior mechanical advantage (PMA)')
title('Pseudosuchian database from Stubbs et al. 2013')

cor.test(stubbs$AMA, stubbs$PMA) #Pearson's r=0.72, p<0.001

dev.off()

#Comparison between AMA/PMA of turtles and pseudosuchians (Stubbs paper)

par(mfrow=c(2,2))
boxplot ( list (turtles=dat$Ch2_AMA, pseudosuchians=stubbs$AMA ),
          col = c('skyblue','salmon'))
title('Anterior mechanical\nadvantage (AMA)')

#T-test comparing them (results are in the main text)
t.test(dat$Ch2_AMA , stubbs$AMA)

boxplot ( list (turtles=dat$Ch3_PMA, pseudosuchians=stubbs$PMA ),
          col = c('skyblue','salmon') )
title('Pnterior mechanical\nadvantage (PMA)')

#T-test comparing them (results are in the main text)
t.test(dat$Ch3_PMA , stubbs$PMA)

dev.off()


#PCA plot showing species as numbers (labelled in the figure caption in Supplementary File S3)

par(mfrow=c(2,2))

xlabel <- paste0('PC1 (', round(summary(pca)$importance[2,1]*100,1),'%)')
ylabel <- paste0('PC2 (', round(summary(pca)$importance[2,2]*100,1),'%)')

#Clade
plot(pca$x[,1:2], 
     pch=ifelse(dat$Clade=='Chelidae' | dat$Clade=='Pelomedusoides',24,21), cex=1.75,
     bg=c('#aa4499','#ee6677','#eecc66','darkblue','#66ccee',
          'lightgreen','forestgreen','#999933') [as.numeric(as.factor(dat$Clade))],
     xlab=xlabel, ylab=ylabel, cex.axis=0.75)
abline(v=0,h=0,lty=3,col='grey50')
title('Clade')
text(pca$x[,1:2], cex=0.45 
    ,col=ifelse(dat$Clade=='Chelonioidea','white','black'))

legend('bottomleft',pch = c(24,21),bty='n',legend=c('pleurodires','cryptodires'),
       cex=0.7, ncol=1,pt.cex=0.9,pt.bg='black')

#Habitat
plot(pca$x[,1:2], 
     pch=ifelse(dat$Terrestrial_feeding=='yes',22,21), cex=1.75,
     bg=c('royalblue','lawngreen','lightsalmon') [as.numeric(as.factor(dat$habitat))],
     xlab=xlabel, ylab=ylabel, cex.axis=0.75)
abline(v=0,h=0,lty=3,col='grey50')
title('Habitat')
text(pca$x[,1:2], cex=0.45
    ,col=ifelse(dat$habitat=='freshwater','white','black'))

legend('topleft',pt.bg=c('lightsalmon','royalblue','lawngreen'),pch=21,pt.cex=0.9,
       bty='n',legend=c('terrestrial','freshwater','marine') ,cex=0.7)
legend('bottomleft',pch = c(21,22),bty='n',legend=c('no','yes'),
       title='terrestrial feeding',cex=0.7,
       ncol=2,pt.cex=0.9)

#Primary diet
plot(pca$x[,1:2], 
     pch=ifelse(dat$Terrestrial_feeding=='yes',22,21), cex=1.75,
     bg=c('hotpink','black','lemonchiffon') [as.numeric(as.factor(dat$primary_diet))],
     xlab=xlabel, ylab=ylabel, cex.axis=0.75)
abline(v=0,h=0,lty=3,col='grey50')

title('Primary diet')
text(pca$x[,1:2], cex=0.45
    ,col=ifelse(dat$primary_diet=='herbivorous','white','black'))

legend('topleft',pt.bg=c('black','lemonchiffon','hotpink'),pch=21,pt.cex=0.9,
       bty='n',legend=c('herbivorous','omnivorous','carnivorous') ,cex=0.7)
legend('bottomleft',pch = c(21,22),bty='n',legend=c('no','yes'),
       title='terrestrial feeding',cex=0.7,
       ncol=2,pt.cex=0.9)

#Specialized diet
plot(pca$x[,1:2], 
     pch=ifelse(dat$Terrestrial_feeding=='yes',22,21), cex=1.75,
     bg=c('red','lemonchiffon','black','dodgerblue') [as.numeric(as.factor(dat$specialized_diet))],
     xlab=xlabel, ylab=ylabel, cex.axis=0.75)
abline(v=0,h=0,lty=3,col='grey50')
title('Specialized diet')
text(pca$x[,1:2], cex=0.45
    ,col=ifelse(dat$specialized_diet=='high_fiber' | dat$specialized_diet=='suction','white','black'))

legend('topleft',pt.bg=c('lemonchiffon','black','red','dodgerblue'),pch=21,pt.cex=0.9,
       bty='n',legend=c('generalist','high fiber','durophagous','suction'), cex=0.7 )
legend('bottomleft',pch = c(21,22),bty='n',legend=c('no','yes'),
       title='terrestrial feeding',cex=0.7,
       ncol=2,pt.cex=0.9)

dev.off()

#PCoA plot showing species as numbers (labelled in the figure caption in Supplementary File S3)

par(mfrow=c(2,2))

xlabel <- paste0('PCo1 (', round(scree.data[1],1),'%)')
ylabel <- paste0('PCo2 (', round(scree.data[2],1),'%)')


#Clade
plot(turtle_pcoa$vectors[rownames(dat),1:2],xlab=xlabel,ylab=ylabel,cex.axis=0.75,
     pch=ifelse(dat$Clade=='Chelidae' | dat$Clade=='Pelomedusoides',24,21), cex=1.75,
     bg=c('#aa4499','#ee6677','#eecc66','darkblue','#66ccee',
          'lightgreen','forestgreen','#999933') [as.numeric(as.factor(dat$Clade))] )
abline(v=0,h=0,lty=3,col='grey50')
title('Clade')
text(turtle_pcoa$vectors[rownames(dat),1:2], cex=0.45 ,
    col=ifelse(dat$Clade=='Chelonioidea','white','black'))

legend('topleft',pch = c(24,21),bty='n',legend=c('pleurodires','cryptodires'),
       cex=0.7, ncol=1,pt.cex=0.9,pt.bg='black')

#Habitat
plot(turtle_pcoa$vectors[rownames(dat),1:2], xlab=xlabel,ylab=ylabel,cex.axis=0.75,
     pch=ifelse(dat$Terrestrial_feeding=='yes',22,21), cex=1.75,
     bg=c('royalblue','lawngreen','lightsalmon') [as.numeric(as.factor(dat$habitat))]
)
abline(v=0,h=0,lty=3,col='grey50')
title('Habitat')
text(turtle_pcoa$vectors[rownames(dat),1:2], cex=0.45,
    col=ifelse(dat$habitat=='freshwater','white','black'))

legend('topleft',pt.bg=c('lightsalmon','royalblue','lawngreen'),pch=21,pt.cex=0.9,cex=0.7,
       bty='n',legend=c('terrestrial','freshwater','marine') )
legend('topright',pch = c(21,22),bty='n',legend=c('no','yes'),
       title='terrestrial feeding',cex=0.7,
       ncol=1,pt.cex=0.9)

#Primary diet
plot(turtle_pcoa$vectors[rownames(dat),1:2], xlab=xlabel,ylab=ylabel,cex.axis=0.75,
     pch=ifelse(dat$Terrestrial_feeding=='yes',22,21), cex=1.75,
     bg=c('hotpink','black','lemonchiffon') [as.numeric(as.factor(dat$primary_diet))]
)
abline(v=0,h=0,lty=3,col='grey50')
title('Primary diet')
text(turtle_pcoa$vectors[rownames(dat),1:2], cex=0.45,
    col=ifelse(dat$primary_diet=='herbivorous','white','black'))

legend('topleft',pt.bg=c('black','lemonchiffon','hotpink'),pch=21,pt.cex=0.9,cex=0.7,
       bty='n',legend=c('herbivorous','omnivorous','carnivorous') )
legend('topright',pch = c(21,22),bty='n',legend=c('no','yes'),
       title='terrestrial feeding',cex=0.7,
       ncol=1,pt.cex=0.9)

#Specialized diet
plot(turtle_pcoa$vectors[rownames(dat),1:2], xlab=xlabel,ylab=ylabel,cex.axis=0.75,
     pch=ifelse(dat$Terrestrial_feeding=='yes',22,21), cex=1.75,
     bg=c('red','lemonchiffon','black','dodgerblue') [as.numeric(as.factor(dat$specialized_diet))]
)
abline(v=0,h=0,lty=3,col='grey50')
title('Specialized diet')
text(turtle_pcoa$vectors[rownames(dat),1:2], cex=0.45
    ,col=ifelse(dat$specialized_diet=='high_fiber' | dat$specialized_diet=='suction','white','black'))

legend('topleft',pt.bg=c('lemonchiffon','black','red','dodgerblue'),pch=21,pt.cex=0.9,cex=0.7,
       bty='n',legend=c('generalist','high-fibre','durophagous','suction') )
legend('topright',pch = c(21,22),bty='n',legend=c('no','yes'),
       title='terrestrial feeding',cex=0.7,
       ncol=1,pt.cex=0.9)

dev.off()

#Table showing the cumulative variance of PCoA axes

pcoa_var_table <- data.frame('var'= round(scree.data,2), 
                             'cumulative_var'=round(cumsum(scree.data),2) )

write.csv(pcoa_var_table,file='PCoA_cumulative_variance.csv',sep=',')


#Numbers for each species (paste on figure captions)

cat(paste( paste0(1:nrow(dat) , '- ', gsub('_',' ',rownames(dat))), collapse = ', '))


#END