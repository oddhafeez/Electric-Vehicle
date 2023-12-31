


install.packages('ggeasy')
install.packages('stringr')
install.packages('patchwork')
install.packages('factoextra')
install.packages('psych')
 



library(ggplot2)
library(GGally)
library(dplyr)
library(ggeasy)
library(stringr)
library(patchwork)
library(factoextra)
library(psych)
library(ggrepel)
library(reshape2)
 



ev <- read.csv("Electric_Vehicle_Population_Data.csv")
is.data.frame(ev)
head(ev)
summary(ev)
str(ev)
clean.ev <- ev[complete.cases(ev), ]
any(is.na(clean.ev))

str(clean.ev)

clean.ev$City <- as.factor(clean.ev$City) 
is.factor(clean.ev$City)
clean.ev$City <- as.factor(clean.ev$City) 
is.factor(clean.ev$City)
clean.ev$Postal.Code <- as.factor(clean.ev$Postal.Code) 
is.factor(clean.ev$Postal.Code)
clean.ev$Model.Year <- as.numeric(clean.ev$Model.Year) 
is.numeric(clean.ev$Model.Year)
clean.ev$Make <- as.factor(clean.ev$Make) 
is.factor(clean.ev$Make)
clean.ev$Model <- as.factor(clean.ev$Model) 
is.factor(clean.ev$Model)
clean.ev$Electric.Vehicle.Type <- as.factor(clean.ev$Electric.Vehicle.Type) 
is.factor(clean.ev$Electric.Vehicle.Type)
clean.ev$Clean.Alternative.Fuel.Vehicle..CAFV..Eligibility <- as.factor(clean.ev$Clean.Alternative.Fuel.Vehicle..CAFV..Eligibility) 
is.factor(clean.ev$Clean.Alternative.Fuel.Vehicle..CAFV..Eligibility)
clean.ev$Electric.Range <- as.numeric(clean.ev$Electric.Range) 
is.numeric(clean.ev$Electric.Range)
clean.ev$Base.MSRP <- as.numeric(clean.ev$Base.MSRP) 
is.numeric(clean.ev$Base.MSRP)
clean.ev$Legislative.District <- as.factor(clean.ev$Legislative.District) 
is.factor(clean.ev$Legislative.District)
clean.ev$DOL.Vehicle.ID <- as.character(clean.ev$DOL.Vehicle.ID) 
is.character(clean.ev$DOL.Vehicle.ID)
clean.ev$Electric.Utility <- as.factor(clean.ev$Electric.Utility) 
is.factor(clean.ev$Electric.Utility)
clean.ev$X2020.Census.Tract <- as.character(clean.ev$X2020.Census.Tract) 
is.character(clean.ev$X2020.Census.Tract)

========================================================= 
#correlation matrix

str(clean.ev)
clean.na <- clean.ev[apply(clean.ev != 0, 1, all), ]
str(clean.na)
ev.data <- clean.na[, c(6,7,11,12)]
str(ev.data)

ev.data.sub <- ev.data[, c(1,3,4)]
str(ev.data.sub)
plot(ev.data.sub)

ggpairs(data=ev.data.sub, columns=c("Model.Year","Electric.Range", "Base.MSRP"))  

ex_fachr <- sapply(clean.na, function(var) !is.factor(var) && !is.character(var))
sub_clean.ev <- (clean.na[, ex_fachr])
head(sub_clean.ev)
cor_matrix <- cor(sub_clean.ev)
print(cor_matrix)

library(reshape2)
meltcor_max <- melt(cor_matrix)
ranks <- order(meltcor_max$value, decreasing=TRUE)
meltcor_max[ranks,]

=====================================================================================
#scatterplot

ggplot(data=clean.ev, mapping=aes(x = Model.Year, y = Electric.Range)) +
geom_point() +
theme_minimal () +
labs(x = "Model Year", y = "Electric Range")


ggplot(data=clean.ev, mapping=aes(x = Model.Year, y = Base.MSRP)) +
geom_point() +
theme_minimal () +
labs(x = "Model Year", y = "Base MSRP")



ggplot(data=clean.ev, mapping=aes(x = Electric.Range, y = Base.MSRP)) +
geom_point() +
theme_minimal () +
labs(x = "Electric Range", y = "Base.MSRP")

    
ggplot(data=clean.ev, mapping=aes(x = Model.Year, y = Electric.Range)) + 
geom_point() + facet_wrap(facets=~Electric.Vehicle.Type)

ggplot(data=clean.na, mapping=aes(x = Model.Year, y = Electric.Range)) + 
geom_point() + facet_wrap(facets=~Clean.Alternative.Fuel.Vehicle..CAFV..Eligibility)


=========================================================================

#contingency table
table(clean.ev$Electric.Vehicle.Type)
prop.table(table(clean.ev$Electric.Vehicle.Type))


table(clean.ev$Electric.Vehicle.Type, clean.ev$Clean.Alternative.Fuel.Vehicle..CAFV..Eligibility)
addmargins(prop.table(table(clean.ev$Electric.Vehicle.Type, clean.ev$
Clean.Alternative.Fuel.Vehicle..CAFV..Eligibility)))

#Pearson’s χ2 test of independence
chisq.test(clean.ev$Electric.Vehicle.Type, clean.ev$Clean.Alternative.Fuel.Vehicle..CAFV..Eligibility)


#heatmap

ggplot(data=clean.ev, mapping=aes(x=Model.Year, y=Electric.Range)) +
stat_density2d(geom = "tile", contour=FALSE, aes(fill = ..density..)) +
scale_fill_gradientn(colours = rainbow(6)) +
theme_minimal() +
labs(x = "Model Year", y = "Electric Range")



ggplot(data=clean.na, mapping=aes(x=Model.Year, y=Base.MSRP)) +
stat_density2d(geom = "tile", contour=FALSE, aes(fill = ..density..)) +
scale_fill_gradientn(colours = rainbow(6)) +
theme_minimal() +
labs(x = "Model Year", y = "Base.MSRP")


ggplot(data=clean.na, mapping=aes(x=Electric.Range, y=Base.MSRP)) +
stat_density2d(geom = "tile", contour=FALSE, aes(fill = ..density..)) +
scale_fill_gradientn(colours = rainbow(6)) +
theme_minimal() +
labs(x = "Electric.Range", y = "Base.MSRP")

ggplot(data=clean.ev, mapping=aes(x=Model.Year)) + 
geom_histogram(binwidth = 2, fill = "sky blue", colour = "white") +
theme_minimal() 


ggplot(data=clean.ev, mapping=aes(x=Model.Year)) + geom_density()

ggplot(data=clean.ev, mapping=aes(x=Electric.Range)) + geom_density()

ggplot(data=clean.ev, mapping=aes(x=Model.Year, fill=Electric.Vehicle.Type)) +
geom_density(position="stack")

ggplot(data=clean.ev, mapping=aes(x=Model.Year, fill=Clean.Alternative.Fuel.Vehicle..CAFV..Eligibility)) +
geom_density(position="stack")



counts <- clean.ev %>% count(Make, sort = TRUE)
counts <- counts %>% mutate(percentage = n / sum(n) * 100)
top20 <- counts %>% top_n(20)
ggplot(top20, aes(x = reorder(Make, -n), y = n, fill = as.factor(Make))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = paste(n, "(", sprintf("%.1f%%", percentage), ")", sep = "")),
            vjust = -0.5, color = "black", size = 3) +
labs(title = "Top 20 EV Make", x = "Electric Vehicle Make", y = "Count") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))




# Filter the data for the selected period (2012 to 2023)
filtered_data <- clean.ev %>%
filter(Model.Year >= 2012 & Model.Year <= 2023)
counts <- filtered_data %>% count(Model.Year, sort = TRUE)
ggplot(counts, aes(x = factor(Model.Year), y = n, fill = as.factor(Model.Year))) +
geom_bar(stat = "identity", fill = "skyblue") +
geom_text(aes(label = paste(n, sep = "")),
vjust = -0.5, color = "black", size = 3) +
labs(title = "Evolution of EV from 2012 to 2023",x = "Model Year",y = "Count") +
theme(axis.text.x = element_text(angle = 45, hjust = 1))



===================================================

# Count the occurrences of each make within each city
counts <- clean.ev %>%
  count(City, Make, sort = TRUE)
top5_cities <- counts %>%
  group_by(City) %>%
  summarize(total = sum(n)) %>%
  top_n(5, total) %>%
  select(City)
filtered_data <- clean.ev %>%
  filter(City %in% top5_cities$City)
top5_makes <- filtered_data %>%
  count(City, Make, sort = TRUE) %>%
  group_by(City) %>%
  top_n(5) %>%
  ungroup()
ggplot(top5_makes, aes(x = reorder(Make, -n), y = n, fill = Make)) +
  geom_bar(stat = "identity") +
  facet_wrap(~City, scales = "free_y", ncol = 2) +
  geom_text(aes(label = n), vjust = -0.1, color = "black", size = 3) +
  labs(title = "Top 5 Makes in Top 5 Cities", x = "Make", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


================================

numeric.var <- select_if(clean.ev, is.numeric)
summary(numeric.var)

===============================

ggplot(data=clean.ev, mapping=aes(x=Clean.Alternative.Fuel.Vehicle..CAFV..Eligibility,y=Model.Year)) +
geom_boxplot() + stat_summary(fun=mean, geom="point", color="red",size=3)

ggplot(data=clean.ev, mapping=aes(x=Electric.Vehicle.Type,y=Model.Year)) +
geom_boxplot() + stat_summary(fun=mean, geom="point", color="red",size=3)

==================================
ev.data <- clean.ev[, c(6,7,11,12)]
ev.data <- ev.data[apply(ev.data != 0, 1, all), ]
str(ev.data)

ev.data.k <- ev.data[, c(1,3,4)]
str(ev.data.k)

ev.data.k <- ev.data.k[apply(ev.data.k != 0, 1, all), ]

# Scale data
ev.data.scale <- scale(ev.data.k)
ev.data.scale
# Distance
ev_dist <- dist(ev.data.scale)

# Calculate how many clusters you need
# Within Sum Squares
fviz_nbclust(ev.data.scale, kmeans, method = "wss") + labs(subtitle="Elbow Method")

# Kmeans
km.ev <- kmeans(ev.data.scale, centers=3, nstart=100)
print(km.ev)

# Visualize the clustering algorithm results.
km.evclusters<-km.ev$cluster

rownames(ev.data.scale)<-paste(ev.data$Make, 1:dim(ev.data)[1], sep = "_")
fviz_cluster(list(data=ev.data.scale, cluster = km.evclusters))
table(km.evclusters, ev.data$Make)
==================================
ev.data <- clean.ev[, c(6,9,11,12)]

ev.data <- ev.data %>%
  mutate(Electric.Vehicle.Type = recode(Electric.Vehicle.Type, "Battery Electric Vehicle (BEV)" = "BEV", "Plug-in Hybrid Electric Vehicle (PHEV)" = "PHEV"))


================
#factor analysis
ev.data <- clean.ev[, c(6,7,11,12)]
str(ev.data)
ev.data.k <- ev.data[apply(ev.data != 0, 1, all), ]

ev.data.fa <- ev.data.k[, c(1,3,4)]
str(ev.data.fa)
is.na(ev.data.fa)


any_infinite_values <- any(!is.finite(ev.data.fa))

#principal axis
pa <- fa(r = ev.data.fa, nfactors = 2, rotate="varimax", fm="pa", residuals=TRUE)
pa

#max likelihood
ml <- fa(r = ev.data.fa, nfactors = 2, rotate="varimax", fm="ml", residuals=TRUE)
ml

==============================
#pca

ev.data <- clean.ev[, c(6,7,11,12)]
str(ev.data)
ev.data.k <- ev.data[apply(ev.data != 0, 1, all), ]

ev.data.pc <- ev.data.k[, c(1,3,4)]
str(ev.data.pc)


pc2 <- principal(ev.data.pc,nfactors = 2,rotate="none")
pc2
----------------

# PCA
# A form of dimension reduction. - Transforms our data set to one with less features
# Maximizes Variance while minimizing.
# PCA is a statistical procedure that uses Eigendecompositions (finding eigenvalues/eigenvectors)
# to convert a set of observations to a set of linearly uncorrelated variables. (can use SVD - a different method)
# Each of these linearly uncorrelated variables (features) are known as Principal components.

   

pc.ev <- princomp(ev.data.pc, cor=TRUE)

# names() tells you what information is available from the output
names(pc.ev)

# summarize the pca   - note the std devs are divided by n not n-1
# makes them the square root of the eigenvalue
summary(pc.ev) 

# Proportion of variance = sd ^2 / number of components.
# cumultive proportion explains total variance.

# do things a bit better
eigenvectors<-pc.ev$loadings
eigenvalues <- pc.ev$sdev*pc.ev$sdev 

# loading is the eigenvector.
pc.ev$loadings    # note that these are scaled so the sum of squares is 1.0

# not loadings but eigenvectors
eigenvectors #These are the principal components that determine the directions of the new feature space
eigenvalues  # Eigenvalues determine the magnitude

# obtain the correlations with the variables
# Based on 2 Principal components (a new coordinate system and orthogonal to each other)
# the observations are then projected onto the coordinate plane.
# For more than 1 Principal Component, the score is
cor(ev.data.pc, pc.teeth$scores)
# make things a bit nicer
round(cor(ev.data.pc, pc.ev$scores) , 3)   

# round the output

# scree plot      - note the default is a barchart
# Scree plot is a line plot of Principal Components vs Variances
# Helps deteremine the number of factors to keep.
# This method is critized for its subjectivity because you are essentially looking
# for an 'elbow' within the graph. The graph can have many 'elbows'
screeplot(pc.ev,type='l',main='Screeplot for Electric Vehicle')  
abline(1,0,col='red',lty=2) #horizontal line at 1

# scatterplot of scores with labels given by mammal in col 1
#  type=n turns off the default symbol
plot(pc.ev$scores[,1:2], type='n',xlab="PC1(59%)", ylab="PC2(18%)" )
points(pc.ev$scores[,1:2],cex=0.5) 
text(pc.ev$scores[,1:2],label=ev.data.k[,2], pos = 3, cex = 0.5, col = "blue")

library(ggmap)
?register_google
 
> register_google(key="PUT YOUR API KEY HERE")
laff <- get_map(location="Lafayette, LA", zoom=12, source="osm")


ggplot(data=clean.ev, mapping=aes(x=Clean.Alternative.Fuel.Vehicle..CAFV..Eligibility,
y=Model.Year)) + 
geom_boxplot() + stat_summary(fun=mean, geom="point", color="red",size=3)