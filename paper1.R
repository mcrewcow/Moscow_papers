library(ggplot2)
library(vegan)
library(ape)
library(ggtern)

#For the dendrogram use ape
my_data1 <- read.table(file = "clipboard", sep = "\t", header=TRUE) 
#All the classes should be transformed to factors
my_data1$Class6 <- factor(my_data1$Class6)
my_data1$Class5 <- factor(my_data1$Class5)
my_data1$Class4 <- factor(my_data1$Class4)
my_data1$Class3 <- factor(my_data1$Class3)
my_data1$Class2 <- factor(my_data1$Class2)
my_data1$Class1 <- factor(my_data1$Class1)
tree <- as.phylo(~Class1/Class2/Class3/Class4/Class5/Class6, data = my_data1, directed = TRUE)
plot(tree)

ggplot(my_data1, aes(Material, Class6, fill = Amount)) + geom_tile() +
    scale_fill_distiller(palette = 'RdBu', breaks = c(0,1,4), limits = c(0,5)) + theme_bw()
    
    
    
rdaplot <- read.table(file = "clipboard", sep = "\t", header=TRUE)
View(rdaplot)
rownames(rdaplot) <- rdaplot$Parameter
rdaplot <- rdaplot[,-1]
data.log <- log1p(rdaplot)
data.hell <- decostand(data.log, 'hell')
tbRDA <- rda(data.hell ~ Y + K + lambda + T90 + Teff, data = rdaplot)
fig <- ordiplot(tbRDA, type = 'points', scaling = 3)
points(fig, 'sites', pch = 20, col = 'black', bg = 'black', cex = 0.8)
text(fig, 'sites', col = 'red', cex = 0.8)
fig

heatmapplot <- read.table(file = "clipboard", sep = "\t", header=TRUE)
ggplot(heatmapplot, aes(x = Amount,y = Taxonomy, fill = Taxonomy)) + geom_bar(stat = 'identity') + facet_wrap(~Material) + theme_bw()
