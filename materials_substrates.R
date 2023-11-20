library(vegan)

rdaplot <- read.table(file = "clipboard", sep = "\t", header=TRUE)
View(rdaplot)
rownames(rdaplot) <- rdaplot$Parameter
rdaplot <- rdaplot[,-1]
data.log <- log1p(rdaplot)
data.hell <- decostand(data.log, 'hell')
tbRDA <- rda(data.hell ~ H2.ase + K..mL.H2.g.COD.d +
               Fe.. + Ni... + Fe2. + Fe3. + γ..mL.H2.g.COD
             + λ..d + Т90...d, data = rdaplot)
fig <- ordiplot(tbRDA, type = 'points', scaling = 3)
points(fig, 'sites', pch = 20, col = 'black', bg = 'black', cex = 0.8)
text(fig, 'sites', col = 'red', cex = 0.8)
fig

tbRDA <- rda(data.hell ~ K + Teff + Fetot + Fe2.start + Fe3.start + γ + Т90., data = rdaplot)
fig <- ordiplot(tbRDA, type = 'points', scaling = 3)
points(fig, 'sites', pch = 20, col = 'black', bg = 'black', cex = 0.8)
text(fig, 'sites', col = 'red', cex = 0.8)
fig

tbRDA <- rda(data.hell ~ K + Teff + Fetot + Fe2.finish + Fe3.finish + γ + λ + Т90., data = rdaplot)
fig <- ordiplot(tbRDA, type = 'points', scaling = 3)
points(fig, 'sites', pch = 20, col = 'black', bg = 'black', cex = 0.8)
text(fig, 'sites', col = 'red', cex = 0.8)
fig

tbRDA <- rda(data.hell ~ K + Teff + Fetot + Fe2.start + Fe3.start + + Т90. + Fe2.finish + Fe3.finish , data = rdaplot)
fig <- ordiplot(tbRDA, type = 'points', scaling = 3)
points(fig, 'sites', pch = 20, col = 'black', bg = 'black', cex = 0.8)
text(fig, 'sites', col = 'red', cex = 0.8)
fig

heatmap <- read.table(file = "clipboard", sep = "\t", header=TRUE)
heatmap$Material <- fct_rev(factor(heatmap$Material, levels = c('Control',
                                                        'Magnetite 10',
                                                        'Magnetite 50',
                                                        'Magnetite 100',
                                                        'Magnetite 200',
                                                        'ZVI 10',
                                                        'ZVI 50',
                                                        'ZVI 100',
                                                        'ZVI 200',
                                                        'Nickel 10',
                                                        'Nickel 50',
                                                        'Nickel 100',
                                                        'Nickel 200',
                                                        'FeCl3 10',
                                                        'FeCl3 50',
                                                        'FeCl3 100',
                                                        'FeCl3 200'
)))

ggplot(heatmap,  aes(x = as.factor(Day),  y = Material, fill = Value)) + geom_tile(color = 'black',
              lwd = 0.5, linetype = 1) + scale_fill_gradient2(low = '#075AFF',mid = "white",
high = "#FF0000", midpoint =60) + theme_bw() + coord_fixed()
