#simple heat map
cs2m<-as.matrix(cs2m)
heatmap(cs2m, scale='none')

install.packages('gplots')
library(gplots)


cs2m<-as.matrix(cs2m)
heatmap.2(cs2m, scale = 'none', col=topo.colors(100),
          trace='none', density.info = 'none')


heatmap.2(cs2m, scale = 'none', col = greenred(100),
trace = 'none', density.info = 'none')

# pretty heat maps
install.packages('pheatmap')
library(pheatmap)

pheatmap(cs2m, cutree_rows = 4)

#d3heatmap()
install.packages('d3heatmap')
library(d3heatmap)

d3heatmap(cs2m, col=topo.colors(100),
          k_row=4, #nos of groups in rows
          k_col=2) #nos of groups in columns

d3heatmap(cs2m, col= greenred(100),
          k_row=4, #nos of groups in rows
          k_col=2) #nos of groups in columns
