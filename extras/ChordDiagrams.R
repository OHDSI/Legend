library(ggraph)
library(igraph)
library(tidyverse)
library(RColorBrewer)

abbreviateHctz <- TRUE

tension <- 0.1

# setwd('c://Rwd')

# create a data frame giving the hierarchical structure of your individuals
d1=data.frame(from="origin", to=paste("group", seq(1,10), sep=""))
d2=data.frame(from=rep(d1$to, each=10), to=paste("subgroup", seq(1,100), sep="_"))
edges=rbind(d1, d2)


# create a dataframe with connection between leaves (individuals)
all_leaves=paste("subgroup", seq(1,100), sep="_")
connect=rbind( data.frame( from=sample(all_leaves, 100, replace=T) , to=sample(all_leaves, 100, replace=T)), data.frame( from=sample(head(all_leaves), 30, replace=T) , to=sample( tail(all_leaves), 30, replace=T)), data.frame( from=sample(all_leaves[25:30], 30, replace=T) , to=sample( all_leaves[55:60], 30, replace=T)), data.frame( from=sample(all_leaves[75:80], 30, replace=T) , to=sample( all_leaves[55:60], 30, replace=T)) )
connect$value=runif(nrow(connect))


edges<-read.csv("extras/LEGEND_HTN_Guidance_comparisons_edges.csv")
connect<-read.csv("extras/LEGEND_HTN_Guidance_comparisons_connect.csv")
connect$value <- connect$value / 100000

# create a vertices data.frame. One line per object of our hierarchy
vertices = data.frame(
  name = unique(c(as.character(edges$from), as.character(edges$to))) ,
  value = .5
)
# Let's add a column with the group of each name. It will be useful later to color points
vertices$group = edges$from[match(vertices$name, edges$to)]


#Let's add information concerning the label we are going to add: angle, horizontal adjustement and potential flip
#calculate the ANGLE of the labels
vertices$id = NA
myleaves = which(is.na( match(vertices$name, edges$from) ))
nleaves = length(myleaves)
vertices$id[ myleaves ] = seq(1:nleaves)
vertices$angle = 90 - 360 * vertices$id / nleaves

# calculate the alignment of labels: right or left
# If I am on the left part of the plot, my labels have currently an angle < -90
vertices$hjust <- ifelse( vertices$angle < -90, 1, 0)

# flip angle BY to make them readable
vertices$angle <- ifelse(vertices$angle < -90, vertices$angle + 180, vertices$angle)

# Create a graph object
mygraph <- graph_from_data_frame(edges, vertices = vertices)

# The connection object must refer to the ids of the leaves:
from = match(connect$from, vertices$name)
to = match(connect$to, vertices$name)

groups <- levels(vertices$group)
groups <- groups[groups != "origin"]
martijnColors <- c(rgb(0,0,0), rgb(0,129/255, 180/255))
colors <- rep(martijnColors, length.out =length(groups))
names(colors) <- groups

png("fullcolor.png", units = "cm", height = 18, width = 18, res = 300)
# pdf("fullcolor_RCT.pdf", height = 8, width= 8)
ggraph(mygraph, layout = 'dendrogram', circular = TRUE) +
    geom_conn_bundle(data = get_con(from = from, to = to), alpha = 0.2, width = 0.9, tension = tension) +
    geom_node_text(aes(x = x*1.1, y = y*1.1, filter = leaf, label = name, angle = angle, hjust = hjust, colour = group), size = 5, alpha = 1) +
    geom_node_point(aes(filter = leaf, x = x*1.04, y = y*1.04, colour = group), size = 5, alpha = 0.6) +
    scale_colour_manual(values = colors) +
    scale_size_continuous(range = c(0.1,10) ) +
    theme_void() +
    theme(
        legend.position="none",
        plot.margin=unit(c(0,0,0,0),"cm"),
    ) +
    expand_limits(x = c(-1.55, 1.45), y = c(-1.45, 1.5))
dev.off()

#
# Get LEGEND exposures IDs
#

# legendIds <- exposures %>% filter(exposureGroup == "Drug") %>% filter(!grepl("&", exposureName))
# legendIds$exposureName <- as.character(legendIds$exposureName)
# legendIds$exposureName[which(legendIds$exposureId == 974166)] <- "hydrochlorothiazide"
# legendIds$exposureName <- tolower(legendIds$exposureName)
# legendIds <- rbind(legendIds,
#                    exposures %>% filter(exposureId == 1035))
# legendIds$exposureName[which(legendIds$exposureId == 1035)] <- "co-amilozide"
#
# mapPatrickToLegend <- legendIds$exposureId
# names(mapPatrickToLegend) <- legendIds$exposureName
#
# legendIds <- legendIds[, c("exposureId", "exposureName")]
#
# allCombinations <- combn(legendIds$exposureName, 2, simplify = FALSE)

# results <- lapply(allCombinations, function(pair) {
#   result <- getMainResults(connection,
#                targetIds = c(mapPatrickToLegend[pair[1]]),
#                comparatorIds = c(mapPatrickToLegend[pair[2]]),
#                databaseIds = "Meta-analysis",
#                outcomeIds = c(36),
#                analysisIds = c(1))
#   total <- 0
#   if (nrow(result) > 0) {
#     total <- result$targetSubjects + result$comparatorSubjects
#   }
#
#   list(x = pair[1],
#        y = pair[2],
#        n = total)
# })
#
# tmp <- as.data.frame(do.call(rbind, results))
# results <- data.frame(
#   from = unlist(tmp$y),
#   to = unlist(tmp$x),
#   value = unlist(tmp$n),
#   study = "Meta"
# )
# write.csv(results, file = "Meta_connect.csv", quote = FALSE)
#
# connectCCAE <- read.csv("extras/CCAE_connect.csv")
# connectCCAE <- connectCCAE %>% filter(value > 0)
# connectCCAE$value <- connectCCAE$value / 100000
#
# # Create a graph object
# graphCCAE <- graph_from_data_frame( edges, vertices=vertices )
#
# # The connection object must refer to the ids of the leaves:
# from = match( connectCCAE$from, vertices$name)
# to = match( connectCCAE$to, vertices$name)
#
# pdf("fullcolor_CCAE.pdf", height = 8, width= 8)
# ggraph(graphCCAE, layout = 'dendrogram', circular = TRUE) +
#   geom_conn_bundle(data = get_con(from = from, to = to), alpha=0.2, width=0.9, # aes(colour=..index..),
#                    tension = tension) +
#   # scale_edge_colour_distiller(palette = "RdPu") +
#
#   geom_node_text(aes(x = x*1.15, y=y*1.15, filter = leaf, label=name, angle = angle, hjust=hjust, colour=group), size=2, alpha=1) +
#
#   geom_node_point(aes(filter = leaf, x = x*1.07, y=y*1.07, colour=group, size=value, alpha=0.2)) +
#   scale_colour_manual(values= rep( brewer.pal(9,"Paired") , 30)) +
#   scale_size_continuous( range = c(0.1,10) ) +
#
#   theme_void() +
#   theme(
#     legend.position="none",
#     plot.margin=unit(c(0,0,0,0),"cm"),
#   ) +
#   expand_limits(x = c(-1.3, 1.3), y = c(-1.3, 1.3))
# dev.off()

#

connectMeta <- read.csv("extras/LEGEND_Meta_connect.csv")
connectMeta$value <- connectMeta$value / max(connectMeta$value)

# Create a graph object
graphMeta <- graph_from_data_frame(edges, vertices = vertices)

# The connection object must refer to the ids of the leaves:
from = match(connectMeta$from, vertices$name)
to = match(connectMeta$to, vertices$name)

png("fullcolorMeta.png", units = "cm", height = 18, width = 18, res = 300)
ggraph(mygraph, layout = 'dendrogram', circular = TRUE) +
    geom_conn_bundle(data = get_con(from = from, to = to), alpha = 0.2, width = 0.9, tension = tension) +
    geom_node_text(aes(x = x*1.1, y = y*1.1, filter = leaf, label = name, angle = angle, hjust = hjust, colour = group), size = 5, alpha = 1) +
    geom_node_point(aes(filter = leaf, x = x*1.04, y = y*1.04, colour = group), size = 5, alpha = 0.6) +
    scale_colour_manual(values = colors) +
    scale_size_continuous(range = c(0.1,10) ) +
    theme_void() +
    theme(
        legend.position="none",
        plot.margin=unit(c(0,0,0,0),"cm"),
    ) +
    expand_limits(x = c(-1.55, 1.45), y = c(-1.45, 1.5))
dev.off()
