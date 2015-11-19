library(networkD3)
library(animation)
library(igraph)
library(ndtv)

# ================ Create EdgeList and Nodelist ================
edges <- read.csv("FINAL_SO_Python_EdgeList.csv", header=T, as.is=T)
nodes <- read.csv("FINAL_SO_Python_NodeList.csv", header=T, as.is=T)

nrow(edges)


# ================ Create Network ================
net3 <- network(edges, vertex.attr = nodes, matrix.type="edgelist", 
                loops=F, multiple=F, ignore.eval = F)

timings <- read.csv("SO_Python_Timings.csv", header = T)
vs <- data.frame(onset=timings[,2], terminus=timings[,3], vertex.id=edges[,2])
es <- data.frame(onset=timings[,2], terminus=timings[,3], 
                 head=as.matrix(net3, matrix.type="edgelist")[,1],
                 tail=as.matrix(net3, matrix.type="edgelist")[,2])

net3.dyn <- networkDynamic(base.net=net3, edge.spells=es)

# ================ Create Animation ================


compute.animation(net3.dyn, animation.mode = "kamadakawai",
                  slice.par=list(start=0, end=64,interval=1, 
                                 aggregate.dur=1, rule='any'))



render.d3movie(net3.dyn, usearrows = F, 
               displaylabels = T, label=net3 %v% "label",
               bg="#ffffff", vertex.border = nodes[,5],
               vertex.cex = 2,  
               vertex.col = nodes[,5],
               edge.length = 100,
               label.cex = 1,
               edge.lwd = (net3.dyn %e% "weight")/10, 
               edge.col = '#55555599',
               vertex.tooltip = paste("<b>Name:</b>", (net3.dyn %v% "label") , "<br>",
                                      "<b>Type:</b>", (net3.dyn %v% "label")),
               edge.tooltip = paste("<b>Edge weight:</b>", (net3.dyn %e% "weight" ) ),
               launchBrowser=T, filename="Py_Dynamic_Viz.html",
               render.par=list(tween.frames = 30, show.time = F),
               plot.par=list(mar=c(0,0,0,0)))
