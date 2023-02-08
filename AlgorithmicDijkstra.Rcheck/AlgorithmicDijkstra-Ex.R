pkgname <- "AlgorithmicDijkstra"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
base::assign(".ExTimings", "AlgorithmicDijkstra-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('AlgorithmicDijkstra')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("BellmanFord")
### * BellmanFord

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: BellmanFord
### Title: BellmanFord naif Algorithm to find shortest path from a node to
###   others
### Aliases: BellmanFord

### ** Examples

vertices <- c("r", "a", "b", "c")
edges <- data.frame(from   = c("r", "a", "b", "c", "c"),
                    to = c("a", "b", "c", "r", "a"),
                    weight = c(3, 1, 2, 7, -4))
start="r"



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("BellmanFord", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("dijkstra")
### * dijkstra

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: dijkstra
### Title: Dijkstra's algorithm, finds shortest distance to other nodes
### Aliases: dijkstra

### ** Examples

graph_DataFrame <-
 data.frame(from=c(1,6,2,2,2,2,7,7,5,3,3),
          to=c(6,1,1,6,5,4,1,5,4,4,2),
          weight=c(5,1,3,11,3,10,5,4,4,7,1))
dijkstra(graph_DataFrame, 3)
graph_DataFrame <-
 data.frame(from=c("1","6","2","2","2","2","7","7","5","3","3"),
          to=c("6","1","1","6","5","4","1","5","4","4","2"),
          weight=c(5,1,3,11,3,10,5,4,4,7,1))
dijkstra(graph_DataFrame,"3")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("dijkstra", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
