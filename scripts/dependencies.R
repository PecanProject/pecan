#!/usr/bin/env Rscript
##--------------------------------------------------------------------------------------------------#
##'
##' Create a dependency graph for a list of packages. Each package is checked for their dependencies
##' both Depends as well Suggests.
##' 
##' @name dependencygraph
##' @title Create a graph of all package dependencies
##' @export 
##' @examples
##' dependencygraph(c("igraph"), suggests=TRUE)
##' @author Rob Kooper
##'
dependencygraph <- function(packages, filename='', suggests=FALSE, filter=NA) {
    require(graphics)
    require(igraph)

    graphData <- data.frame(from=numeric(0), to=numeric(0), relationship=numeric(0))
    seen <- c()

    scanPackageField <- function(package, field) {
        packages <- packageDescription(package, fields=field)
        packages <- gsub('\n', '', packages)
        packages <- strsplit(packages, ',[ \t]*')[[1]]
        packages <- gsub(' ?\\(.*', '', packages)
        for(x in na.omit(packages)) {
            if (x == 'R') {
                next
            }
            if (!is.na(filter) && length(grep(filter, x)) == 0) {
                next
            }
            p <- gsub(' (.*)', '', x)
            graphData[nrow(graphData)+1,] <<- c(package, p, field)
            scanPackage(p)
        }
    }

    scanPackage <- function(package) {
        if (!(package %in% seen)) {
            seen <<- c(seen, package)
            scanPackageField(package, "Depends")
            if (suggests) {
                scanPackageField(package, "Suggests")
            }
        }
    }

    for(p in packages) {
        scanPackage(p)
    }

    graph <- graph.data.frame(graphData)

    V(graph)$size <- 10
    V(graph)$color <- 'white'
    V(graph)$label.color <- 'black'

    E(graph)$arrow.size <- 0.5
    E(graph)[relationship == 'Depends']$color <- 'red'
    E(graph)[relationship == 'Suggests']$color <- 'blue'
    #colors <- heat.colors(vcount(graph))
    #E(graph)$color <- colors[match(graphData$to, unique(graphData$to))]

    set.seed(3952)
    layout <- layout.fruchterman.reingold.grid(graph)
    #layout <- layout.circle(graph)

    if (length(grep("\\.pdf$", filename)) != 0) {
        pdf(file=filename)
        plot(graph, layout=layout, frame=true)
        dev.off()
    } else if (length(grep("\\.png$", filename)) != 0) {
        png(file=filename, width=1280, heigh=1024)
        plot(graph, layout=layout)
        dev.off()
    } else if (length(grep("\\.jpg$", filename)) != 0) {
        jpeg(file=filename, width=1280, heigh=1024)
        plot(graph, layout=layout)
        dev.off()
    } else if (length(grep("\\.svg$", filename)) != 0) {
        svg(file=filename, width=10, heigh=10)
        plot(graph, layout=layout, frame=TRUE)
        dev.off()
    } else  if (length(grep("\\.csv$", filename)) != 0) {
        write.table(graphData, file=filename, row.names=FALSE, sep=',')
    } else  if (length(grep("\\.txt$", filename)) != 0) {
        write.table(graphData, file=filename, row.names=FALSE, sep='\t')
    } else {
        plot(graph)
    }
}

packages <- c("PEcAn.BIOCRO", "PEcAn.DB", "PEcAn.ED2", "PEcAn.MA",
              "PEcAn.SIPNET", "PEcAn.assim.batch", "PEcAn.assim.sequential",
              "PEcAn.data.atmosphere", "PEcAn.data.land", "PEcAn.priors",
              "PEcAn.settings", "PEcAn.uncertainty", "PEcAn.utils",
              "PEcAn.visualization")

#dependencygraph(packages, suggests=FALSE, filename="graph.png")
dependencygraph(packages, suggests=TRUE, filename="graph.png", filter="PEcAn")
