library(data.tree)
library(treemap)
library(networkD3)
library(htmltools)
library(shinyjqui)
library(plotly)
cat("\014")
site <- "ALL"
nodes <- read.csv(file.path("sitestructure.csv"), header=TRUE, sep=",",stringsAsFactors=TRUE,as.is=TRUE)

#fix data field so it os of the appropriate datatype for date calculations
nodes$lastmodified <- as.Date(nodes$lastmodified, "%m/%d/%Y")
#categorise the number of months as red/amber/green
nodes$months <- difftime(Sys.Date(), nodes$lastmodified, units = "weeks")/4
nodes$colour[nodes$months >= 0 & nodes$months <  3]  = "<3 mo"
nodes$colour[nodes$months >= 3 & nodes$months <  12]  = ">3 mo <12 mo"
nodes$colour[nodes$months >= 12 ] = ">12 mo"

nodes$colour <- as.character(nodes$colour)
#sizes of the nodes target portray the stargetcoloure a site contains

#relationsships between the nodes intarget an links dataframe
nodes$source <- seq.int(nrow(nodes))-1
nodes$target<-with(nodes, source[match(nodes$parent, uri)])
nodes[is.na(nodes)] <- 0
nodes$logsize <- (log10(as.double(gsub(",","",nodes$size)))+2)
nodes$logsize <- as.integer(nodes$logsize)
nodes$size <- (as.double(gsub(",","",nodes$size)))+2
#nodes$source <- as.character(nodes$source)
#nodes$target <- as.character(nodes$target)
nodes$target <- as.integer(nodes$target)
nodes$title <- as.character(nodes$title)
links <- nodes[c("source","target","size","uri")]
links$size <- as.integer(nodes$logsize)
#links$source <- as.character(links$source)
#links$target <- as.character(links$target)
links$target <- as.integer(links$target)
links$value = 1
head(nodes,6)
head(links,6)
str(links)
str(nodes)

sorted <- sort(nodes$lastmodified)

# script <- 'alert(123);
# alert("row: " + (d.index + 1) +
# ", name: "  + d.name +
# ", group: " + d.group +
# ", size: "  + d.size +
# ", lastmodified: "  + d.lastmodified +
# ", logsize: "  + d.logsize); ' ;#+ noquote('$("#infoPanel").html("qgwgfkygfkjg")')


script <- noquote('$("#infoLeft").html("no. of sites: "+d.count+"<br/>name: "+d.name+"<br/>template: "+d.template+" <br/>size: "+d.size+"Mb<br/>last modified:"+d.lastmodified)')
infoRightContent <- HTML("<div id='infoLeft' style='float:left;width:50%'>no. of sites: ",nrow(nodes),"<br/>name:<br/>template: <br/>size: <br/>last modified:</div><div id='infoRight'
                         style='float:right;width:32%;'>mean size of nodes: ",as.character(round(mean(nodes$size),2))," <em>stdev: ",round(sd(nodes$size),2),"</em><br/>min size:",as.character(min(nodes$size)),
                         "<br/>max size:",as.character(max(nodes$size)),"<br/>date range from: ",as.character.Date(sorted[1])," to: ",as.character.Date(sorted[nrow(nodes)]),"</div>");
# issue the ploty is emitting html not being parsed
# infoRightContent <- HTML("<div id='infoLeft' style='float:left;width:32%'>no. of sites: ",nrow(nodes),"<br/>name:<br/>template: <br/>size: <br/>last modified:</div><div id='infoRight'
#                          style='float:right;width:32%;'>mean size of nodes: ",as.character(round(mean(nodes$size),2))," <em>stdev: ",round(sd(nodes$size),2),"</em><br/>min size:",as.character(min(nodes$size)),
#                          "<br/>max size:",as.character(max(nodes$size)),"<br/>date range from: ",as.character.Date(sorted[1])," to: ",as.character.Date(sorted[nrow(nodes)]),"</div><div id='infoPlot' style='float:right;width:32%;'>",htmlPreserve(plot_ly(x = rnorm(100))),"</div>")
sorted <- sort(nodes$lastmodified)
ColourScale <- 'd3.scaleOrdinal()
.domain(["<3 mo", ">3 mo <12 mo",">12 mo"])
.range(["#00FF00", "#FFFF00", "#FF0000"]);'
fn <- browsable(
  tagList(
    tags$head(
      tags$script(src="/Users/tswo10/tris\ R/jquery-3.3.1.min.js"),
      tags$script(HTML('$(document).ready(function(){
                       $(".node circle").attr("style","stroke-width:0px");
                       });
                       ')),
      tags$style('
                 body{
                    background-color: #020269 !important;
                    font-family: "Source Code Pro", Consolas, monaco, monospace;
                    line-height: 160%;
                    font-size: 16px !important;
                    margin: 0;
                 }
                 h1 {color:#FFFFFF}
                 h2 {color:#FFFFFF}
                 .nodetext{fill: #FFFFFF;font-family: "Source Code Pro", Consolas, monaco, monospace !important; font-size: 12px !important;}
                 .legend text{fill: #FFFFFF;font-family: "Source Code Pro", Consolas, monaco, monospace !important;}
                 ')
    ),
    tags$h1("All site contents"),
    tags$div(infoRightContent,
              id='infoPanel',
              style='position: absolute;
                      color:white;
                      float:right;
                       bottom: 2px;
                       width: 100%;
                       border: 3px solid yellow;'
    ), 
    tags$button("Show labels", id='toggle',onclick='
                if($(".nodetext").first().css("opacity")==0) {
                  console.log("ok");
                  $(".nodetext").animate({
                    opacity: 1
                  }, 1000, function() {
                    // Animation complete.
                  });
                  $("#toggle").html("Hide Labels");
                } else {
                  console.log("not ok");
                  $(".nodetext").animate({
                    opacity: 0
                  }, 1000, function() {
                    // Animation complete.
                  });
                  $("#toggle").html("Show Labels");
                }'
    ),   
    forceNetwork(
      width = 2400,
      height = 1100,
      Links = links,
      Nodes = nodes,
      Source = "target",
      Target = "source",
      NodeID ="title",
      opacityNoHover = 0,
      opacity = 1,
      Nodesize = "logsize",
      radiusCalculation = "d.nodesize+4",
      Group = "colour",
      legend = TRUE,
      zoom = TRUE,
      arrows = TRUE,
      bounded = FALSE,
      Value="value",
      colourScale = JS(ColourScale))
  )

)
fn[[5]]$x$nodes$hyperlink <- paste0(
  nodes$uri
)

#fn[[5]]$x$options$clickAction = '$("infoPanel").html("qgwgfkygfkjg");//window.open(d.hyperlink);'

fn[[5]]$x$nodes$logsize <- paste0(
  nodes$logsize
)
fn[[5]]$x$nodes$size <- paste0(
  nodes$size
)
fn[[5]]$x$nodes$lastmodified <- paste0(
  nodes$lastmodified
)
fn[[5]]$x$nodes$template <- paste0(
  nodes$template
)

fn[[5]]$x$nodes$count <- paste0(
  nrow(nodes)
)

fn[[5]]$x$options$clickAction = script;
fn


# mean(nodes$logsize)

# hist(nodes$size)