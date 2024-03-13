library(data.table)
library(circlize)
library(RColorBrewer)
library(ComplexHeatmap)
library(gridBase)
library(shinycssloaders)

# calculate the circus position 
getpos <- function(x)
{
   x/sum(x)  ->  y
   x.right  <-  cumsum(y)
   x.left  <-  c(0,head(x.right,-1))
   x.mid  <-  (x.left+x.right)/2
   data.table(left=x.left,mid=x.mid,right=x.right)
}

############
library(shiny)
library(bslib)
library(shinyWidgets)
library(DT)
library(bsicons)

# define UI
ui <- page_navbar(
window_title = 'plot',
theme = bs_theme(verion = '5',bootswatch = 'cosmo') %>%
    bs_add_rules(list('div.accordion-title {font-weight:bold; font-size:16pt}',
	                  ".shiny-output-error { visibility: hidden; }",
                      ".shiny-output-error:before { visibility: hidden; }")),

nav_panel('IBD circos plot',
layout_sidebar(
sidebar = sidebar(
switchInput('shiliData', "Use example data",labelWidth = "80px",onLabel = 'Example',offLabel = 'upload data',value = TRUE),
uiOutput('shiliDataQ'),
uiOutput('popSel'),

verbatimTextOutput('outP'),
width = 300,
open = 'always'
),


navset_underline(

nav_panel('Table',
accordion(
accordion_panel('Pop info',DTOutput('tabSeOUTpop')),
accordion_panel('inter pop info',DTOutput('tabSeOUT'))
)
),

nav_panel('Circos Plot',tags$div(style = "height:800px",withSpinner(plotOutput('outTU'))))
)
)
),

nav_panel('About',
tags$p('circos plot')
)

)



# define server
server <- function(input,output,session){

output$shiliDataQ <- renderUI(
{

if(!input$shiliData)
{
wellPanel(
fileInput('popFile', tooltip(span("Upload Pop csv file",bs_icon("info-circle")), 
                             "Please check example data and familiar with format of csv file: must including two columns: pop, pop_size",  placement = "right"),
accept = c('text/csv',
'text/comma-separated-values,text/plain',
'.csv')),

fileInput('interPopFile', tooltip(span("Upload Inter Pop csv file",bs_icon("info-circle")),  
                                  "Please check example data and familiar with format of inter pop info csv file: must including three columns: pop1, pop2, and mean_pairwise_IBD_length",  placement = "right"),
accept = c('text/csv',
'text/comma-separated-values,text/plain',
'.csv'))

)} else NULL;

}
)

getDataFromFile <- reactive({

file.pop <- ifelse(input$shiliData,'intra_pop_mean_pairwise_ibd_length.csv',input$popFile$datapath)
file.interpop <- ifelse(input$shiliData,'inter_pop_mean_pairwise_ibd_length.csv',input$interPopFile$datapath)

fread(file.pop) -> popinfo
fread(file.interpop) -> interinfo
popinfo[order(pop_size)] -> popinfo

list(popinfo,interinfo)

})



output$popSel <- renderUI({

getDataFromFile() -> fileData

popinfo <- fileData[[1]]
interinfo <- fileData[[2]]

tagList(
selectInput(
   inputId = "whats",
   label = 'Select Pops (please select at least three pops) ', 
    choices = popinfo[,pop],
	selected = if(input$shiliData) c('Latvia-AJ','Basque-France','Baloch','Yemenite-Desert') else popinfo[1:4,pop],
	multiple = TRUE
),
actionButton('tosub','Start!')
)

})



getData <- eventReactive(input$tosub,{

getDataFromFile() -> fileData
popinfo <- fileData[[1]]
interinfo <- fileData[[2]]


   secs <- input$whats
   
   nn <- popinfo[pop %in% secs]
   nn$pop -> pops
   interinfo[pop1 %in% pops & pop2 %in% pops] -> ee
   nn <- nn[,.(pop,pop_size)]
   ee <- ee[,.(pop1,pop2,v = mean_pairwise_IBD_length)]
   ee2 <- copy(ee)
   names(ee2) <- c('pop2','pop1','v')
   eez <- rbind(ee,ee2)
   
   lapply(setNames(,secs),function(x)
   {
      eez[pop1 == x] -> inter
      inter[,pop1z:=match(pop1,secs)]
      inter[,pop2z:=match(pop2,secs)]
      inter[order(pop2z)] ->inter
      getpos(inter$v) -> pos
      pos[,what:=inter[,paste(pop1z,pop2z,sep = "&")]]
      pos
   }
   ) |> rbindlist() -> poss
   
   ee[,pop1z:=match(pop1,secs)]
   ee[,pop2z:=match(pop2,secs)]
   ee[,from:=paste(pop1z,pop2z,sep="&")]
   ee[,to:=paste(pop2z,pop1z,sep="&")]
   ee[,from.left:=poss[match(ee$from,what),left]]
   ee[,from.right:=poss[match(ee$from,what),right]]
   ee[,to.left:=poss[match(ee$to,what),left]]
   ee[,to.right:=poss[match(ee$to,what),right]]
   
   ####
   ee[,range(v)]  ->  color.lim
   col_fun  <-  colorRamp2(color.lim, c("red","blue"))
   
   ####
   #secs.colors  <-  brewer.pal(length(secs),'Set3')
   secs.colors <- rand_color(length(secs))
   secs.colors <- setNames(secs.colors,secs)
   
   list(nn = nn,ee = ee,secs = secs,secs.colors = secs.colors,poss = poss,col_fun = col_fun)
})

output$outTU <- renderPlot({
getData() -> inter
nn <- inter$nn
secs <- inter$secs
ee <- inter$ee
secs.colors <- inter$secs.colors
col_fun <- inter$col_fun

leg1 <- Legend(labels = secs, title = "Pop", legend_gp = gpar(fill = secs.colors))
leg2 <- Legend(col_fun = col_fun, title = "Link",legend_height = unit(50,'mm'))
leg <- packLegend(leg1,leg2)

    plot.new()
circle_size = unit(1, "snpc") # snpc unit gives you a square region

pushViewport(viewport(x = 0, y = 0.5, width = circle_size, height = circle_size,
    just = c("left", "center")))
par(omi = gridOMI(), new = TRUE)


#####circos plot   
    circos.clear()         
    circos.par(cell.padding = c(0,0,0,0), track.margin = c(0,0.01), gap.degree = 1)   
    circos.initialize(sectors = secs,
                      xlim = c(0,1),
                      sector.width = nn[match(secs,pop),pop_size])
    
    				  
    circos.track(ylim = c(0,1),track.height = 0.15,bg.border = 'transparent')
    
    			  
    circos.track(ylim = c(0,1),bg.col = secs.colors,bg.border = 'transparent',
        panel.fun = function(x, y) {
    	CELL_META$sector.index -> lab
    	nn[match(lab,pop),pop_size] -> lab.size
    	#paste0(lab,'(',lab.size,')')  ->  lab
    	lab <- lab.size
            circos.text(CELL_META$xcenter, 
                #CELL_META$cell.ylim[2] + mm_y(2), 
    			CELL_META$ycenter, 
                lab,facing = 'clockwise',niceFacing = T)
    })
    
    
    toe <- ee
    
    for(ind in 1:nrow(toe))
    {
    circos.link(
    sector.index1 = toe[ind,pop1],
    sector.index2 = toe[ind,pop2],
    point1 = c(toe[ind,from.left],toe[ind,from.right]),
    point2 = c(toe[ind,to.left],toe[ind,to.right]),
    border = 'black',
    col = scales::alpha(col_fun(toe[ind,v]),0.5)
    
    )
    }
    
    circos.clear()  
	
	
####
upViewport()
draw(leg, x = circle_size, just = "left")
 
},height=600)

output$tabSeOUT <- renderDT({
  getData() -> inter
  ee <- inter$ee
  datatable(ee[,.(pop1,pop2,mean_pairwise_IBD_length = v)],rownames = F,selection = 'none',options = list(dom = 'tp'))
})

output$tabSeOUTpop <- renderDT({
  getData() -> inter
  datatable(inter$nn,rownames = F,selection = 'none',options = list(dom = 'tp'))
})

}

shinyApp(ui,server)


