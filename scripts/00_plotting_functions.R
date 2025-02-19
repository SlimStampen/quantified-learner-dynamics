theme_ml <- function() { 
  require(ggplot2)
  require(showtext)
  font <- font_add_google("Lato", family = "Lato")
  showtext_auto()
  showtext_opts(dpi = 300)
  
  theme_bw() %+replace%    #replace elements we want to change
    
    theme(
      
      strip.background = element_rect(
        color="black", fill= 'white', size=0),
      
      axis.line = element_line(colour = "black"),
      panel.grid.major = element_blank(),
      panel.grid.major.y = element_line(colour = "grey90"),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      
      #text elements
      plot.title = element_text(             #title
        family = "Lato",            #set font family
        size = 13,                #set font size
        face = 'bold',            #bold typeface
        hjust = 0,                #left align
        margin=margin(10,0,0,0),
        vjust = 2),               #raise slightly
      
      plot.subtitle = element_text(          #subtitle
        family = "Lato",            #font family
        size = 11,
        margin=margin(10,0,10,0),
        hjust = 0),               #font size
      
      plot.caption = element_text(           #caption
        family = "Lato",            #font family
        color = "grey50",
        size = 8, 
        margin=margin(10,0,0,0), #font size
        hjust = 1),               #right align
      
      axis.title = element_text(             #axis titles
        family = "Lato",            #font family
        size = 13),               #font size
      
      axis.text = element_text(              #axis text
        family = "Lato",            #axis famuly
        size = 13),                #font size
      
      axis.text.x = element_text(            #margin for axis text
        margin=margin(5, b = 10)) 
      
      #since the legend often requires manual tweaking 
      #based on plot content, don't define it here
    )
}