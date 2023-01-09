## Session 3, Visualisation in R, Nick's cohort
## 09/01/2023
## Can be combined with powerpoint and excel sheets for a general overview of R. All materials kept with Nick Edmondson

#----Visulisation in R with ggplot2 and plotly-----------------------------------------------------------------------------------

## Visualisation and introduction to ggplot2
## Fantastic learning for graphs in R https://rkabacoff.github.io/datavis/preface.html#how-to-use-this-book
## Load package
library(ggplot2)

## ggplot2
# ggplot2 works by creating multiple geoms that are then layered on top of each other
# This gives the ability to create "bespoke" graphs and show your data in interesting ways
# You should always start a graph with a "simple" graph layout and add geoms on top of this

#----First things first, creating a graph area-----------------------------------------------------------------------------------

# Set working directory to point to data
setwd("C:/Users/lby76f/Desktop/Teaching/Session 1")
all_rounders<-read.csv("All_rounders.csv", header=TRUE, row.names = 1)

# First graph specifies area and data but does not specify what we want to do with the data
ggplot(data = all_rounders, 
       mapping = aes(x = Runs, y = HS))

#----Adding data and defining the graph look-------------------------------------------------------------------------------------

# This adds how we want to show the data (as points), and adds a smoothing effect with specified method (geom_smooth)
ggplot(data = all_rounders, 
       mapping = aes(x = Runs, y = HS)) +
  geom_point() +
  #add smoothing effect as generlaised linear model
  geom_smooth(method = "glm")

# This shows how to change point colour and size, notice that the specifications are contained in the geom_point brackets
ggplot(data = all_rounders, 
       mapping = aes(x = Runs, y = HS)) +
  geom_point(color = "Yellow",
             alpha = .2,
             size = 20) +
  geom_smooth(method = "glm")

#----Defining data with colours-------------------------------------------------------------------------------------------------

# Define whether batters have achieved 100, this code can be found in first session material
all_rounders$hun<-ifelse(all_rounders$Hundreds == 0, 'No', 'Yes')

# We can now define colours using our data above, notice that the smoothing is also colour defined
ggplot(data = all_rounders, 
       mapping = aes(x = Runs, y = HS, color = hun)) +
  geom_point() +
  geom_smooth(method = "glm")

#----Defining graphs with other binary variables--------------------------------------------------------------------------------

# We can now seperate data into bins of bowling average, again, the basics of this code can be found in session 1
all_rounders$average_bin<-ifelse(all_rounders$Ave.1 < 25, "Bowling average under 25",
                                 ifelse(all_rounders$Ave.1 < 30, "Bowling average 25-30",
                                        ifelse(all_rounders$Ave.1 < 35, "Bowling average 30-35", "Bowling average over 35")))

# We can facet the graph (seperate into graphs depending on a variable) with facet_wrap
ggplot(data = all_rounders, 
       mapping = aes(x = Runs, y = HS, color = hun)) +
  geom_point() +
  geom_smooth(method = "glm") +
  facet_wrap(~average_bin)

#----Adding labels to the graph-------------------------------------------------------------------------------------------------

# We can now improve the graph by detailing the labels we would like on the axis, the title, and our colour key with labs(). This is the general code to change labels
# We can also start to change the background of the graph with theme_ code. theme_bw() is a base theme. Other themes can be found https://ggplot2.tidyverse.org/reference/ggtheme.html
ggplot(data = all_rounders, 
       mapping = aes(x = Runs, y = HS, color = hun)) +
  geom_point() +
  geom_smooth(method = "glm") +
  facet_wrap(~average_bin) +
  labs(title = "International all rounders",
       subtitle = "1900 - 2022",
       x = "Total Runs",
       y = "High Score",
       color = "Have they got a hundred?") +
  theme_bw()

# We can change small things within a theme by specifying what we might want to do to the graph within the theme_()
ggplot(data = all_rounders, 
       mapping = aes(x = Runs, y = HS, color = hun)) +
  geom_point() +
  geom_smooth(method = "glm") +
  facet_wrap(~average_bin) +
  labs(title = "International all rounders",
       subtitle = "1900 - 2022",
       x = "Total Runs",
       y = "High Score",
       color = "Have they got a hundred?") +
  theme_bw(base_size = 25,
           base_line_size = 3)

#----Creating your own theme------------------------------------------------------------------------------------------------

# We can also create our own theme (potentially would want to do this if you are typing loads of changes into theme_)) by using preexisitng themes and changing them slightly (of just choose a theme you like!)
# If you want to have a closer look at the details of any themes, just type them into the console.
theme_james <- function(base_size = 15){
  font <- "Georgia"
  theme_bw(base_size = base_size) %+replace%
    theme(panel.background = element_rect(fill = "green"),
          plot.title = element_text(family=font,
                                    size=40,
                                    face="bold",
                                    color = "red"))
}

# Now, lets run our graph again and see what horible things I've done to it
ggplot(data = all_rounders, 
       mapping = aes(x = Runs, y = HS, color = hun)) +
  geom_point() +
  geom_smooth(method = "glm") +
  facet_wrap(~average_bin) +
  labs(title = "International all rounders",
       subtitle = "1900 - 2022",
       x = "Total Runs",
       y = "High Score",
       color = "Have they got a hundred?") +
  theme_james()

#----Defining your graph--------------------------------------------------------------------------------------------------------------------

# We can save the graph as we would for any vector or list
figure_1<-ggplot(data = all_rounders, 
       mapping = aes(x = Runs, y = HS, color = hun)) +
  geom_point() +
  geom_smooth(method = "glm") +
  facet_wrap(~average_bin) +
  labs(title = "International all rounders",
       subtitle = "1900 - 2022",
       x = "Total Runs",
       y = "High Score",
       color = "Have they got a hundred?") +
  theme_bw()

#----Defining graphing area------------------------------------------------------------------------------------------------------------
# Can now fit this graph into an area (only needed if the graph is not fitting nicely into your viewer or if you want to export it in a particular size)
# Opens a new window of width and height as specified, have a play and compare
dev.new(width=5, height=5, unit="in")
figure_1
dev.new(width=40, height=20, unit="in")
figure_1

#----Saving a graph--------------------------------------------------------------------------------------------------------------------
# How to save your graph
# This can be done by following plots->export->save as in the visualisation tab in R studio (can save as PDF/TIFF etc)
# Can also be performed by specifiying a save space, creating your graph, then closing the save space
# speciify save space
jpeg("example_graph.jpg")
# create graph, using one created earlier, but can define with full graphing code
figure_1
# Close save space
dev.off()

#----Plotly----------------------------------------------------------------------------------------------------------------------------

# Plotly works differently from ggplot2 but allows you to create interactive graphs that interested parties can hover over to reveal information
# A good introduction can be found on the plotly website https://plotly.com/r/getting-started/
# Repeating the graphs that we designed in ggplot allows us to compare betwene plotly and ggplot2
library(plotly)
# Plotly has a slightly different figure structure to ggplot2 but still works by creating a plotting "space" and then adding points and information on top of this. The main difference to ggplot2 is that you have to provide a greater amount of information to plotly but the base graphs are cleaner than the base graphs produced in ggplot2
# Base plotly is similar to ggplot2, note the ~before defining axis though
plot_ly(data = all_rounders, x = ~Runs, y = ~HS)

plot_ly(data = all_rounders, x = ~Runs, y = ~HS, color = ~hun, colors = "Set1")

# The big difference in plotly is the ability to define what a point says when you hover over it
plot_ly(data = all_rounders, x = ~Runs, y = ~HS,
        text = ~paste("Total runs:", Runs, "High score:", HS),
        color = ~hun, colors = "Set1")

#----Using ggplot in plotly to make graphs interactive----------------------------------------------------------------------------------
# You can also incorporate ggplot2 into plotly by simply saving a ggplot2 graph as a figure then running this through plotly. This might be the best was of creating impressive figures
p<-ggplotly(figure_1)

#----Creating animations of graphs------------------------------------------------------------------------------------------------------
# As well as creating interactive graphs you can make graphs as animations to provide time series understanding, or just for a bit of fun
fig<-all_rounders %>% 
  plot_ly(x = ~Runs, y = ~HS, color = ~hun, frame = ~average_bin,
        text = ~paste("Total runs:", Runs, "High score:", HS))
fig %>% animation_opts(frame = 2000, transition = 0, redraw = FALSE)