library(tidyverse)
library(ggthemes)
library(ggExtra)
library(viridis)
library(beepr)

library(ggpubr)
library(egg)
#library(cowplot)
library(ggradar)
library(scales)

library(extrafont)
library(extrafontdb)
font_import()



#devtools::install_github("brooke-watson/BRRR")
library("BRRR")
alarm <- function(n, x){
  Sys.sleep(n)
  skrrrahh(x)
}

for (i in 1:51) {
  alarm(1, i)  
}
alarm(0.2, 23)

is_my_script_done <- function(){
  Sys.sleep(0.2)
  skrrrahh(23)
}
is_my_script_done()


ggplot(mtcars, aes(wt,mpg))+
  geom_point(aes(color = ifelse(wt>3, mpg, 0)))+
  scale_y_continuous(expand = c(0,0))+
  expand_limits(y = c(0,1.05 * max(mtcars$mpg)))+
  theme(text=element_text(family="Roboto Condensed", colour="black"), axis.ticks = element_blank())+
  xlab("Car weight (lb/1000)")+ 
  ylab("Miles per gallon of fuel")+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0, unit = "pt")))+
  theme(axis.line.x = element_line(colour = "blue", size = 3))

extrafont::loadfonts()
extrafont::fonttable()
fonttable()


ggplot(mtcars, aes(wt,mpg))+
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x)

anova(lm(mtcars, formula = mpg ~ wt))

# here's a range-frame rug scatterplot, with the axis labels denoting the quantile values
p1 <- ggplot(mtcars, aes(wt,mpg))+
  geom_point()+
  geom_rug()+
  theme_tufte()+
  theme(text=element_text(family="Roboto Condensed", colour="black"), axis.ticks = element_blank())+
  xlab("Car weight (lb/1000)")+ 
  ylab("Miles per gallon of fuel")+ 
  scale_x_continuous(breaks = round(as.vector(quantile(mtcars$wt)), digits = 1))+
  scale_y_continuous(breaks = round(as.vector(quantile(mtcars$mpg)), digits = 1))
p1

# You can take out the geom_rug if you don't want the rug plot in the margins, for an extra minimal look
p2 <- ggplot(mtcars, aes(wt,mpg))+
  geom_point()+
  theme_tufte()+
  theme(text=element_text(family="Roboto Condensed", colour="black"), axis.ticks = element_blank())+
  xlab("Car weight (lb/1000)")+ 
  ylab("Miles per gallon of fuel")+ 
  scale_x_continuous(breaks = round(as.vector(quantile(mtcars$wt)), digits = 1))+
  scale_y_continuous(breaks = round(as.vector(quantile(mtcars$mpg)), digits = 1))
p2

# here we'll make some marginal histogram and density plots
p <- ggplot(mtcars, aes(wt,mpg))+
  geom_point()+
  theme_tufte()+
  theme(text=element_text(family="Roboto Condensed", colour="black"), axis.ticks = element_blank())+
  xlab("Car weight (lb/1000)")+ 
  ylab("Miles per gallon of fuel")+ 
  scale_x_continuous(breaks = round(as.vector(quantile(mtcars$wt)), digits = 1))+
  scale_y_continuous(breaks = round(as.vector(quantile(mtcars$mpg)), digits = 1))

p3 <- ggMarginal(p, type = "histogram", size = 6, fill = "white", xparams = c(binwidth=0.05), yparams = c(binwidth=0.3))
p3
p4 <- ggMarginal(p, size = 6, lty = 3)
p4

# adding in a little color
p5 <- ggplot(mtcars, aes(wt,mpg))+
  geom_point(aes(color=as.factor(gear)))+
  theme_tufte()+
  theme(text=element_text(family="Roboto Condensed", colour="black"), axis.ticks = element_blank())+
  xlab("Car weight (lb/1000)")+ 
  ylab("Miles per gallon of fuel")+ 
  scale_x_continuous(breaks = round(as.vector(quantile(mtcars$wt)), digits = 1))+
  scale_y_continuous(breaks = round(as.vector(quantile(mtcars$mpg)), digits = 1))+
  scale_color_brewer(type = "seq", palette = 3, name = "# of\nCylinders")+
  theme(legend.position = c(0.8,0.7), legend.title.align = 0.5)
p5


ggplot(mtcars, aes(x=wt,y=mpg))+
  geom_point(aes(color=cyl))+
  xlab('Car weight (lb/1000)')+
  ylab('Miles per gallon of fuel')+
  theme(panel.background = element_blank())+
  theme(panel.grid.minor = element_blank())+
  theme(panel.grid.major = element_line(colour = rgb(0,0,0,0.2), linetype = "dotted"))+
  theme(panel.border = element_blank())+
  theme(axis.ticks = element_blank())+
  scale_x_continuous(breaks = round(as.vector(quantile(mtcars$wt)), digits = 1))+
  scale_y_continuous(breaks = round(as.vector(quantile(mtcars$mpg)), digits = 1))+
  scale_color_viridis()+
  theme(legend.position = c(0.9,0.75), legend.title.align = 0.5)

ggpubr::ggarrange(p1+theme(axis.title.x = element_blank()), 
                  p2+theme(axis.title.x = element_blank())+theme(axis.title.y = element_blank()), 
                  p4, 
                  p5+theme(axis.title.y = element_blank()), ncol = 2, nrow = 2)



library(skimr)
skim(mtcars)
mtcars %>% 
  group_by(cyl) %>% 
  summarise(mean(wt)) %>% 
  skim()

skim(mtcars, cyl)
skim(mtcars)[[6]]
str(skim(mtcars))
as.data.frame(skim(mtcars))

library(deconstructR)
N <- 20
x <- rnorm(N)
y <- rnorm(N)+0.3*x

xlab <- "Chronic Functional Limitations"
ylab <- "Depressive Affect"


gravity(x,y, xlab=xlab, ylab=ylab)
deconstructR::blackhole(x,y, xlab=xlab, ylab=ylab)


library(particles)
library(tidygraph)
library(jsonlite)
library(magick)
library(ggraph)

sim <- create_ring(10) %>% 
  simulate(velocity_decay = 0.6, setup = petridish_genesis(vel_max = 0)) %>% 
  wield(link_force) %>% 
  wield(manybody_force) %>% 
  impose(polygon_constraint, 
         polygon = cbind(c(-100, -100, 100, 100), c(-100, 100, 100, -100))) %>% 
  evolve(100)


ggraph(as_tbl_graph(sim)) + 
  geom_edge_link() + 
  geom_node_point() + 
  theme_graph()

sim <- sim %>% 
  unwield(2) %>% 
  wield(manybody_force, strength = 30) %>% 
  reheat(1) %>% 
  evolve()

ggraph(as_tbl_graph(sim)) + 
  geom_edge_link() + 
  geom_node_point() + 
  theme_graph()

sim <- play_islands(3, 10, 0.6, 3) %>% 
  mutate(group = group_infomap()) %>% 
  activate(edges) %>% 
  mutate(weight = ifelse(.N()$group[to] == .N()$group[from], 1, 0.25)) %>% 
  simulate() %>% 
  wield(link_force, strength = weight, distance = 10/weight) %>% 
  evolve()

ggraph(as_tbl_graph(sim)) + 
  geom_edge_link(aes(width = weight), alpha = 0.3, lineend = 'round') + 
  geom_node_point() + 
  theme_graph() + 
  theme(legend.position = 'none')

sim <- sim %>% 
  activate(edges) %>% 
  mutate(weight = 1) %>% 
  reheat(1) %>% 
  evolve()

ggraph(as_tbl_graph(sim)) + 
  geom_edge_link(aes(width = weight), alpha = 0.3, lineend = 'round') + 
  geom_node_point() + 
  theme_graph() +
  theme(legend.position = 'none')

volcano_field <- (volcano - min(volcano)) / diff(range(volcano)) * 2 * pi
sim <- create_empty(1000) %>% 
  simulate(alpha_decay = 0, setup = aquarium_genesis(vel_max = 0)) %>% 
  wield(reset_force, xvel = 0, yvel = 0) %>% 
  wield(field_force, angle = volcano_field, vel = 0.1, xlim = c(-5, 5), ylim = c(-5, 5)) %>% 
  evolve(100, record)

traces <- data.frame(do.call(rbind, lapply(sim$history, position)))
names(traces) <- c('x', 'y')
traces$age <- rep(100:1, each = 1000)
traces$particle <- rep(1:1000, 100)

ggplot(traces) +
  geom_path(aes(x, y, group = particle, alpha = age), size = 0.1) + 
  theme_void() + 
  theme(legend.position = 'none')

create_empty(1000) %>% 
  simulate(alpha_decay = 0, setup = aquarium_genesis(vel_max = 0)) %>% 
  wield(reset_force, xvel = 0, yvel = 0) %>% 
  wield(field_force, angle = volcano_field, vel = 0.1, xlim = c(-5, 5), ylim = c(-5, 5)) %>% 
  evolve(100, function(sim) {
    ggplot(as_tibble(sim)) + 
      geom_point(aes(x, y), size = 0.25) + 
      theme_void()
  })

animation1



#### another try

# Data preparation
d3_col <- c(
  '0' = "#98df8a",
  '1' = "#1f77b4",
  '2' = "#aec7e8",
  '3' = "#ff7f0e",
  '4' = "#ffbb78",
  '5' = "#2ca02c",
  '6' = "#d62728",
  '7' = "#ff9896",
  '8' = "#9467bd",
  '9' = "#c5b0d5",
  '10' =  "#8c564b"
)

raw_data <- 'https://gist.githubusercontent.com/mbostock/4062045/raw/5916d145c8c048a6e3086915a6be464467391c62/miserables.json'
miserable_data <- jsonlite::read_json(raw_data, simplifyVector = TRUE)
miserable_data$nodes$group <- as.factor(miserable_data$nodes$group)
miserable_data$links <- miserable_data$links %>% 
  mutate(from = match(source, miserable_data$nodes$id),
         to = match(target, miserable_data$nodes$id))

# Actual particles part
mis_graph <- miserable_data %>% 
  simulate() %>% 
  wield(link_force) %>% 
  wield(manybody_force) %>% 
  wield(center_force) %>% 
  evolve() %>% 
  as_tbl_graph()

# Plotting with ggraph
ggraph(mis_graph, 'nicely') + 
  geom_edge_link(aes(width = sqrt(value)), colour = '#999999', alpha = 0.6) + 
  geom_node_point(aes(fill = group), shape = 21, colour = 'white', size = 4, 
                  stroke = 1.5) + 
  scale_fill_manual('Group', values = d3_col) + 
  scale_edge_width('Value', range = c(0.5, 3)) + 
  coord_fixed() +
  theme_graph()

# Random overlapping circles
graph <- as_tbl_graph(igraph::erdos.renyi.game(100, 0)) %>% 
  mutate(x = runif(100) - 0.5, 
         y = runif(100) - 0.5, 
         radius = runif(100, min = 0.1, 0.2))

# Plotting function
graph_plot <- . %>% {
  gr <- as_tbl_graph(.)
  p <- ggraph(gr, 'manual', node.position = as_tibble(gr)) +
    geom_node_circle(aes(r = radius), fill = 'forestgreen', alpha = 0.5) + 
    coord_fixed(xlim = c(-2.5, 2.5), ylim = c(-2.5, 2.5)) + 
    theme_graph()
  plot(p)
}

# Simulation
graph %>% simulate(velocity_decay = 0.7, setup = predefined_genesis(x, y)) %>% 
  wield(collision_force, radius = radius, n_iter = 2) %>% 
  wield(x_force, x = 0, strength = 0.002) %>% 
  wield(y_force, y = 0, strength = 0.002) %>% 
  evolve(on_generation = graph_plot)

beepr::beep(sound = "wilhelm")
library(lobstr)
ast(a + b + c*d)

