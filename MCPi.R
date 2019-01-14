# ----- Load necessary libraries ------------------------------------------------------------------

# Packages for plotting and graphics
library(tidyverse)
library(grid)
library(gridBase)





# ----- Functions to estimate pi and generate graphics --------------------------------------------

# Write function to estimate pi and visualise estimations
MCPi <- function(n){
  
  # Let n = the number of simulation repetitions
  
  # Estimate pi
  rand.x <- sample(seq(0, 1, 1e-6), size = n, replace = TRUE)
  rand.y <- sample(seq(0, 1, 1e-6), size = n, replace = TRUE)
  radius <- (rand.x^2) + (rand.y^2)
  piE <- length(radius[radius <= 1])/n*4
  
  # Show visualisations only for certain values of n
  if(n == 100 || n == 1000 || n == 10000 || n == 100000){
    
    # Set up plotting area
    grid.newpage()
    plot.new()
    vp <- viewport(x = 0.5, y = 0.5)
      
    # Points inside quarter-circle are black, points outside are red
    grid.points(x = rand.x, y = rand.y, pch = 16, size = unit(3, "pt"), vp = vp,
                gp = gpar(col = ifelse((rand.x)^2 + (rand.y)^2 < 1, 
                                        "dodgerblue", "yellow")))
    
    # Plot boundary line for quarter circle
    grid.xspline(x = c(0, sin((1:19)*pi/(40)), 1), 
                 y = c(1, cos((1:19)*pi/(40)), 0), 
                 shape = -1, gp = gpar(col = "firebrick1", lwd = 3))
    
    # Save graph to global environment to use later
    assign(paste0("PiPlot", n), grid.grab(), envir = as.environment(.GlobalEnv))}
  
  return(piE)}





# ----- Estimate pi and show estimations as function of n -----------------------------------------

# x-values, spaced for logarithmic x-axis
val.x <- c(1:9, seq(10, 20, 2), seq(20, 50, 5), seq(60, 90, 10), seq(100, 900, 100),
           seq(1000, 9000, 1000), seq(10000, 100000, 10000), 200000, 500000, 1000000,
           2000000, 5000000, 10000000)
val.y <- sapply(val.x, MCPi)

ggplot() + aes(x = val.x, y = val.y) +
  geom_hline(yintercept = pi, colour = "firebrick1", size = 1) +
  geom_line(size = 1.15, colour = "dodgerblue") +
  scale_x_log10(breaks = c(1, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8),
                labels = scales::trans_format("log10", scales::math_format(10^.x)),
                minor_breaks = NULL) +
  scale_y_continuous(minor_breaks = NULL) +
  xlab("Total number of points placed (n)") +
  ylab("Approximation of pi  ") +
  theme(panel.grid.major = element_line(colour = "white"), 
        panel.grid.minor = element_line(colour = "white"),
        panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"),
        axis.text.x = element_text(colour = "white", size = 23, 
                                   margin = margin(t = 4, r = 0, b = 0, l = 0)),
        axis.text.y = element_text(colour = "white", size = 24, 
                                   margin = margin(t = 0, r = 4, b = 0, l = 0)),
        axis.title.x = element_text(colour = "white", size = 31,
                                    margin = margin(t = 8, r = 0, b = 0, l = 6)),
        axis.title.y = element_text(colour = "white", size = 31,
                                    margin = margin(t = 80, r = 8, b = 0, l = 0)),
        axis.ticks.x = element_line(colour = "white", size = 0.5),
        axis.ticks.y = element_line(colour = "white", size = 0.5),
        axis.ticks.length = unit(8, "points")) -> MCPiPlot





# ----- Plot all graphs in one visualisation ------------------------------------------------------

# Prepare graphics device
jpeg(filename = "MCPiPlots.jpeg", width = 2200, height = 1500, units = "px")

# Create blank page
grid.newpage()
plot.new()

# Set grid layout and activate it; create black background
gly <- grid.layout(1500, 2200)
pushViewport(viewport(layout = gly, gp = gpar(fill = "black")))
grid.rect(gp = gpar(lwd = 2, fill = "black", col = "black"))

# Place graphs and encolse them in borders
print(MCPiPlot, vp = viewport(layout.pos.row = 200:850, layout.pos.col = 50:2150))
pushViewport(viewport(layout.pos.row = 900:1400, layout.pos.col = 50:550))
grid.draw(PiPlot100)
grid.rect(gp = gpar(lwd = 2.5, col = "white", fill = "transparent"))
popViewport()
pushViewport(viewport(layout.pos.row = 900:1400, layout.pos.col = 583:1083))
grid.draw(PiPlot1000)
grid.rect(gp = gpar(lwd = 2.5, col = "white", fill = "transparent"))
popViewport()
pushViewport(viewport(layout.pos.row = 900:1400, layout.pos.col = 1116:1616))
grid.draw(PiPlot10000)
grid.rect(gp = gpar(lwd = 2.5, col = "white", fill = "transparent"))
popViewport()
pushViewport(viewport(layout.pos.row = 900:1400, layout.pos.col = 1650:2150))
grid.draw(`PiPlot1e+05`)
grid.rect(gp = gpar(lwd = 2.5, col = "white", fill = "transparent"))
popViewport()

# Add title text
grid.text(label = c("A Monte Carlo approximation of pi",
                    "using randomly-generated points"),
          x = rep(0.022, 2), y = c(0.961, 0.912), just = "left",
          gp = gpar(fontsize = 80, col = "white"))

# Add subtitle text
grid.text(label = c("Points are randomly generated inside of a 1 x 1 unit",
                    "square. The ratio of blue points (inside the quarter-",
                    "circle) to the total number of points in the square,", 
                    "when multiplied by 4, gives an approximation of pi."), 
          x = rep(0.59, 4), y = c(0.967, 0.945, 0.923, 0.901), just = "left",
          gp = gpar(fontsize = 37, col = "white", fontface = "italic"))

# Add text below plots using for loop since bquote doesn't play nice with label vector
for(i in 1:4){
  vals <- c(1e2, 1e3, 1e4, 1e5)
  placement <- c(0.134, 0.378, 0.618, 0.859)
  grid.text(label = bquote("n = 10"^.(i + 1)*", "*pi %~~% .(sprintf("%.3f", round(val.y[which(val.x >= vals[i])[1]], 3)))),
            x = placement[i], y = 0.04, just = "centre",
            gp = gpar(fontsize = 37, col = "white", fontface = "italic"))}

# Deactivate grid layout; finalise graphics save
popViewport()
dev.off()

# Note that graphics device may vary between computers
# Thus, adjustments may need to be made to this code section

# Clean up mess
remove(i, val.x, val.y, vals)

