##### Load necessary libraries ----------------------------------------------------------------------------

# Packages for plotting and graphics
library(tidyverse)
library(grid)
library(gridBase)





##### Write statistical functions -------------------------------------------------------------------------

# Function to calculate number of flips required for n consecutive heads or tails
CoinFlip <- function(n){

  # Let h.con = current number of consecutive heads
  #     t.con = current number of consecutive tails
  #     curr = result of current coin flip
  #     prev = result of coin flip prior to current flip
  
  # Set initial conditions
  h.con <- 0
  t.con <- 0
  i <- 0

  # First coin flip
  curr <- sample(c("H", "T"), size = 1, prob = c(0.5, 0.5))

  # First flip gives 1 "consecutive" heads or tails; also serves as "previous" flip
  if(curr == "H"){
    h.con <- 1
    t.con <- 0
    prev <- "H"} else {
                   h.con <- 0
                   t.con <- 1
                   prev <- "T"}

  # Increase flip counter by 1
  i <- 1

  # Keep flipping until n consecutive heads or tails occur
  while(h.con < n && t.con < n){
    
    # Current coin flip
    curr <- sample(c("H", "T"), size = 1, prob = c(0.5, 0.5))
    
    # Previous and current flips are both heads
    if(prev == "H" & curr == "H"){
      h.con <- h.con + 1
      t.con <- 0}
    
    # Previous flip is heads but current flip is tails
    if(prev == "H" & curr == "T"){
      h.con <- 0
      t.con <- 1}
    
    # Previous flip is tails but current flip is heads
    if(prev == "T" & curr == "H"){
      h.con <- 1
      t.con <- 0}
    
    # Previous and current flips are both tails
    if(prev == "T" & curr == "T"){
      h.con <- 0
      t.con <- t.con + 1}
    
    # Result of current flip becomes previous flip relative to next flip
    prev <- curr
    
    # Increase flip counter by 1
    i <- i + 1}
  
  # Output final number of flips
  return(i)}      

# Function to calculate expected value (EV) of i (i.e. mean number of flips needed)
EV <- function(m, n){
  
  # Let m = number of times the coin flip simulation is performed
  #     n = number of consecutive heads or tails
  
  # Calculate EV
  mean(replicate(m, CoinFlip(n), simplify = "vector"))}





##### Plot EV convergence for select values of n ----------------------------------------------------------

# Function to create plots of EV for several different values of n
EVPlots <- function(n){
  
  # x-values, spaced for logarithmic x-axis
  val.x <- c(1:9, seq(10, 20, 2), seq(20, 50, 5), seq(60, 90, 10), seq(100, 900, 100),
             seq(1000, 9000, 1000), seq(10000, 100000, 10000), 200000, 500000, 1000000)
  
  # y-values of EV for the x-values of m listed above
  val.y <- sapply(val.x, EV, n = n)
  
  # Plot and add line along which EV converges
  ggplot() + aes(x = val.x, y = val.y) +
    geom_hline(yintercept = val.y[length(val.y)], colour = "firebrick1", size = 1) +
    geom_line(size = 1.15, colour = "dodgerblue") +
    scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                  labels = scales::trans_format("log10", scales::math_format(10^.x)),
                  minor_breaks = NULL) +
    scale_y_continuous(minor_breaks = NULL) +
    xlab("Number of simulation repetitions    ") +
    ylab("Mean number of flips     ") +
    theme(panel.grid.major = element_line(colour = "white"), 
          panel.grid.minor = element_line(colour = "white"),
          panel.background = element_rect(fill = "black"),
          plot.background = element_rect(fill = "black"),
          axis.text.x = element_text(colour = "white", size = 14, 
                                     margin = margin(t = 4, r = 0, b = 0, l = 0)),
          axis.text.y = element_text(colour = "white", size = 14, 
                                     margin = margin(t = 0, r = 4, b = 0, l = 0)),
          axis.title.x = element_text(colour = "white", size = 21,
                                      margin = margin(t = 8, r = 0, b = 0, l = 6)),
          axis.title.y = element_text(colour = "white", size = 21,
                                      margin = margin(t = 80, r = 8, b = 0, l = 0)),
          axis.ticks.x = element_line(colour = "white", size = 0.5),
          axis.ticks.y = element_line(colour = "white", size = 0.5),
          axis.ticks.length = unit(8, "points"))}

# Save plot for each n; in this case for n = 3, 4, 5
for(j in 3:5){
  assign(paste0("PlotN", j), EVPlots(j))}





##### Plot EV as a function of n --------------------------------------------------------------------------

# x-values
val.x <- c(1:6)
  
# y-values for expected number of coin flips for a given n
val.y <- round(sapply(val.x, EV, m = 1000000), digits = 0)
  
# Create plot
ggplot() + aes(x = val.x, y = val.y) +
  geom_point(colour = "dodgerblue", size = 5) +
  geom_line(colour = "dodgerblue", size = 1.15) +
  scale_x_continuous(breaks = c(1:6), minor_breaks = NULL) +
  scale_y_continuous(breaks = seq(0, 70, 10), minor_breaks = NULL) +
  xlab("Number of consecutive heads or tails      ") +
  ylab("Expected number of coin flips        ") +
  theme(panel.grid.major = element_line(colour = "white"), 
        panel.grid.minor = element_line(colour = "white"),
        panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"),
        axis.text.x = element_text(colour = "white", size = 14,
                                   margin = margin(t = 4, r = 0, b = 0, l = 0)),
        axis.text.y = element_text(colour = "white", size = 14,
                                   margin = margin(t = 0, r = 4, b = 0, l = 0)),
        axis.title.x = element_text(colour = "white", size = 21, 
                                    margin = margin(t = 8, r = 40, b = 0, l = 0)),
        axis.title.y = element_text(colour = "white", size = 21, 
                                    margin = margin(t = 10, r = 8, b = 0, l = 0)),
        axis.ticks.x = element_line(colour = "white", size = 0.5),
        axis.ticks.y = element_line(colour = "white", size = 0.5),
        axis.ticks.length = unit(8, "points")) -> EVChange





##### Plot all graphs in one visualisation ----------------------------------------------------------------

# Prepare graphics device
jpeg(filename = "CoinFlip.jpeg", width = 1500, height = 1500, units = "px")

# Create blank page
grid.newpage()
plot.new()

# Set grid layout and activate it; create black background
gly <- grid.layout(1500, 1500)
pushViewport(viewport(layout = gly, gp = gpar(fill = "black")))
grid.rect(gp = gpar(lwd = 2, fill = "black", col = "black"))

# Place graphs
print(EVChange, vp = viewport(layout.pos.row = 300:1450, layout.pos.col = 50:750))
print(PlotN5, vp = viewport(layout.pos.row = 300:675, layout.pos.col = 775:1450))
print(PlotN4, vp = viewport(layout.pos.row = 688:1063, layout.pos.col = 775:1450))
print(PlotN3, vp = viewport(layout.pos.row = 1075:1450, layout.pos.col = 775:1450))

# Create coloured borders in which the graphs will be enclosed
pushViewport(viewport(layout.pos.row = 300:1450, layout.pos.col = 50:750))
grid.rect(gp = gpar(lwd = 2.5, col = "white", fill = "transparent"))
popViewport()
pushViewport(viewport(layout.pos.row = 300:675, layout.pos.col = 775:1450))
grid.rect(gp = gpar(lwd = 2.5, col = "darkorchid3", fill = "transparent"))
popViewport()
pushViewport(viewport(layout.pos.row = 688:1063, layout.pos.col = 775:1450))
grid.rect(gp = gpar(lwd = 2.5, col = "magenta2", fill = "transparent"))
popViewport()
pushViewport(viewport(layout.pos.row = 1075:1450, layout.pos.col = 775:1450))
grid.rect(gp = gpar(lwd = 2.5, col = "violetred1", fill = "transparent"))
popViewport()

# Frame specific points on left graph to indicate insets on right
pushViewport(viewport(layout.pos.row = 857:877, layout.pos.col = 591:611))
grid.rect(gp = gpar(lwd = 2, col = "darkorchid3", fill = "transparent"))
popViewport()
pushViewport(viewport(layout.pos.row = 1112:1132, layout.pos.col = 476:496))
grid.rect(gp = gpar(lwd = 2, col = "magenta2", fill = "transparent"))
popViewport()
pushViewport(viewport(layout.pos.row = 1240:1260, layout.pos.col = 361:381))
grid.rect(gp = gpar(lwd = 2, col = "violetred1", fill = "transparent"))
popViewport()

# Create line segments that correspond to inset graphs
grid.segments(x0 = c(0.4060, 0.4060), y0 = c(0.4300, 0.4155),
              x1 = c(0.5160, 0.5160), y1 = c(0.8010, 0.5510), gp = gpar(lwd = 2, col = "darkorchid3"))  
grid.segments(x0 = c(0.3310, 0.3310), y0 = c(0.2600, 0.2460),
              x1 = c(0.5160, 0.5160), y1 = c(0.5430, 0.2920), gp = gpar(lwd = 2, col = "magenta2"))
grid.segments(x0 = c(0.2540, 0.2540), y0 = c(0.1750, 0.1610),
              x1 = c(0.5160, 0.5160), y1 = c(0.2850, 0.0340), gp = gpar(lwd = 2, col = "violetred1"))

# Add title text
grid.text(label = c("How many fair coin flips are needed to achieve",
                    "a given number of consecutive heads or tails?"),
          x = rep(0.031, 2), y = c(0.961, 0.916), just = "left",
          gp = gpar(fontsize = 68, col = "white"))

# Add subtitle text
grid.text(label = c(paste("To answer this question, Monte Carlo methods",
                          "are used to calculate the probability of achieving",
                          "a given", sep = " "),
                    paste("number of consecutive outcomes. Coin-flipping simulations",
                          "were performed at different numbers of", sep = " "),
                    paste("repetitions to show convergence around the true", 
                          "expected number of coin flips needed.", sep = " ")), 
          x = rep(0.031, 3), y = c(0.874, 0.850, 0.826), just = "left",
          gp = gpar(fontsize = 30, col = "white", fontface = "italic"))

# Deactivate grid layout; finalise graphics save
popViewport()
dev.off()

# Note that graphics device may vary between computers
# Thus, adjustments may need to be made to this code section

# Clean up variables from global environment
remove(j, val.x, val.y)
