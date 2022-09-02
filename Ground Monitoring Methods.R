library(ggplot2)

x <- c(2,3.2,5,7,3)
y <- c(7,3,5.7,6,1)
z <- c(12,18,14,12,15)

points <- as.data.frame(cbind(x,y,z))

plot1 <- ggplot() + 
  geom_segment(aes(x = 4, y = 4, xend = 2, yend = 7)) +
  geom_segment(aes(x = 4, y = 4, xend = 3.2, yend = 3), color = "red") +
  geom_segment(aes(x = 4, y = 4, xend = 5, yend = 5.7)) +
  geom_segment(aes(x = 4, y = 4, xend = 7, yend = 6)) +
  geom_segment(aes(x = 4, y = 4, xend = 3, yend = 1))+
  geom_point(aes(x = 4, y = 4), color = "red", shape = 3, size = 4) +
  geom_point(data = points, aes(x = x, y = y, color = z), size = 2, show.legend = FALSE) +
  scale_color_viridis(option = "C") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(), axis.line = element_blank())
  
plot2 <- ggplot() + 
  geom_segment(aes(x = 4, y = 4, xend = 2, yend = 7), color = "black") +
  annotate(geom = "text", x = 3, y = 6, label = "3.6 km") +
  
  geom_segment(aes(x = 4, y = 4, xend = 3.2, yend = 3), color = "red") +
  annotate(geom = "text", x = 3.4, y = 3.7, label = "1.3 km") +
  
  geom_segment(aes(x = 4, y = 4, xend = 5, yend = 5.7), color = "red") +
  annotate(geom = "text", x = 4.3, y = 5.1, label = "1.8 km") +
  
  geom_segment(aes(x = 4, y = 4, xend = 7, yend = 6), color = "black") +
  annotate(geom = "text", x = 5.5, y = 4.5, label = "3.5 km") +
  
  geom_segment(aes(x = 4, y = 4, xend = 3, yend = 1), color = "red")+
  annotate(geom = "text", x = 3.6, y = 2, label = "3.2 km") +
  
  geom_point(aes(x = 4, y = 4), color = "red", shape = 3, size = 4) +
  geom_point(data = points, aes(x = x, y = y, color = z), size = 2, show.legend = FALSE) +
  scale_color_viridis(option = "C") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(), axis.line = element_blank())

library(gstat)
library(sp)
library(maptools)
library(viridis)
library(gridExtra)

points2 <- points
coordinates(points2) = ~ x + y
x.range <- as.integer(range(points2@coords[,1]))
y.range <- as.integer(range(points2@coords[,2]))

grd <- expand.grid(x=seq(from=x.range[1], to=x.range[2], by=0.2), y=seq(from=y.range[1], to=y.range[2], by=0.2))
coordinates(grd) <- ~ x + y
gridded(grd) <- TRUE
plot(grd)
points(points2)

idw <- idw(formula = z ~ 1, locations = points2, newdata = grd)
idw.output=as.data.frame(idw)
names(idw.output)[1:3]<-c("x","y","var1.pred")

x <- c(2,3.2,5,7,3)
y <- c(7,3,5.7,6,1)

plot3 <- ggplot(data = idw.output, aes(x = x, y = y)) +
  geom_tile(data = idw.output, aes(fill=var1.pred), show.legend = FALSE) +
  scale_fill_viridis(option = "C") +
  geom_point(aes(x = 4, y = 4), color = "red", shape = 3, size = 4) +
  geom_point(data = points, aes(x = x, y = y),color = "black", size = 3, show.legend = FALSE) +
  geom_point(data = points, aes(x = x, y = y, color = z), size = 2, show.legend = FALSE) +
  scale_color_viridis(option = "C") +
  #geom_point(aes(x = 2, y = 7), color = "black", size = 2) +
  #geom_point(aes(x = 3.2, y = 3), color = "black", size = 2) +
  #geom_point(aes(x = 5, y = 5.7), color = "black", size = 2) +
  #geom_point(aes(x = 7, y = 6), color = "black", size = 2) +
  #geom_point(aes(x = 3, y = 1), color = "black", size = 2) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(), axis.line = element_blank())

plot1
plot2
plot3


grid.arrange(plot1, plot2, plot3, nrow = 1)
