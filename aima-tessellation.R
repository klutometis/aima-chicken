library(debug)
library(ggplot2)

plot.voronoi <- function(voronoi,
                         path.x,
                         path.y,
                         start.x,
                         start.y,
                         end.x,
                         end.y,
                         filename,
                         title) {
  png(filename, width=1024, heigh=768)
  q <- qplot(voronoi$x1,
             voronoi$y1,
             xlab="",
             ylab="") +
  opts(legend.position='none',
       title=title) +
  geom_point(aes(x=start.x,
                 y=start.y,
                 alpha=1.0),
             shape=1,
             size=5) +
  geom_text(aes(x=start.x,
                y=start.y,
                label='Start',
                alpha=1.0),
            size=3) +
  geom_point(aes(x=end.x,
                 y=end.y,
                 alpha=1.0),
             shape=1,
             size=5) +
  geom_text(aes(x=end.x,
                y=end.y,
                label='End',
                alpha=1.0),
            size=3) +
  geom_segment(aes(x=voronoi$x1,
                   xend=voronoi$x2,
                   y=voronoi$y1,
                   yend=voronoi$y2,
                   alpha=1.0)) +
  geom_text(aes(x=voronoi$x1 + (voronoi$x2 - voronoi$x1) / 2,
                y=voronoi$y1 + (voronoi$y2 - voronoi$y1) / 2,
                label=signif(sqrt((voronoi$x2 - voronoi$x1) ^ 2 +
                  (voronoi$y2 - voronoi$y1) ^ 2),
                  digits=2),
                alpha=1.0),
            size=3) +
  ## We have to do this in segments, if we want to be able to model
  ## bi-directional search.
  geom_path(aes(x=path.x,
                y=path.y,
                alpha=1.0),
            color='red',
            size=2.0) +
  geom_point(aes(x=voronoi$x1,
                 y=voronoi$y1,
                 alpha=1.0))
  plot(q)
  dev.off()
}
