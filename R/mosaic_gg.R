mosaic_gg <-
function(tbl, base_family = "", 
                      ggtitle = "", 
                      xlab = "", 
                      ylab = "", 
                      fill.name = ""){
tbl.df <- as.data.frame(tbl)
N <- length(levels(tbl.df[, 1]))
RdYlBu <- brewer.pal(max(N, 3), "RdYlBu")
Accent <- brewer.pal(max(N, 3), "Accent")
Dark2 <- brewer.pal(max(N, 3), "Dark2")
Paired <- brewer.pal(max(N, 3), "Paired")
Set1 <- brewer.pal(max(N, 3), "Set1")
Set2 <- brewer.pal(max(N, 3), "Set2")
Set3 <- brewer.pal(max(N, 3), "Set3")
tbl.sum <- tapply(tbl.df$Freq, tbl.df[, 2], sum)
tbl.p.m <- prop.table(tbl.sum)
tbl.p <- prop.table(tbl)
tbl.p.2 <- prop.table(tbl, margin = 2)
tbl.p.df <- as.data.frame(tbl.p)
tbl.p.df$width <- tbl.p.m[match(tbl.p.df[, 2], names(tbl.p.m))]
tbl.p.df$height <- as.data.frame(tbl.p.2)$Freq
tbl.p.df$label.height <- unlist(tapply(tbl.p.df$height, tbl.p.df[, 2], function(x) x / 2 + c(0, cumsum(head(x, -1)))))
tbl.p.df$y.breaks <- unlist(tapply(tbl.p.df$height, tbl.p.df[, 2], cumsum))
x.center <- tbl.p.m / 2 + c(0, cumsum(head(tbl.p.m, -1)))
# x.center <- (cumsum(tbl.p.m) + c(0, head(cumsum(tbl.p.m), -1)))/2
tbl.p.df$center <- x.center[match(tbl.p.df[, 2], names(x.center))]
m1 <- ggplot(tbl.p.df, aes(x = center, y = height, width = width)) + 
  geom_bar(aes(fill = tbl.df[, 1]), 
           stat = "identity", 
           col = "white", 
           size = 1, 
           position = position_stack(reverse = TRUE)) 
m2 <- m1 + 
  theme_bw(base_family = base_family)
m3 <- m2 + 
  geom_text(aes(x = center, y = 1.05), 
            label = tbl.p.df[, 2], 
            family = base_family)
m4 <- m3 + 
  geom_text(aes(x = center, y = label.height), 
            label = format(tbl.df$Freq, big.mark = ","), 
            position = position_identity())
x.breaks <- c(0, ifelse(cumsum(tbl.p.m) < 0.1, 0.0, cumsum(tbl.p.m)))
x.label <- format(x.breaks * 100, 
                  digits = 3, 
                  nsmall = 1)
y.breaks <- tbl.p.df$y.breaks
delta <- (max(y.breaks) - min(y.breaks)) / 20
y.breaks.sort <- sort(y.breaks)
diff(y.breaks.sort) < delta 
index <- which(diff(y.breaks.sort)  > delta)
y.breaks <- c(0, y.breaks.sort[c(index, length(y.breaks.sort))])
y.label <- format(y.breaks * 100,
                  digits = 2,
                  nsmall = 1)
m5 <- m4 + 
  scale_x_continuous(name = xlab, 
                     breaks = x.breaks, 
                     label = x.label) + 
  scale_y_continuous(name = ylab,
                     breaks = y.breaks,
                     label = y.label) + 
  scale_fill_manual(name = fill.name, 
                    values = Set1[N:1], 
                    labels = tbl.df[[1]], 
                    guide = guide_legend()) +
  ggtitle(ggtitle) +
  theme(plot.margin = unit(c(1, 2, 1, 1), "lines"))
return(m5)
}
