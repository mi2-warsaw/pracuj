my.tree <- function (id, area, group, color, scale = NULL, lab = c(group = TRUE, 
                                                        id = FALSE), main = "Map of the Market", print = TRUE) 
{
  if (any(length(id) != length(area), length(id) != length(group), 
          length(id) != length(color))) {
    stop("id, area, group, and color must be the same length.")
  }
  if (length(lab) == 1) {
    lab[2] <- lab[1]
  }
  if (missing(id)) {
    id <- seq_along(area)
    lab["id"] <- FALSE
  }
  stopifnot(all(!is.na(id)))
  data <- data.frame(label = id, group, area, color)
  data <- data[order(data$area, decreasing = TRUE), ]
  na.idx <- which(is.na(data$area) | is.na(data$group) | is.na(data$color))
  if (length(na.idx)) {
    warning("Stocks with NAs for area, group, or color will not be shown")
    data <- data[-na.idx, ]
  }
  zero.area.idx <- which(data$area == 0)
  if (length(zero.area.idx)) {
    data <- data[-zero.area.idx, ]
  }
  if (nrow(data) == 0) {
    stop("No records to display")
  }
  data$color.orig <- data$color
  if (is.null(scale)) {
    data$color <- data$color * 1/max(abs(data$color))
  }
  else {
    data$color <- sapply(data$color, function(x) {
      if (x/scale > 1) 
        1
      else if (-1 > x/scale) 
        -1
      else x/scale
    })
  }
  data.by.group <- split(data, data$group, drop = TRUE)
  group.data <- lapply(data.by.group, function(x) {
    sum(x[, 3])
  })
  group.data <- data.frame(area = as.numeric(group.data), label = names(group.data))
  group.data <- group.data[order(group.data$area, decreasing = TRUE), 
                           ]
  group.data$color <- rep(NULL, nrow(group.data))
  color.ramp.pos <- colorRamp(c("red", "green"))
  color.ramp.neg <- colorRamp(c("black", "red"))
  color.ramp.rgb <- function(x) {
    col.mat <- mapply(function(x) {
      if (x < 0) {
        color.ramp.neg(abs(x))
      }
      else {
        color.ramp.pos(abs(x))
      }
    }, x)
    mapply(rgb, col.mat[1, ], col.mat[2, ], col.mat[3, ], 
           max = 255)
  }
  add.viewport <- function(z, label, color, x.0, y.0, x.1, 
                           y.1) {
    for (i in 1:length(label)) {
      if (is.null(color[i])) {
        filler <- gpar(col = "blue", fill = "transparent", 
                       cex = 1)
      }
      else {
        filler.col <- color.ramp.rgb(color[i])
        filler <- gpar(col = filler.col, fill = filler.col, 
                       cex = 0.6)
      }
      new.viewport <- viewport(x = x.0[i], y = y.0[i], 
                               width = (x.1[i] - x.0[i]), height = (y.1[i] - 
                                                                      y.0[i]), default.units = "npc", just = c("left", 
                                                                                                               "bottom"), name = as.character(label[i]), clip = "on", 
                               gp = filler)
      z <- append(z, list(new.viewport))
    }
    z
  }
  squarified.treemap <- function(z, x = 0, y = 0, w = 1, h = 1, 
                                 func = add.viewport, viewport.list) {
    cz <- cumsum(z$area)/sum(z$area)
    # cz <- cumsum(sqrt(z$area))/sum(sqrt(z$area))
    n <- which.min(abs(log(max(w/h, h/w) * sum(z$area) * 
                             ((cz^2)/z$area))))
    more <- n < length(z$area)
    a <- c(0, cz[1:n])/cz[n]
    if (h > w) {
      viewport.list <- func(viewport.list, z$label[1:n], 
                            z$color[1:n], x + w * a[1:(length(a) - 1)], rep(y, 
                                                                            n), x + w * a[-1], rep(y + h * cz[n], n))
      if (more) {
        viewport.list <- Recall(z[-(1:n), ], x, y + h * 
                                  cz[n], w, h * (1 - cz[n]), func, viewport.list)
      }
    }
    else {
      viewport.list <- func(viewport.list, z$label[1:n], 
                            z$color[1:n], rep(x, n), y + h * a[1:(length(a) - 
                                                                    1)], rep(x + w * cz[n], n), y + h * a[-1])
      if (more) {
        viewport.list <- Recall(z[-(1:n), ], x + w * 
                                  cz[n], y, w * (1 - cz[n]), h, func, viewport.list)
      }
    }
    viewport.list
  }
  map.viewport <- viewport(x = 0.05, y = 0.05, width = 0.9, 
                           height = 0.75, default.units = "npc", name = "MAP", just = c("left", 
                                                                                        "bottom"))
  map.tree <- gTree(vp = map.viewport, name = "MAP", children = gList(rectGrob(gp = gpar(col = "dark grey"), 
                                                                               name = "background")))
  group.viewports <- squarified.treemap(z = group.data, viewport.list = list())
  for (i in 1:length(group.viewports)) {
    this.group <- data.by.group[[group.data$label[i]]]
    this.data <- data.frame(this.group$area, this.group$label, 
                            this.group$color)
    names(this.data) <- c("area", "label", "color")
    stock.viewports <- squarified.treemap(z = this.data, 
                                          viewport.list = list())
    group.tree <- gTree(vp = group.viewports[[i]], name = group.data$label[i])
    for (s in 1:length(stock.viewports)) {
      stock.tree <- gTree(vp = stock.viewports[[s]], name = this.data$label[s], 
                          children = gList(rectGrob(name = "color")))
      if (lab[2]) {
        stock.tree <- addGrob(stock.tree, textGrob(x = unit(1, 
                                                            "lines"), y = unit(1, "npc") - unit(1, "lines"), 
                                                   label = this.data$label[s], gp = gpar(col = "white", cex = 0.5), 
                                                   name = "label", just = c("left", "top")))
      }
      group.tree <- addGrob(group.tree, stock.tree)
    }
    group.tree <- addGrob(group.tree, rectGrob(gp = gpar(col = "grey"), 
                                               name = "border"))
    if (lab[1]) {
      group.tree <- addGrob(group.tree, textGrob(label = group.data$label[i], 
                                                 name = "label", gp = gpar(col = "blue", cex = 0.5)))
    }
    map.tree <- addGrob(map.tree, group.tree)
  }
  op <- options(digits = 1)
  top.viewport <- viewport(x = 0.05, y = 1, width = 0.9, height = 0.2, 
                           default.units = "npc", name = "TOP", , just = c("left", 
                                                                           "top"))
  legend.ncols <- 51
  l.x <- (0:(legend.ncols - 1))/(legend.ncols)
  l.y <- unit(0.25, "npc")
  l.cols <- color.ramp.rgb(seq(0, 1, by = 1/(legend.ncols - 
                                                1)))
  if (is.null(scale)) {
    l.end <- max(abs(data$color.orig))
  }
  else {
    l.end <- scale
  }
  top.list <- gList(textGrob(label = main, y = unit(0.7, "npc"), 
                             just = c("center", "center"), gp = gpar(cex = 2)), segmentsGrob(x0 = seq(0, 
                                                                                                      1, by = 0.25), y0 = unit(0.25, "npc"), x1 = seq(0, 1, 
                                                                                                                                                      by = 0.25), y1 = unit(0.2, "npc")), rectGrob(x = l.x, 
                                                                                                                                                                                                   y = l.y, width = 1/legend.ncols, height = unit(1, "lines"), 
                                                                                                                                                                                                   just = c("left", "bottom"), gp = gpar(col = NA, fill = l.cols), 
                                                                                                                                                                                                   default.units = "npc"), textGrob(label = format(l.end * 
                                                                                                                                                                                                                                                     seq(0, 1, by = 0.25), trim = TRUE), x = seq(0, 1, by = 0.25), 
                                                                                                                                                                                                                                    y = 0.1, default.units = "npc", just = c("center", "center"), 
                                                                                                                                                                                                                                    gp = gpar(col = "black", cex = 0.8, fontface = "bold")))
  options(op)
  top.tree <- gTree(vp = top.viewport, name = "TOP", children = top.list)
  mapmarket <- gTree(name = "MAPMARKET", children = gList(rectGrob(gp = gpar(col = "dark grey", 
                                                                             fill = "dark grey"), name = "background"), top.tree, 
                                                          map.tree))
  if (print) {
    grid.newpage()
    grid.draw(mapmarket)
  }
  invisible(mapmarket)
}