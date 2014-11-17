drawMeta <- function(matrix, ...) {
    UseMethod("drawMeta")
}

###====================metabin=====================###
drawMeta.metabinM <- function(matrix,
                              plotCol = NCOL(matrix$matrix) + 1,
                              plotHead = "",
                              xlab = NULL,
                              refLine = 0,
                              plotWidth = unit(0.3,"npc"),
                              plotPar = metaPar(),
                              xlog = TRUE,
                              xticks = NULL,
                              boxSize = NULL,
                              align = NULL,
                              clip = log(c(0.05,6)),
                              newpage = TRUE,
                              fit = TRUE,
                              abbreviate = FALSE,
                              vpName = "Forest", ...)
{
  drawMetaBasic(matrix,
                plotCol = plotCol,
                plotHead = plotHead,
                xlab = xlab,
                refLine = refLine,
                plotWidth = plotWidth,
                plotPar = plotPar,
                xlog = xlog,
                xticks = xticks,
                boxSize = boxSize,
                align = align,
                clip = clip,
                newpage = newpage,
                fit = fit,
                abbreviate = abbreviate,
                vpName = vpName, ...)

}

###====================metacont=====================###
drawMeta.metacontM <- function(matrix,
                               plotCol = NCOL(matrix$matrix) + 1,
                               plotHead = "",
                               xlab = NULL,
                               refLine = 0,
                               plotWidth = unit(0.3,"npc"),
                               plotPar = metaPar(),
                               xlog = FALSE,
                               xticks = NULL,
                               boxSize = NULL,
                               align = NULL,
                               clip = c(-Inf,Inf),
                               newpage = TRUE,
                               fit = TRUE,
                               abbreviate = FALSE,
                               vpName = "Forest", ...)
{
  drawMetaBasic(matrix,
                plotCol = plotCol,
                plotHead = plotHead,
                xlab = xlab,
                refLine = refLine,
                plotWidth = plotWidth,
                plotPar = plotPar,
                xlog = xlog,
                xticks = xticks,
                boxSize = boxSize,
                align = align,
                clip = clip,
                newpage = newpage,
                fit = fit,
                abbreviate = abbreviate,
                vpName = vpName, ...)

}

###=======================drawMetaBasic============================###
drawMetaBasic <- function(matrix,  plotCol, plotHead, xlab, refLine,
                          plotWidth, plotPar, xlog, xticks, boxSize,
                          align, clip, newpage, fit,
                          abbreviate, vpName, ...)
{

  require("grid") || stop("'grid' package not found")
  require("rmeta") || stop("'rmeta' package not found")

  ## assigning names to "matrix" components
  labeltext <- matrix$matrix
  mean <- matrix$plotDF[, "mean"]
  lower <- matrix$plotDF[, "lower"]
  upper <- matrix$plotDF[, "upper"]
  is.summary <- matrix$plotDF[, "is.summary"]

  ## creating vectors of study(row) and column names to label grobs
  if (abbreviate) {
    studynames <- make.names(abbreviate(labeltext[, "study"], 6))
    colnames <- make.names(abbreviate(colnames(labeltext), 6))
  } else {
    studynames <- make.names(labeltext[, "study"])
    colnames <- make.names(colnames(labeltext))
  }
  ## calculate number of rows and columns
  nc <- NCOL(labeltext)
  nr <- NROW(labeltext)
  ## set height of each row as unit 'lines'
  ## where cex of texts is the number of lines per row
  lineScale <- plotPar$text$cex
  if (rownames(labeltext[1, , drop = FALSE]) == "title") {
    rowHeights <- unit(c(plotPar$title$cex, rep(lineScale, nr - 1), 0.5),
                       "lines")
  } else {
    rowHeights <- unit(c(rep(lineScale, nr), 0.5), "lines")
  }
  ## see if any row has NA
  rowWidth <- !apply(is.na(labeltext), 1, any)
  ## start new page
  if (newpage)
    grid.newpage()
  ## work out plot scale
  if (fit) {
    scale <- fitPlot(labeltext, align, is.summary, plotPar,
                     plotWidth, plotCol, rowWidth, rowHeights)
  } else {
    scale <- 1
  }
  ## generate labels and width/height calculations with scaling
  labels <- generateTextGrobs(labeltext, align, is.summary, plotPar,
                              studynames = studynames, colnames = colnames,
                              scale = scale)
  colWidth <- columnWidths(labels, nc, rowWidth, colgap = 3*scale,
                           plotCol, plotWidth)
  rowHeights <- scale * rowHeights
  ## push viewports
  pushViewport(viewport(layout = grid.layout(nr + 1, nc*2 + 2,
                                             widths = colWidth,
                                             heights = rowHeights),
                        name = vpName))
  cwidth <- (upper - lower)
  xrange <- c(max(min(lower, na.rm = TRUE), clip[1]),
              min(max(upper, na.rm = TRUE), clip[2]))
  info <- 1/cwidth
  info <- info/max(info[!is.summary], na.rm = TRUE)
  info[is.summary] <- 1
  if (!is.null(boxSize)) info <- rep(boxSize, length = length(info))
  ## push viewports with layout to draw texts
  for(j in 1:nc){
    for(i in 1:nr){
      if (!is.null(labels[[j]][[i]])){
        if (j < plotCol) {
          pushViewport(viewport(layout.pos.row = i,
                                layout.pos.col = 2*j - 1,
                                name = paste(labels[[j]][[i]]$name,
                                             i, j, sep = ".")))
        } else {
          pushViewport(viewport(layout.pos.row = i,
                                layout.pos.col = 2*(j + 1) - 1,
                                name = paste(labels[[j]][[i]]$name,
                                             i, j, sep = ".")))
        }
        grid.draw(labels[[j]][[i]])
        upViewport()
      }
    }
  }
  ## push viewport for plotting ##
  pushViewport(viewport(layout.pos.col = plotCol*2 - 1, xscale = xrange,
                        gp = gpar(cex = scale), name = "Graph"))
  ## draw no effect line
  grid.lines(name = "refLine",
             x = unit(refLine, "native"),
             y = unit(c(0, (nr - 2)*lineScale), "lines"),
             gp = do.call("gpar", plotPar$refLine))
  ## draw overall effects line
  for (i in 1:nr) {
    if (is.summary[i])
      grid.lines(name = paste("summaryLine", studynames[i], sep = "."),
                 x = unit(mean[i], "native"),
                 y = unit(c(0, (nr - 2)*lineScale), "lines"),
                 gp = do.call("gpar", plotPar$summaryLine)) }
  ## draw x-axis
  if (xlog){
    if(is.null(xticks)){
      ticks <- pretty(exp(xrange))
      if (clip[1] == -Inf) {
        ## add 0.5 and 1 to axis label
        ticks <- unique(sort(c(0.5, 1, ticks)))
      }
      ticks <- ticks[ticks > 0]
    } else{
      ticks <- xticks
    }
    if (length(ticks)){
      if (min(lower, na.rm = TRUE) < clip[1]) {
        ticks <- c(exp(clip[1]), ticks)
      }
      if (max(upper, na.rm = TRUE) > clip[2]) {
        ticks <- c(ticks, exp(clip[2]))
      }
      xax <- xaxisGrob(gp = do.call("gpar", plotPar$axis),
                       at = log(ticks), name = "xax")
      xax1 <- editGrob(xax, gPath("labels"),
                       label = sprintf("%g",
                                       as.numeric(format(ticks, digits = 2))))
      ## sprintf get rid of trailing zeros in label
      grid.draw(xax1)
    }
  } else {
    if (is.null(xticks)){
      grid.xaxis(name = "xax",
                 gp = do.call("gpar", plotPar$axis))
    } else if(length(xticks)) {
      grid.xaxis(name = "xax", at = xticks,
                 gp = do.call("gpar", plotPar$axis))
    }
  }
  ## draw plot heading
  grid.text(name = "plotHead", plotHead,
            y = unit((nr - 0.5)*lineScale, "lines"),
            gp = do.call("gpar", plotPar$heading))
  ## draw axis labels for effect tendency
  if(!is.null(xlab)) {
    plotPar$label$cex <- plotPar$label$cex
    grid.text(name = "xlab1", xlab[1],
              x = unit(refLine - 0.3, "native"),
              y = unit(lineScale, "lines"),
              just = "right", gp = do.call("gpar", plotPar$label))
    grid.text(name = "xlab2", xlab[2],
              x = unit(refLine + 0.3, "native"),
              y = unit(lineScale, "lines"),
              just = "left", gp = do.call("gpar", plotPar$label))
  }
  upViewport()
  ## draw confidence interval lines and polygons
  for (i in 1:nr) {
    if (is.na(mean[i])) next
    ##1
    pushViewport(viewport(layout.pos.row = i,
                          layout.pos.col = plotCol*2 - 1,
                          xscale = xrange, gp = gpar(cex = scale),
                          name = paste("CI", studynames[i], i, j,
                                       sep = ".")))
    if (is.summary[i]){
      drawSummaryCI(lower[i], mean[i], upper[i], info[i],
                    studynames[i], plotPar)
    } else {
      drawNormalCI(lower[i], mean[i], upper[i], info[i],
                   studynames[i], plotPar)
    }

    upViewport()
  }
  upViewport()
}

###================ Draw a non-summary rect-plus-CI ================###
drawNormalCI <- function(LL, OR, UL, size,
                         studynames, plotPar) {
  size <- 0.75*size
  clipupper <- convertX(unit(UL, "native"), "npc", valueOnly = TRUE) > 1
  cliplower <- convertX(unit(LL, "native"), "npc", valueOnly = TRUE) < 0
  box <- convertX(unit(OR, "native"), "npc", valueOnly = TRUE)
  clipbox <- (box < 0) || (box > 1)
  ## draw arrow if exceed col range
  ## convertX() used to convert between coordinate systems
  if (clipupper || cliplower){
    ends <- "both"
    lims <- unit(c(0, 1), c("npc", "npc"))
    if (!clipupper) {
      ends <- "first"
      lims <- unit(c(0, UL), c("npc","native"))
    }
    if (!cliplower) {
      ends <- "last"
      lims <- unit(c(LL, 1), c("native", "npc"))
    }
    grid.lines(name = paste("line", studynames, sep = "."),
               x = lims, y = 0.5,
               arrow = arrow(ends = ends,length = unit(0.05, "inches")),
               gp = do.call("gpar", plotPar$lines))
    if (!clipbox)
      grid.rect(name = paste("rect", studynames, sep = "."),
                x = unit(OR, "native"),
                width = unit(size, "snpc"), height = unit(size, "snpc"),
                gp = do.call("gpar", plotPar$box))
  } else {
    ## Draw line white if totally inside rect
    grid.lines(name = paste("line", studynames, sep = "."),
               x = unit(c(LL, UL), "native"), y = 0.5,
               gp = do.call("gpar", plotPar$lines))
    grid.rect(name = paste("rect", studynames, sep = "."),
              x = unit(OR, "native"),
              width = unit(size, "snpc"), height = unit(size, "snpc"),
              gp = do.call("gpar", plotPar$box))
    if ((convertX(unit(OR, "native") + unit(0.5*size, "lines"), "native",
                  valueOnly = TRUE) > UL) &&
          (convertX(unit(OR, "native") - unit(0.5*size, "lines"), "native",
                    valueOnly = TRUE) < LL))
      grid.lines(name = paste("line", studynames, sep = "."),
                 x = unit(c(LL, UL), "native"), y = 0.5,
                 gp = do.call("gpar", plotPar$lines))
  }
}

###================  Draw a summary "diamond" ===================###
drawSummaryCI <- function(LL, OR, UL, size, studynames, plotPar)
{
  grid.polygon(name = paste("diamond", studynames, sep = "."),
               x = unit(c(LL, OR, UL, OR), "native"),
               y = unit(0.5 + c(0, 0.5*size, 0, -0.5*size), "npc"),
               gp = do.call("gpar", plotPar$diamond))
}

###===============  Generates lists of text grobs ==============###
generateTextGrobs <- function(labeltext, align, is.summary, plotPar,
                              studynames = NULL, colnames = NULL, scale = 1)
{
  nc <- NCOL(labeltext)
  nr <- NROW(labeltext)
  labels <- vector("list",nc)
  if (is.null(align)) {
    align <- c("l",rep("r",nc-1))
  } else {
    align <- rep(align,length = nc)
  }
  is.summary <- rep(is.summary,length = nr)
  for(j in 1:nc){
    labels[[j]] <- vector("list", nr)
    for(i in 1:nr){
      if (is.na(labeltext[i,j]))
        next
      x <- switch(align[j],l = 0,r = 1,c = 0.5)
      just <- switch(align[j],l = "left",r = "right",c = "center")
      if (rownames(labeltext[i,,drop = FALSE]) == "title") {
        ## set up gp for title
        titlegp <- plotPar$title
        titlegp$cex <- scale*titlegp$cex
        gp <- do.call("gpar", titlegp)
        studynames[i] <- "title"
      } else if (rownames(labeltext[i,,drop = FALSE]) == "subtitle") {
        ## set up gp for subtitle
        subtitlegp <- plotPar$subtitle
        subtitlegp$cex <- scale*subtitlegp$cex
        gp <- do.call("gpar", subtitlegp)
        studynames[i] <- "subtitle"
      } else if (rownames(labeltext[i,,drop = FALSE]) == "hetero") {
        ## set up gp for hetero statistics
        statgp <- plotPar$stat
        if (is.summary[i])
          statgp$fontface <- "bold"
        statgp$cex <- scale*statgp$cex
        gp <- do.call("gpar", statgp)
        studynames[i] <- "hetero"
      } else {
        ## set up gp for text
        textgp <- plotPar$text
        if (is.summary[i])
          textgp$fontface <- "bold"
        textgp$cex <- scale*textgp$cex
        gp <- do.call("gpar", textgp)
      }
      labels[[j]][[i]] <- textGrob(name = paste("text", studynames[i],
                                                colnames[j], sep = "."),
                                   labeltext[i,j], x = x, just = just,
                                   gp = gp)}
  }
  return(labels)
}

###================== Calculates width of labels ================###
columnWidths <- function(labels, nc, rowWidth, colgap, plotCol, plotWidth)
{
  colgap <- unit(colgap,"mm")
  if (1 == plotCol)  {
    colWidth <- unit.c(plotWidth, colgap)
    for(i in 2:(nc+1)) {
      colWidth <- unit.c(colWidth,
                         max(unit(rep(1, sum(rowWidth)),
                                  "grobwidth",
                                  labels[[i - 1]][rowWidth])), colgap)
    }
  } else {
    colWidth <- unit.c(max(unit(rep(1, sum(rowWidth)), "grobwidth",
                                labels[[1]][rowWidth])), colgap)
    for(i in 2:(nc+1)){
      colWidth <-
        if (i == plotCol) {
          unit.c(colWidth, plotWidth, colgap)
        } else {
          if (i < plotCol) {
            unit.c(colWidth,
                   max(unit(rep(1,sum(rowWidth)),
                            "grobwidth",
                            labels[[i]][rowWidth])), colgap)
          } else {
            unit.c(colWidth,
                   max(unit(rep(1,sum(rowWidth)), "grobwidth",
                            labels[[i-1]][rowWidth])), colgap) }
        }
    }
  }
  return(colWidth)
}

###============== Calculates scale to fit plot on page ==============###
fitPlot <- function(labeltext, align, is.summary, plotPar,
                    plotWidth, plotCol, rowWidth, rowHeights)
{
  nc <- NCOL(labeltext)
  nr <- NROW(labeltext)
  ## generate text grobs from labeltext
  labels <- generateTextGrobs(labeltext, align, is.summary, plotPar)
  ## calculate plot and page widths
  colWidth <- columnWidths(labels, nc, rowWidth, colgap = 3,
                           plotCol, plotWidth)
  totalwidth <- convertWidth(sum(colWidth),"inches", valueOnly = TRUE)
  pagewidth <- convertWidth(unit(1, "npc"), "inches", valueOnly = TRUE)
  ## include only text in width calculation
  plotvalue <-  convertWidth(plotWidth, "inches", valueOnly = TRUE)
  textwidth <- totalwidth - plotvalue
  pagetextwidth <- pagewidth - plotvalue
  wscale <- 1
  ###### calculate width scale to fit current viewport #####
  while (textwidth > pagetextwidth && pagewidth > plotvalue) {
    wscale <- wscale * pagetextwidth / textwidth
    labels <- generateTextGrobs(labeltext, align, is.summary,
                                plotPar, scale = wscale)
    colWidth <- columnWidths(labels, nc, rowWidth, colgap = 3*wscale,
                             plotCol, plotWidth)
    textwidth <- convertWidth(sum(colWidth), "inches", valueOnly = TRUE) -
                   plotvalue
  }
  ###### calculate height scale to fit current viewport #####
  heights <- rowHeights
  totalheight <- convertHeight(sum(rowHeights),"inches", valueOnly = TRUE)
  pageheight <- convertHeight(unit(1, "npc"), "inches", valueOnly = TRUE)
  hscale <- 1
  while (totalheight > pageheight) {
    hscale <- hscale * pageheight / totalheight
    heights <- hscale * rowHeights
    totalheight <- convertHeight(sum(heights), "inches", valueOnly = TRUE)
  }
  ## use the smaller scale
  if (hscale > wscale) {
    scale <- wscale
  } else {
    scale <- hscale
  }
  scale
}
