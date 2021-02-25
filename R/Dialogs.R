

# ToDo 2021-02-04:
# BookmarkDlg   update of deleted list entrys 
#               rename bookmark




## GUI-Elements: select variables by dialog, FileOpen, DescDlg, ObjectBrowse ====


.LsDataFrame <- function(){
  # list all data.frames in the GlobalEnvironment
  lst <- unlist(eapply(.GlobalEnv, is.data.frame))
  if(!is.null(lst))
    res <- names(which(lst))
  else
    res <- NULL
  
  return(res)
  
}



.InitDlg <- function(width, height, x=NULL, y=NULL, resizex=FALSE, resizey=FALSE, main="Dialog", ico="R"){

  top <- tcltk::tktoplevel()

  # Alternative for Windows:
  # if(Sys.info()["sysname"]=="Windows") {
  #   res <- system("wmic path Win32_VideoController get CurrentVerticalResolution,CurrentHorizontalResolution /format:value", intern = TRUE)
  #   res <- as.integer(StrExtract(grep("Cur", res, val=TRUE), "[0-9]+"))
  #   if(is.null(x)) x <- round(res[1]/2 - 50)
  #   if(is.null(y)) y <- round(res[2]/2 - 25)
  # }

  # if(is.null(x)) x <- as.integer(tcltk::tkwinfo("screenwidth", top))/2 - 50
  # if(is.null(y)) y <- as.integer(tcltk::tkwinfo("screenheight", top))/2 - 25

  if(is.null(x)) x <- round((as.integer(tcltk::tkwinfo("screenwidth", top)) - width)/2)
  if(is.null(y)) y <- round((as.integer(tcltk::tkwinfo("screenheight", top)) - height)/2)

  geom <- gettextf("%sx%s+%s+%s", width, height, x, y)
  tcltk::tkwm.geometry(top, geom)
  tcltk::tkwm.title(top, main)
  tcltk::tkwm.resizable(top, resizex, resizey)
  # alternative:
  #    system.file("extdata", paste(ico, "ico", sep="."), package="DescTools")
  tcltk::tkwm.iconbitmap(top, file.path(find.package("DescToolsAddIns"), "extdata", paste(ico, "ico", sep=".")))

  return(top)

}



.ToClipboard <- function (x, ...) {

  # This fails on Linux with
  #
  # * checking examples ... ERROR
  # Running examples in 'DescTools-Ex.R' failed The error most likely occurred in:
  #
  #   > base::assign(".ptime", proc.time(), pos = "CheckExEnv") ### Name:
  # > ToClipboard ### Title: Write Text to Clipboard ### Aliases:
  # > ToClipboard

  sn <- Sys.info()["sysname"]
  if (sn == "Darwin") {
    file <- pipe("pbcopy")
    cat(x, file = file, ...)
    close(file)
  }
  else if (sn == "Windows") {
    cat(x, file = "clipboard", ...)
  }
  else {
    stop("Writing to the clipboard is not implemented for your system (",
         sn, ") in this package.")
  }
}






FileOpenDlg <- function(fmt=NULL) {

  fn <- file.choose()
  # fn <- tcltk::tclvalue(tcltk::tkgetOpenFile())

  op <- options(useFancyQuotes = FALSE)
  # switch from backslash to slash
  fn <- gsub("\\\\", "/", fn)

  # parse the filename into path, filename, filextension
  fnamelong <- rev(unlist(strsplit(fn, "/")))[1]
  ext <- rev(unlist(strsplit( fnamelong, "\\.")))[1]
  fname <- substr(fnamelong, 1, nchar(fnamelong) - nchar(ext) - 1)
  path <- substr(fn, 1, nchar(fn) - nchar(fname) - nchar(ext) - 1)


  if(is.null(fmt)) {
    if(ext %in% c("rda", "RData"))
      fmt <- 3
    else if(ext %in% c("dat", "csv"))
      fmt <- 2
    else
      fmt <- 1
  }


  # read.table text:
  if(fmt == 1) {
    fmt <- "\"%path%%fname%.%ext%\""

  } else if( fmt == 2) {
    fmt="d.%fname% <- read.table(file = \"%path%%fname%.%ext%\", header = TRUE, sep = \";\", na.strings = c(\"NA\",\"NULL\"), strip.white = TRUE)"

  } else if( fmt == 3) {
    fmt="load(file = \"%path%%fname%.%ext%\")"

  }


  rcmd <- gsub("%fname%", fname, gsub("%ext%", ext, gsub( "%path%", path, fmt)))

  # utils::writeClipboard(rcmd)
  # .ToClipboard(rcmd)

  options(op)

  return(rcmd)

}






# http://infohost.nmt.edu/tcc/help/pubs/tkinter/web/ttk-Label.html
# good documentation
# http://infohost.nmt.edu/tcc/help/pubs/tkinter/web/index.html

ModelDlg <- function(x, ...){

  # require(DescTools)
  requireNamespace("tcltk")

  .GetModTxt <- function()
    tcltk::tclvalue(tcltk::tkget(tfModx, "0.0", "end"))

  .EmptyListBox <- function(){
    n <- as.character(tcltk::tksize(tlist.var))
    for (i in (n:0)) tcltk::tkdelete(tlist.var, i)
  }

  .PopulateListBox <- function(x){
    for (z in x) {
      tcltk::tkinsert(tlist.var, "end", paste0(" ", z))
    }
  }
  
  .AddVar <- function(sep, pack = NULL, connect="+") {

    var.name <- as.numeric(tcltk::tkcurselection(tlist.var))
    lst <- .GetVarName(as.character(tcltk::tkget(tlist.var, 0, "end")))

    if (length(var.name) == 0)
      tcltk::tkmessageBox(message = "No variable selected",
                          icon = "info", type = "ok")

    if (length(var.name) > 0) {

      txt <- DescTools::StrTrim(.GetModTxt())
      if(is.null(pack))
        vn <- DescTools::StrTrim(lst[var.name + 1])
      else
        vn <- DescTools::StrTrim(gettextf(pack, lst[var.name + 1]))

      txt <- StrTrim(paste(ifelse(txt=="", "", connect), paste(vn, collapse=sep), ""), method="left")
      if(connect == "-" & .GetModTxt() == "\n")
        txt <- paste(" . - ", txt)
      
      tcltk::tkinsert(tfModx, "insert", txt, "notwrapped")
    }
  }

  .BtnAddVar <- function() .AddVar(" + ")
  .BtnAddMult <- function() .AddVar(" * ")
  .BtnAddInt <- function() .AddVar(" : ")
  .BtnAddPoly <- function() .AddVar(sep=" + ", pack="poly(%s, 2)")
  .BtnAddMin <- function() .AddVar(" - ", connect="-")
  # .BtnAddPipe <- function() .AddVar(" | ")
  .BtnAddI <- function() .AddVar(sep=" + ", pack="I(%s)")
  


  imgAsc <-  tcltk::tclVar()
  tclimgAsc <-  tcltk::tkimage.create("photo", imgAsc, file = file.path(find.package("DescToolsAddIns"), "extdata", "SortListAsc.gif"))
  imgDesc <-  tcltk::tclVar()
  tclimgDesc <-  tcltk::tkimage.create("photo", imgDesc, file = file.path(find.package("DescToolsAddIns"), "extdata", "SortListDesc.gif"))
  imgNone <-  tcltk::tclVar()
  tclimgNone <-  tcltk::tkimage.create("photo", imgNone, file = file.path(find.package("DescToolsAddIns"), "extdata", "SortListNo.gif"))

  .BtnSortVarListAsc <- function() .SortVarList("a")
  .BtnSortVarListDesc <- function() .SortVarList("d")
  .BtnSortVarListNone <- function() .SortVarList("n")


  .InsertLHS <- function() {

    var.name <- as.numeric(tcltk::tkcurselection(tlist.var))
    lst <- .GetVarName(as.character(tcltk::tkget(tlist.var, 0, "end")))

    if (length(var.name) == 0)
      tcltk::tkmessageBox(message = "No variable selected",
                          icon = "info", type = "ok")

    if (length(var.name) > 0) {
      tcltk::tclvalue(tflhs) <- paste(lst[var.name + 1], collapse=", ")
    }
  }

  .SortVarList <- function(ord){

    lst <- as.character(tcltk::tkget(tlist.var, 0, "end"))

    # for (i in (length(names(x)):0)) tkdelete(tlist.var, i)
    .EmptyListBox()

    if(ord == "a"){
      v <- DescTools::StrTrim(sort(lst, decreasing = FALSE))
    } else if(ord == "d"){
      v <- DescTools::StrTrim(sort(lst, decreasing = TRUE))
    } else {
      v <- DescTools::StrTrim(.VarNames()[names(x) %in% .GetVarName(lst)])
    }

    .PopulateListBox(v)

  }

  .FilterVarList <- function(){

    pat <- DescTools::StrTrim(tcltk::tclvalue(tffilter))
    # print(pat)
    if(pat=="")
      v <- .VarNames()
    else
      v <- grep(pattern = pat, .VarNames(), value=TRUE, fixed=TRUE)

    for (i in (length(names(x)):0)) tcltk::tkdelete(tlist.var, i)

    .PopulateListBox(v)

    # tcltk::tclvalue(frmVar$text) <- gettextf("Variables (%s/%s):", length(v), length(names(x)))
    tcltk::tkconfigure(frmVar, text=gettextf("Variables (%s/%s):", length(v), length(names(x))))
  }

  .SelectVarList <- function(){

    var.name <- as.numeric(tcltk::tkcurselection(tlist.var))
    lst <- .GetVarName(as.character(tcltk::tkget(tlist.var, 0, "end")))

    if (length(var.name) > 0) {
      txt <- StrTrunc(Label(x[, StrTrim(lst[var.name + 1])]), 30)
      if(length(txt) == 0) txt <- " "
      cltxt <- class(x[, StrTrim(lst[var.name + 1])])
      if(any(cltxt %in% c("factor","ordered")))
        cltxt <- paste0(cltxt, "(", max(nlevels(x[, StrTrim(lst[var.name + 1])])), ")")
      tcltk::tclvalue(tflbl) <- gettextf("%s\n  %s", paste(cltxt, collapse=", "), txt)
    } else {
      tcltk::tclvalue(tflbl) <- "\n"
    }
  }

  
  .VarNames <- function(){
    
    cabbr <- function(x){
      
      if(class(x)[1]=="integer") "i"
      else if(class(x)[1]=="numeric") "n"  
      else if(class(x)[1]=="factor") gettextf("f_%s", nlevels(x))  
      else if(class(x)[1]=="ordered") gettextf("o_%s", nlevels(x))  
      else if(class(x)[1]=="Date") "d"  
      else if(class(x)[1]=="character") "c"  
      else if(class(x)[1]=="logical") "l"  
      else class(x)[1]   
    }
    
    sapply(names(x), function(z) gettextf(" %s   - %s", z, cabbr(x[, z])))
    
  }
  
  .GetVarName <- function(x){
    StrTrim(gsub("-.*", "", x))
  }
  

  fam <- "comic"
  size <- 10
  myfont <- tcltk::tkfont.create(family = fam, size = size)
  mySerfont <- tcltk::tkfont.create(family = "Times", size = size)
  
  tfmodtype <- tcltk::tclVar("")
  tfmodx <- tcltk::tclVar("")
  tflhs <- tcltk::tclVar("")
  tffilter <- tcltk::tclVar("")
  tflbl <- tcltk::tclVar("\n")
  tfframe <- tcltk::tclVar("Variables:")
  # gettextf("Variables (%s):", length(names(x)))
  mod_x <- NA_character_

  e1 <- environment()
  modx <- character()
  # old, repl. by 0.99.22: xname <- deparse(substitute(x))
  xname <- paste(StrTrim(deparse(substitute(x))), collapse=" ")

  if (!missing(x)) {
    if(class(x) == "formula") {

      # would be nice to pick up a formula here, to be able to edit the formula
      # https://rviews.rstudio.com/2017/02/01/the-r-formula-method-the-good-parts/

      # try to extract the name of the data.frame from match.call
      xname <- StrExtract(gsub("^.+data = ", "\\1", paste(deparse(match.call()), collapse=" ")), ".+[[:alnum:]]")

      tcltk::tclvalue(tflhs) <- deparse(x[[2]])
      mod_x <- deparse(x[[3]])

      x <- eval(parse(text=xname, parent.env()))

    } else if(!is.data.frame(x))
      stop("x must be a data.frame")


  } else {
    stop("Some data must be provided, example: ModelDlg(iris)")
  }


  OnOK <- function() {
    if(tcltk::tclvalue(tfmodtype)=="")
      modelx <- "%s"
    else
      modelx <- models[tcltk::tclvalue(tfmodtype)]
    
    assign("modx", gettextf(modelx, paste(
      DescTools::StrTrim(tcltk::tclvalue(tflhs)), " ~ ",
      DescTools::StrTrim(.GetModTxt()), ", data=", xname, sep="")), envir = e1)
    
    tcltk::tkdestroy(root)
  }

  # do not update screen
  tcltk::tclServiceMode(on = FALSE)

  # create window
  root <- .InitDlg(width = 880, height = 532, resizex=TRUE, resizey=TRUE,
                   main=gettextf("Build Model Formula (%s)", xname), ico="R")

  # define widgets
  content <- tcltk::tkframe(root, padx=10, pady=10)


  # Variable list
  frmVar <- tcltk::tkwidget(content, "labelframe", text=gettextf("Variables (%s/%s):", length(names(x)), length(names(x))),
                            fg = "black", padx = 10, pady = 10, font = myfont)


  tfFilter <- tcltk::tkentry(frmVar, textvariable=tffilter, width= 20, bg="white")
  tfButSortAsc <- tcltk::tkbutton(frmVar, image = tclimgAsc, compound="none",
                                  command = .BtnSortVarListAsc, height = 21, width = 21)
  tfButSortDesc <- tcltk::tkbutton(frmVar, image = tclimgDesc, compound="none",
                                   command = .BtnSortVarListDesc, height = 21, width = 21)
  tfButSortNone <- tcltk::tkbutton(frmVar, image=tclimgNone, compound="none",
                                   command = .BtnSortVarListNone, height = 21, width = 21)
  var.scr <- tcltk::tkscrollbar(frmVar, repeatinterval = 5,
                                command = function(...) tcltk::tkyview(tlist.var, ...))

  tlist.var <- tcltk::tklistbox(frmVar, selectmode = "extended",
                                yscrollcommand = function(...)
                                  tcltk::tkset(var.scr, ...), background = "white",
                                exportselection = FALSE, activestyle= "none", highlightthickness=0,
                                height=20, width=20, font = myfont)
  tfVarLabel <- tcltk::tklabel(frmVar, justify="left", width=26, anchor="w", textvariable=tflbl, font=myfont)


  
  .PopulateListBox(.VarNames())


  tcltk::tkbind(tlist.var)
  tcltk::tkgrid(tfFilter, row=0, padx=0, sticky = "n")
  tcltk::tkgrid(tcltk::tklabel(frmVar, text="  "), row=0, column=1)
  tcltk::tkgrid(tfButSortAsc, row=0, column=2, padx=0, sticky = "n")
  tcltk::tkgrid(tfButSortDesc, row=0, column=3,  sticky = "n")
  tcltk::tkgrid(tfButSortNone, row=0, column=4, sticky = "n")
  tcltk::tkgrid(tcltk::tklabel(frmVar, text=" "))
  tcltk::tkgrid(tlist.var, var.scr, row=2, columnspan=5, sticky = "news")
  tcltk::tkgrid(tfVarLabel, row=3, columnspan=5, pady=3, sticky = "es")
  tcltk::tkgrid.configure(var.scr, sticky = "news")
  # tcltk2::tk2tip(tlist.var, "List of variables in data frame")

  # Buttons
  frmButtons <- tcltk::tkwidget(content, "labelframe", text = "",  bd=0,
                                fg = "black", padx = 5, pady = 25)

  tfButLHS <- tcltk::tkbutton(frmButtons, text = ">",
                              command = .InsertLHS, height = 1, width = 2, font=myfont)

  tfButAdd <- tcltk::tkbutton(frmButtons, text = "+",
                              command = .BtnAddVar, height = 1, width = 2, font=myfont)
  tfButMult <- tcltk::tkbutton(frmButtons, text = "*",
                               command = .BtnAddMult, height = 1, width = 2, font=myfont)
  tfButInt <- tcltk::tkbutton(frmButtons, text = ":",
                              command = .BtnAddInt,
                              height = 1, width = 2, font=myfont)
  tfButPoly <- tcltk::tkbutton(frmButtons, text = "x\U00B2",
                               command = .BtnAddPoly,
                               height = 1, width = 2, font=myfont)
  tfButMin <- tcltk::tkbutton(frmButtons, text = "-",
                              command = .BtnAddMin, height = 1, width = 2, font=myfont)
#  tfButPipe <- tcltk::tkbutton(frmButtons, text = "|",
#                              command = .BtnAddPipe, height = 1, width = 2, font=myfont)
  tfButI <- tcltk::tkbutton(frmButtons, text="I",
                               command = .BtnAddI, height = 1, width = 2, font=mySerfont)
  
  tcltk::tkgrid(tfButLHS, row = 0, rowspan=10, padx = 5, sticky = "s")
  tcltk::tkgrid(tcltk::tklabel(frmButtons, text="\n\n"))
  tcltk::tkgrid(tfButAdd, row = 40, padx = 5, sticky = "s")
  tcltk::tkgrid(tfButMin, row = 50, padx = 5, sticky = "s")
  tcltk::tkgrid(tfButMult, row = 60, padx = 5, sticky = "s")
  tcltk::tkgrid(tfButInt, row = 70, padx = 5, sticky = "s")
  # tcltk::tkgrid(tfButPipe, row = 80, padx = 5, sticky = "s")
  tcltk::tkgrid(tfButPoly, row = 80, padx = 5, sticky = "s")
  tcltk::tkgrid(tfButI, row = 90, padx = 5, sticky = "s")


  # Model textbox
  frmModel <- tcltk::tkwidget(content, "labelframe", text = "Model:",
                              fg = "black", padx = 10, pady = 10, font = myfont)

  # get the model list from the options
  models <- getOption("DTAmodels", default = options(DTAmodels = list(
                "linear regression (OLS)" = "r.lm <- lm(%s)"
                ,"logistic regression" = 'r.logit <- glm(%s, fitfn="binomial")'
                 )))
            
  
  
  tfComboModel <- ttkcombobox(frmModel,
                              values = if(!is.null(models)) names(models) else "",
                              textvariable = tfmodtype,  # font = myfont, 
                              state = "normal",     # or "readonly"
                              justify = "left", width=30)
  
  tfLHS <- tcltk::tkentry(frmModel, textvariable=tflhs, bg="white",  width=45)
  tfModx <- tcltk::tktext(frmModel, bg="white", height=20, width=70, wrap="word", padx=7, pady=5, font=myfont)
  tcltk::tkgrid(tfLHS, column=0, row=0, pady=10, sticky="nws")
  tcltk::tkgrid(tfComboModel, column=0, row=0, pady=10, sticky="e")
  tcltk::tkgrid(tcltk::tklabel(frmModel, text="~"), row=1, sticky="w")
  tcltk::tkgrid(tfModx, column=0, row=2, pady=10, sticky="nws")
  if(!all(is.na(mod_x)))
    tcltk::tkinsert(tfModx, "insert", mod_x, "notwrapped")


  
  
  # root
  tfButOK = tcltk::tkbutton(content, text="OK", command=OnOK, width=6)
  tfButCanc = tcltk::tkbutton(content, text="Cancel", width=7,
                              command=function() tcltk::tkdestroy(root))

  tcltk::tkbind(tfFilter, "<KeyRelease>", .FilterVarList)
  tcltk::tkbind(tlist.var, "<ButtonRelease>", .SelectVarList)
  tcltk::tkbind(tlist.var, "<KeyRelease>", .SelectVarList)
  tcltk::tkbind(tlist.var, "<Double-1>", .InsertLHS)


  # build GUI
  tcltk::tkgrid(content, column=0, row=0, sticky = "nwes")
  tcltk::tkgrid(frmVar, padx = 5, pady = 5, row = 0, column = 0,
                rowspan = 20, columnspan = 1, sticky = "ns")

  tcltk::tkgrid(frmButtons, padx = 5, pady = 5, row = 0, column = 2,
                rowspan = 20, columnspan = 1, sticky = "ns")

  tcltk::tkgrid(frmModel, padx = 5, pady = 5, row = 0, column = 3,
                rowspan = 20,
                sticky = "nes")

  tcltk::tkgrid(tfButOK, column=3, row=30, ipadx=15, padx=5, sticky="es")
  tcltk::tkgrid(tfButCanc, column=0, row=30, ipadx=15, padx=5, sticky="ws")

  tcltk::tkfocus(tlist.var)
  tcltk::tclServiceMode(on = TRUE)

  tcltk::tcl("wm", "attributes", root, topmost=TRUE)

  tcltk::tkwait.window(root)

  return(modx)

}



Xplore <- function (x) {


  .PrepCmd <- function(xvar, yvar, data, dcol, col, dpch, pch, alpha, cex, grid, smooth, desc, show) {
    if(desc){
      if(yvar == "none"){
        s <- gettextf("Desc(%s$%s, plotit=FALSE)", deparse(substitute(data)), xvar)
      } else {
        s <- gettextf("Desc(%s ~ %s, data=%s, plotit=FALSE)", yvar, xvar, deparse(substitute(data)))
      }
    } else {

      if(xvar=="none" & yvar == "none"){
        s <- "Canvas()"

      } else if (yvar == "none") {
        s <- gettextf("PlotDesc(%s$%s, na.rm=TRUE)",
                      deparse(substitute(data)), xvar)
      } else {
        s <- gettextf("plot(%s ~ %s, data=%s", yvar,
                      xvar, deparse(substitute(data)))
        if (!is.na(dcol)) {
          s <- paste(s, gettextf(", col=as.numeric(%s)", dcol))
        } else  if (!is.na(col)) {
          s <- paste(s, gettextf(", col=SetAlpha('%s', %s)", col, alpha))
        }
        if (!is.na(dpch)) {
          s <- paste(s, gettextf(", pch=as.numeric(%s)", dpch))
        } else if (!is.na(pch)) {
          s <- paste(s, gettextf(", pch=as.numeric(%s)", pch))
        }
        if (!is.na(cex)) {
          s <- paste(s, gettextf(", cex=as.numeric(%s)", cex))
        }
        s <- paste(s, ")")
      }
      if (show)
        cat(s, "\n")
    }
    if(grid) s <- paste(s, ";grid()")
    if (!is.na(smooth)) {
      scmd <- ""
      if(smooth == "linear"){
        scmd <- gettextf("lines(lm(%s ~ %s, data=%s))", yvar,
                         xvar, deparse(substitute(data)))
      } else if(smooth == "loess"){
        scmd <- gettextf("lines(loess(%s ~ %s, data=%s))", yvar,
                         xvar, deparse(substitute(data)))
      }
      s <- paste(s, ";", scmd)
    }


    return(s)

  }

  if (requireNamespace("manipulate", quietly = FALSE)){

    # define the variables here, as the Rcmd check as CRAN will note miss a visible binding:
    #    Explore: no visible binding for global variable 'xvar'

    xvar <- character()
    yvar <- character()
    dcol <- character()
    dpch <- character()
    col <- character()
    pch <- character()
    alpha <- character()
    cex <- character()
    desc <- logical()
    show <- logical()

    variables <- c("none", as.list(names(x)))
    snames <- c(none = NA, as.list(names(x)[!sapply(x, IsNumeric)]))
    cols <- as.list(colors())
    smoothers <- as.list(c("none", "loess", "linear", "spline"))

    manipulate::manipulate({
      eval(parse(text = .PrepCmd(xvar, yvar, x, dcol, col, dpch, pch, alpha, cex, grid, smooth, desc, show)))
    },
    yvar = manipulate::picker(variables, initial = "none", label = "y-variable     "),
    xvar = manipulate::picker(variables, initial = "none", label = "x-variable     "),
    dcol = manipulate::picker(snames, initial = "none", label = "data color          "),
    col = manipulate::picker(cols, initial = "black", label = "color          "),
    dpch = manipulate::picker(snames, initial = "none", label = "data point character"),
    pch = manipulate::picker(as.list(as.character(1:25)), initial = "1", label = "point character"),
    alpha = manipulate::slider(min=0, max = 1, step = 0.1, ticks = TRUE, initial = 1, label = "transparency"),
    cex = manipulate::slider(min=0.1, max = 5, step = 0.1, ticks = TRUE, initial = 1, label = "point character extension"),
    grid = manipulate::checkbox(initial = FALSE, label = "grid"),
    smooth = manipulate::picker(smoothers, initial = "none", label = "smoother          "),
    desc = manipulate::button("Describe"),
    show = manipulate::button("Print command")
    )

  }
}


ColorDlg <- function() {
  requireNamespace("tcltk", quietly = FALSE)
  return(as.character(tcltk::tcl("tk_chooseColor", title="Choose a color")))
}


dir.choose <- function(default = "", caption = "Select directory"){
  requireNamespace("tcltk", quietly = FALSE)
  tcltk::tk_choose.dir(default = default, caption = caption)
}




SelectVarDlg <- function (x, ...) {
  UseMethod("SelectVarDlg")
}


SelectVarDlg.default <- function(x, useIndex = FALSE, ...){

  # example: Sel(d.pizza)
  xsel <- select.list(x, multiple = TRUE, graphics = TRUE)
  if(useIndex == TRUE) {
    xsel <- which(x %in% xsel)
  } else {
    xsel <- shQuote(xsel)
  }

  if(!identical(xsel, "\"\""))
    txt <- paste("c(", paste(xsel, collapse=","),")", sep="")
  else
    txt <- ""

  .ToClipboard(txt)

  invisible(txt)
}


SelectVarDlg.numeric <- function(x, ...) {

  if(!is.null(names(x)))
    z <- names(x)
  else
    z <- as.character(x)

  txt <- paste(deparse(substitute(x)), "[", SelectVarDlg.default( x = z, ...), "]",
               sep="", collapse="")
  .ToClipboard(txt)

  invisible(txt)

}


SelectVarDlg.factor <- function(x, ...) {

  sel <- SelectVarDlg.default( x = levels(x), ...)
  if(sel!="")
    txt <- paste(deparse(substitute(x)), " %in% ",
                 sel, sep="", collapse="")
  else
    txt <- ""

  .ToClipboard(txt)

  invisible(txt)
}


SelectVarDlg.data.frame <- function(x, ...) {

  sel <- SelectVarDlg.default( x = colnames(x), ...)
  if(sel!="")
    txt <- paste(deparse(substitute(x)), "[,",
                 sel, "]", sep="", collapse="")
  else
    txt <- ""

  .ToClipboard(txt)

  invisible(txt)
}



.ImportSPSS <- function(datasetname = "dataset") {
  # read.spss
  # function (file, use.value.labels = TRUE, to.data.frame = FALSE,
  #           max.value.labels = Inf, trim.factor.names = FALSE, trim_values = TRUE,
  #           reencode = NA, use.missings = to.data.frame)
  e1 <- environment()
  env.dsname <- character()
  env.use.value.labels <- logical()
  env.to.data.frame <- logical()
  env.max.value.labels <- character()
  env.trim.factor.names <- logical()
  env.trim.values <- logical()
  env.reencode <- character()
  env.use.missings <- logical()
  lst <- NULL

  OnOK <- function() {
    assign("lst", list(), envir = e1)
    assign("env.dsname", tcltk::tclvalue(dsname), envir = e1)
    assign("env.use.value.labels", tcltk::tclvalue(use.value.labels), envir = e1)
    assign("env.to.data.frame", tcltk::tclvalue(to.data.frame), envir = e1)
    assign("env.max.value.labels", tcltk::tclvalue(max.value.labels), envir = e1)
    assign("env.trim.factor.names", tcltk::tclvalue(trim.factor.names), envir = e1)
    assign("env.trim.values", tcltk::tclvalue(trim.values), envir = e1)
    assign("env.reencode", tcltk::tclvalue(reencode), envir = e1)
    assign("env.use.missings", tcltk::tclvalue(use.missings), envir = e1)
    tcltk::tkdestroy(top)
  }

  top <- .InitDlg(350, 300, main="Import SPSS Dataset")

  dsname <- tcltk::tclVar(datasetname)
  dsnameFrame <- tcltk::tkframe(top, padx = 10, pady = 10)
  entryDsname <- tcltk::ttkentry(dsnameFrame, width=30, textvariable=dsname)

  optionsFrame <- tcltk::tkframe(top, padx = 10, pady = 10)

  use.value.labels <- tcltk::tclVar("1")
  use.value.labelsCheckBox <- tcltk::ttkcheckbutton(optionsFrame, text="Use value labels", variable=use.value.labels)

  to.data.frame <- tcltk::tclVar("1")
  to.data.frameCheckBox <- tcltk::ttkcheckbutton(optionsFrame,
                                                 text="Convert value labels to factor levels", variable=to.data.frame)
  max.value.labels <- tcltk::tclVar("Inf")
  entryMaxValueLabels <- tcltk::ttkentry(optionsFrame, width=30, textvariable=max.value.labels)

  trim.values <- tcltk::tclVar("1")
  trim.valuesCheckBox <- tcltk::ttkcheckbutton(optionsFrame, text="Ignore trailing spaces when matching"
                                               , variable=trim.values)
  trim.factor.names <- tcltk::tclVar("1")
  trim.factor.namesCheckBox <- tcltk::ttkcheckbutton(optionsFrame, text="Trim trailing spaces from factor levels"
                                                     , variable=trim.factor.names)
  reencode <- tcltk::tclVar("")
  entryReencode <- tcltk::ttkentry(optionsFrame, width=30, textvariable=reencode)

  use.missings <- tcltk::tclVar("1")
  use.missingsCheckBox <- tcltk::ttkcheckbutton(optionsFrame, text="Use missings",
                                                variable=use.missings)

  tcltk::tkgrid(tcltk::tklabel(dsnameFrame, text="Enter name for data set:  "), entryDsname, sticky="w")
  tcltk::tkgrid(dsnameFrame, columnspan=2, sticky="w")
  tcltk::tkgrid(use.value.labelsCheckBox, sticky="w")
  tcltk::tkgrid(to.data.frameCheckBox, sticky="nw")
  tcltk::tkgrid(tcltk::ttklabel(optionsFrame, text="Maximal value label:"), sticky="nw")
  tcltk::tkgrid(entryMaxValueLabels, padx=20, sticky="nw")
  tcltk::tkgrid(trim.valuesCheckBox, sticky="w")
  tcltk::tkgrid(trim.factor.namesCheckBox, sticky="w")
  tcltk::tkgrid(tcltk::ttklabel(optionsFrame, text="Reencode character strings to the current locale:"), sticky="nw")
  tcltk::tkgrid(entryReencode, padx=20, sticky="nw")
  tcltk::tkgrid(use.missingsCheckBox, sticky="w")
  tcltk::tkgrid(optionsFrame, sticky="w")

  buttonsFrame <- tcltk::tkframe(top, padx = 10, pady = 10)
  tfButOK <- tcltk::tkbutton(buttonsFrame, text = "OK", command = OnOK, width=10)
  tfButCanc <- tcltk::tkbutton(buttonsFrame, width=10, text = "Cancel", command = function() tcltk::tkdestroy(top))

  tcltk::tkgrid(tfButOK, tfButCanc)
  tcltk::tkgrid.configure(tfButCanc, padx=c(6,6))
  tcltk::tkgrid.columnconfigure(buttonsFrame, 0, weight=2)
  tcltk::tkgrid.columnconfigure(buttonsFrame, 1, weight=1)

  tcltk::tkgrid(buttonsFrame, sticky="ew")
  tcltk::tkwait.window(top)

  if(!is.null(lst)){
    lst <- list(dsname=env.dsname, use.value.labels=as.numeric(env.use.value.labels),
                to.data.frame=as.numeric(env.to.data.frame),
                max.value.labels=env.max.value.labels, trim.factor.names=as.numeric(env.trim.factor.names),
                trim.values=as.numeric(env.trim.values), reencode=env.reencode, use.missings=as.numeric(env.use.missings)  )
  }
  return(lst)

}


.ImportSYSTAT <- function(datasetname = "dataset") {

  e1 <- environment()
  env.dsname <- character()
  env.to.data.frame <- logical()
  lst <- NULL

  top <- .InitDlg(350, 140, main="Import SYSTAT Dataset")

  OnOK <- function() {
    assign("lst", list(), envir = e1)
    assign("env.dsname", tcltk::tclvalue(dsname), envir = e1)
    assign("env.to.data.frame", tcltk::tclvalue(to.data.frame ), envir = e1)
    tcltk::tkdestroy(top)
  }

  dsname <- tcltk::tclVar(datasetname)
  dsnameFrame <- tcltk::tkframe(top, padx = 10, pady = 10)
  entryDsname <- tcltk::ttkentry(dsnameFrame, width=30, textvariable=dsname)

  optionsFrame <- tcltk::tkframe(top, padx = 10, pady = 10)
  to.data.frame <- tcltk::tclVar("1")
  to.data.frameCheckBox <- tcltk::ttkcheckbutton(optionsFrame,
                                                 text="Convert dataset to data.frame", variable=to.data.frame)

  tcltk::tkgrid(tcltk::tklabel(dsnameFrame, text="Enter name for data set:  "), entryDsname, sticky="w")
  tcltk::tkgrid(dsnameFrame, columnspan=2, sticky="w")
  tcltk::tkgrid(to.data.frameCheckBox, sticky="w")
  tcltk::tkgrid(optionsFrame, sticky="w")

  buttonsFrame <- tcltk::tkframe(top, padx = 10, pady = 10)
  tfButOK <- tcltk::tkbutton(buttonsFrame, text = "OK", command = OnOK, width=10)
  tfButCanc <- tcltk::tkbutton(buttonsFrame, width=10, text = "Cancel", command = function() tcltk::tkdestroy(top))

  tcltk::tkgrid(tfButOK, tfButCanc)
  tcltk::tkgrid.configure(tfButCanc, padx=c(6,6))
  tcltk::tkgrid.columnconfigure(buttonsFrame, 0, weight=2)
  tcltk::tkgrid.columnconfigure(buttonsFrame, 1, weight=1)

  tcltk::tkgrid(buttonsFrame, sticky="ew")
  tcltk::tkwait.window(top)

  if(!is.null(lst)){
    lst <- list(dsname=env.dsname, to.data.frame=as.numeric(env.to.data.frame))
  }
  return(lst)

}



.ImportStataDlg <- function(datasetname = "dataset") {

  #   function (file, convert.dates = TRUE, convert.factors = TRUE,
  #             missing.type = FALSE, convert.underscore = FALSE, warn.missing.labels = TRUE)

  e1 <- environment()
  env.dsname <- character()
  env.convert.dates <- logical()
  env.convert.factors <- logical()
  env.convert.underscore <- logical()
  env.missing.type <- logical()
  env.warn.missing.labels <- logical()
  lst <- NULL

  OnOK <- function() {
    assign("lst", list(), envir = e1)
    assign("env.dsname", tcltk::tclvalue(dsname), envir = e1)
    assign("env.convert.dates", tcltk::tclvalue(convert.dates), envir = e1)
    assign("env.convert.factors", tcltk::tclvalue(convert.factors), envir = e1)
    assign("env.convert.underscore", tcltk::tclvalue(convert.underscore), envir = e1)
    assign("env.missing.type", tcltk::tclvalue(missing.type), envir = e1)
    assign("env.warn.missing.labels", tcltk::tclvalue(warn.missing.labels), envir = e1)
    tcltk::tkdestroy(top)
  }

  top <- .InitDlg(350, 220, main="Import Stata Dataset")

  dsname <- tcltk::tclVar(datasetname)
  dsnameFrame <- tcltk::tkframe(top, padx = 10, pady = 10)
  entryDsname <- tcltk::ttkentry(dsnameFrame, width=30, textvariable=dsname)

  optionsFrame <- tcltk::tkframe(top, padx = 10, pady = 10)

  convert.factors <- tcltk::tclVar("1")
  convert.factorsCheckBox <- tcltk::ttkcheckbutton(optionsFrame,
                                                   text="Convert value labels to factor levels", variable=convert.factors)
  convert.dates <- tcltk::tclVar("1")
  convert.datesCheckBox <- tcltk::ttkcheckbutton(optionsFrame, text="Convert dates to R format", variable=convert.dates)

  missing.type <- tcltk::tclVar("1")
  missing.typeCheckBox <- tcltk::ttkcheckbutton(optionsFrame, text="Multiple missing types (>=Stata 8)"
                                                , variable=missing.type)
  convert.underscore <- tcltk::tclVar("1")
  convert.underscoreCheckBox <- tcltk::ttkcheckbutton(optionsFrame, text="Convert underscore to period"
                                                      , variable=convert.underscore)
  warn.missing.labels <- tcltk::tclVar("1")
  warn.missing.labelsCheckBox <- tcltk::ttkcheckbutton(optionsFrame, text="Warn on missing labels",
                                                       variable=warn.missing.labels)

  tcltk::tkgrid(tcltk::tklabel(dsnameFrame, text="Enter name for data set:  "), entryDsname, sticky="w")
  tcltk::tkgrid(dsnameFrame, columnspan=2, sticky="w")
  tcltk::tkgrid(convert.datesCheckBox, sticky="w")
  tcltk::tkgrid(convert.factorsCheckBox, sticky="nw")
  tcltk::tkgrid(missing.typeCheckBox, sticky="w")
  tcltk::tkgrid(convert.underscoreCheckBox, sticky="w")
  tcltk::tkgrid(warn.missing.labelsCheckBox, sticky="w")
  tcltk::tkgrid(optionsFrame, sticky="w")

  buttonsFrame <- tcltk::tkframe(top, padx = 10, pady = 10)
  tfButOK <- tcltk::tkbutton(buttonsFrame, text = "OK", command = OnOK, width=10)
  tfButCanc <- tcltk::tkbutton(buttonsFrame, width=10, text = "Cancel", command = function() tcltk::tkdestroy(top))

  tcltk::tkgrid(tfButOK, tfButCanc)
  tcltk::tkgrid.configure(tfButCanc, padx=c(6,6))
  tcltk::tkgrid.columnconfigure(buttonsFrame, 0, weight=2)
  tcltk::tkgrid.columnconfigure(buttonsFrame, 1, weight=1)

  tcltk::tkgrid(buttonsFrame, sticky="ew")
  tcltk::tkwait.window(top)

  if(!is.null(lst)){
    lst <- list(dsname=env.dsname, convert.factors=as.numeric(env.convert.factors),
                convert.dates=as.numeric(env.convert.dates), convert.underscore=as.numeric(env.convert.underscore),
                missing.type=as.numeric(env.missing.type), warn.missing.labels=as.numeric(env.warn.missing.labels)  )
  }
  return(lst)

}


FileImportDlg <- function(auto_type = TRUE, env = .GlobalEnv)  {

  requireNamespace("tcltk", quietly = FALSE)

  filename <- tcltk::tclvalue(tcltk::tkgetOpenFile(filetypes= "{{All files} *}
     {{SPSS Files} {.sav}} {{SAS xport files} {.xpt, .xport}}
     {{SYSTAT} {*.sys, *.syd}} {{MiniTab} {.mtp}}
     {{Stata Files} {.dta}}"))

  # nicht topmost, aber wie mach ich das dann??
  # tcl("wm", "attributes", root, topmost=TRUE)

  if (filename=="") return()

  path <- SplitPath(filename)

  fformats <- c("SPSS","SAS","SYSTAT", "Minitab","Stata")

  if(auto_type){
    xsel <- switch(toupper(path$extension),
                   "SAV"="SPSS",
                   "DTA"="Stata",
                   "SYD"="SYSTAT",
                   "SYS"="SYSTAT",
                   "MTP"="MiniTab",
                   "XPT"="SAS",
                   "XPORT"="SAS",
                   "SAS"="SAS",
                   select.list(fformats, multiple = FALSE, graphics = TRUE))
  } else {
    xsel <- select.list(fformats, multiple = FALSE, graphics = TRUE)
  }

  switch(xsel,
         "MiniTab"={
           zz <- foreign::read.mtp(file=filename)
         },
         "SYSTAT"={
           dlg <- .ImportSYSTAT(paste("d.", path$filename, sep=""))
           if(is.null(dlg)) return()
           zz <- foreign::read.systat(file=filename, to.data.frame = dlg$to.data.frame)
         },
         "SPSS"={
           dlg <- .ImportSPSS(paste("d.", path$filename, sep=""))
           if(is.null(dlg)) return()
           zz <- foreign::read.spss(file=filename, use.value.labels = dlg$use.value.labels,
                                    to.data.frame = dlg$to.data.frame,
                                    max.value.labels = dlg$max.value.labels,
                                    trim.factor.names = dlg$trim.factor.names,
                                    trim_values = dlg$trim_value,
                                    reencode = ifelse(dlg$reencode=="", NA, dlg$reencode),
                                    use.missings = dlg$use.missings)
         },
         "SAS"={
           print("not yet implemented.")
         },
         "Stata"={
           dlg <- .ImportStataDlg(paste("d.", path$filename, sep=""))
           if(is.null(dlg)) return()
           zz <- foreign::read.dta(file=filename, convert.dates = dlg[["convert.dates"]], convert.factors = dlg[["convert.factors"]],
                                   missing.type = dlg[["missing.type"]], convert.underscore = dlg[["convert.underscore"]],
                                   warn.missing.labels = dlg[["warn.missing.labels"]])
         })
  assign(dlg[["dsname"]], zz, envir=env)
  message(gettextf("Dataset %s has been successfully created!\n\n", dlg[["dsname"]]))
  # Exec(gettextf("print(str(%s, envir = %s))", dlg[["dsname"]],  deparse(substitute(env))))
}





ColPicker <- function(locator=TRUE, ord=c("hsv","default"), label=c("text","hex","dec"),
                      mdim = c(38, 12), newwin = FALSE) {

  usr <- par(no.readonly=TRUE)
  opt <- options(locatorBell = FALSE)

  on.exit({
    par(usr)
    options(opt)
  })

  # this does not work and CRAN does not allow windows()
  # dev.new(width=13, height=7)
  if(newwin == TRUE)
    dev.new(width=13, height=7, noRStudioGD = TRUE)

  # plots all named colors:   PlotRCol(lbel="hex") hat noch zuviele Bezeichnungen
  if( !is.null(dev.list()) ){
    curwin <- dev.cur()
    on.exit({
      dev.set(curwin)
      par(usr)
    })
  }


  # colors without greys (and grays...) n = 453
  cols <- colors()[-grep( pattern="^gr[ea]y", colors())]

  # set order
  switch( match.arg( arg=ord, choices=c("hsv","default") )
          , "default" = { # do nothing
          }
          , "hsv" = {
            rgbc <- col2rgb(cols)
            hsvc <- rgb2hsv(rgbc[1,],rgbc[2,],rgbc[3,])
            cols <- cols[ order(hsvc[1,],hsvc[2,],hsvc[3,]) ]
          }
  )


  zeilen <- mdim[1]; spalten <- mdim[2] # 660 Farben
  farben.zahlen <- matrix( 1:spalten, nrow=zeilen, ncol=spalten, byrow=TRUE) # Matrix fuer Punkte

  if(zeilen*spalten > length(cols))
    cols <- c(cols, rep(NA, zeilen*spalten - length(cols)) ) # um 3 NULL-Werte erweitern

  x_offset <- 0.5
  x <- farben.zahlen[, 1:spalten]  # x-Werte (Zahlen)
  y <- -rep(1:zeilen, spalten)     # y-Werte (Zahlen)

  par(mar=c(0,0,0,0), mex=0.001, xaxt="n", yaxt="n", ann=F)
  plot( x, y
        , pch=22    # Punkttyp Rechteck
        , cex=2     # Vergroesserung Punkte
        , col=NA
        , bg=cols   # Hintergrundfarben
        , bty="n"   # keine Box
        , xlim=c(1, spalten+x_offset) # x-Wertebereich
  )
  switch( match.arg( arg=label, choices=c("text","hex","dec") )
          , "text" = {
            text( x+0.1, y, cols, adj=0, cex=0.6 ) # Text Farben
          }
          , "hex" = {     # HEX-Codes
            text( x+0.1, y, adj=0, cex=0.6,
                  c(apply(apply(col2rgb(cols[1:(length(cols)-3)]), 2, sprintf, fmt=" %02X"), 2, paste, collapse=""), rep("",3))
            )
          }
          , "dec" = {     # decimal RGB-Codes
            text( x+0.1, y, adj=0, cex=0.6,
                  c(apply(apply(col2rgb(cols[1:(length(cols)-3)]), 2, sprintf, fmt=" %03d"), 2, paste, collapse=""), rep("",3))
            )
          }
  )

  z <- locator()

  idx <- with(lapply(z, round), (x-1) * zeilen + abs(y))
  return(cols[idx])

}



PlotPar <- function(){
  # plots the most used plot parameters

  usr <- par(no.readonly=TRUE);  on.exit(par(usr))

  if( !is.null(dev.list()) ){
    curwin <- dev.cur()
    on.exit({
      dev.set(curwin)
      par(usr)
      })
  }

  # this does not work and CRAN does not allow windows()
  # dev.new(width=7.2, height=4)

  par( mar=c(0,0,0,0), mex=0.001, xaxt="n", yaxt="n", ann=F, xpd=TRUE)
  plot( x=1:25, y=rep(11,25), pch=1:25, cex=2, xlab="", ylab=""
      , frame.plot=FALSE, ylim=c(-1,15), col=2, bg=3)
  points( x=1:25, y=rep(12.5,25), pch=1:35, cex=2, col=1)
  text( x=1:25, y=rep(9.5,25), labels=1:25, cex=0.8 )
  segments( x0=1, x1=4, y0=0:5, lty=6:1, lwd=3 )
  text( x=5, y=6:0, adj=c(0,0.5), labels=c("0 = blank", "1 = solid (default)", "2 = dashed", "3 = dotted", "4 = dotdash", "5 = longdash", "6 = twodash") )
  segments( x0=10, x1=12, y0=0:6, lty=1, lwd=7:1 )
  text( x=13, y=0:6, adj=c(0,0.5), labels=7:1 )
  points( x=rep(15,7), y=0:6, cex=rev(c(0.8,1,1.5,2,3,4,7)) )
  text( x=16, y=0:6, adj=c(0,0.5), labels=rev(c(0.8,1,1.5,2,3,4,7)) )
  text( x=c(1,1,10,15,18,18), y=c(14,7.5,7.5,7.5,7.5,2.5), labels=c("pch","lty","lwd","pt.cex","adj","col"), cex=1.3, col="grey40")
  adj <- expand.grid(c(0,0.5,1),c(0,0.5,1))
  for( i in 1:nrow(adj)  ){
    text( x=18+adj[i,1]*7, y=3.5+adj[i,2]*3, label=paste("text", paste(adj[i,], collapse=",") ), adj=unlist(adj[i,]), cex=0.8 )
  }
  points( x=18:25, y=rep(1,8), col=1:8, pch=15, cex=2 )
  text( x=18:25, y=0, adj=c(0.5,0.5), labels=1:8, cex=0.8 )

}



PlotPch <- function (col = NULL, bg = NULL, newwin = FALSE) {

  if (newwin == TRUE)
    dev.new(width=2, height=5, noRStudioGD=TRUE)
    # dev.new(width=3, height=2, xpos=100, ypos=600, noRStudioGD = TRUE)

  usr <- par(no.readonly = TRUE)
  on.exit(par(usr))
  if (!is.null(dev.list())) {
    curwin <- dev.cur()
    on.exit({
      dev.set(curwin)
      par(usr)
    })
  }

  if(is.null(col))
    col <- DescTools::Pal("Helsana")[1]  # DescTools::hred
  if(is.null(bg))
    bg <- DescTools::Pal("Helsana")[4]   # hecru

  par(mar = c(0, 0, 0, 0), mex = 0.001, xaxt = "n", yaxt = "n",
      ann = F, xpd = TRUE)
  plot(y = 1:25, x = rep(3, 25), pch = 25:1, cex = 1.5, xlab = "",
       ylab = "", frame.plot = FALSE, xlim = c(-1, 15))
  points(y = 1:25, x = rep(6, 25), pch = 25:1, cex = 1.5,
         col = col, bg = bg)
  text(y = 25:1, x = rep(9, 25), labels = 1:25, cex = 0.8)

}




PlotMar <- function(){

  hred <- DescTools::Pal("Helsana")[1]
  hgreen <- DescTools::Pal("Helsana")[7]
  horange <- DescTools::Pal("Helsana")[2]
  hecru <- DescTools::Pal("Helsana")[4]

  par(oma=c(3,3,3,3))  # all sides have 3 lines of space
  #par(omi=c(1,1,1,1)) # alternative, uncomment this and comment the previous line to try

  # - The mar command represents the figure margins. The vector is in the same ordering of
  #   the oma commands.
  #
  # - The default size is c(5,4,4,2) + 0.1, (equivalent to c(5.1,4.1,4.1,2.1)).
  #
  # - The axes tick marks will go in the first line of the left and bottom with the axis
  #   label going in the second line.
  #
  # - The title will fit in the third line on the top of the graph.
  #
  # - All of the alternatives are:
  #	- mar: Specify the margins of the figure in number of lines
  #	- mai: Specify the margins of the figure in number of inches

  par(mar=c(5,4,4,2) + 0.1)
  #par(mai=c(2,1.5,1.5,.5)) # alternative, uncomment this and comment the previous line

  # Plot
  plot(x=1:10, y=1:10, type="n", xlab="X", ylab="Y")	# type="n" hides the points

  # Place text in the plot and color everything plot-related red
  text(5,5, "Plot", col=hred, cex=2)
  text(5,4, "text(5,5, \"Plot\", col=\"red\", cex=2)", col=hred, cex=1)
  box("plot", col=hred)

  # Place text in the margins and label the margins, all in green
  mtext("Figure", side=3, line=2, cex=2, col=hgreen)
  mtext("par(mar=c(5,4,4,2) + 0.1)", side=3, line=1, cex=1, col=hgreen)
  mtext("Line 0", side=3, line=0, adj=1.0, cex=1, col=hgreen)
  mtext("Line 1", side=3, line=1, adj=1.0, cex=1, col=hgreen)
  mtext("Line 2", side=3, line=2, adj=1.0, cex=1, col=hgreen)
  mtext("Line 3", side=3, line=3, adj=1.0, cex=1, col=hgreen)
  mtext("Line 0", side=2, line=0, adj=1.0, cex=1, col=hgreen)
  mtext("Line 1", side=2, line=1, adj=1.0, cex=1, col=hgreen)
  mtext("Line 2", side=2, line=2, adj=1.0, cex=1, col=hgreen)
  mtext("Line 3", side=2, line=3, adj=1.0, cex=1, col=hgreen)
  box("figure", col=hgreen)

  # Label the outer margin area and color it blue
  # Note the 'outer=TRUE' command moves us from the figure margins to the outer
  # margins.
  mtext("Outer Margin Area", side=1, line=1, cex=2, col=horange, outer=TRUE)
  mtext("par(oma=c(3,3,3,3))", side=1, line=2, cex=1, col=horange, outer=TRUE)
  mtext("Line 0", side=1, line=0, adj=0.0, cex=1, col=horange, outer=TRUE)
  mtext("Line 1", side=1, line=1, adj=0.0, cex=1, col=horange, outer=TRUE)
  mtext("Line 2", side=1, line=2, adj=0.0, cex=1, col=horange, outer=TRUE)
  box("outer", col=horange)

  usr <- par("usr")
  # inner <- par("inner")
  fig <- par("fig")
  plt <- par("plt")

  # text("Figure", x=fig, y=ycoord, adj = c(1, 0))
  text("Inner", x=usr[2] + (usr[2] - usr[1])/(plt[2] - plt[1]) * (1 - plt[2]),
       y=usr[3] - diff(usr[3:4])/diff(plt[3:4]) * (plt[3]), adj = c(1, 0))
  #text("Plot", x=usr[1], y=usr[2], adj = c(0, 1))

  figusrx <- grconvertX(usr[c(1,2)], to="nfc")
  figusry <- grconvertY(usr[c(3,4)], to="nfc")
  points(x=figusrx[c(1,1,2,2)], y=figusry[c(3,4,3,4)], pch=15, cex=3, xpd=NA)

  points(x=usr[c(1,1,2,2)], y=usr[c(3,4,3,4)], pch=15, col=hred, cex=2, xpd=NA)

  arrows(x0 = par("usr")[1], 8, par("usr")[2], 8, col="black", cex=2, code=3, angle = 15, length = .2)
  text(x = mean(par("usr")[1:2]), y=8.2, labels = "pin[1]", adj=c(0.5, 0))

}


.BringToFront <- function(main){
  
  info_sys <- Sys.info() # sniff the O.S.
  
  if (info_sys['sysname'] == 'Windows') { # MS Windows trick
    shell(gettextf("powershell -command [void] [System.Reflection.Assembly]::LoadWithPartialName('Microsoft.VisualBasic') ; [Microsoft.VisualBasic.Interaction]::AppActivate('%s') ", main))
  }
  
}




BookmarkDlg <- function(){
  
  requireNamespace("tcltk")
  
  .ManipBM <- function(action, newname=NULL) {
    
    var.name <- as.numeric(tcltk::tkcurselection(tlist.var))
    lst <- .GetVarName(as.character(tcltk::tkget(tlist.var, 0, "end")))
    
    if (length(var.name) == 0)
      tcltk::tkmessageBox(message = "No variable selected",
                          icon = "info", type = "ok")
    
    if (length(var.name) > 0) {
      vn <- DescTools::StrTrim(lst[var.name + 1])
      if(action=="select"){
        DescTools::WrdGoto(vn)  
      }
      else if(action=="delete"){
        res <- DescTools::WrdDeleteBookmark(vn)  
        if(res){
          
          # remove listentry
          # tcltk::tkdelete(tlist.var, var.name)
          
          d.bm <- d.bm[d.bm$name != vn, ]
          
          tclvalue(tbm_name) <- d.bm$name
          tclvalue(tbm_type) <- d.bm$type
          
          .PopulateListBox(d.bm$name)
          
          return(list(id=var.name, name=vn))
          
        }
      }
      else if(action=="rename"){
        newname <- .GetNewName(vn)
        if(!newname==FALSE)
          RenameBookmark(vn, newname)  
      }
    }
  }
  
  .GetNewName <- function(x){
    .SimpEntryDlg("Enter new bookmark name:", default = x, main="Rename Bookmark")
  }
  
  .BtnSelect <- function() .ManipBM("select")
  
  .BtnDelete <- function() {
      res <- .ManipBM("delete")
  }
  
  .BtnRename <- function() .ManipBM("rename", .GetNewName())

  
  imgAsc <-  tcltk::tclVar()
  tclimgAsc <-  tcltk::tkimage.create("photo", imgAsc, file = file.path(find.package("DescToolsAddIns"), "extdata", "SortListAsc.gif"))
  imgDesc <-  tcltk::tclVar()
  tclimgDesc <-  tcltk::tkimage.create("photo", imgDesc, file = file.path(find.package("DescToolsAddIns"), "extdata", "SortListDesc.gif"))
  imgNone <-  tcltk::tclVar()
  tclimgNone <-  tcltk::tkimage.create("photo", imgNone, file = file.path(find.package("DescToolsAddIns"), "extdata", "SortListNo.gif"))
  
  .BtnSortVarListAsc <- function() .SortVarList("a")
  .BtnSortVarListDesc <- function() .SortVarList("d")
  .BtnSortVarListNone <- function() .SortVarList("n")
  
  
  .SortVarList <- function(ord){
    
    lst <- as.character(tcltk::tkget(tlist.var, 0, "end"))
    
    if(ord == "a"){
      v <- DescTools::StrTrim(sort(lst, decreasing = FALSE))
    } else if(ord == "d"){
      v <- DescTools::StrTrim(sort(lst, decreasing = TRUE))
    } else {
      v <- DescTools::StrTrim(.VarNames()[strsplit(tclvalue(tbm_name), split=" ")[[1]] %in% .GetVarName(lst)])
    }
    
    .PopulateListBox(v)
    
  }
  
  .FilterVarList <- function(){
    
    pat <- DescTools::StrTrim(tcltk::tclvalue(tffilter))
    # print(pat)
    if(pat=="")
      v <- .VarNames()
    else
      v <- grep(pattern = pat, .VarNames(), value=TRUE, fixed=TRUE)
    
    # for (i in (length(names(x)):0)) tcltk::tkdelete(tlist.var, i)
    
    .PopulateListBox(v)

  }
  
  .SelectVarList <- function(){
    
    var.name <- as.numeric(tcltk::tkcurselection(tlist.var))
    lst <- .GetVarName(as.character(tcltk::tkget(tlist.var, 0, "end")))
    
    if (length(var.name) > 0) {
      
      .EnableBtn(TRUE)
      
      z <- d.bm[d.bm$name==StrTrim(lst[var.name + 1]), ]
      txt <- gettextf(" ID:\t%s\n Type:\t%s\n Page:\t%s", z$id, z$type, z$pagenr)
      tcltk::tclvalue(tflbl) <- txt
      
    } else {
      tcltk::tclvalue(tflbl) <- " ID:\t.\n Type:\t.\n Page:\t."  # "\n"
    }

  }
  
  
  .VarNames <- function(){
    
    # gettextf("%s (%s)", d.bm$name, d.bm$type)
    gettextf("%s (%s)", strsplit(tclvalue(tbm_name), split = " ")[[1]], 
             strsplit(tclvalue(tbm_type), split = " ")[[1]])
  }
  
  .GetVarName <- function(x){
    StrTrim(gsub(" .*", "", StrTrim(x)))
  }
  
  .EmptyListBox <- function(){
    n <- as.character(tcltk::tksize(tlist.var))
    for (i in (n:0)) tcltk::tkdelete(tlist.var, i)
  }
  
  
  .PopulateListBox <- function(x, empty=TRUE){
    
    if(empty)
      .EmptyListBox()
    
    for (z in x) {
      tcltk::tkinsert(tlist.var, "end", paste0(" ", z))
    }
    
    # update frame label
    tcltk::tkconfigure(frmVar, text=gettextf("Bookmarks (%s/%s):", length(x), 
                                             length(strsplit(tclvalue(tbm_name), split=" ")[[1]])))
    
    # as far as there are no selections we should disable action btns
    .EnableBtn(FALSE)
    .SelectVarList()

  }


  .OnOK <- function() {
    tcltk::tkdestroy(root)
  }
  
  .EnableBtn <- function(enable=TRUE){
    if(enable)
      state <- "active"
    else
      state <- "disabled"
    
    tcltk::tkconfigure(tfButSelect, state = state)
    # Renaming needs more work...
    tcltk::tkconfigure(tfButRename, state = "disabled")
    tcltk::tkconfigure(tfButDelete, state = state)
  }
  

# start main proc **************
  
  # get the bookmarks of current wrd
  d.bm <- WrdBookmarks()

  if(identical(d.bm, NA))
    d.bm <- data.frame(id=NA, name="[...no bookmarks found!]", pagenr="", type="" )
  

  fam <- "comic"
  size <- 10
  myfont <- tcltk::tkfont.create(family = fam, size = size)
  mySerfont <- tcltk::tkfont.create(family = "Times", size = size)
  
  tffilter <- tcltk::tclVar("")
  tflbl <- tcltk::tclVar("\n")
  tfframe <- tcltk::tclVar("Variables:")
  
  # the bookmarks for the list
  tbm_name <- tcltk::tclVar(d.bm$name)
  tbm_type <- tcltk::tclVar(d.bm$type)
  
  wrd <- DescToolsOptions("lastWord")
  xname <- wrd[["ActiveDocument"]][["name"]]

  # do not update screen
  tcltk::tclServiceMode(on = FALSE)
  
  # create window
  root <- .InitDlg(width = 380, height = 550, resizex=TRUE, resizey=TRUE,
                   main=gettextf("Bookmarks (%s)", xname), ico="R")
  
  # define widgets
  content <- tcltk::tkframe(root, padx=10, pady=10)
  
  
  # Variable list
  frmVar <- tcltk::tkwidget(content, "labelframe", text=gettextf("Bookmarks (%s/%s):", nrow(d.bm), nrow(d.bm)),
                            fg = "black", padx = 10, pady = 10, font = myfont)
  
  
  tfFilter <- tcltk::tkentry(frmVar, textvariable=tffilter, width= 20, bg="white")
  tfButSortAsc <- tcltk::tkbutton(frmVar, image = tclimgAsc, compound="none",
                                  command = .BtnSortVarListAsc, height = 21, width = 21)
  tfButSortDesc <- tcltk::tkbutton(frmVar, image = tclimgDesc, compound="none",
                                   command = .BtnSortVarListDesc, height = 21, width = 21)
  tfButSortNone <- tcltk::tkbutton(frmVar, image=tclimgNone, compound="none",
                                   command = .BtnSortVarListNone, height = 21, width = 21)
  var.scr <- tcltk::tkscrollbar(frmVar, repeatinterval = 5,
                                command = function(...) tcltk::tkyview(tlist.var, ...))
  
  tlist.var <- tcltk::tklistbox(frmVar, selectmode = "single",
                                yscrollcommand = function(...)
                                  tcltk::tkset(var.scr, ...), background = "white",
                                exportselection = FALSE, activestyle= "none", highlightthickness=0,
                                height=20, width=20, font = myfont)
  tfVarLabel <- tcltk::tklabel(frmVar, justify="left", width=26, anchor="w", textvariable=tflbl, font=myfont)
  
  tcltk::tclvalue(tflbl) <- " ID:\n Type:\n Page:"
  

  tcltk::tkbind(tlist.var)
  tcltk::tkgrid(tfFilter, row=0, padx=0, sticky = "n")
  tcltk::tkgrid(tcltk::tklabel(frmVar, text="  "), row=0, column=1)
  tcltk::tkgrid(tfButSortAsc, row=0, column=2, padx=0, sticky = "n")
  tcltk::tkgrid(tfButSortDesc, row=0, column=3,  sticky = "n")
  tcltk::tkgrid(tfButSortNone, row=0, column=4, sticky = "n")
  tcltk::tkgrid(tcltk::tklabel(frmVar, text=" "))
  tcltk::tkgrid(tlist.var, var.scr, row=2, columnspan=5, sticky = "news")
  tcltk::tkgrid(tfVarLabel, row=3, columnspan=5, pady=3, sticky = "es")
  tcltk::tkgrid.configure(var.scr, sticky = "news")
  # tcltk2::tk2tip(tlist.var, "List of variables in data frame")
  
  # Buttons
  frmButtons <- tcltk::tkwidget(content, "labelframe", text = "",  bd=0,
                                fg = "black", padx = 5, pady = 25)

  tfButSelect <- tcltk::tkbutton(frmButtons, text = "Select",
                              command = .BtnSelect, height = 1, width = 7, font=myfont)
  tfButDelete <- tcltk::tkbutton(frmButtons, text = "Delete",
                               command = .BtnDelete, height = 1, width = 7, font=myfont)
  tfButRename <- tcltk::tkbutton(frmButtons, text = "Rename",
                              command = .BtnRename,
                              height = 1, width = 7, font=myfont)

  tcltk::tkgrid(tcltk::tklabel(frmButtons, text="\n\n"))
  tcltk::tkgrid(tfButSelect, row = 40, padx = 5, sticky = "s")
  tcltk::tkgrid(tfButRename, row = 50, padx = 5, sticky = "s")
  tcltk::tkgrid(tfButDelete, row = 60, padx = 5, sticky = "s")

  # root
  tfButOK = tcltk::tkbutton(content, text="Close", width=6, command=.OnOK)

  tcltk::tkbind(tfFilter, "<KeyRelease>", .FilterVarList)
  tcltk::tkbind(tlist.var, "<ButtonRelease>", .SelectVarList)
  tcltk::tkbind(tlist.var, "<KeyRelease>", .SelectVarList)
  tcltk::tkbind(tlist.var, "<Double-1>", .BtnSelect)
  
  .PopulateListBox(.VarNames())
  
  # build GUI
  tcltk::tkgrid(content, column=0, row=0, sticky = "nwes")
  tcltk::tkgrid(frmVar, padx = 5, pady = 5, row = 0, column = 0,
                rowspan = 20, columnspan = 1, sticky = "ns")
  
  tcltk::tkgrid(frmButtons, padx = 5, pady = 5, row = 0, column = 2,
                rowspan = 20, columnspan = 1, sticky = "ns")
  
  tcltk::tkgrid(tfButOK, column=2, row=30, ipadx=15, padx=5, sticky="es")

  tcltk::tkfocus(tlist.var)
  tcltk::tclServiceMode(on = TRUE)
  
  tcltk::tcl("wm", "attributes", root, topmost=TRUE)
  
  tcltk::tkwait.window(root)
  
  invisible()
  
}



.SimpEntryDlg <- function(text, default, main){

  requireNamespace("tcltk", quietly = FALSE)
  
  e1 <- environment()
  txt <- character()
  
  tfpw <- tcltk::tclVar("")
  
  OnOK <- function() {
    assign("txt", tcltk::tclvalue(tfpw), envir = e1)
    tcltk::tkdestroy(root)
  }
  
  # do not update screen
  tcltk::tclServiceMode(on = FALSE)

  # create window
  root <- .InitDlg(205, 110, resizex=FALSE, resizey=FALSE, main=main, ico="R")
  
  # define widgets
  content <- tcltk::tkframe(root, padx=10, pady=10)
  tfEntrPW <- tcltk::tkentry(content, width="30", textvariable=tfpw)
  tfButOK <- tcltk::tkbutton(content,text="OK", command=OnOK, width=6)
  tfButCanc <- tcltk::tkbutton(content, text="Cancel", width=7,
                               command=function() tcltk::tkdestroy(root))
  
  # build GUI
  tcltk::tkgrid(content, column=0, row=0)
  tcltk::tkgrid(tcltk::tklabel(content, text=text), column=0, row=0,
                columnspan=3, sticky="w")
  tcltk::tkgrid(tfEntrPW, column=0, row=1, columnspan=3, pady=10)
  tcltk::tkgrid(tfButOK, column=0, row=2, ipadx=15, sticky="w")
  tcltk::tkgrid(tfButCanc, column=2, row=2, ipadx=5, sticky="e")
  
  # binding event-handler
  tcltk::tkbind(tfEntrPW, "<Return>", OnOK)
  
  tcltk::tkfocus(tfEntrPW)
  tcltk::tclServiceMode(on = TRUE)
  
  tcltk::tcl("wm", "attributes", root, topmost=TRUE)
  
  tcltk::tkwait.window(root)
  
  return(txt)
  
}







