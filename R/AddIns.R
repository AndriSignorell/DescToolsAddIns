
# insert_in <- function() {
#   rstudioapi::insertText(" %in% ", location = )
# }


Str <- function(){

  requireNamespace("DescTools")

  sel <- getActiveDocumentContext()$selection[[1]]$text
  if(sel != "") {
    rstudioapi::sendToConsole(gettextf("DescTools::Str(%s)", sel), focus = FALSE)
  } else {
    cat("No selection!\n")
  }
}

Example <- function(){
  sel <- getActiveDocumentContext()$selection[[1]]$text
  if(sel != "") {
    rstudioapi::sendToConsole(gettextf("example(%s)", sel), focus = FALSE)
  } else {
    cat("No selection!\n")
  }
}


Abstract <- function(){

  requireNamespace("DescTools")

  sel <- getActiveDocumentContext()$selection[[1]]$text
  if(sel != "") {
    rstudioapi::sendToConsole(gettextf("DescTools::Abstract(%s)", sel), focus = FALSE)
  } else {
    cat("No selection!\n")
  }
}

Summary <- function(){
  sel <- getActiveDocumentContext()$selection[[1]]$text
  if(sel != "") {
    rstudioapi::sendToConsole(gettextf("summary(%s)", sel), focus = FALSE)
  } else {
    cat("No selection!\n")
  }
}


Edit <- function(){
  sel <- getActiveDocumentContext()$selection[[1]]$text
  if(sel != "") {
    rstudioapi::sendToConsole(gettextf("fix(%s)", sel), focus = FALSE)
  } else {
    cat("No selection!\n")
  }
}


Unclass <- function(){
  sel <- getActiveDocumentContext()$selection[[1]]$text
  if(sel != "") {
    rstudioapi::sendToConsole(gettextf("unclass(%s)", sel), focus = FALSE)
  } else {
    cat("No selection!\n")
  }
}



Desc <- function(){

  requireNamespace("DescTools")

  sel <- getActiveDocumentContext()$selection[[1]]$text
  if(sel != "") {
    rstudioapi::sendToConsole(gettextf("DescTools::Desc(%s)", sel), focus = FALSE)
  } else {
    cat("No selection!\n")
  }
}



Select <- function(){

  sel <- getActiveDocumentContext()$selection[[1]]$text
  if(sel != "") {
    if(sel == "pch") {
      if(!exists("pch"))
        PlotPch(newwin = TRUE)
    } else if(sel=="col"){
        txt <- eval(parse(text="ColPicker(newwin=TRUE)"))
        dev.off()
        rstudioapi::insertText(gettextf("col=c(%s)", paste(shQuote(txt), collapse=", ")))

    } else if(sel %in% c("fn", "file")) {
      txt <- eval(parse(text="FileOpenDlg(fmt='%path%%fname%.%ext%')"))
      if(txt != "")
        rstudioapi::insertText(gettextf("%s=%s", sel, shQuote(txt)))

    } else if(sel %in% c("path","dir")) {
      txt <- eval(parse(text="dir.choose()"))
      if(txt != "")
        rstudioapi::insertText(gettextf("%s=%s", sel, shQuote(txt)))

    } else {
      if(sel != ""){
        txt <- eval(parse(text=gettextf("SelectVarDlg(%s)", sel)))
        if(txt != "") rstudioapi::insertText(txt)
      }
    }
  } else {
    cat("No selection!\n")
  }

}


BuildModel <- function(){

  sel <- getActiveDocumentContext()$selection[[1]]$text
  if(sel != ""){
    txt <- eval(parse(text=gettextf("ModelDlg(%s)", sel)))
    rstudioapi::insertText(txt)
  } else {
    cat("No selection!\n")
  }

}




Plot <- function(){
  sel <- getActiveDocumentContext()$selection[[1]]$text
  if(sel != "") {
    if(sel=="mar")
      rstudioapi::sendToConsole("PlotMar()", focus = FALSE)
    else
      rstudioapi::sendToConsole(gettextf("plot(%s)", sel), focus = FALSE)
  } else {
    cat("No selection!\n")
  }
}


PlotD <- function(){

  requireNamespace("DescTools")

  sel <- getActiveDocumentContext()$selection[[1]]$text
  if(sel != "") {
    rstudioapi::sendToConsole(gettextf("plot(DescTools::Desc(%s))", sel), focus = FALSE)
  } else {
    cat("No selection!\n")
  }
}


Head <- function(){
  sel <- getActiveDocumentContext()$selection[[1]]$text
  if(sel != ""){
    rstudioapi::sendToConsole(gettextf("head(%s)", sel), execute = TRUE, focus = FALSE)
  } else {
    cat("No selection!\n")
  }

}




Info <- function(){

  .Info <- function(x){

    class_x <- strwrap(paste(class(x), collapse=", "),
                       width= getOption("width") - nchar("  Class(es):   "))
    class_x[-1] <- paste(strrep(" ", nchar("  Class(es):  ")), class_x[-1])

    cat(gettextf("Properties -------- \n  Object:      %s\n  TypeOf:      %s\n  Class(es):   %s\n  Mode:        %s\n  Dimension:   %s\n  Length:      %s\n  Size:        %s\n  Attributes:  ",
                 deparse(substitute(x)), typeof(x),
                 paste(class_x, collapse="\n"),
                 mode(x),
                 ifelse(is.null(dim(x)), "NULL", toString(dim(x))), length(x),
                 paste0(Format(as.numeric(object.size(x)), fmt="engabb",  digits=1), "B")
                 ))
    if(!is.null(attributes(x))) {
      cat("\n")
      opt <- options(width=getOption("width") - 4)
      cat(paste("    ", capture.output(attributes(x))), sep="\n")
      options(opt)
    } else
      cat("none\n\n")
  }

  sel <- getActiveDocumentContext()$selection[[1]]$text

  if(sel != ""){
#    rstudioapi::sendToConsole(gettextf(".Info(%s)", sel), execute = TRUE, focus = FALSE)
    eval(parse(text = gettextf(".Info(%s)", sel)))

  } else {
    cat("No selection!\n")
  }

}


Some <- function(){

  requireNamespace("DescTools")

  sel <- getActiveDocumentContext()$selection[[1]]$text
  if(sel != ""){
    rstudioapi::sendToConsole(gettextf("DescTools::Some(%s)", sel), execute = TRUE, focus = FALSE)
  } else {
    cat("No selection!\n")
  }

}


# Save <- function(){
#   sel <- getActiveDocumentContext()$selection[[1]]$text
#
#   if(sel != "") {
#     f <- tclvalue(eval(parse(text=gettextf("tkgetSaveFile(initialfile='%s.rda', title='Save a file...')", sel))))
#     if(f != "")
#       rstudioapi::sendToConsole(gettextf("save(x=%s, file='%s')", sel, f), focus = FALSE)
#   } else {
#     cat("No selection!\n")
#   }
#
# }


FileOpen <- function(){

  txt <- eval(parse(text="FileOpenDlg(fmt=NULL)"))
  if(txt != "") {
    rstudioapi::insertText(txt)
  }
}


FileBrowserOpen <- function(){
  
  sel <- getActiveDocumentContext()$selection[[1]]$text
  if(sel != ""){
    
    path <- eval(parse(text=sel)) # should we do some cleansing here?
    
    si <- Sys.info()["sysname"]
    
    if (si == "Darwin") {
      # mac
      system2("open", path)
      
    } else if (si == "Windows") {
      # win
      shell.exec(path)
      
    } else if (si == "Linux") {
      # linux
      system(paste0("xdg-open ", path))
      
    } else {
      stop("Open browser is not implemented for your system (",
           si, ") in this package (due to incompetence of the author).")
    }

  } else {
    cat("No selection!\n")
  }
  
}



FileImport <- function(){

  txt <- eval(parse(text="FileImportDlg()"))
  if(txt != "") {
    rstudioapi::insertText(txt)
  }
}


FileSaveAs <- function() {

  sel <- getActiveDocumentContext()$selection[[1]]$text
  if (sel != "") {

    f <- tclvalue(eval(parse(text = gettextf("tkgetSaveFile(initialfile='%s', title='Save a file...', filetypes = '{{R (binary)} {.rda}} {{Comma separated} {.csv}} {Text {.txt}} {{Excel} {*.xlsx} }', defaultextension = '.rda')",
                                             sel))))
    if (f != "") {
      ext <- tools::file_ext(f)

      if(ext=="rda"){
        rstudioapi::sendToConsole(gettextf("save(x=%s, file='%s')",
                                           sel, f), focus = FALSE)
      } else if(ext=="csv"){
        rstudioapi::sendToConsole(gettextf("write.csv(x=%s, file='%s')",
                                           sel, f), focus = FALSE)

      } else if(ext=="xlsx"){

        requireNamespace("writexl")
        rstudioapi::sendToConsole(gettextf("writexl::write_xlsx(x=%s, path='%s')",
                                           sel, f), focus = FALSE)

      } else if(ext=="txt"){
        if(eval(parse(text = gettextf("inherits(%s, 'character')", sel)))){
          rstudioapi::sendToConsole(gettextf("writeLines(text=%s, con='%s')", sel, f), focus = FALSE)

        } else {
          rstudioapi::sendToConsole(gettextf("dput(x=%s, file='%s')", sel, f), focus = FALSE)

        }
      }
    }

  } else {
    cat("No selection!\n")
  }
}



XLView <- function(){

  requireNamespace("DescTools")

  sel <- getActiveDocumentContext()$selection[[1]]$text
  if(sel != "") {
    rstudioapi::sendToConsole(gettextf("DescTools::XLView(%s)", sel), focus = FALSE)
  } else {
    cat("No selection!\n")
  }
}


ToWrd <- function(){
  
  requireNamespace("DescTools")
  
  sel <- getActiveDocumentContext()$selection[[1]]$text
  if(sel != "") {
    if(sel=="\n") sel <- "'\n'"
    rstudioapi::sendToConsole(gettextf("DescTools::ToWrd(%s)", sel), focus = FALSE)
  } else {
    cat("No selection!\n")
  }
}




IntView <- function(){
  sel <- getActiveDocumentContext()$selection[[1]]$text
  if(sel != "") {
    rstudioapi::sendToConsole(gettextf("View(%s)", sel), focus = FALSE)
  } else {
    cat("No selection!\n")
  }
}



FlipBackSlash <- function() {
  txt <- getActiveDocumentContext()$selection[[1]]$text
  if(txt != "") {
    txt <- gsub("\\\\", "/", txt)
    # replace double // by /
    txt <- gsub("/+", "/", txt)
    rstudioapi::modifyRange(txt)
  } else {
    cat("No selection!\n")
  }

}


FlipSlash <- function() {
  txt <- getActiveDocumentContext()$selection[[1]]$text
  if(txt != "") {
    txt <- gsub("/", "\\\\", txt)
    rstudioapi::modifyRange(txt)
  } else {
    cat("No selection!\n")
  }
  
}



SetArrow <- function(){

  requireNamespace("DescTools")

  xy <- eval(parse(text="locator(n = 2)"))
  eval(parse(text="Arrow(x0 = xy$x[2], y0 = xy$y[2], x1 = xy$x[1], y1 = xy$y[1], head=3)"))
  txt <- gettextf("Arrow(x0 = %s, y0 = %s, x1 = %s, y1 = %s, head = 3)\n",
                  round(xy$x[2],2), round(xy$y[2],2), round(xy$x[1],2), round(xy$y[1],2))
  rstudioapi::modifyRange(txt)
}



Enquote <- function(){
  
  rng <- getActiveDocumentContext()
  txt <- getActiveDocumentContext()$selection[[1]]$text
  if(txt != "") {
    txt <- paste(shQuote(strsplit(txt, split="\n")[[1]]), collapse=",")
    
    # store selection
    sel <- rng$selection[[1]]$range
    
    # insert the text
    rstudioapi::modifyRange(txt)
    
    # select inserted text
    nsel <- getActiveDocumentContext()$selection[[1]]$range
    sel$end <- nsel$start
    rstudioapi::setSelectionRanges(sel)

  } else {
    cat("No selection!\n")
  }


}



EnquoteS <- function(){

  rng <- getActiveDocumentContext()
  txt <- getActiveDocumentContext()$selection[[1]]$text
  if(txt != "") {
    txt <- paste(sQuote(strsplit(txt, split="\n")[[1]]), collapse=",")

    # store selection
    sel <- rng$selection[[1]]$range
    
    # insert the text
    rstudioapi::modifyRange(txt)

    # select inserted text
    nsel <- getActiveDocumentContext()$selection[[1]]$range
    sel$end <- nsel$start
    rstudioapi::setSelectionRanges(sel)
    
  } else {
    cat("No selection!\n")
  }


}



EvalEnquote <- function(){

  txt <- getActiveDocumentContext()$selection[[1]]$text
  if(txt != "") {

    txt <- eval(parse(text=txt))

    txt <- paste(shQuote(txt), collapse=",")
    rstudioapi::modifyRange(txt)

  } else {
    cat("No selection!\n")
  }


}



SortAsc <- function(){
  
  rng <- getActiveDocumentContext()
  txt <- getActiveDocumentContext()$selection[[1]]$text

  if(txt != "") {
    rep_txt <- paste(sort(strsplit(txt, split="\n")[[1]]), collapse="\n")
    if(length(grep("\\n$", txt))!=0)
      rep_txt <- paste0(rep_txt, "\n")
    
    rstudioapi::modifyRange(rep_txt)
    rstudioapi::setSelectionRanges(rng$selection[[1]]$range)
    
  } else {
    cat("No selection!\n")
  }
  
  
}


SortDesc <- function(){
  rng <- getActiveDocumentContext()
  txt <- getActiveDocumentContext()$selection[[1]]$text
  
  if(txt != "") {
    rep_txt <- paste(sort(strsplit(txt, split="\n")[[1]], decreasing = TRUE), collapse="\n")
    if(length(grep("\\n$", txt))!=0)
      rep_txt <- paste0(rep_txt, "\n")
    rstudioapi::modifyRange(rep_txt)
    rstudioapi::setSelectionRanges(rng$selection[[1]]$range)
    
  } else {
    cat("No selection!\n")
  }
  
  
}



RemoveDuplicates <- function () {
  
  rng <- getActiveDocumentContext()
  txt <- getActiveDocumentContext()$selection[[1]]$text
  if (txt != "") {
    
    txt <- strsplit(txt, split = "\n")[[1]]
    u <- unique(txt)
    utxt <- paste(u, collapse = "\n")
    # add the last cr if the original already had it
    if(length(grep("\\n$", txt))!=0)
      utxt <- paste0(utxt, "\n")
    
    rstudioapi::modifyRange(utxt)
    
    sel <- rng$selection[[1]]$range
    sel$end[1] <- sel$end[1] - (length(txt) - length(u))
    
    rstudioapi::setSelectionRanges(sel)
    
    note <- gettextf("\033[36m\nNote: ------\n  %s duplicates have been found and removed. %s values remain.\n\n\033[39m", 
                    length(txt) - length(u), length(u)) 
    cat(note)
    
  }
  else {
    cat("No selection!\n")
  }
}





NewObject <- function(){

  obj <- getActiveDocumentContext()$selection[[1]]$text
  if(obj == "") obj <- "m"

  m <- edit(data.frame())

  switch(obj,
     "m" = {
        m <- as.matrix(m)

        if(!all(dimnames(m)[[2]] == paste("var", 1:length(dimnames(m)[[2]]), sep="")))
          dnames <- gettextf(", \n       dimnames=list(%s)", toString(dimnames(m)))
        else
          dnames <- ""

        if(!is.numeric(m))
          m[!is.na(m)] <- shQuote(m[!is.na(m)])

        txt <- gettextf("m <- matrix(c(%s), nrow=%s%s)\n",
                        toString(m), dim(m)[1], dnames)
      },
     "c"={
       m <- as.vector(m)
       txt <- gettextf("v <- %s\n", toString(m))

     },
     "d"={
       txt <- paste("d <- data.frame(", paste(names(m), "=", m, collapse = ", "), ")\n", sep="")
       # genuine data.frame
     }
  )

  rstudioapi::insertText(txt)

}



InspectPnt <- function(){

  requireNamespace("DescTools")

  .ToClipboard <- function (x, ...) {

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


  sel <- getActiveDocumentContext()$selection[[1]]$text

  if(sel != ""){
    i <- eval(parse(text=gettextf("DescTools::IdentifyA(%s, poly=TRUE)", sel)))

    .ToClipboard(paste("c(", paste(i, collapse=","), ")", sep=""))

    # Todo:
    # Display directly by looking up the data in the formula
    # View(mtcars[i,])

  } else {
    cat("No selection!\n")
  }



}



GetExcelRange <- function(env=.GlobalEnv){

  requireNamespace("DescTools")
  
  # we need to declare the variable here to avoid the barking of the check
  rng <- data.frame()   
  eval(parse(text="rng <- DescTools::XLGetRange()"))
  
  txt <- getActiveDocumentContext()$selection[[1]]$text
  if(txt != "") {
    # remove any assignment
    txt <- StrTrim(gsub("<- *$|= *$", "", txt))
    # assign the imported data to the selected name in GlobalEnv
    assign(txt, rng, envir = env)
    # add the assignment to the selected name
    txt <- paste(txt, "<-")
  }
  txt <- paste(txt, attr(rng, "call"), "\n")
  rstudioapi::modifyRange(txt)

  print(rng)

}



FlushToSource <- function(){

  sel <- getActiveDocumentContext()$selection[[1]]$text
  if(sel != ""){
    txt <- capture.output(eval(parse(text=gettextf("dput(%s)", sel))))
    rstudioapi::insertText(paste(sel, "<-", paste(txt, collapse="\n"), "\n"))
  } else {
    cat("No selection!\n")
  }

}



ToWrdWithBookmark <- function(){
  
  requireNamespace("DescTools")
  
  sel <- getActiveDocumentContext()$selection[[1]]$text
  if(sel != "") {
    bm <- eval(parse(text=gettextf("bm <- DescTools::ToWrdB({%s})", sel)))
    rstudioapi::modifyRange(gettextf("## BookmarkName: %s\n{\n%s}\n", bm$name(), sel))
    
  } else {
    cat("No selection!\n")
  }
}



ToWrdPlotWithBookmark <- function(){
  
  requireNamespace("DescTools")
  
  sel <- getActiveDocumentContext()$selection[[1]]$text
  if(sel != "") {
    bm <- eval(parse(text=gettextf("bm <- DescTools::ToWrdPlot(%s)", sQuote(gettextf("{%s}", sel)))))
    rstudioapi::modifyRange(gettextf("## BookmarkName: %s (width=15)\n{\n%s}\n", 
                                     bm$bookmark$name(), sel))
    
  } else {
    cat("No selection!\n")
  }
}



CreateBookmark <- function(){
  
  requireNamespace("DescTools")
  
  sel <- getActiveDocumentContext()$selection[[1]]$text
  if(sel != "") {
    if(sel=="\n") sel <- "'\n'"
    rstudioapi::sendToConsole(gettextf("DescTools::WrdInsertBookmark(name='%s')", sel), focus = FALSE)
  } else {
    cat("No selection!\n")
  }
}



UpdateBookmark <- function(){
  
  requireNamespace("DescTools")
  
  sel <- getActiveDocumentContext()$selection[[1]]$text
  
  if(sel != "") {
    
    # expected structure of the code (args are optional)
    # ## Bookmark: <bookmarkname> (<args>) { <code> }
    # the bookmarkname must consist of bm(p|t)000000000, p standing for plot, t for text
    # the type of the bookmark must be visible in the name, as 
    # updatebookmark gets nothing else...
    
    # first separate the bookmark name between : and { of the selected text
    bm <- StrTrim(regmatches(sel, gregexpr("(?s)(?<=:).*?(?=\\{)", sel, perl=TRUE)))
    
    # split name from args and get the bookmark type
    # greedy to the last )
    args <- regmatches(bm, gregexpr("(?<=\\().*(?=\\))", bm, perl=TRUE))[[1]]
    if(length(args) > 0) args <- paste(",", args) else args <- ""
    
    bm <- gsub(" .*", "", bm)   # take first word only as name
    bmtype <- substr(bm, 1, 3)
    
    # get the commands between the brackets
    code <- regmatches(sel, gregexpr("(?s)(?<=\\{).*(?=\\})", sel, perl=TRUE))[[1]]
    
    if(!is.null(DescTools::WrdBookmark(bookmark = bm))){
      
      DescTools::WrdGoto(name = bm)
      wrd <- DescTools::DescToolsOptions("lastWord")
      wrd[["Selection"]]$delete()
      
      if(bmtype=="bmt") {         # text bookmark
        eval(parse(text=gettextf('DescTools::ToWrdB({%s}, bookmark="%s" %s)', code, bm, args)))
        
      } else if(bmtype=="bmp") {  # plot bookmark
        eval(parse(text=gettextf("DescTools::ToWrdPlot(%s, bookmark='%s' %s)", sQuote(gettextf("{%s}", code)), bm, args)))
        
      } else {
        warning("unknown bookmark type")
      }
      
    } else {
      warning(gettextf("bookmark %s not found", bm))
      
    }
    
    
    
  } else {
    cat("No selection!\n")
  }
  
}




# ToDo:
# Make Addin UpdateAllBookmarks() to update all bookmark sections





