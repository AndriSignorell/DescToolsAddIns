

# ToDo:
# Make Addin UpdateAllBookmarks() to update all bookmark sections


SelectDlgBookmark <- function(x, ...){
  
  # should we use GetCurrWrd() here??
  wrd <- DescTools::DescToolsOptions("lastWord")
  
  wbms <- wrd[["ActiveDocument"]][["Bookmarks"]]
  if (wbms$count() > 0) {
    bmnames <- sapply(seq(wbms$count()), function(i) wbms[[i]]$name())
  }
  
  sel <- SelectVarDlg.default(x = bmnames, ...)
  
  # sel comes as c("bm1")
  WrdGoto(name = eval(parse(text = sel))[1])
  
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



.ParseBookmark <- function(sel){
  
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
  
  return(list(name=bm, type=bmtype, args=args, code=code))  
  
}


.UpdateBookmark <- function(bm, wrd=DescTools::DescToolsOptions("lastWord")){
  
  with(bm, {
    
    DescTools::WrdGoto(name = name)
    wrd[["Selection"]]$delete()
    
    if(type=="bmt") {         # text bookmark
      eval(parse(text=gettextf('DescTools::ToWrdB({%s}, bookmark="%s" %s)', code, name, args)))
      
    } else if(type=="bmp") {  # plot bookmark
      eval(parse(text=gettextf("DescTools::ToWrdPlot(%s, bookmark='%s' %s)", sQuote(gettextf("{%s}", code)), name, args)))
      
    } else {
      warning(gettextf("unknown bookmark type %s", type)) # warning("unknown bookmark type")
    }
  }) 
  
  invisible()
  
}


UpdateBookmark <- function(){
  
  requireNamespace("DescTools")
  
  sel <- getActiveDocumentContext()$selection[[1]]$text
  
  if(sel != "") {
    
    bm <- .ParseBookmark(sel)
    
    with(bm,    
         if(!is.null(DescTools::WrdBookmark(name = name))){
           .UpdateBookmark(bm)
           
         } else {
           warning(gettextf("bookmark %s not found", name))
         }
    )    
    
  } else {
    cat("No selection!\n")
  }
  
}


SelectBookmark <- function() {
  
  requireNamespace("DescTools")
  
  sel <- getActiveDocumentContext()$selection[[1]]$text
  if(sel != "") {
    if(sel=="\n") sel <- "'\n'"
    rstudioapi::sendToConsole(gettextf("DescTools::WrdGoto(name='%s')", sel), 
                              focus = FALSE)
  } else {
    cat("No selection!\n")
  }
  
}


DeleteBookmark <- function() {
  requireNamespace("DescTools")
  
  sel <- getActiveDocumentContext()$selection[[1]]$text
  if(sel != "") {
    if(sel=="\n") sel <- "'\n'"
    rstudioapi::sendToConsole(gettextf("DescTools::WrdDeleteBookmark(name='%s')", sel), focus = FALSE)
  } else {
    cat("No selection!\n")
  }
  
}



RenameBookmark <- function(name, newname, wrd = DescToolsOptions("lastWord")) {
  
  requireNamespace("DescTools")
  
  wrdBookmarks <- wrd[["ActiveDocument"]][["Bookmarks"]]
  
  if (wrdBookmarks$exists(name)) {
    bm <- DescTools::WrdBookmark(name) 
    bm[["name"]] <- newname
    res <- TRUE
  } else {
    warning(gettextf("Bookmark %s does not exist, so there's nothing to delete", 
                     name))
    res <- FALSE
  }
  return(res)
  
}





RecreateBookmarkChunk <- function(){
  
  requireNamespace("DescTools")
  
  sel <- getActiveDocumentContext()$selection[[1]]$text
  
  if(sel != "") {
    
    bm <- .ParseBookmark(sel)
    
    # create bookmark at current position if it's not already there
    if(is.null(WrdBookmark(bm$name))){
      # create new bookmark with the name in bm
      if(sel=="\n") sel <- "'\n'"
        DescTools::WrdInsertBookmark(name=bm$name)
      # rstudioapi::sendToConsole(gettextf("DescTools::WrdInsertBookmark(name='%s')", bm$name), 
      #                           focus = FALSE)
    }
    
    .UpdateBookmark(bm)
    
  } else {
    cat("No selection!\n")
  }
  
}



WrdBookmarks <- function() {
  
  wrd <- DescToolsOptions("lastWord")
  
  wdoc <- wrd[["ActiveDocument"]]
  wbms <- wrd[["ActiveDocument"]][["Bookmarks"]]
  
  bmtype <- function(x){
    if(StrLeft(x, 3) == "bmt")
      "text"
    else if(StrLeft(x, 3) == "bmp")
      "plot"
    else 
      "other"
  }
  
  if(wbms$count() > 0){
    
    # Names in order of their appearing in the document 
    lst <- list()
    for(i in seq(wbms$count())){
      
      bm <- wdoc$range()$bookmarks(i)
      
      lst[[i]] <- data.frame(name=bm$name(),
                             pagenr=bm[["range"]]$information(DescTools::wdConst$wdActiveEndAdjustedPageNumber), 
                             id=bm[["range"]]$bookmarkid()
      )
    }
    
    bms <- do.call(rbind, lst)
    bms$type <- sapply(bms$name, bmtype)
    bms <- bms[, c("id", "name", "pagenr", "type")]
    
  } else {
    bms <- NA
  }
  
  return(bms)
  
}





