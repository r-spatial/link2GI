#'@title Get OTB modules 
#'@name parseOTBAlgorithms
#'@description retrieve the OTB module folder content and parses the module names
#'@param gili optional gis linkage as done by `linkOTB()``
#'@export parseOTBAlgorithms
#'
#'@examples
#' \dontrun{
#'  ## link to the OTB binaries
#'    otbLinks<-link2GI::linkOTB()
#'    path_OTB<-otbLinks$pathOTB
#'  ##p arse all modules
#' algo<-parseOTBAlgorithms(gili = otbLinks)
#' 
#' ##print the list
#' print(algo)
#' } 
#' 
parseOTBAlgorithms<- function(gili=NULL) {
  if (is.null(gili)) {
    otb<-link2GI::linkOTB()
    path_OTB<- otb$pathOTB
  } else path_OTB<- gili$pathOTB
  
  if (substr(path_OTB,nchar(path_OTB) - 1,nchar(path_OTB)) == "n/")   path_OTB <- substr(path_OTB,1,nchar(path_OTB)-1)
    
  algorithms <-list.files(pattern="otbcli", path=path_OTB, full.names=FALSE)
  algorithms <- substr(algorithms,8,nchar(algorithms)) 
  return(algorithms)
}



#'@title Get OTB function calls
#'@name parseOTBFunction
#'@description retrieve the choosen function and parses all arguments with the defaults
#'@param algos number of the algorithm as provided by `getOTBAlgorithm`
#'@param gili optional gis linkage as done by `linkOTB()`
#'@export parseOTBFunction
#'
#'@examples
#' \dontrun{
## link to the OTB binaries
#' otbLinks<-link2GI::linkOTB()
#' if (otbLinks$exist) {
#' 
#' path_OTB<-otbLinks$pathOTB
#' 
#' 
#' ## parse all modules
#' algo<-parseOTBAlgorithms(gili = otbLinks)
#' 
#' 
#' ## take edge detection
#' otb_algorithm<-algo[27]
#' algo_cmd<-parseOTBFunction(algo = otb_algorithm,gili = otbLinks)
#' ## print the current command
#' print(algo_cmd)
#' }
#' 
#' ###########
#' ### usecase
#' ###########
#' 
#' ## link to OTB
#' otblink<-link2GI::linkOTB()
#' path_OTB<-otblink$pathOTB
#' 
#' ## get data
#' setwd(tempdir())
#' ## get some typical data as provided by the authority
#' url<-"http://www.ldbv.bayern.de/file/zip/5619/DOP%2040_CIR.zip"
#' res <- curl::curl_download(url, "testdata.zip")
#' unzip(res,junkpaths = TRUE,overwrite = TRUE)
#' 
#' ## get all available OTB modules
#' algo<-parseOTBAlgorithms(gili = otblink)
#' 
#' ## for the example we use the edge detection, 
#' ## because of the windows call via a batch file 
#' ## we have to distinguish the module name
#' ifelse(Sys.info()["sysname"]=="Windows", 
#' algo_keyword<- "EdgeExtraction.bat",
#' algo_keyword<- "EdgeExtraction")
#' 
#' # write it to a variable
#' otb_algorithm<-algo[algo[]==algo_keyword]
#' # now create the command list
#' algo_cmd<-parseOTBFunction(algo = otb_algorithm,gili = otblink)
#' 
#' ## define the current run arguments
#' algo_cmd$`-in`<- file.path(getwd(),"4490600_5321400.tif")
#' algo_cmd$`-filter`<- "sobel"
#' 
#' ## create out name
#' outName<-paste0(getwd(),"/out",algo_cmd$`-filter`,".tif")
#' algo_cmd$`-out`<- outName
#' 
#' ## generate full command
#' command<-paste(paste0(path_OTB,"otbcli_",otb_algorithm," "),
#'                paste(names(algo_cmd),algo_cmd,collapse = " "))
#' 
#' ## make the system call
#' system(command,intern = TRUE)
#' 
#' ##create raster
#' retStack<-assign(outName,raster::raster(outName))
#' 
#' ## plot raster
#' raster::plot(retStack)
#' } 
#' ##+##
parseOTBFunction <- function(algos=NULL,gili=NULL) {
  if (is.null(gili)) {
    otb<-link2GI::linkOTB()
    path_OTB<- otb$pathOTB
  } else path_OTB<- gili$pathOTB
  ocmd<-tmp<-list()
  otbcmd <- list()
  otbhelp <- list()
  
  otbtype <- list()
  
  for (algo in algos){
    if (algo != ""){
      system("rm otb_module_dump.txt",intern = FALSE,ignore.stderr = TRUE)
      ifelse(Sys.info()["sysname"]=="Windows",
             system(paste0(path_OTB,"otbcli_",paste0(algo," -help >> otb_module_dump.txt 2>&1"))), 
             system2(paste0(path_OTB,"otbcli"),paste0(algo," -help >> otb_module_dump.txt 2>&1"))
             )
      
      
      
         txt<-readLines("otb_module_dump.txt")
         file.remove("otb_module_dump.txt")
      # Pull out the appropriate line
      args <- txt[grep("-", txt)]
      # obviously the format has changed. TODO
      #if (Sys.info()["sysname"]=="Linux") args <- args[-grep("http",args)]
      
      # Delete unwanted characters in the lines we pulled out
      args <- gsub("MISSING", "     ", args, fixed = TRUE)
      args <- gsub("\t", "     ", args, fixed = TRUE)
      args <- gsub(" <", "     <", args, fixed = TRUE)
      args <- gsub("> ", ">     ", args, fixed = TRUE)
      args <- gsub("          ", "   ", args, fixed = TRUE)
      args <- gsub("         ", "   ", args, fixed = TRUE)
      args <- gsub("        ", "   ", args, fixed = TRUE)
      args <- gsub("       ", "   ", args, fixed = TRUE)
      args <- gsub("      ", "   ", args, fixed = TRUE)
      args <- gsub("     ", "   ", args, fixed = TRUE)
      args <- gsub("    ", "   ", args, fixed = TRUE)
      args <- gsub("   ", "   ", args, fixed = TRUE)
      args<-strsplit(args,split ="   ")
      
      param<-list()

      
      otbcmd[[algo]] <- sapply(args, "[", 2)[[1]]
      #otbtype[[algo]] <- sapply(args, "[", 2:4)
      otbhelp[[algo]] <- sapply(args, "[", 2:4)
      for (j in 1:(length(args)-1)){
        drop<-FALSE
        default<-""
        extractit <-FALSE
        ltmp<-length(grep("default value is",sapply(args, "[", 4)[[j]])) 
        if(ltmp>0) extractit=TRUE
        if (extractit)  {

          tmp<-strsplit(sapply(args, "[", 4)[[j]],split ="default value is ")[[1]][2]
          tmp <-strsplit(tmp,split =")")[[1]][1]
          cat("iwas")
          default <- tmp
        }
        else if (length(grep("(OTB-Team)",args[[j]])) > 0) {drop <- TRUE}
        else if (length(grep("(-help)",args[[j]])) > 0) {drop <- TRUE}
        else if (length(grep("(otbcli_)",args[[j]])) > 0) {drop <- TRUE}
        else if (length(grep("(-inxml)",args[[j]])) > 0) {drop <- TRUE}
        else if (length(grep("(mandatory)",sapply(args, "[", 4)[[j]])) > 0) {default <- "mandatory"}
        else if  (sapply(args, "[", 4)[[j]] == "Report progress " & !is.na(sapply(args, "[", 4)[[j]] == "Report progress ")) {
          default <- "false"}
       else {default < sapply(args, "[", 4)[[j]]}

        if (!drop &default  != "") param[[paste0(sapply(args, "[", 2)[[j]])]] <- default
        
      }

      if (length(ocmd) > 0)
        ocmd[[algo]]<- append(otbcmd,assign(algo, as.character(param)))
      else
        ocmd<-param
      #params <- get_args_man(alg = "otb:localstatisticextraction")
      
    }
  }
  return(ocmd)
}



