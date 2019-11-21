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



#'@title Get OTB function argument list
#'@name parseOTBFunction
#'@description retrieve the choosen function and parses all arguments with the defaults
#'@param algo number or name of the algorithm as provided by `getOTBAlgorithm`
#'@param gili optional gis linkage as done by `linkOTB()`
#'@export parseOTBFunction
#'
#'@examples

#' \dontrun{
## link to the OTB binaries
#' otbLink<-link2GI::linkOTB()
#' if (otbLink$exist) {
#' 
#' ## parse all modules
#' algos<-parseOTBAlgorithms(gili = otbLink)
#' 
#' 
#' ## take edge detection
#' otb_algorithm<-algos[27]
#' algo_cmd<-parseOTBFunction(algo = otb_algorithm,gili = otbLink)
#' ## print the current command
#' print(algo_cmd)
#' }
#' }

#' ##+##
parseOTBFunction <- function(algo=NULL,gili=NULL) {
  if (is.null(gili)) {
    otb<-link2GI::linkOTB()
    path_OTB<- otb$pathOTB
  } else path_OTB<- gili$pathOTB
  
  ocmd<-tmp<-list()
  otbcmd <- list()
  otbhelp <- list()
  
  otbtype <- list()

  
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
          #cat("iwas")
          default <- tmp
        }
        else if (length(grep("(OTB-Team)",args[[j]])) > 0) {drop <- TRUE}
        else if (length(grep("(-help)",args[[j]])) > 0) {drop <- TRUE}
        else if (length(grep("(otbcli_)",args[[j]])) > 0) {drop <- TRUE}
        else if (length(grep("(-inxml)",args[[j]])) > 0) {drop <- TRUE}
        else if (length(grep("(mandatory)",sapply(args, "[", 4)[[j]])) > 0) {default <- "mandatory"}
        else if  (sapply(args, "[", 4)[[j]] == "Report progress " & !is.na(sapply(args, "[", 4)[[j]] == "Report progress ")) {
          default <- "false"}
       else {
         
         default < sapply(args, "[", 4)[[j]]}

        if (!drop &default  != "") {
          arg<-sapply(args, "[", 2)[[j]]
          if (arg == "-in") arg<-"-input"
          param[[paste0(substr(arg,2,nchar(arg)))]] <- default
        }
      }

      if (length(ocmd) > 0)
        ocmd[[algo]]<- append(otbcmd,assign(algo, as.character(param)))
      else
        ocmd<-R.utils::insert(param,1,algo)
      #params <- get_args_man(alg = "otb:localstatisticextraction")
      
    } else {print("no valid algorithm provided")}
    
  
  return(ocmd)
}


#' Execute the OTB command list via system call
#'@description Wrapper function which paste the OTB command list into a system call compatible string and execute this command. 
#'@param otbCmdList the OTB algorithm parameter list
#'@param gili optional gis linkage as done by `linkOTB()`
#'@param quiet boolean  switch for supressing messages default is TRUE
#'@param retRaster boolean if TRUE a raster stack is returned


#'@examples



#' \dontrun{
#' ## link to OTB
#' otblink<-link2GI::linkOTB()
#' 
#' ## get data
#' setwd(tempdir())
#' ## get some typical data as provided by the authority
#' url<-'http://www.ldbv.bayern.de/file/zip/5619/DOP%2040_CIR.zip'
#' res <- curl::curl_download(url, "testdata.zip")
#' unzip(res,junkpaths = TRUE,overwrite = TRUE)
#' 
#' ## for the example we use the edge detection, 
#' algoKeyword<- "EdgeExtraction"
#'
#' ## extract the command list for the choosen algorithm 
#' cmd<-parseOTBFunction(algo = algoKeyword, gili = otblink)
#' 
#' 
#' ## define the mandantory arguments all other will be default
#' cmd$input  <- file.path(getwd(),"4490600_5321400.tif")
#' cmd$filter <- "touzi"
#' cmd$out <- paste0(getwd(),"/out",cmd$filter,".tif")
#' 
#' ## run algorithm
#' retStack<-runOTB(cmd,gili = otblink)
#' 
#' ## plot raster
#' raster::plot(retStack)
#' }



#'@export
runOTB <- function(otbCmdList=NULL,
                  gili=NULL,
                  retRaster=TRUE,
                  quiet = TRUE
                  
                  ){
  
  
  if (is.null(gili)) {
    otb<-link2GI::linkOTB()
    path_OTB<- otb$pathOTB
  } else path_OTB<- gili$pathOTB

otb_algorithm<-otbCmdList[1]  
otbCmdList[1]<-NULL

if(Sys.info()["sysname"]=="Windows") otb_algorithm <- paste0(otb_algorithm,".bat")
  
if (names(otbCmdList)[1] =="input")  names(otbCmdList)[1]<-"in"

        command<-paste(paste0(path_OTB,"otbcli_",otb_algorithm," "),
                 paste0("-",names(otbCmdList)," ",otbCmdList,collapse = " "))
        
        
          if (quiet){
            system(command,ignore.stdout = TRUE,ignore.stderr = TRUE,intern = FALSE)
            if (retRaster){
              rStack <- assign(otbCmdList$out,raster::stack(otbCmdList$out))
              return(rStack)
            }
              
              
          }
          else {
            system(command,ignore.stdout = FALSE,ignore.stderr = FALSE,intern = TRUE)
            if (retRaster){
              r<-assign(otbCmdList$out,raster::stack(otbCmdList$out))
              return(rStack)
            }
              
            }
          }

