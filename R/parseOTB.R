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
#'  ## link to the OTB binaries
#'  otbLinks<-link2GI::linkOTB()
#'  path_OTB<-otbLinks$pathOTB
#'  
#'  ## parse all modules
#'  algo<-parseOTBAlgorithms(gili = otbLinks)
#'  
#'  ## take edge detection
#'  otb_algorithm<-algo[27]
#'  algo_cmd<-parseOTBFunction(algo = otb_algorithm,gili = gili)
#'  ## print the current command
#'  print(algo_cmd)
#' 
#' ### usecase
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
#' ## get all modules
#' algo<-parseOTBAlgorithms(gili = otblink)
#' 
#' ## use edge detection
#' otb_algorithm<-algo[27]
#' algo_cmd<-parseOTBFunction(algo = algo[27],gili = otblink)
#'
#' ## create out name
#' outName<-paste0(getwd(),"/",tools::file_path_sans_ext("out"),"_","1","_f",filter,".tif")
#' 
#' ## set arguments
#' algo_cmd$`-progress`<-1
#' algo_cmd$`-in`<- file.path(getwd(),"4490600_5321400.tif")
#' algo_cmd$`-filter`<- "sobel"
#' if (filter == "touzi") {
#'   algo_cmd$`-filter.touzi.xradius`<- filter.touzi.xradius
#'   algo_cmd$`-filter.touzi.yradius`<- filter.touzi.yradius
#' }
#' algo_cmd$`-out`<- outName
#' 
#' ## generate basic command 
#' command<-paste0(path_OTB,"otbcli_",otb_algorithm," ")
#' ## generate full command
#' command<-paste(command,paste(names(algo_cmd),algo_cmd,collapse = " "))
#'
#' ## make the system call
#' system(command,intern = TRUE)
#' 
#' ##create raster
#' retStack<-assign(paste0(tools::file_path_sans_ext(basename(outName)),"band_1"),
#'                  raster::stack(outName))
#' 
#' ## plot raster
#' raster::plot(retStack)
#' 
#' } 
#' 
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
      system("rm tst.txt",intern = FALSE,ignore.stderr = TRUE)
      system2(paste0(path_OTB,"otbcli"),paste0(algo," -help >> tst.txt 2>&1"))
      txt<-readLines("tst.txt")
      # Pull out the appropriate line
      args <- txt[grep("-", txt)]
      args <- args[-grep("http",args)]
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
        
        if (length(grep("default value is",sapply(args, "[", 4)[[j]])) > 0)  {
          tmp<-strsplit(sapply(args, "[", 4)[[j]],split ="default value is ")[[1]][2]
          tmp <-strsplit(tmp,split =")")[[1]][1]
          
          default <- tmp
        }
        else if (length(grep("(mandatory)",sapply(args, "[", 4)[[j]])) > 0) default <- "mandatory"
        else if  (sapply(args, "[", 4)[[j]] == "Report progress ") default <- "false"
        else default < sapply(args, "[", 4)[[j]]
        
        param[[paste0(sapply(args, "[", 2)[[j]])]] <- default
        
      }
      
      if (length(ocmd) > 0)
        ocmd[[algo]]<- append(otbcmd,assign(algo, param))
      else
        ocmd<-param
      #params <- get_args_man(alg = "otb:localstatisticextraction")
      
    }
  }
  return(ocmd)
}



