#'@title Get OTB modules 
#'@name parseOTBAlgorithms
#'@description retrieve the OTB module folder content and parses the module names
#'@param gili optional list of avalailable `OTB` binaries if not provided `linkOTB()` is called
#'@export parseOTBAlgorithms
#'
#'@examples
#' \dontrun{
#' ## link to the OTB binaries
#' otblink<-link2GI::linkOTB()
#' 
#'  if (otblink$exist) {
#' 
#'  ## parse all modules
#'  moduleList<-parseOTBAlgorithms(gili = otblink)
#' 
#'  ## print the list
#'  print(moduleList)
#'  
#'  } 
#' }
#' 
parseOTBAlgorithms<- function(gili=NULL) {
  if (is.null(gili)) {
    otb<-link2GI::linkOTB()
    path_OTB<- otb$pathOTB
  } else path_OTB<- gili$pathOTB
  path_OTB = ifelse(Sys.info()["sysname"]=="Windows", utils::shortPathName(path_OTB),path_OTB)
  
  if (substr(path_OTB,nchar(path_OTB) - 1,nchar(path_OTB)) == "n/")   path_OTB <- substr(path_OTB,1,nchar(path_OTB)-1)
  
  algorithms <-list.files(pattern="otbcli", path=path_OTB, full.names=FALSE)
  algorithms <- substr(algorithms,8,nchar(algorithms)) 
  return(algorithms)
}

#'@title Get OTB function argument list
#'@name parseOTBFunction
#'@description retrieve the choosen function and returns a full argument list with the default settings
#'@param algo either the number or the plain name of the `OTB` algorithm that is wanted. Note the correct (of current/choosen version) information is probided by `parseOTBAlgorithms()`
#'@param gili optional list of avalailable `OTB` binaries if not provided `linkOTB()` is called
#'@export parseOTBFunction
#'
#'@examples

#' \dontrun{
## link to the OTB binaries
#' otblink<-link2GI::linkOTB()
#' if (otblink$exist) {
#' 
#' ## parse all modules
#' algos<-parseOTBAlgorithms(gili = otblink)
#' 
#' 
#' ## take edge detection
#' cmdList<-parseOTBFunction(algo = algos[27],gili = otblink)
#' ## print the current command
#' print(cmdList)
#' }
#' }

#' ##+##
parseOTBFunction <- function(algo=NULL,gili=NULL) {
  if (is.null(gili)) {
    otb<-link2GI::linkOTB()
    path_OTB<- otb$pathOTB
  } else path_OTB<- gili$pathOTB
  otb<-gili
  ocmd<-tmp<-list()
  otbcmd <- list()
  otbhelp <- list()
  
  otbtype <- list()
  path_OTB = ifelse(Sys.info()["sysname"]=="Windows", utils::shortPathName(path_OTB),path_OTB)
  
  if (algo != "" & otb$exist){
    system("rm otb_module_dump.txt",intern = FALSE,ignore.stderr = TRUE)
    if (!identical(grep(path_OTB,pattern = "OTB-8."), integer(0) )){
      system(file.path(dirname(path_OTB[[1]]),"otbenv.profile")) 
      system(paste0("otbcli_",algo," -help >> " ,file.path(R.utils::getAbsolutePath(tempdir()),paste0("otb_module_dump.txt 2>&1")))) 
    } else {
      
      ifelse(Sys.info()["sysname"]=="Windows",
             system(paste0("env -i ", path_OTB, "otbcli ", algo, " -help >> ", file.path(R.utils::getAbsolutePath(tempdir()), paste0("otb_module_dump.txt 2>&1")))),
             #system(paste0(file.path(R.utils::getAbsolutePath(utils::shortPathName(path_OTB)),paste0("otbcli_",algo))," -help >> " ,file.path(R.utils::getAbsolutePath(tempdir()),paste0("otb_module_dump.txt 2>&1")))), 
             #system(paste0("env -i ", path_OTB, "otbcli ", algo, " -help >> ", file.path(R.utils::getAbsolutePath(tempdir()), paste0("otb_module_dump.txt 2>&1"))))
             system(paste0(file.path(R.utils::getAbsolutePath(path_OTB),paste0("otbcli_",algo))," -help >> " ,file.path(R.utils::getAbsolutePath(tempdir()),paste0("otb_module_dump.txt 2>&1"))))
      )}
    system(paste0("env -i ", path_OTB, "otbcli ", algo, " -help >> ", file.path(R.utils::getAbsolutePath(tempdir()), paste0("otb_module_dump.txt 2>&1"))))
    txt <-readLines(file.path(tempdir(),"otb_module_dump.txt"))
    file.remove(file.path(tempdir(),"otb_module_dump.txt"))
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
        if (arg == "-in" ) arg<-"-input_in"
        if (arg == "-il" ) arg<-"-input_il"
        param[[paste0(substr(arg,2,nchar(arg)))]] <- default
      }
    }
    
    if (length(ocmd) > 0)
      ocmd[[algo]]<- append(otbcmd,assign(algo, as.character(param)))
    else
      ocmd<-R.utils::insert(param,1,algo)
    #params <- get_args_man(alg = "otb:localstatisticextraction")
    
  } else {print("no valid algorithm provided")}
  
  ## now parse help
  t<-ocmd
  t[[1]]<-NULL
  helpList<-list()
  for (arg in names(t)){
    #    if (arg =="input_in")  arg<-"in"
    #    if (arg =="input_il")  arg<-"il"
    if (arg != "progress")  {
      system(paste0(path_OTB,"otbcli_",paste0(algo," -help ",arg ,paste0(" >> ",file.path(tempdir(),ocmd[[1]]),"-",arg,".txt 2>&1"))))
      helpList[[arg]]<-unique(readLines(paste0(file.path(tempdir(),ocmd[[1]]),"-",arg,".txt")))
      #file.remove(paste0(file.path(tempdir(),ocmd[[1]]),"-",arg,".txt"),showWarnings = TRUE)
      drop <-grep(x = helpList[[arg]],pattern =  "\\w*no version information available\\w*")
      drop<-append(drop,grep(x = helpList[[arg]],pattern =  '^$'))
      helpList[[arg]]<-helpList[[arg]][-drop]
    }
    else if  (arg=="progress") helpList[["progress"]]<- "Report progress: It must be 0, 1, false or true"
  }
  ocmd$help<-helpList
  return(ocmd)
}


#' Execute the OTB command list via system call
#'@description Wrapper function which paste the OTB command list into a system call compatible string and execute this command. 
#'@param otbCmdList the OTB algorithm parameter list
#'@param gili optional list providing the linkage to OTB as done by `linkOTB()`. If not provided the `runOTB` function try to link automatically.
#'@param quiet boolean  if TRUE suppressing messages default is TRUE
#'@param retRaster boolean if TRUE a raster stack is returned default is FALSE
#'@param retCommand boolean if TRUE only the OTB API command is returned default is FALSE
#'@details #' Please NOTE: You must check the help to identify the correct input file argument codewort ($input_in or $input_il). 
#'@examples
#'\dontrun{
#' require(link2GI)
#' require(terra)
#' require(raster)
#' require(listviewer)
#' rgdal::set_thin_PROJ6_warnings(TRUE)
#' 
#' ## link to OTB
#' otblink<-link2GI::linkOTB()
#' 
#' if (otblink$exist) {
#'  projRootDir<-tempdir()
#'  data('rgb', package = 'link2GI')  
#'  terra::plotRGB(rgb)
#'  r<-terra::writeRaster(rgb, 
#'                         filename=file.path(projRootDir,"test.tif"),
#'                         overwrite=TRUE)
#' 
#' ## for an image output example we use the Statistic Extraction, 
#' algoKeyword<- "LocalStatisticExtraction"
#' 
#' ## extract the command list for the choosen algorithm 
#' cmd<-parseOTBFunction(algo = algoKeyword, gili = otblink)
#' 
#' ## Please NOTE:
#' ## You must check the help to identify the correct argument codewort ($input_in or $input_il)
#' listviewer::jsonedit(cmd$help)
#' 
#' ## define the mandatory arguments all other will be default
#' cmd$input_in  <- file.path(tempdir(),"test.tif")
#' cmd$out <- file.path(tempdir(),"test_otb_stat.tif")
#' cmd$radius <- 7
#' 
#' ## run algorithm
#' retStack<-runOTB(cmd,gili = otblink)
#' 
#' ## plot image
#' terra::plot(retStack)
#' 
#' ## for a data output example we use the 
#' 
#' algoKeyword<- "ComputeImagesStatistics"
#' 
#' ## extract the command list for the chosen algorithm 
#' cmd<-parseOTBFunction(algo = algoKeyword, gili = otblink)
#' 
#' ## get help using the convenient listviewer
#' listviewer::jsonedit(cmd$help)
#' 
#' ## define the mandatory arguments all other will be default
#' cmd$input_il  <- file.path(tempdir(),"test.tif")
#' cmd$ram <- 4096
#' cmd$out.xml <- file.path(tempdir(),"test_otb_stat.xml")
#' cmd$progress <- 1
#' 
#' ## run algorithm
#' ret <- runOTB(cmd,gili = otblink, quiet = F)
#' 
#' ## as vector
#' print(ret)
#' 
#' ## as xml
#' XML::xmlParse(cmd$out)
#'  
#' }
#'}
#'@export

runOTB <- function(otbCmdList=NULL,
                   gili=NULL,
                   retRaster=TRUE,
                   retCommand=FALSE,
                   quiet = TRUE){
  
  if (is.null(gili)) {
    otb<-link2GI::linkOTB()
    path_OTB<- otb$pathOTB
  } else path_OTB<- gili$pathOTB
  
  otb_algorithm<-unlist(otbCmdList[1])  
  otbCmdList[1]<-NULL
  otbCmdList$help<-NULL
  
  if(Sys.info()["sysname"]== "Windows") otb_algorithm <- paste0(otb_algorithm,".bat")
  
  if (names(otbCmdList)[1] == "input_in")  {
    otbCmdList$input_in <- gsub(" ", "\\/ ", R.utils::getAbsolutePath(otbCmdList$input_in))  
    names(otbCmdList)[1]<-"in"
  }
  else if (names(otbCmdList)[1] == "input_il")  {
    otbCmdList$input_il <- gsub(" ", "\\/ ", R.utils::getAbsolutePath(otbCmdList$input_il))  
    names(otbCmdList)[1]<-"il"
  }
  else if (names(otbCmdList)[1] == "io.il")  {
    otbCmdList$io.il <- gsub(" ", "\\/ ", R.utils::getAbsolutePath(otbCmdList$io.il))
  }
  if(otbCmdList$mode == "vector"){
    outn = otbCmdList$mode.vector.out
  }
  
  if(!is.null(otbCmdList$out.xml)){
    otbCmdList$out.xml <-gsub(" ", "\\/ ", R.utils::getAbsolutePath(otbCmdList$out.xml))  
    outn = otbCmdList$out.xml
  } 
  else if(!is.null(otbCmdList$out)){
    otbCmdList$out <-gsub(" ", "\\/ ", R.utils::getAbsolutePath(otbCmdList$out))  
    outn = otbCmdList$out
  }else if (!is.null(otbCmdList$io.out)) {
    otbCmdList$io.out <-gsub(" ", "\\/ ", R.utils::getAbsolutePath(otbCmdList$io.out))
    outn = otbCmdList$io.out
    # xml2::read_xml(outn)
  }
  
  
  if (!identical(grep(path_OTB,pattern = "OTB-8."), integer(0) )){
    command<-paste(paste0("otbcli_",otb_algorithm," "),
                   paste0("-",names(otbCmdList)," ",otbCmdList,collapse = " "))
    
  } else {
    command<-paste(paste0(path_OTB,"otbcli_",otb_algorithm," "),
                   paste0("-",names(otbCmdList)," ",otbCmdList,collapse = " "))
  }
  command = gsub("\\\\", "/", command)
  if (retCommand)
    return(command)
  else {
    if (quiet){
      if (!identical(grep(path_OTB,pattern = "OTB-8."), integer(0) ))
        system(file.path(dirname(path_OTB),"otbenv.profile")) 
      
      
      res = system(command,ignore.stdout =TRUE,ignore.stderr = TRUE,intern = FALSE)
      if (retRaster){
        #outn=gsub("\\/", "", path.expand(otbCmdList$out))
        if (length(grep("xml", outn)) == 0 & otbCmdList$mode != "vector") {
          rStack <- assign(tools::file_path_sans_ext(basename(outn)),terra::rast(outn))
          return(rStack)}
        
        
        else if (length(grep("xml", outn)) > 0 & otbCmdList$mode != "vector"){
          #warning("NOTE: ", outn," is not a raster\n")
          return(xml2::read_xml(outn)) 
        } else if ( otbCmdList$mode == "vector"){
          
          return(sf::st_read(outn)) 
        }
      }
    }
    else {
      if (!identical(grep(path_OTB,pattern = "OTB-8."), integer(0) ))
        system(file.path(dirname(path_OTB),"otbenv.profile"))
      
      ret=system(command,ignore.stdout = FALSE,ignore.stderr = FALSE,intern = TRUE)
      lapply(ret, print)
      message(command)
      if (retRaster){
        #outn=gsub("\\/", "", path.expand(otbCmdList$out))
        if (length(grep("xml", outn)) == 0 & otbCmdList$mode != "vector") {
          rStack <- assign(tools::file_path_sans_ext(basename(outn)),terra::rast(outn))
          return(rStack)}
        
        
        else if (length(grep("xml", outn)) > 0 & otbCmdList$mode != "vector"){
          #warning("NOTE: ", outn," is not a raster\n")
          return(xml2::read_xml(outn)) 
        } else if ( otbCmdList$mode == "vector"){
          
          return(sf::st_read(outn)) 
        }
      }
    }
  }
}

