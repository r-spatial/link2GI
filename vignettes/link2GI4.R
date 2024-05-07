## ----eval=FALSE---------------------------------------------------------------
#  # link to the installed OTB
#  otblink<-link2GI::linkOTB()
#  
#  
#  # get the list of modules from the linked version
#  algo<-parseOTBAlgorithms(gili = otblink)

## ----eval=FALSE---------------------------------------------------------------
#  ## for the example we use the edge detection,
#  algoKeyword<- "EdgeExtraction"
#  
#  ## extract the command list for the choosen algorithm
#  cmd<-parseOTBFunction(algo = algoKeyword, gili = otblink)
#  
#  ## print the current command
#  print(cmd)

## ----eval=FALSE---------------------------------------------------------------
#  require(link2GI)
#  require(raster)
#  require(listviewer)
#  
#  otblink<-link2GI::linkOTB()
#   root_dir<-tempdir()
#  
#  fn <- system.file("ex/elev.tif", package = "terra")
#  
#  ## for the example we use the edge detection,
#  algoKeyword<- "EdgeExtraction"
#  
#  ## extract the command list for the choosen algorithm
#  cmd<-parseOTBFunction(algo = algoKeyword, gili = otblink)
#  
#  ## get help using the convenient listviewer
#  listviewer::jsonedit(cmd$help)
#  
#  ## define the mandantory arguments all other will be default
#  cmd$input_in  <- fn
#  cmd$filter <- "touzi"
#  cmd$channel <- 1
#  cmd$out <- paste0(root_dir,paste0("\\out",cmd$filter,".tif"))
#  
#  ## run algorithm
#  retStack<-runOTB(cmd,gili = otblink)
#  
#  ## plot filter raster on the green channel
#  plot(retStack)
#  
#  
#  
#  
#  
#  
#  
#  

