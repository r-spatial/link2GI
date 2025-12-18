#'@title Retrieve available OTB modules 
#'@name parseOTBAlgorithms
#'@description Read in the selected OTB module folder and create a list of available functions.
#'@param gili optional list of available `OTB` installations, if not specified, 
#'`linkOTB()` is called to automatically try to find a valid OTB installation 
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
parseOTBAlgorithms <- function(gili = NULL) {
  
  if (is.null(gili)) gili <- link2GI::linkOTB()
  if (is.null(gili) || !isTRUE(gili$exist)) stop("No valid OTB installation found (gili$exist is FALSE).")
  
  path_OTB <- gili$pathOTB
  path_OTB <- if (Sys.info()[["sysname"]] == "Windows") utils::shortPathName(path_OTB) else path_OTB
  
  # normalize trailing slash
  path_OTB <- normalizePath(path_OTB, mustWork = FALSE)
  if (!dir.exists(path_OTB)) stop("OTB bin directory does not exist: ", path_OTB)
  
  algos <- list.files(path_OTB, pattern = "^otbcli_", full.names = FALSE)
  algos <- sub("^otbcli_", "", algos)
  
  # some installs ship 'otbcli' wrapper without underscore; ignore it
  algos <- algos[nzchar(algos)]
  sort(unique(algos))
}


#'@title Retrieve the argument list from a selected OTB function
#'@name parseOTBFunction
#'@description retrieve the selected function and returns a full argument list with the default settings
#'@param algo either the number or the plain name of the `OTB` algorithm that is wanted. Note the correct (of current/selected version) information is provided by `parseOTBAlgorithms()`
#'@param gili optional list of available `OTB` installations, if not specified, 
#'`linkOTB()` is called to automatically try to find a valid OTB installation 
##'@export parseOTBFunction
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
parseOTBFunction <- function(algo = NULL, gili = NULL) {
  
  if (is.null(algo) || !nzchar(algo)) stop("`algo` must be a non-empty character string.")
  
  if (is.null(gili)) gili <- link2GI::linkOTB()
  if (is.null(gili) || !isTRUE(gili$exist)) stop("No valid OTB installation found (gili$exist is FALSE).")
  
  sys <- Sys.info()[["sysname"]]
  path_OTB <- normalizePath(gili$pathOTB, mustWork = FALSE)
  
  if (!file.exists(file.path(path_OTB, "otbcli")) && sys != "Windows") {
    stop("Could not find 'otbcli' in: ", path_OTB)
  }
  
  # --- helper: get full help output (stdout+stderr) safely ---
  otb_help_stdout <- function(otb_bin, env_script, algo) {
    
    if (Sys.info()[["sysname"]] == "Windows") {
      # Windows: call otbcli_<algo>.bat if present, else otbcli_<algo>
      exe <- file.path(otb_bin, paste0("otbcli_", algo))
      if (file.exists(paste0(exe, ".bat"))) exe <- paste0(exe, ".bat")
      out <- system2(exe, c("-help"), stdout = TRUE, stderr = TRUE)
      return(out)
    }
    
    # Linux: ALWAYS use bash -lc, and dot-source the env script
    if (!is.na(env_script) && nzchar(env_script)) {
      env_script <- normalizePath(env_script, mustWork = FALSE)
      if (!file.exists(env_script)) stop("envScript does not exist: ", env_script)
      prefix <- paste0(". ", shQuote(env_script), "; ")
    } else {
      prefix <- ""
    }
    
    cmd <- paste0(prefix,
                  shQuote(file.path(otb_bin, "otbcli")), " ",
                  shQuote(algo), " -help")
    
    system2("bash", c("-lc", cmd), stdout = TRUE, stderr = TRUE)
  }
  
  txt <- otb_help_stdout(
    otb_bin    = path_OTB,
    env_script = if (!is.null(gili$envScript)) gili$envScript else NA_character_,
    algo       = algo
  )
  
  # If OTB failed, parsing will be empty -> hard-stop with context
  arg_lines <- txt[grepl("^-", txt)]
  if (length(arg_lines) == 0) {
    stop(
      "OTB help output could not be parsed (no argument lines found). ",
      "This usually means OTB did not initialise its environment / modules.\n",
      "First lines of output:\n",
      paste(utils::head(txt, 60), collapse = "\n")
    )
  }
  
  # --- keep your original parsing logic, but on arg_lines ---
  args <- arg_lines
  
  args <- gsub("MISSING", "     ", args, fixed = TRUE)
  args <- gsub("\t", "     ", args, fixed = TRUE)
  args <- gsub(" <", "     <", args, fixed = TRUE)
  args <- gsub("> ", ">     ", args, fixed = TRUE)
  
  # normalize spaces (your cascade)
  args <- gsub("          ", "   ", args, fixed = TRUE)
  args <- gsub("         ", "   ", args, fixed = TRUE)
  args <- gsub("        ", "   ", args, fixed = TRUE)
  args <- gsub("       ", "   ", args, fixed = TRUE)
  args <- gsub("      ", "   ", args, fixed = TRUE)
  args <- gsub("     ", "   ", args, fixed = TRUE)
  args <- gsub("    ", "   ", args, fixed = TRUE)
  args <- gsub("   ", "   ", args, fixed = TRUE)
  
  args <- strsplit(args, split = "   ")
  
  param <- list()
  ocmd  <- list()
  
  # Guard: structure assumptions
  if (length(args) < 2 || length(args[[1]]) < 2) {
    stop("Unexpected OTB help format. First parsed row:\n",
         paste(args[[1]], collapse = " | "))
  }
  
  # algo name as first element (like your old code)
  ocmd <- R.utils::insert(param, 1, algo)
  
  for (j in seq_len(length(args) - 1)) {
    drop <- FALSE
    default <- ""
    extractit <- FALSE
    
    # ensure 4th field exists
    field4 <- if (length(args[[j]]) >= 4) args[[j]][4] else ""
    field2 <- if (length(args[[j]]) >= 2) args[[j]][2] else ""
    
    if (grepl("default value is", field4, fixed = TRUE)) extractit <- TRUE
    
    if (extractit) {
      tmp <- strsplit(field4, "default value is ", fixed = TRUE)[[1]]
      if (length(tmp) >= 2) {
        tmp <- strsplit(tmp[2], ")", fixed = TRUE)[[1]][1]
        default <- tmp
      }
    } else if (length(grep("(OTB-Team)", args[[j]])) > 0) {
      drop <- TRUE
    } else if (length(grep("(-help)", args[[j]])) > 0) {
      drop <- TRUE
    } else if (length(grep("(otbcli_)", args[[j]])) > 0) {
      drop <- TRUE
    } else if (length(grep("(-inxml)", args[[j]])) > 0) {
      drop <- TRUE
    } else if (grepl("(mandatory)", field4)) {
      default <- "mandatory"
    } else if (identical(field4, "Report progress ")) {
      default <- "false"
    } else {
      default <- field4
    }
    
    if (!drop && nzchar(default)) {
      arg <- field2
      if (identical(arg, "-in")) arg <- "-input_in"
      if (identical(arg, "-il")) arg <- "-input_il"
      if (nzchar(arg) && nchar(arg) >= 2) {
        param[[substr(arg, 2, nchar(arg))]] <- default
      }
    }
  }
  
  ocmd <- R.utils::insert(param, 1, algo)
  
  # --- help per parameter (Linux again via bash -lc) ---
  helpList <- list()
  t <- ocmd
  t[[1]] <- NULL
  
  for (arg in names(t)) {
    if (arg == "progress") {
      helpList[["progress"]] <- "Report progress: It must be 0, 1, false or true"
      next
    }
    
    if (sys == "Windows") {
      exe <- file.path(path_OTB, paste0("otbcli_", algo))
      if (file.exists(paste0(exe, ".bat"))) exe <- paste0(exe, ".bat")
      out <- system2(exe, c("-help", arg), stdout = TRUE, stderr = TRUE)
    } else {
      env_script <- if (!is.null(gili$envScript)) gili$envScript else NA_character_
      prefix <- if (!is.na(env_script) && nzchar(env_script)) paste0(". ", shQuote(env_script), "; ") else ""
      cmd <- paste0(prefix,
                    shQuote(file.path(path_OTB, "otbcli")), " ",
                    shQuote(algo), " -help ", shQuote(arg))
      out <- system2("bash", c("-lc", cmd), stdout = TRUE, stderr = TRUE)
    }
    
    out <- unique(out)
    drop <- grep("\\w*no version information available\\w*", out)
    drop <- c(drop, grep("^$", out))
    if (length(drop)) out <- out[-drop]
    
    helpList[[arg]] <- out
  }
  
  ocmd$help <- helpList
  ocmd
}

#' Execute the OTB command via system call
#'@description Wrapper function that inserts the OTB command list into a system call compatible string and executes that command.  
#'@param otbCmdList the correctly populated OTB algorithm parameter list
#'@param gili optional list of available `OTB` installations, if not specified, 
#'`linkOTB()` is called to automatically try to find a valid OTB installation 
#'@param quiet boolean  if TRUE suppressing messages default is TRUE
#'@param retRaster boolean if TRUE a raster stack is returned default is FALSE
#'@param retCommand boolean if TRUE only the OTB API command is returned default is FALSE
#'@details #' Please NOTE: You must check the help to identify the correct input file argument ($input_in or $input_il). 
#'@examples
#'\dontrun{
#' require(link2GI)
#' require(terra)
#' require(listviewer)
#' 
#' ## link to OTB
#' otblink<-link2GI::linkOTB()
#' 
#' if (otblink$exist) {
#'  root_folder<-tempdir()
#'  fn <- system.file('ex/elev.tif', package = 'terra')
#' 
#' ## for an image output example we use the Statistic Extraction, 
#' algoKeyword<- 'LocalStatisticExtraction'
#' 
#' ## extract the command list for the choosen algorithm 
#' cmd<-parseOTBFunction(algo = algoKeyword, gili = otblink)
#' 
#' ## Please NOTE:
#' ## You must check the help to identify the correct argument codewort ($input_in or $input_il)
#' listviewer::jsonedit(cmd$help)
#' 
#' ## define the mandatory arguments all other will be default
#' cmd$input_in  <- fn
#' cmd$out <- file.path(tempdir(),'test_otb_stat.tif')
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
#' algoKeyword<- 'ComputeImagesStatistics'
#' 
#' ## extract the command list for the chosen algorithm 
#' cmd<-parseOTBFunction(algo = algoKeyword, gili = otblink)
#' 
#' ## get help using the convenient listviewer
#' listviewer::jsonedit(cmd$help)
#' 
#' ## define the mandatory arguments all other will be default
#' cmd$input_il  <- file.path(tempdir(),'test.tif')
#' cmd$ram <- 4096
#' cmd$out.xml <- file.path(tempdir(),'test_otb_stat.xml')
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
runOTB <- function(otbCmdList = NULL, gili = NULL,
                   retRaster = TRUE, retCommand = FALSE, quiet = TRUE) {
  
  if (is.null(otbCmdList) || length(otbCmdList) == 0) stop("`otbCmdList` must be a non-empty list.")
  if (is.null(gili)) gili <- link2GI::linkOTB()
  if (!isTRUE(gili$exist)) stop("No valid OTB installation found (gili$exist is FALSE).")
  
  sys <- Sys.info()[["sysname"]]
  path_OTB <- normalizePath(gili$pathOTB, mustWork = FALSE)
  
  otb_algorithm <- unlist(otbCmdList[1])
  otbCmdList[1] <- NULL
  otbCmdList$help <- NULL
  
  # map input keyword if present
  if (names(otbCmdList)[1] == "input_in") {
    otbCmdList$input_in <- gsub(" ", "\\/ ", R.utils::getAbsolutePath(otbCmdList$input_in))
    names(otbCmdList)[1] <- "in"
  } else if (names(otbCmdList)[1] == "input_il") {
    otbCmdList$input_il <- gsub(" ", "\\/ ", R.utils::getAbsolutePath(otbCmdList$input_il))
    names(otbCmdList)[1] <- "il"
  }
  
  # output detection (unchanged)
  outn <- NULL
  if (!is.null(otbCmdList$out.xml)) {
    otbCmdList$out.xml <- gsub(" ", "\\/ ", R.utils::getAbsolutePath(otbCmdList$out.xml))
    outn <- otbCmdList$out.xml
  } else if (!is.null(otbCmdList$out)) {
    otbCmdList$out <- gsub(" ", "\\/ ", R.utils::getAbsolutePath(otbCmdList$out))
    outn <- otbCmdList$out
  } else if (!is.null(otbCmdList$io.out)) {
    otbCmdList$io.out <- gsub(" ", "\\/ ", R.utils::getAbsolutePath(otbCmdList$io.out))
    outn <- otbCmdList$io.out
  } else if (!is.null(otbCmdList$mode) && identical(otbCmdList$mode, "vector")) {
    outn <- otbCmdList$mode.vector.out
  }
  
  # build argument string
  arg_str <- paste0("-", names(otbCmdList), " ", unlist(otbCmdList), collapse = " ")
  arg_str <- gsub("\\\\", "/", arg_str)
  
  if (sys == "Windows") {
    exe <- file.path(path_OTB, paste0("otbcli_", otb_algorithm))
    if (file.exists(paste0(exe, ".bat"))) exe <- paste0(exe, ".bat")
    command <- paste(shQuote(exe), arg_str)
  } else {
    # Linux: call wrapper otbcli, not otbcli_<algo> (works reliably with env)
    env_script <- if (!is.null(gili$envScript)) gili$envScript else NA_character_
    prefix <- if (!is.na(env_script) && nzchar(env_script)) paste0(". ", shQuote(env_script), "; ") else ""
    command <- paste0(prefix,
                      shQuote(file.path(path_OTB, "otbcli")), " ",
                      shQuote(otb_algorithm), " ",
                      arg_str)
  }
  
  if (retCommand) return(command)
  
  # execute
  if (sys == "Windows") {
    if (quiet) {
      system(command, ignore.stdout = TRUE, ignore.stderr = TRUE, intern = FALSE)
      ret <- character(0)
    } else {
      message(command)
      ret <- system(command, intern = TRUE)
    }
  } else {
    # Linux: always bash -lc for correct env initialisation
    if (quiet) {
      ret <- system2("bash", c("-lc", command), stdout = FALSE, stderr = FALSE)
      ret <- character(0)
    } else {
      message(command)
      ret <- system2("bash", c("-lc", command), stdout = TRUE, stderr = TRUE)
      lapply(ret, print)
    }
  }
  
  if (!isTRUE(retRaster)) return(ret)
  
  # read output (best effort)
  if (!is.null(outn) && file.exists(outn)) {
    ext <- xfun::file_ext(outn)
    if (ext == "tif") return(terra::rast(outn))
    if (ext == "xml") return(xml2::read_xml(outn))
    if (!is.null(otbCmdList$mode) && identical(otbCmdList$mode, "vector")) return(sf::st_read(outn, quiet = TRUE))
  }
  
  # if nothing to read, return raw output
  ret
}
