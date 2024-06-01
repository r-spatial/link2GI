#' Define working environment default settings
#'
#'@details After adding new project settings run [setup_default()] to update and savew the default settings. For compatibility reasons you may also run [lutUpdate()].
#'
#'@return  A list containing the default project settings
#'@param  new_folder_list containing a list of arbitrary folders to be generated
#'@param  new_folder_list_name name of this list
#'@param default name of default list
#'
#'
#' @examples
#' \dontrun{
#' # Standard setup for baseSpatial
#' setup_default()
#' }
#' @name setup_default
#' @export setup_default


setup_default = function(default=NULL, new_folder_list=NULL,new_folder_list_name=NULL)
{
  # Read master configuration
  setup_default <- yaml::read_yaml(system.file("templates/","config-default-projects.yml", package = "link2GI" ))
  
  if (is.null(new_folder_list) & !is.null(default)){
    setup_dflt <- setup_default[[default]]
  } 
  else if (is.null(new_folder_list) & is.null(default)) {
    setup_dflt <- setup_default
  }  else {
    setup_dflt <- setup_default
    setup_dflt[[new_folder_list_name]] = new_folder_list
  }
  return(setup_dflt )
}


#' Compile folder list and create folders
#'
#' @description  Compile folder list with absolut paths and create folders if
#' necessary.
#'
#' @param root_folder root directory of the project.
#' @param folders list of subfolders within the project directory.
#' @param create_folders create folders if not existing already.
#'
#' @return  List with folder paths and names.
#'
#' @export createFolders
#' @aliases createFolder
#'
#' @examples
#' \dontrun{
#'  createFolders(root_folder = tempdir(), folders = c("data/", "data/tmp/"))
#' }
#' # Create folder list and set variable names pointing to the path values
createFolders <- function(root_folder, folders,
                          create_folders = TRUE) {
  
  folders <- lapply(folders, function(f) {
    file.path(root_folder, f)
  })
  folders <- folders[!duplicated(folders)]
  
  
  names(folders) <- basename(unlist(folders))
  tmplt <- unlist(folders)
  
  while (any(duplicated(names(folders)))) {
    tmplt <- dirname(tmplt)
    dplcts <- which(duplicated(names(folders), fromLast = FALSE) |
                      duplicated(names(folders), fromLast = TRUE))
    names(folders)[dplcts] <-
      paste(basename(tmplt)[dplcts], names(folders[dplcts]), sep = "_")
  }
  
  
  # Check paths for existance and create if necessary
  for (f in folders) {
    if (!file.exists(f)) dir.create(f, recursive = TRUE)
  }
  
  return(folders)
}


#' Simple creation and reproduction of an efficient project environment
#'
#' @description Set up the project environment with a defined folder structure, an RStudio project, initial scripts and configuration files and optionally with Git and Renv support.
#' @param root_folder root directory of the project.
#' @param folders list of sub folders within the project directory that will be created.
#' @param init_git logical: init git repository in the project directory.
#' @param init_renv logical: init renv in the project directory.
#' @param code_subfolder sub folders for scripts and functions within the project directory that will be created. The
#' folders src, src/functions and src/config are mandatory.
#' @param global logical: export path strings as global variables?
#' @param appendlibs  vector with the  names of libraries that are required for the initial project.
#' settings required for the project, such as additional libraries, optional settings, 
#' colour schemes, etc. Important: It should not be used to control the runtime
#' parameters of the scripts.  This file is not read in automatically, even if 
#' it is located in the "fcts_folder" folder.
#' @param standard_setup select one of the predefined settings c("base", "baseSpatial", "advancedSpatial"). 
#' In this case, only the name of the base folder is required, but individual additional 
#' folders can be specified under 'folders' 
#' name of the git repository must be supplied to the function.
#' @param newsession open project in a new session? default is FALSE
#' @param loc_name  NULL by default, defines the research area of the analysis in the data folder as a subfolder and serves as a code tag
#' @param ymlFN filename for a yaml file containing a non standard_setup 
#' @param OpenFiles default NULL
#' @param openproject default NULL if TRUE the project is opened in a new session
#' @note For yaml based setup you need to use one of the default configurations 
#' c("base", "baseSpatial","advancedSpatial") or you provide a yaml file this 
#' MUST contain the standard_setup arguments, where \code{ mysetup} is the yaml root, all other items are mandatory keywords that can be filled in as needed.  
#'  \preformatted{
#'mysetup:
#'   dataFolder:  
#'   docsFolder:  
#'   tmpFolder:   
#'   init_git: true/false 
#'   init_renv: true/false 
#'   code_subfolder: ["src", "src/functions" , "src/config"] 
#'   global: true/false 
#'   libs: 
#'   create_folders: true/false
#'   files:
#'}
#'    Alternatively you may set default_setup to NULL and provide the arguments via command line.
#' @details The function uses [setupProj] for setting up the folders. Once the project is creaeted, manage the overall
#' configuration of the project by the `src/functions/000_settings.R script`. It is sourced at the begining of the
#' template scripts that are created by default. Define additional constans, required libraries etc. in the
#' 000_settings.R at any time. If additonal folders are required later, just add them manually. They will be parsed as
#' part of the 000_settings.R and added to a variable called dirs that allows easy acces to any of the folders. Use
#' this variable to load/save data to avoid any hard coded links in the scripts except the top-level root folder which
#' is defined once in the main control script located at src/main.R.
#'
#' @return dirs, i.e. a list containing the project pathes.
#'
#' @name initProj
#' 
#' @export initProj
#'
#' @examples
#' \dontrun{
#' root_folder <- tempdir() # Mandatory, variable must be in the R environment.
#' dirs <- initProj(root_folder = root_folder, standard_setup = "baseSpatial")
#' }
#'
initProj <- function(root_folder = ".", 
                     folders = NULL, 
                     init_git = NULL, 
                     init_renv = NULL, 
                     code_subfolder = c("src", "src/functions", "src/configs"),
                     global = FALSE,  
                     openproject = NULL, newsession = TRUE,
                     standard_setup = "baseSpatial",
                     loc_name = NULL, 
                     ymlFN = NULL ,
                     appendlibs = NULL, 
                     OpenFiles = NULL) {
  
  
  notes = TRUE
  if (is.null(init_git)) init_git = FALSE
  if (is.null(init_renv)) init_renv = FALSE
  if (!is.null(loc_name)) 
    if (loc_name == "") loc_name = NULL
  if (is.null(appendlibs)) appendlibs = "dplyr"
  if (is.null(openproject)) openproject = FALSE
  # Setup project directory structure
  if (standard_setup %in% c("base", "baseSpatial","advancedSpatial"))
  {
    envrmt = setup_default(standard_setup)
  } else {
    envrmt = yaml::read_yaml(file = ymlFN)
  }
  
  if (is.null(appendlibs)){
    libs = envrmt$libs
  } else {
    libs = append(appendlibs,envrmt$libs)
  }
  
  if (!is.null(code_subfolder) | length(code_subfolder) > 0){
    code_subfolder = unique(append(code_subfolder,envrmt$code_subfolder))
  } else {
    code_subfolder = envrmt$code_subfolder
  }
  projectDirList = as.list(strsplit(names(envrmt)[grepl("Folder", names(envrmt))],split = "Folder",fixed = TRUE))
  
  if (is.null(loc_name)){
    projectDirList =  append(projectDirList,file.path("data",envrmt$dataFolder))
    projectDirList =  append(projectDirList,file.path("docs",envrmt$docsFolder))
    projectDirList =  append(projectDirList,file.path("tmp",envrmt$tmpFolder))
  } else{
    projectDirList =  append(projectDirList,file.path("data",loc_name,envrmt$dataFolder))
    projectDirList =  append(projectDirList,file.path("docs",loc_name,envrmt$docsFolder))
    projectDirList =  append(projectDirList,file.path("tmp",loc_name,envrmt$tmpFolder))
    
  }
  # append additional folders if defined by calling script
  if (!is.null(folders) && folders[[1]] != "") 
  {
    projectDirList = append(projectDirList,folders)
  }
  
  if (is.null(folders)) {
    use_standard_setup <- TRUE
    dirs <- setupProj(root_folder = root_folder, 
                      folders = projectDirList, 
                      code_subfolder = code_subfolder, 
                      standard_setup = standard_setup,
                      libs = libs)
  } else {
    use_standard_setup <- FALSE
    dirs <- setupProj(root_folder = root_folder, 
                      folders = projectDirList, 
                      code_subfolder = code_subfolder,
                      global = global, libs = libs,
                      standard_setup = NULL
    )
  }
  
  # create R project and scripts
  brew::brew(system.file(sprintf("templates/%s.brew", "rstudio_proj"), package = "link2GI"), file.path(root_folder, paste0(basename(root_folder), ".Rproj")))
  brew::brew(system.file(sprintf("templates/%s.brew", "script_control"), package = "link2GI"),  file.path(dirs$src, "main-control.R"))
  brew::brew(system.file(sprintf("templates/%s.brew", "pre-processing"), package = "link2GI"),  file.path(dirs$src, "pre-processing.R"))
  brew::brew(system.file(sprintf("templates/%s.brew", "processing"), package = "link2GI"),  file.path(dirs$src, "10-processing.R"))
  brew::brew(system.file(sprintf("templates/%s.brew", "post-processing"), package = "link2GI"),  file.path(dirs$src, "post-processing.R"))
  brew::brew(system.file(sprintf("templates/%s.brew", "config-master-yml"), package = "link2GI"),  file.path(dirs$config, "config-master.yml"))
  brew::brew(system.file(sprintf("templates/%s.brew", standard_setup), package = "link2GI"),file.path(dirs$functions, "000_setup.R"))
  brew::brew(system.file(sprintf("templates/%s.brew", "yml"), package = "link2GI"),file.path(dirs$config, "pre-processing.yml"))
  brew::brew(system.file(sprintf("templates/%s.brew", "yml"), package = "link2GI"),file.path(dirs$config, "processing.yml"))
  brew::brew(system.file(sprintf("templates/%s.brew", "yml"), package = "link2GI"),file.path(dirs$config, "post-processing.yml"))
  brew::brew(system.file(sprintf("templates/%s.brew", "readme"), package = "link2GI"),file.path(dirs$config, "README.md"))
  
  
  
  # Init git
  # if (use_standard_setup) init_git <- setup_default()[[standard_setup[1]]]$init_git
  if (init_git) {
    if (!file.exists(file.path(root_folder, ".git"))) {
      git2r::init(root_folder) #system(paste("git init", root_folder))
    }
    template_path <- system.file(sprintf("templates/%s.brew", "gitignore"), package = "link2GI")
    brew::brew(template_path, file.path(root_folder, ".gitignore"))
  } 
  
  if (init_renv) renv::init(root_folder)
  
  if (openproject) rstudioapi::openProject(file.path(root_folder, paste0(basename(root_folder), ".Rproj")),newSession = newsession)
  return(dirs)
}

#' Setup project folder structure
#'
#' @description Defines folder structures and creates them if necessary, loads
#' libraries, and sets other project relevant parameters.
#'
#' @param root_folder root directory of the project.
#' @param folders list of sub folders within the project directory.
#' @param code_subfolder sub folders for scripts and functions within the 
#' project directory that will be created. The
#' folders src, src/functions and src/config are recommended.
#' @param global logical: export path strings as global variables?
#' @param libs  vector with the  names of libraries
#' @param setup_script Name of the installation script that contains all the 
#' settings required for the project, such as additional libraries, optional settings, 
#' colour schemes, etc. Important: It should not be used to control the runtime
#' parameters of the scripts.  This file is not read in automatically, even if 
#' it is located in the "fcts_folder" folder.
#' @param fcts_folder  path of the folder holding the functions. All files in
#' this folder will be sourced at project start.
#' @param source_functions logical: should functions be sourced? Default is TRUE if fcts_folder exists.
#' @param standard_setup select one of the predefined settings c("base", "baseSpatial", "advancedSpatial"). 
#' In this case, only the name of the base folder is required, but individual additional 
#' folders can be specified under 'folders' 
#' name of the git repository must be supplied to the function.
#' @param create_folders default is TRUE  so create folders if not existing already.
#'
#' @return A list containing the project settings.
#'
#' @name setupProj
#' 
#' @export setupProj
#'
#'
#' @examples
#' \dontrun{
#' setupProj(
#'   root_folder = "~/edu", folders = c("data/", "data/tmp/"),
#'   libs = c("link2GI")
#' )
#' }
#'
setupProj <- function(root_folder = tempdir(), folders = c("data", "data/tmp"), 
                      code_subfolder = NULL, 
                      global = FALSE, libs = NULL, setup_script = "000_setup.R", fcts_folder = NULL,
                      source_functions = !is.null(fcts_folder),
                      standard_setup = NULL,  create_folders = TRUE ){
  #setup_default() # new_folder_list=NULL,new_folder_list_name=NULL
  
  
  
  if (!is.null(standard_setup)) {
    dflt <- setup_default()[[standard_setup]]
    for (i in seq(length(dflt))) {
      assign(names(dflt[i]), dflt[[i]])
    }
  }
  
  
  # Add code folders to folders
  if (!is.null(code_subfolder)) {
    folders <- c(folders, code_subfolder)
  }
  
  
  # Create folders
  folders <- createFolders(root_folder, folders,
                           
                           create_folders = create_folders
  )
  
  # Set global environment if necessary
  if (global) makeGlobalVariable(names = names(folders), values = folders)
  
  # Load and install libraries
  loadLibraries(libs)
  
  # Source functions
  if (source_functions) sourceFunctions(fcts_folder, setup_script)
  
  return(folders)
}


#' Generates a variable with a certain value in the R environment
#'
#' @description  Generates a variable with a certain value in the R environment.
#'
#' @param names  vector with the  names of the variable(s)
#' @param values vector with values of the variable(s)
#'
#' @name makeGlobalVariable
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # creates the global variable \\code{path_data} with the value \\code{~/data}
#' makeGlobalVariable(names = "path_data", values = "~/data")
#' }
#'
makeGlobalVariable <- function(names, values) {
  if (!exists("enivmaR")) enivmaR <- new.env(parent = globalenv())
  
  for (i in seq(length(names))) {
    if (exists(names[i], envir = enivmaR)) {
      warning(paste("The variable", names[i], "already exist in .GlobalEnv"))
    } else {
      assign(names[i], values[i], envir = enivmaR, inherits = TRUE)
    }
  }
}




#' Extent folder list by git repository
#'
#' @description  Extent folder list by git repository and create subdirectories
#' according to default values.
#'
#' @param folders list of subfolders within the project directory.

#'
#' @name addGitFolders
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' addGitFolders(folders = c("data", "data/tmp"), git_repository = "myproject")
#' }
#'
addGitFolders <- function(folders, git_repository = NULL, git_subfolders = NULL) {
  if (is.null(git_subfolders)) {
    folders <- c(folders, git_repository)
  } else {
    folders <- c(folders, file.path(git_repository, git_subfolders))
  }
}


#' Load libraries and try to install missing ones
#'
#' @description  Load libaries in the R environment and try to install misssing
#' ones.
#'
#' @param libs  vector with the  names of libraries
#'
#' @return  List indicating which library has been loaded successfully.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # loadLibraries(libs = C("link2GI"))
#' }
loadLibraries <- function(libs) {
  options(install.packages.check.source = "no")
  
  success <- lapply(libs, function(l) {
    if (!l %in% utils::installed.packages()) {
      utils::install.packages(l)
    }
    require(l, character.only = TRUE)
  })
  names(success) <- libs
  options(install.packages.check.source = "yes")
  
  return(success)
}




#' Source functions from standard or given directory
#'
#' @description  Source functions into the R environment located in a specified
#' folder.
#'
#' @param fcts_folder path of the folder holding the functions. All files in
#' this folder will be sourced at project start.
#'
#' @return  Information if sourcing was successfull based on try function.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # sourceFunctions(fcts_folder = "~/project/src/fcts")
#' }
sourceFunctions <- function(fcts_folder, setup_script) {
  fcts <- list.files(fcts_folder, full.names = TRUE, recursive = TRUE)
  if (TRUE %in% grepl(setup_script, fcts))  fcts <- fcts[-grep(pattern = setup_script, x = fcts)]
  success <- lapply(fcts, function(f) {
    try(source(f), silent = TRUE)
  })
  names(success) <- fcts
  return(success)
}





#' Saves data in rds format and adds a yaml metadata file.
#'
#' @description Saves data in rds format and saves metadata in a corresponding yaml file.
#'
#' @param variable name of the data variable to be saved.
#' @param file_path name and path of the rds file.
#' @param meta name of the metadata list.
#'
#' @return NULL
#'
#' @name saveEnvi
#' @export saveEnvi
#'
#' @examples
#' \dontrun{
#' a <- 1
#' meta <- list(a = "a is a variable")
#' saveEnvi(a, file.path(tempdir(), "test.rds"), meta)
#' }
#'
saveEnvi <- function(variable, file_path, meta) {
  saveRDS(variable, file_path)
  yaml::write_yaml(meta, paste0(tools::file_path_sans_ext(file_path), ".yaml"))
}


#' Create list of metadata from project environment.
#'
#' @description Create list of metadata from project environment.
#'
#' @param prj_name name of the project
#'
#' @return list of metadata.
#'
#' @name createMeta
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' createMeta(tempdir())
#' }
#'
createMeta <- function(prj_name) {
  meta <- list()
  meta$prj <- prj_name
  meta$git_commit <- system("git rev-parse HEAD", intern = TRUE)
  return(meta)
}
#' Load data from rds format and associated yaml metadata file.
#'
#' @description Load data from rds format and associated yaml metadata file.
#'
#' @param file_path name and path of the rds file.
#'
#' @return list of 2 containing data and metadata.
#'
#' @name loadEnvi
#' @export loadEnvi
#'
#' @examples
#' \dontrun{
#' a <- 1
#' meta <- list(a = "a is a variable")
#' saveEnvi(a, file.path(tempdir(), "test.rds"), meta)
#' b <- loadEnvi(file.path(tempdir(), "test.rds"))
#' }
#'
loadEnvi <- function(file_path) {
  dat <- readRDS(file_path)
  meta <- yaml::read_yaml(paste0(tools::file_path_sans_ext(file_path), ".yaml"))
  return(list(dat = dat, meta = meta))
}


#' Create files or scripts from templates
#'
#' @description Create files or scripts from brew templates supplied with the package.
#'
#' @param new_file name of the file to be created
#' @param template template to be used for the new file ("script_function", "script_control")
#' @param template_path path to template to be used 

#' @param notes logical: include notes from the template in the file
#'
#' @return NULL
#'
#' @name createScript
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' createScript()
#' }
#'
createScript <- function(new_file = file.path(tempdir(), "tmp.R"), template = c("script_function", "script_control"),
                         notes = TRUE, template_path =  system.file(sprintf("templates/%s.brew", template[1]), package = "link2GI")) {
  
  brew::brew(template_path, new_file)
}

