#' Define working environment default settings
#'
#'@details After adding new project settings run [setup_default()] to update and savew the default settings. For compatibility reasons you may also run [lutUpdate()].
#'
#'@return  A list containing the default project settings
#'@param  new_envrmt_list containing a list of arbitrary folders to be generated
#'@param  new_envrmt_list_name name of this list
#'
#'
#' @examples
#' \dontrun{
#' # Standard setup for baseSpatial_git
#' setup_default()
#' }
#' @name setup_default
#' @export setup_default


setup_default = function(new_envrmt_list=NULL,new_envrmt_list_name=NULL)
{
  baseSpatial_git <- list(
    folders = c("docs", "docs/figures","tmp","data/source", "data/results", "data/level0","data/level1"),
    folder_names = NULL,
    init_git = TRUE,
    code_subfolders = c("src", "src/functions"),
    path_prefix = NULL,
    global = FALSE,
    libs = NULL,
    lut_mode = FALSE,
    create_folders = TRUE
    #git_repository = "." # Historic reasons, remove once var git_repository in setupProj is deprecated.
  )
  
  baseproj_no_git <- list(
    folders = c("docs", "docs/figures","tmp","data/source", "data/results", "data/level0","data/level1"),
    folder_names = NULL,
    init_git = FALSE,
    code_subfolders = c("src", "src/functions"),
    path_prefix = NULL,
    global = FALSE,
    libs = NULL,
    lut_mode = FALSE,
    create_folders = TRUE
    #git_repository = "." # Historic reasons, remove once var git_repository in setupProj is deprecated.
  )
  if (is.null(new_envrmt_list)){
    setup_dflt <- list(
      baseSpatial_git = baseSpatial_git, baseproj_no_git = baseproj_no_git)
  } else {
    setup_dflt <- list(baseSpatial_git = baseSpatial_git, baseproj_no_git = baseproj_no_git)
    setup_dflt[[new_envrmt_list_name]] = new_envrmt_list
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
#' @param folder_names names of the variables that point to subfolders. If not
#' provided, the base paths of the folders is used.
#' @param path_prefix a prefix for the folder names.
#' @param create_folders create folders if not existing already.
#'
#' @return  List with folder paths and names.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # createFolders(root_folder = "~/edu", folders = c("data/", "data/tmp/"))
#' }
#' # Create folder list and set variable names pointing to the path values
createFolders <- function(root_folder, folders,
                          folder_names = NULL, path_prefix = "p_",
                          create_folders = FALSE) {
  folders <- lapply(folders, function(f) {
    file.path(root_folder, f)
  })
  folders <- folders[!duplicated(folders)]
  
  if (is.null(folder_names)) {
    names(folders) <- basename(unlist(folders))
    tmplt <- unlist(folders)
    
    while (any(duplicated(names(folders)))) {
      tmplt <- dirname(tmplt)
      dplcts <- which(duplicated(names(folders), fromLast = FALSE) |
                        duplicated(names(folders), fromLast = TRUE))
      names(folders)[dplcts] <-
        paste(basename(tmplt)[dplcts], names(folders[dplcts]), sep = "_")
    }
  } else {
    names(folders) <- folder_names
  }
  
  if (!is.null(path_prefix)) names(folders) <- paste0(path_prefix, names(folders))
  
  # Check paths for existance and create if necessary
  for (f in folders) {
    if (!file.exists(f)) dir.create(f, recursive = TRUE)
  }
  
  return(folders)
}


#' Set up a project environment
#'
#' @description Set up the project environment with a defined folder structure, an RStudio project, initial script and
#' settings templates and git and dvc repository, if necessary.
#'
#' @param root_folder root directory of the project.
#' @param folders list of subfolders within the project directory that will be created.
#' @param folder_names names of the variable values that point to subfolders. If not
#' provided, the base paths of the folders is used.
#' @param path_prefix a prefix for the variable values that point to the created folders.
#' @param init_git logical: init git repository in the project directory.
#' @param init_renv logical: init renv in the project directory.
#' @param code_subfolders subfolders for scripts and functions within the project directory that will be created. The
#' folders src and src/functions are mandatory.
#' @param global logical: export path strings as global variables?
#' @param libs  vector with the  names of libraries that are required for the initial project.
#' @param standard_setup use predefined settings. In this case, only the name of the root folder is required.
#' @param openproject open project after creating it, d default = TRUE
#' @param newsession open project in a new session? default is FALSE
#' @details The function uses [setupProj] for setting up the folders. Once the project is creaeted, manage the overall
#' configuration of the project by the src/functions/000_settings.R script. It is sourced at the begining of the
#' template scripts that are created by default. Define additional constans, required libraries etc. in the
#' 000_settings.R at any time. If additonal folders are required later, just add them manually. They will be parsed as
#' part of the 000_settings.R and added to a variable called envrmt that allows easy acces to any of the folders. Use
#' this variable to load/save data to avoid any hard coded links in the scripts except the top-level root folder which
#' is defined once in the main control script located at src/main.R.
#'
#' @return envrmt, i.e. a list containing the project settings.
#'
#' @name initProj
#' 
#' @export initProj
#'
#' @examples
#' \dontrun{
#' root_folder <- tempdir() # Mandatory, variable must be in the R environment.
#' envrmt <- initProj(root_folder = root_folder, standard_setup = "baseSpatial_git")
#' }
#'
initProj <- function(root_folder = ".", folders = NULL, folder_names = NULL, path_prefix = NULL,
                        init_git = TRUE, init_renv = TRUE, code_subfolders = c("src", "src/functions"),
                        global = FALSE, libs = NULL, openproject =TRUE, newsession=FALSE,
                        standard_setup = c("baseSpatial_git", "baseproj_no_git")) {

  # Setup project directory structure
  if (is.null(folders)) {
    use_standard_setup <- TRUE
    envrmt <- setupProj(root_folder = root_folder, standard_setup = standard_setup[1])
  } else {
    use_standard_setup <- FALSE
    envrmt <- setupProj(
      root_folder = root_folder, folders = folders, folder_names = folder_names, path_prefix = path_prefix,
      code_subfolders = code_subfolders,
      global = global, libs = libs,
      standard_setup = NULL
    )
  }
  
  # Init R project and scripts
  template_path <- system.file(sprintf("templates/%s.brew", "rstudio_proj"), package = "link2GI")
  brew::brew(template_path, file.path(root_folder, paste0(basename(root_folder), ".Rproj")))
  createScript(new_file = file.path(envrmt$src, "main.R"), template = "script_control", notes = TRUE)
  createScript(new_file = file.path(envrmt$functions, "000_setup.R"), template = "script_setup", notes = TRUE)
  createScript(new_file = file.path(root_folder, "README.md"), template = "readme", notes = TRUE)
  
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
  
  # ppath=yaml::as.yaml(envrmt)
  #yaml::write_yaml(ppath,file = file.path("pPath.yaml"))
  #envrmt2 = createFolders(root_folder = here::here(root_folder),folders = envrmt,create_folders = FALSE)
  # yaml::write_yaml(envrmt,file.path(here::here(root_folder),"src/functions/pPath.yaml"))
  if (openproject) rstudioapi::openProject(file.path(root_folder, paste0(basename(root_folder), ".Rproj")),newSession = newsession)
  return(envrmt)
}

#' Setup project folder structure
#'
#' @description Defines folder structures and creates them if necessary, loads
#' libraries, and sets other project relevant parameters.
#'
#' @param root_folder root directory of the project.
#' @param folders list of subfolders within the project directory.
#' @param folder_names names of the variables that point to subfolders. If not
#' provided, the base paths of the folders is used.
#' @param code_subfolders define subdirectories for code should be created.
#' @param path_prefix a prefix for the folder names.
#' @param global logical: export path strings as global variables?
#' @param libs  vector with the  names of libraries
#' @param setup_script name of the setup script. This file will not be sourced from the functions folder even if
#' fcts_folder is provided.
#' @param fcts_folder  path of the folder holding the functions. All files in
#' this folder will be sourced.
#' @param source_functions logical: should functions be sourced?
#' @param standard_setup use predefined settings. In this case, only the name of the root folder is required. 
#' name of the git repository must be supplied to the function.
#' @param create_folders create folders if not existing already.
#' @param lut_mode deprecated, use standard_setup instead.
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
setupProj <- function(root_folder = tempdir(), folders = c("data", "data/tmp"), folder_names = NULL,
                       path_prefix = NULL, code_subfolders = NULL, 
                       global = FALSE, libs = NULL, setup_script = "000_setup.R", fcts_folder = NULL,
                       source_functions = !is.null(fcts_folder),
                       standard_setup = NULL, lut_mode = NULL, create_folders = TRUE ){
  #setup_default() # new_envrmt_list=NULL,new_envrmt_list_name=NULL

  
  
 if (!is.null(standard_setup)) {
    dflt <- setup_default()[[standard_setup]]
    for (i in seq(length(dflt))) {
      assign(names(dflt[i]), dflt[[i]])
    }
  }
  
  
  # Add code folders to folders
  if (!is.null(code_subfolders)) {
    folders <- c(folders, code_subfolders)
  }
  
  
  # Create folders
  folders <- createFolders(root_folder, folders,
                           folder_names = folder_names, path_prefix = path_prefix,
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

#' @param lut_mode use predefined environmental settings. In this case, only the
#' name of the git repository must be supplied to the function.
#'
#' @name addGitFolders
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' addGitFolders(folders = c("data", "data/tmp"), git_repository = "myproject")
#' }
#'
addGitFolders <- function(folders, git_repository = NULL, git_subfolders = NULL,
                          lut_mode = FALSE) {
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
  success <- lapply(libs, function(l) {
    if (!l %in% utils::installed.packages()) {
      utils::install.packages(l)
    }
    require(l, character.only = TRUE)
  })
  names(success) <- libs
  return(success)
}




#' Source functions from standard or given directory
#'
#' @description  Source functions into the R environment located in a specified
#' folder.
#'
#' @param fcts_folder path of the folder holding the functions. All files in
#' this folder will be sourced.
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
                         notes = TRUE) {
  template_path <- system.file(sprintf("templates/%s.brew", template[1]), package = "link2GI")
  brew::brew(template_path, new_file)
}

