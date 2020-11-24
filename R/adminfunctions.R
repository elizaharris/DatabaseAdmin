
#' Function to query the postgres database from R
#'
#' This function sends a query written in PostgreSQL to the Functional Ecology database. The username
#' and password must be present in the workspace.
#' @param query needs to be in postgres code
#' @return The output of the query from postgres is returned; there will be no return if the query did not generate an output
#' @examples queryDatabase("SELECT datetime, precip_tot FROM z_micromet WHERE datetime BETWEEN '01/01/2009 00:00' AND '01/01/2010 00:00'")
#' @export
queryDatabase <- function(query){
  on.exit(dbDisconnect(con))
  con <- dbConnect(drv = dbDriver("PostgreSQL"),
                   dbname = "c7701050", host = "db06.intra.uibk.ac.at",
                   port = 5432, user = user,
                   password = password)
  tmp <- dbGetQuery(con, query)
}

#' Checks/converts the column types of an imported tibble to the selected column types
#'
#' The tibble can have the first row as the expected column types (removed from the output) or they can be given separately
#' Int = convert all to integer
#' String = convert to character
#' Name(s) = convert to character
#' Real = convert to numeric
#' Boolean = convert to logical and check if conversion was valid
#' (Long,Lat) = check that the coordinates are valid
#' YYYY-MM-DD or yyyy-mm-dd = check that year month and day are valid; no conversion
#' yyyy-mm-dd HH:MM:SS or yyyy-mm-dd hh:mm:ss = check that year month day h m s are valid; no conversion
#' DD.MM.YY HH:MM" or dd.mm.yy hh:mm = check that year month day h m s are valid and convert with to datetime as yyyy-mm-dd hh:mm:ss with lubridate
#'
#' @param data tibble to have columns checked
#' @param coltypes column types that are currently valid
#' @return Data with the column types checked. If coltypes was the first row of the input data, this will now be removed.
#' @examples projects = read_excel(path, sheet = "Projects"); projects = checkColTypes(projects)
#' @export
checkColTypes = function(data,coltypes=0){
  if (coltypes[1]==0){
    coltypes = data[1,]
    data = data[2:dim(data)[1],] }
  for (n in seq_along(data)){
    if (coltypes[n] == "Int"){data = data %>% mutate_at(n, as.integer)}
    if (coltypes[n] == "String"){data = data %>% mutate_at(n, as.character)}
    if (coltypes[n] == "Name(s)"){data = data %>% mutate_at(n, as.character)}
    if (coltypes[n] == "Real"){data = data %>% mutate_at(n, as.numeric)}
    if (coltypes[n] == "Boolean"){
      data[,n][[1]] = as.logical(data[,n][[1]])
      if (sum(is.na(data[,n][[1]])) > 0){print("invalid logical!")} }
    if (coltypes[n] == "YYYY-MM-DD" | coltypes[n] == "yyyy-mm-dd"){
      tmp = data[,n]
      tmp = separate(tmp,1,into=c("y","m","d"),sep="-") # date parts as characters
      tmp2 = mutate_at(tmp, 1:3, as.integer) # date parts as integers
      if (mean((nchar(tmp$y)==4 & tmp2$y>1990 & tmp2$y<2030) | is.na(tmp$y))==1){validy = TRUE} else {validy = FALSE} # check validity of year, month, day
      if (mean((nchar(tmp$m)==2 & tmp2$m<13) | is.na(tmp$m))==1){validm = TRUE} else {validm = FALSE}
      if (mean((nchar(tmp$d)==2 & tmp2$d<32) | is.na(tmp$d))==1){validd = TRUE} else {validd = FALSE}
      if (!validy | !validm | !validd){print("invalid date format!")}
    }
    if (coltypes[n] == "DD.MM.YY HH:MM" | coltypes[n] == "dd.mm.yy hh:mm"){
      tmp = data[,n]
      tmp = separate(tmp,1,into=c("d","m","y","hh","mm"),sep="[. :]") # date parts as characters
      tmp2 = mutate_at(tmp, 1:5, as.integer) # date parts as integers
      if (mean(nchar(tmp$y)==2 | is.na(tmp$y))==1){validy = TRUE} else {validy = FALSE} # check validity of year, month, day
      if (mean((nchar(tmp$m)==2 & tmp2$m<13) | is.na(tmp$m))==1){validm = TRUE} else {validm = FALSE}
      if (mean((nchar(tmp$d)==2 & tmp2$d<32) | is.na(tmp$d))==1){validd = TRUE} else {validd = FALSE}
      if (mean((nchar(tmp$hh)==2 & tmp2$hh<24) | is.na(tmp$hh))==1){validhh = TRUE} else {validhh = FALSE}
      if (mean((nchar(tmp$mm)==2 & tmp2$mm<61) | is.na(tmp$mm))==1){validmm = TRUE} else {validmm = FALSE}
      if (!validy | !validm | !validd | !validhh | !validmm){print("invalid date format!")}
      newdate = data[,n]
      newdate[which(tmp$y<50),] = paste0("20",tmp$y[which(tmp$y<50)],"-",tmp$m[which(tmp$y<50)],"-",tmp$d[which(tmp$y<50)]," ",tmp$hh[which(tmp$y<50)],":",tmp$mm[which(tmp$y<50)],":00")
      newdate[which(tmp$y>=50),] = paste0("19",tmp$y[which(tmp$y>=50)],"-",tmp$m[which(tmp$y>=50)],"-",tmp$d[which(tmp$y>=50)]," ",tmp$hh[which(tmp$y>=50)],":",tmp$mm[which(tmp$y>=50)],":00")
      data[,n] = ymd_hms(newdate[[1]])
      print("Date format was DD.MM.YY HH:MM; changed to yyyy-mm-dd HH:MM:SS")
    }
    if (coltypes[n] == "yyyy-mm-dd HH:MM:SS" | coltypes[n] == "yyyy-mm-dd hh:mm:ss"){
      tmp = data[,n]
      tmp = separate(tmp,1,into=c("y","m","d","hh","mm","ss"),sep="[- :]") # date parts as characters
      tmp2 = mutate_at(tmp, 1:6, as.integer) # date parts as integers
      if (mean(nchar(tmp$y)==4 | is.na(tmp$y))==1){validy = TRUE} else {validy = FALSE} # check validity of year, month, day
      if (mean((nchar(tmp$m)==2 & tmp2$m<13) | is.na(tmp$m))==1){validm = TRUE} else {validm = FALSE}
      if (mean((nchar(tmp$d)==2 & tmp2$d<32) | is.na(tmp$d))==1){validd = TRUE} else {validd = FALSE}
      if (mean((nchar(tmp$hh)==2 & tmp2$hh<24) | is.na(tmp$hh))==1){validhh = TRUE} else {validhh = FALSE}
      if (mean((nchar(tmp$mm)==2 & tmp2$mm<61) | is.na(tmp$mm))==1){validmm = TRUE} else {validmm = FALSE}
      if (mean((nchar(tmp$ss)==2 & tmp2$ss<61) | is.na(tmp$ss))==1){validss = TRUE} else {validss = FALSE}
      if (!validy | !validm | !validd | !validhh | !validmm | !validss){print("invalid date format!")}
    }
    if (coltypes[n] == "(Long,Lat)"){
      tmp = data[,n]
      tmp = separate(tmp,1,into=c("p1","lon","lat","p2"),sep="([(,)])") # coord parts as characters
      tmp2 = mutate_at(tmp, 2:3, as.numeric) # date parts as integers
      if (mean(tmp2$lon>-180 & tmp2$lon<180)==1){validlon = TRUE} else {validlon = FALSE} # check validity of year, month, day
      if (mean(tmp2$lat>-90 & tmp2$lat<90)==1){validlat = TRUE} else {validlat = FALSE}
      if (!validlon | !validlat){print("invalid coordinates!")}
    }
  }
  # fix db names also
  colnames(data) = dbSafeNames(colnames(data))
  return(data)
}

#' Function for creating "safe" names for postgres
#'
#' Removes protected characters and puts everything into lower case
#'
#' @param names names to be fixed; eg. from colnames(dataframe)
#' @param uniq if the names should all be unique set to true and _1, _2 etc. will be added (default = false)
#' @export
dbSafeNames = function(names,uniq=FALSE) {
  names = gsub('[^a-z0-9]+','_',tolower(names))
  names = gsub('.','_',names, fixed=TRUE)
  if (uniq){names = make.names(names, unique=TRUE, allow_=TRUE)}
  return(names)
}

#' Fix all columns identified as "id" into db safenames (these columns are often used as keys)
#'
#' This function finds all columns with "id" in their names, as these are expected to be primary
#' or foreign keys. dbSafeNames is applied to all IDs, as keys must conform to PostgreSQL requirements
#' for correct matching. Columns that have been fixed are listed in a print out.
#'
#' @param data data tibble to be checked and edited
#' @param uniq if the names should all be unique set to true and _1, _2 etc. will be added (default = false)
#' @export
FixIDs <- function(data,uniq=FALSE){
  tmp = grep("id",colnames(data))
  for (n in seq_along(tmp)){
    data[,tmp[n]][[1]] = dbSafeNames(data[,tmp[n]][[1]],uniq=uniq)
    print(paste0("Safe names for ",colnames(data)[tmp[n]]))
  }
  return(data)
}

#' Function for writing a tibble as a table the database
#'
#' This function will: i) connect to database, ii) write table; append if desired, iii) exit connection
#' @param values tibble to be added to the server
#' @param tableName name of table in the database
#' @param appendChoice TRUE to append to existing table of the same name; otherwise false
#' @param appendType match is the default; this means the existing and new tables have the same columns. "outer" can also be chosen for an outer join of existing and new data.
#' @param key column used to compare the old and new data; only new data unique in this column will be added (eg. use date or a key/id). With outer join appending, multiple keys are allowed. With match appending, so far only one key is implemented.
#' @keywords database
#' @export
writeValuesToDatabase <- function(values, tableName, appendChoice, appendType="match", key=0){
  on.exit(dbDisconnect(con))
  con <- dbConnect(drv = dbDriver("PostgreSQL"),
                   dbname = "c7701050", host = "db06.intra.uibk.ac.at",
                   port = 5432, user = user,
                   password = password)
  # case = no appending, new data
  if (!appendChoice){
    dbWriteTable(con, tableName, value = values, append = FALSE)
  }
  # case = append, same columns names in new and old data
  if (appendChoice & appendType=="match" & dbExistsTable(con, tableName)){
    # if appending, compare the pub id keys in the existing and new table and add only new data
    tmp = queryDatabase(paste0("SELECT * FROM ",tableName))
    values[,key][[1]] = dbSafeNames(values[,key][[1]])
    new = setdiff(values[,key][[1]],tmp[,key])
    if (length(new)>0){
      r = as.integer(new)
      for (n in 1:length(new)){r[n] = which(values[,key]==new[n])}
      dbWriteTable(con, tableName, value = values[r,], append = TRUE)
    } else { print("No new rows in table") }
  }
  # case = append, new columns added, use outer join
  if (appendChoice & appendType=="outer" & dbExistsTable(con, tableName)){
    print("Before appending new data:")
    prev_size = showSize(tableName)
    # compare the chosen keys in the existing and new table and add only new data
    for (n in seq_along(key)){ # get the key from the database
      tmp = queryDatabase(paste0("SELECT ",key[n]," FROM ",tableName))[[1]]
      if (class(tmp)[1]=="POSIXct"){ tmp = as.numeric(tmp) }
      tmp[is.na(tmp)] = "X"
      if (n==1){ db_key = dbSafeNames(tmp)
      } else { db_key = cbind(db_key,dbSafeNames(tmp)) }
    }
    for (n in seq_along(key)){ # get the key from the new data
      tmp = values[,key[n]][[1]]
      if (class(tmp)[1]=="POSIXct"){ tmp = as.numeric(tmp) }
      if (n==1){ v_key = dbSafeNames(tmp)
      } else { v_key = cbind(v_key,dbSafeNames(tmp)) }
    }
    for (n in seq_along(key)){ # combine multiple keys into a single string for better comparison
      if (n == 1){ db_key_comb = db_key[,1]
      v_key_comb = v_key[,1]
      } else { db_key_comb = paste(db_key_comb,db_key[,n],sep="_")
      v_key_comb = paste(v_key_comb,v_key[,n],sep="_")
      }
    }
    new = setdiff(v_key_comb,db_key_comb)
    # find which columns are new and add these to the database
    values = values[,colnames(values)!="X1"]
    existing_cols = queryDatabase(paste0("SELECT column_name FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = '",tableName,"';"))[,1]
    new_cols = setdiff(colnames(values),existing_cols)
    for (n in seq_along(new_cols)){
      type = class(values[,new_cols[n]][[1]])[1]
      if (type=="POSIXct"){type="timestamp"}
      else if (type=="logical"){type="boolean"}
      else if (type!="numeric"){print("column type is not recognised and needs to be added to function")}
      queryDatabase(paste0("ALTER TABLE ",tableName," ADD COLUMN ",new_cols[n]," ",type,";"))
    }
    if (length(new)!=0){
      print("New columns were added:")
      new_cols
    } else {print("No new columns were added.")}
    # add the new data to the database
    if (length(new)>0){
      r = as.integer(new)
      for (n in 1:length(new)){r[n] = which(v_key_comb==new[n])}
      newdata = data.frame(values[r,])
      for (n in seq_along(newdata[1,])){
        if (!is.POSIXct(newdata[,n])){
          tmp = which(newdata[,n]>99999999 | newdata[,n]=="Inf")
          newdata[tmp,n] = NA # get rid of Inf values (not allowed in postgres)
        } }
      dbWriteTable(con, tableName, value = newdata, append = TRUE)
    } else { print("No new rows in table") }
    print("After appending new data:")
    post_size = showSize(tableName)
  }
  # case = table does't exist and can't append
  if (appendChoice & !dbExistsTable(con, tableName)){ print("Cannot append; table does not exist") }
}

#' Function for writing a single new dataset from a csv to the database
#' This function will: i) connect to database, ii) drop the old table if exists and write the new table,
#' iii) add datasetid column and foreign key referencing datasets table, iv) exit connection
#' @param base_path Path from root to the data folder (normally set as the server: "/Volumes/daten_inst/oekophysiologie/" but you must be connected to the server for this to work)
#' @param data_path Path from the base path data folder to the csv data file
#' @param datasetid ID for the dataset to be added (the dataset must already have been added to the datasets table!)
#' @keywords database
#' @examples New example, test online
#' @export
writeNewDataset_single <- function(data_path, datasetid, base_path="/Volumes/daten_inst/oekophysiologie/"){
  pwd = getwd()
  setwd(base_path)
  datasetid = dbSafeNames(datasetid)
  # first drop any tables that already exist with the same name
  queryDatabase(paste0("DROP TABLE IF EXISTS ",paste0("z_",datasetid)," CASCADE;"))
  # get the data into R
  data = read_csv(data_path, col_names = TRUE)
  data2 = read_csv(data_path, col_names = TRUE,col_types = cols(.default = "d"))
  types = sapply(data, class) # check logicals are correct (1000 NA values is parsed as logical)
  for (n in seq_along(types)){
    if (types[n]=="logical"){
      if (length(unique(data2[,n])[[1]])>3){ # if multiple unique values it is not logical
        data[,n] = data2[,n]
      }
    }
  }
  colnames(data) = dbSafeNames(colnames(data))
  # check the columns names of data and metadata match
  writeValuesToDatabase(values = data, tableName = paste0("z_",datasetid), appendChoice = FALSE)
  # add dataset id and foreign key
  queryDatabase(paste0("ALTER TABLE ",paste0("z_",datasetid)," ADD COLUMN datasetID nchar(10);"))
  queryDatabase(paste0("UPDATE ",paste0("z_",datasetid)," SET datasetID = '",datasetid,"';"))
  queryDatabase(paste0("ALTER TABLE ",paste0("z_",datasetid)," ADD CONSTRAINT ref_datasetid FOREIGN KEY (datasetid) REFERENCES datasets (id_dataset);"))
  setwd(pwd)
}

#' Function for writing a new dataset and associated metadata file(s) from a csv to the database
#'
#' This function will: i) connect to database, ii) drop the old tables if they exist and write the new tables,
#' iii) check that all the data columns are contained in the main metadata table
#' iv) add datasetid columns and foreign key referencing datasets table, v) exit connection
#' @param base_path Path from root to the data folder (normally set as the server: "/Volumes/daten_inst/oekophysiologie/" but you must be connected to the server for this to work)
#' @param data_path Path from the base path data folder to the csv data file
#' @param metadata_path Path from the base path data folder to the csv main metadata data file
#' @param othermeta Path from the base path data folder to extra csv metadata data files (optional)
#' @param datasetid ID for the dataset to be added (the dataset must already have been added to the datasets table!)
#' @keywords database
#' @export
writeNewDataset <- function(data_path, metadata_path, datasetid, base_path="/Volumes/daten_inst/oekophysiologie/", othermeta=0){
  pwd = getwd()
  setwd(base_path)
  datasetid = dbSafeNames(datasetid)
  # first drop any tables that already exist with the same name
  queryDatabase(paste0("DROP TABLE IF EXISTS ",paste0("z_",datasetid)," CASCADE;"))
  queryDatabase(paste0("DROP TABLE IF EXISTS ",paste0("z_",datasetid,"_metadata")," CASCADE;"))
  queryDatabase(paste0("DROP TABLE IF EXISTS ",paste0("z_",datasetid,"_exptdata")," CASCADE;"))
  # get the data into R
  data = read_csv(data_path, col_names = TRUE)
  colnames(data) = dbSafeNames(colnames(data))
  metadata = read_csv(metadata_path, col_names = TRUE)
  colnames(metadata) = dbSafeNames(colnames(metadata))
  # check the columns names of data and metadata match
  tmp = setdiff(colnames(data),colnames(metadata))
  tmp1 = setdiff(colnames(metadata),colnames(data))
  a = "y"
  if (length(tmp)>0){
    print(paste0("Column *",str_c(tmp,collapse = " "),"* is present in data but not in metadata; data cannot be accepted"))
    a="n" }
  if (length(tmp1)>0){
    print(paste0("Column *",str_c(tmp1,collapse = " "),"* is present in metadata but not in data")) }
  # if column names accepted, check and add the data
  if (a=="y"){
    r = which(metadata[,1]=="ColType" | metadata[,1]=="Type" | metadata[,1]=="coltype" | metadata[,1]=="type")
    if (length(r)!=1){print("Column types not found in metadata, column types will not be checked!")
    } else { data = checkColTypes(data,coltypes=metadata[r,colnames(data)]) }
    writeValuesToDatabase(values = data, tableName = paste0("z_",datasetid), appendChoice = FALSE)
    writeValuesToDatabase(values = metadata, tableName = paste0("z_",datasetid,"_metadata"), appendChoice = FALSE)
    # add dataset id and foreign key
    queryDatabase(paste0("ALTER TABLE ",paste0("z_",datasetid)," ADD COLUMN datasetID nchar(10);"))
    queryDatabase(paste0("UPDATE ",paste0("z_",datasetid)," SET datasetID = '",datasetid,"';"))
    queryDatabase(paste0("ALTER TABLE ",paste0("z_",datasetid)," ADD CONSTRAINT ref_datasetid FOREIGN KEY (datasetid) REFERENCES datasets (id_dataset);"))
    queryDatabase(paste0("ALTER TABLE ",paste0("z_",datasetid,"_metadata")," ADD COLUMN datasetID nchar(10);"))
    queryDatabase(paste0("UPDATE ",paste0("z_",datasetid,"_metadata")," SET datasetID = '",datasetid,"';"))
    queryDatabase(paste0("ALTER TABLE ",paste0("z_",datasetid,"_metadata")," ADD CONSTRAINT ref_datasetid FOREIGN KEY (datasetid) REFERENCES datasets (id_dataset);"))
  }
  if (othermeta!=0){
    othermetadata = read_csv(othermeta, col_names = TRUE)
    colnames(othermetadata) = dbSafeNames(colnames(othermetadata))
    writeValuesToDatabase(values = othermetadata, tableName = paste0("z_",datasetid,"_exptdata"), appendChoice = FALSE)
    queryDatabase(paste0("ALTER TABLE ",paste0("z_",datasetid,"_exptdata")," ADD COLUMN datasetID nchar(10);"))
    queryDatabase(paste0("UPDATE ",paste0("z_",datasetid,"_exptdata")," SET datasetID = '",datasetid,"';"))
    queryDatabase(paste0("ALTER TABLE ",paste0("z_",datasetid,"_exptdata")," ADD CONSTRAINT ref_datasetid FOREIGN KEY (datasetid) REFERENCES datasets (id_dataset);"))
  }
  setwd(pwd)
}

#' Function for combining files into a single csv
#'
#' Specify a folder, a format and a pattern, and all files of the specified format with the
#' pattern in their name in the chosen folder will be combined. The name of the output combined
#' file is given under the "save" input; if this is not include the data will be combined but not
#' automatically saved.
#' Note: all files to be combined MUST have the same column names!
#' @param path The folder containing the data to be combined
#' @param filepattern A string pattern that must be contained in the names of files to be combined
#' @param fileformat The format of the files to be combined
#' @param sheetchoice For excel files: the name of the sheet to be used
#' @param save The name of the combined file to be saved (default = "n" = don't save)
#' @export
combineFiles = function(path,filepattern,fileformat,sheetchoice="Daten",save="n"){
  filenames = list.files(path)
  filenames = filenames[grep(filepattern,filenames)]
  if (fileformat=="xlsx"){
    for (n in seq_along(filenames)){
      if (n==1){
        names = colnames(read_excel(paste0(path,filenames[n]),skip=1,sheet=sheetchoice))
        data = read_excel(paste0(path,filenames[n]),col_names=names,skip=4,sheet=sheetchoice)
      } else {
        newnames = colnames(read_excel(paste0(path,filenames[n]),skip=1,sheet=sheetchoice))
        newdata = read_excel(paste0(path,filenames[n]),col_names=names,skip=4,sheet=sheetchoice)
        data = bind_rows(data,newdata)
      }
    } }
  if (fileformat=="dat"){
    for (n in seq_along(filenames)){
      print(n)
      if (n==1){
        names = colnames(read_delim(paste0(path,filenames[n]),skip=1,delim=","))
        data = read_delim(paste0(path,filenames[n]),col_names=names,skip=4,delim=",")
      } else {
        names = colnames(read_delim(paste0(path,filenames[n]),skip=1,delim=","))
        newdata = read_delim(paste0(path,filenames[n]),col_names=names,skip=4,delim=",")
        data = bind_rows(data,newdata)
      }
    } }
  if (save!="n"){
    write.csv(data,paste0(path,save,".csv"))
  }
  return(data)
}


