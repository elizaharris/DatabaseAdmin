
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
                   port = 5432,
                   user = keyring::key_list("databaseadminlogin", keyring ="DBcredentials")[1,2],
                   password = keyring::key_get("databaseadminlogin", "c7701050", keyring ="DBcredentials"))
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
#' @param appendType "match" is the default; this means the existing and new tables have the same columns. "outer" can also be chosen for an outer join of existing and new data.
#' @param key column used to compare the old and new data; only new data unique in this column will be added (eg. use date or a key/id). With outer join appending, multiple keys are allowed. With match appending, so far only one key is implemented.
#' @keywords database
#' @export
writeValuesToDatabase <- function(values, tableName, appendChoice=FALSE, appendType="match", key=0){
  on.exit(dbDisconnect(con))
  con <- dbConnect(drv = dbDriver("PostgreSQL"),
                   dbname = "c7701050", host = "db06.intra.uibk.ac.at",
                   port = 5432,
                   user = keyring::key_list("databaseadminlogin", keyring ="DBcredentials")[1,2],
                   password = keyring::key_get("databaseadminlogin", "c7701050", keyring ="DBcredentials"))
  # case = no appending, new data
  if (!appendChoice){
    dbWriteTable(con, tableName, value = values, append = FALSE)
  }
  # if appending, check row matchs
  if (appendChoice){
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
      if (n == 1){ db_key_comb = db_key
      v_key_comb = v_key
      } else { db_key_comb = paste(db_key_comb,db_key[,n],sep="_")
      v_key_comb = paste(v_key_comb,v_key[,n],sep="_")
      }
    }
    newrows = setdiff(v_key_comb,db_key_comb)
  }
  # case = append, same columns names in new and old data
  if (appendChoice & appendType=="match" & dbExistsTable(con, tableName)){
    if (length(newrows)>0){
      r = suppressWarnings(as.integer(newrows))
      for (n in 1:length(newrows)){r[n] = which(v_key_comb==newrows[n])}
      dbWriteTable(con, tableName, value = values[r,], append = TRUE)
    } else { print("No new rows in table") }
  }
  # case = append, new columns added, use outer join
  if (appendChoice & appendType=="outer" & dbExistsTable(con, tableName)){
    print("Before appending new data:")
    prev_size = showSize(tableName)
    # find which columns are new and add these to the database
    values = values[,colnames(values)!="X1"]
    colnames(values) = dbSafeNames(colnames(values))
    for (x in seq_along(colnames(values))){colnames(values)[x] = str_replace(colnames(values)[x],"-","")}
    existing_cols = queryDatabase(paste0("SELECT column_name FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = '",tableName,"';"))[,1]
    existing_cols = dbSafeNames(existing_cols)
    new_cols = setdiff(colnames(values),existing_cols)
    for (n in seq_along(new_cols)){
      type = class(values[,new_cols[n]][[1]])[1]
      if (type=="POSIXct"){type="timestamp"
      } else if (type=="logical"){type="boolean"
      } else if (type!="numeric"){print("column type is not recognised and needs to be added to function")}
      queryDatabase(paste0("ALTER TABLE ",tableName," ADD COLUMN ",new_cols[n]," ",type,";"))
    }
    if (length(new_cols)!=0){
      print("New columns were added:")
      new_cols
    } else {print("No new columns were added.")}
    # add the new data to the database
    if (length(newrows)>0){
      r = suppressWarnings(as.integer(newrows))
      for (n in 1:length(newrows)){r[n] = which(v_key_comb==newrows[n])}
      newdata = data.frame(values[r,])
      for (n in seq_along(newdata[1,])){
        if (!is.POSIXct(newdata[,n]) && !is.character(newdata[,n])){
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

#' Function for writing updating line(s) of a table already in the database
#'
#' This function will: i) connect to database, ii) find lines matching keyvalue[1] in the designated key
#' column of the specified table and replace these line(s) of data with the line matching
#' keyvalue[1] in the new data table, iii) repeat for keyvalues[n], iv) exit connection
#' @param values updated data to go into table
#' @param tableName name of table in the database
#' @param key column used to compare the old and new data; new data will be updated where key matches keyvalue(s)
#' @param keyvalues value(s) used to compare the old and new data; do not set this parameter if you wish to
#' check all lines (can be slow!). More than one value can be set eg. c(key1,key2,...keyn).
#' @keywords database
#' @export
editValuesInDatabase <- function(values, tableName, key, keyvalues=NA){
  on.exit(dbDisconnect(con))
  con <- dbConnect(drv = dbDriver("PostgreSQL"),
                   dbname = "c7701050", host = "db06.intra.uibk.ac.at",
                   port = 5432,
                   user = keyring::key_list("databaseadminlogin", keyring ="DBcredentials")[1,2],
                   password = keyring::key_get("databaseadminlogin", "c7701050", keyring ="DBcredentials"))
  # if no keyvalues were input, use check all keyvalues
  if (is.na(keyvalues[1])){keyvalues = values[,key][[1]]}
  # check the keyvalues one by one...
  for (n in seq_along(keyvalues)){
      # get the matching row(s) from the database
      tmp = queryDatabase(paste0("SELECT * FROM ",tableName," WHERE ",key," = '",keyvalues[n],"'"))
      # get the matching row(s) from the new data table
      tmp2 = values[which(values[key]==keyvalues[n]),]
      # update the columns that need updating
      if (dim(tmp2)[1]!=1){
        print(paste0("Error: More than one column in the new data matches the keyvalue: ",keyvalues[n]," - data cannot be replaced"))
      } else {
        for (i in seq_along(tmp2)){
          tmpname = colnames(tmp2)[i]
          if (is.na(tmp2[tmpname])){tmp2[tmpname]="NA"}
          if (is.na(tmp[tmpname][1])){tmp[tmpname][1]="NA"}
          if (tmp2[tmpname] != tmp[tmpname][1]){
            # update the value
            queryDatabase(paste0("UPDATE ",tableName,
            " SET ",tmpname," = '",tmp2[tmpname],
            "' WHERE ",key," = '",keyvalues[n],"' RETURNING *"))
            # print confirmation of the update
            print(paste0("For ",keyvalues[n],"; column ",tmpname,
                         "; ",tmp[tmpname][1]," was changed to ",tmp2[tmpname]))
          }
        }
      }
  }
}

#' Function for writing a single new dataset from a csv to the database
#' This function will: i) connect to database, ii) drop the old table if exists and write the new table,
#' iii) add datasetid column and foreign key referencing datasets table, iv) exit connection
#' @param base_path Path from root to the data folder (normally set as the server: "/Volumes/daten_inst/oekophysiologie/" but you must be connected to the server for this to work)
#' @param data_path Path from the base path data folder to the csv data file
#' @param datasetid ID for the dataset to be added (the dataset must already have been added to the datasets table!)
#' @param converttime If 0, dataset is added as normal. If "timename" then the column timename is converted from PosixCT to a time string before it is added to the database. It is then converted back to a timestamp in Postgres. The reason for this is because timestamps are causing R to crash in newest versions of DBI?
#' @param maxlines Maximum number of lines that should be read from the csv. Default is Inf (all lines), can be set lower for very large files.
#' @keywords database
#' @examples New example, test online
#' @export
writeNewDataset_single <- function(data_path, datasetid, base_path="/Volumes/daten_inst/oekophysiologie/",converttime=0,maxlines=Inf){
  pwd = getwd()
  setwd(base_path)
  datasetid = dbSafeNames(datasetid)
  # first drop any tables that already exist with the same name
  queryDatabase(paste0("DROP TABLE IF EXISTS ",paste0("z_",datasetid)," CASCADE;"))
  # get the data into R
  data_alltypes = data.frame(read_csv(data_path, col_names = TRUE,n_max=maxlines))
  data = data.frame(read_csv(data_path, col_names = TRUE,col_types = cols(.default = "d"),n_max=maxlines))
  types = sapply(data_alltypes, class) # most logicals are not correct (1000 NA values is parsed as logical)
  tmp = which(sapply(types,function(x){x[[1]]=="POSIXct" | x[[1]]=="character"}))
  for (n in seq_along(tmp)){ data[,tmp[n]] = data_alltypes[,tmp[n]] } # this puts the non-logical columns into data
  rm(data_alltypes)
  colnames(data) = dbSafeNames(colnames(data))
  #data = as_tibble(data)
  # check the columns names of data and metadata match
  if (converttime==0){
    writeValuesToDatabase(values = data, tableName = paste0("z_",datasetid), appendChoice = FALSE)
  } else {
    data[,converttime] = format(data[,converttime], "%Y-%m-%d %H:%M:%S")
    writeValuesToDatabase(values = data, tableName = paste0("z_",datasetid), appendChoice = FALSE)
    queryDatabase(paste0("ALTER TABLE ",paste0("z_",datasetid)," ALTER COLUMN ",converttime,
                         " TYPE TIMESTAMP WITH TIME ZONE USING to_timestamp(",converttime,", 'YYYY-MM-DD HH24:MI:SS');"))
  }
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
#' @param sheetchoice For excel files: the name of the sheet to be used (enter NULL if only one sheet and you don't want to specify name)
#' @param skiphead Number of lines to be skipped before colnames are given (default = 1 as MM data has colnames in second line)
#' @param skipdata Number of lines to be skipped before data begins (default = 4 as MM data has 4 header lines)
#' @param save The name of the combined file to be saved (default = "n" = don't save)
#' @export
combineFiles = function(path,filepattern,fileformat,sheetchoice="Daten",save="n",skiphead=1,skipdata=4){
  filenames = list.files(path)
  filenames = filenames[grep(filepattern,filenames)]
  if (length(filenames)==0){print(paste0("No matching files in ",path)); return(0)}
  if (fileformat=="xlsx"){
    for (n in seq_along(filenames)){
      if (n==1){
        names = colnames(read_excel(paste0(path,filenames[n]),skip=skiphead,sheet=sheetchoice))
        data = read_excel(paste0(path,filenames[n]),col_names=names,skip=skipdata,sheet=sheetchoice)
      } else {
        newnames = colnames(read_excel(paste0(path,filenames[n]),skip=skiphead,sheet=sheetchoice))
        newdata = read_excel(paste0(path,filenames[n]),col_names=names,skip=skipdata,sheet=sheetchoice)
        data = bind_rows(data,newdata)
      }
    } }
  if (fileformat=="dat"){
    for (n in seq_along(filenames)){
      print(n)
      if (n==1){
        names1 = colnames(read_delim(paste0(path,filenames[n]),skip=skiphead,delim=","))
        data = read_delim(paste0(path,filenames[n]),col_names=names1,skip=skipdata,delim=",")
      } else {
        names = colnames(read_delim(paste0(path,filenames[n]),skip=skiphead,delim=","))
        newdata = read_delim(paste0(path,filenames[n]),col_names=names,skip=skipdata,delim=",")
        if (path == "/Volumes/daten_inst/oekophysiologie/Data/Old_Datenbank/Dokumentationen/ClimLUC/Data/Meteo 2020/CR1000_MC2_MR2/dat/"){
          print("Colnames changed in summer 2020! Using old names")
          colnames(newdata) = names1
        }
        data = bind_rows(data,newdata)
      }
    } }
  if (fileformat=="txt"){
    for (n in seq_along(filenames)){
      print(n)
      if (n==1){
        names = colnames(read_delim(paste0(path,filenames[n]),skip=skiphead,delim="\t"))
        data = read_delim(paste0(path,filenames[n]),col_names=names,skip=skipdata,delim="\t")
      } else {
        names = colnames(read_delim(paste0(path,filenames[n]),skip=skiphead,delim="\t"))
        newdata = read_delim(paste0(path,filenames[n]),col_names=names,skip=skipdata,delim="\t")
        data = bind_rows(data,newdata)
      }
    } }
  if (save!="n"){
    write.csv(data,paste0(path,save,".csv"))
  }
  return(data)
}

#' Function to unify the names between the old and new micromet
#'
#' This function gets the names from the adminstuff metadata table and unifies them from the old
#' micromet data so that they match the new micromet metadata scheme
#' 1. create the x_micromet table with the unedited data
#' 2. split into a data and an uncertainty (std) table
#' 3. unify the data table names with the naming scheme for the newer metadata
#' 4. add to the database
#' @export
fixOldMetadata <- function(){
  # add the raw data to the database (run this if needed!)
  #queryDatabase("DROP TABLE IF EXISTS x_micromet CASCADE")
  #tmp = allLegacyMicrometData
  #colnames(tmp) = make.names(colnames(allLegacyMicrometData),unique=TRUE)
  #writeValuesToDatabase(values = tmp, tableName = "x_micromet", appendChoice = FALSE)
  #rm(tmp)
  # split into good names and not updated names
  std_columns = sapply(colnames(allLegacyMicrometData),FUN=function(x){StrMatch(x,str1="_Std") | StrMatch(x,str1="std")})
  allLegacyMicrometData_data = allLegacyMicrometData[,!std_columns]
  allLegacyMicrometData_unc = allLegacyMicrometData[,std_columns]
  # get the column names of the old version
  existing_cols = colnames(allLegacyMicrometData_data)
  # match the column names using the table Johnny generated for the new naming scheme
  mm_metadata = queryDatabase("SELECT * FROM adminstuff.micrometmetadata")
  newnames = mm_metadata$`colnamesnew-varname_offset_unit_treatment_duplicate_location`
  # first quick match to newnames
  tmp = map(existing_cols,function(x){which(newnames==x)})
  newcolnames = matrix(nrow = 1,ncol=length(existing_cols))
  for (n in 1:length(tmp)){
    if (length(tmp[[n]])==0){
      tmp[[n]] = which(mm_metadata$colnames_original==existing_cols[n]) }
    if (length(tmp[[n]])==0){
      tmp[[n]] = which(mm_metadata$alt_names==existing_cols[n]) }
    if (length(tmp[[n]])!=0){ newcolnames[n]=newnames[tmp[[n]][1]] }
  }
  # check for duplicate naming and merge columns
  newcolnames = newcolnames[seq_along(newcolnames)]
  dupnames = which(duplicated(newcolnames))
  nomoredups = 0;
  while (nomoredups == 0){
    for (n in seq_along(dupnames)){
      if (is.na(newcolnames[dupnames[n]])){next}
      tmp = which(newcolnames == newcolnames[dupnames[n]])
      combine = NA
      # check if data overlap
      overlap = sum(!is.na(allLegacyMicrometData_data[[tmp[1]]]) & !is.na(allLegacyMicrometData_data[[tmp[2]]]))/length(allLegacyMicrometData_data[[tmp[2]]])*100
      # check if data are different
      difference = t.test(allLegacyMicrometData_data[[tmp[1]]],allLegacyMicrometData_data[[tmp[2]]])$p.value
      # check if data are identical
      identical = abs(mean(allLegacyMicrometData_data[[tmp[1]]]-allLegacyMicrometData_data[[tmp[2]]],na.rm=TRUE)/mean(allLegacyMicrometData_data[[tmp[2]]],na.rm=TRUE)*100)
      if (overlap == 0){identical = 100}
      # combine as needed
      if ((overlap<1 & difference > 0.001) | identical < 1){ # merge if similar and not overlapping
        allLegacyMicrometData_data[[tmp[1]]][is.na(allLegacyMicrometData_data[[tmp[1]]])] = allLegacyMicrometData_data[[tmp[2]]][is.na(allLegacyMicrometData_data[[tmp[1]]])]
        newcolnames[tmp[2]] = "merged"
        print(paste0(n,": ",newcolnames[tmp[1]]," merged (cols ",tmp[1]," & ",tmp[2],")"))
      } else { # mark as duplicate if not similar or overlapping
        strlen = str_length(newcolnames[tmp[2]])
        currentdup = as.numeric(substr(newcolnames[tmp[2]],strlen,strlen))
        duplicatename = paste0(substr(newcolnames[tmp[2]],0,strlen-1),currentdup+1)
        while (sum(newcolnames == duplicatename,na.rm=TRUE)==1){ # make sure we're not creating a double duplicate...
          currentdup = currentdup + 1
          duplicatename = paste0(substr(newcolnames[tmp[2]],0,strlen-1),currentdup+1)
        }
        print(paste0(n,": ",newcolnames[tmp[2]]," assigned to ",duplicatename," (cols ",tmp[1]," & ",tmp[2],")"))
        newcolnames[tmp[2]] = duplicatename
      }
    }
    dupnames = which(duplicated(newcolnames))
    if (sum(!is.na(newcolnames[dupnames]) & newcolnames[dupnames]!="merged",na.rm=TRUE)==0){nomoredups = 1}
  }
  # check the matches and replace the colnames
  newcolnames[existing_cols=="subsite"] = "subsite" # add any manual col names...
  newcolnames = dbSafeNames(newcolnames)
  for (n in seq_along(newcolnames)){
    if (!is.na(newcolnames[n])){
      print(paste0(n," - ",existing_cols[n]," : ",newcolnames[n] ))
      #newcolnames[n] = mm_metadata$`colnamesnew-varname_offset_unit_treatment_duplicate_location`[tmp[[n]]]
    } else { print(paste0(n," - ",existing_cols[n]," : NO MATCH"))}
  }
  # split into good names and not updated names
  good_columns = which(!is.na(newcolnames) & newcolnames!="merged")
  allLegacyMicrometData_data = allLegacyMicrometData_data[,good_columns]
  colnames(allLegacyMicrometData_data) = newcolnames[good_columns]
  # add to the database
  queryDatabase("DROP TABLE IF EXISTS z_micromet CASCADE")
  writeValuesToDatabase(values = allLegacyMicrometData_data, tableName = "z_micromet", appendChoice = FALSE)
}

#' Function to run an R query that needs a con
queryInR <- function(query){
  on.exit(dbDisconnect(con))
  con <- dbConnect(drv = dbDriver("PostgreSQL"),
                   dbname = "c7701050", host = "db06.intra.uibk.ac.at",
                   port = 5432,
                   user = keyring::key_list("databaseadminlogin", keyring ="DBcredentials")[1,2],
                   password = keyring::key_get("databaseadminlogin", "c7701050", keyring ="DBcredentials"))
  eval(parse(text=query))
}

# Internal functions
StrMatch = function(str1,str2){return(sum(grep(str1,str2))==1)}
