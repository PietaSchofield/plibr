#' get results from nicks spike-in db
#'
#' @param expid 
#' @param custom custom sql string
#'
#' @export
getSpikeInNorm <- function(expID=NULL,custom=NULL){
  require(RMySQL)
  drv <- DBI::dbDriver("MySQL")
  dbCon <- tryCatch(RMySQL::dbConnect(drv,group="spikein"),
                    condition = function(c){
                                 stop("could not open database")
                                }
                    )

  if(is.null(expID)){
    if(is.null(custom)){
      sSQL <- "SELECT id, title FROM spikeinDB_experiment"
    }else if(custom=="spikes"){
      sSQL="SELECT * from spikeinDB_spikein"
    }else{
      sSQL=custom
    }
  }else{
    if(is.null(custom)){
      sSQL <- paste0("SELECT DISTINCT e.id, n.group_id, n.name, n.value ",
                   "FROM spikeinDB_experiment e, spikeinDB_sample s, ", 
                   "spikeinDB_group g, spikeinDB_sample_groups sg, ",
                   "spikeinDB_normalization n ",
                   "WHERE e.id = s.experiment_id and s.id=sg.sample_id and ",
                   "sg.group_id=g.id and n.group_id=g.id and ",
                   "sg.group_id=n.group_id and e.id= ",expID," ",
                   "ORDER by g.id")
    }else if(custom=="data"){
      sSQL=paste0("SELECT DISTINCT  s.name, sp.ercc_id, r.value, r.reads_per_kilobase ",
                    "FROM spikeinDB_experiment e, spikeinDB_dataset d, ", 
                    "spikeinDB_group g, spikeinDB_dataset_groups dg, ",
                    "spikeinDB_sample s, spikeinDB_raw_count r , spikeinDB_spikein sp ",
                    "WHERE e.id = s.experiment_id and d.id=dg.dataset_id and ",
                    "dg.group_id=g.id and d.sample_id=s.id and r.dataset_id=d.id ",
                    "and e.id= ",expID," ",
                    "ORDER by g.id")
    }else{
      sSQL=custom
    }
  }
  rset <- RMySQL::dbSendQuery(dbCon,sSQL)
  res <- RMySQL::fetch(rset)
  RMySQL::dbClearResult(rset)
  RMySQL::dbDisconnect(dbCon)
  detach("package:RMySQL")
  res
}


