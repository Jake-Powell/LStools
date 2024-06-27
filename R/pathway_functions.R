
#' Remove factors from a data frame.
#'
#' @param df  data frame
#'
#' @return data frame without factors
#'
remove.factors = function(df) {
  for(varnum in 1:length(df)) {
    if("factor" %in% class(df[,varnum])) {
      df[varnum]=as.character(df[,varnum])
    }
  }
  return(df)
}

#' Filter the raw data
#'
#' @param data data
#' @param filters filters
#' @param union union
#'

#' @export
#'
filter_data <- function(data,filters, union = T){
  #Get information on the filters.
  no_filters = length(filters)
  if(no_filters == 0){
    return(data)
  }

  cat_val = strsplit(filters, ": ")

  # If union == True, use or on filters.
  if(union){
    # Manipulate cat_val to be a list of available options for each category.
    if (!(no_filters == 0)){
      cat = unique(unlist(lapply(cat_val, `[[`, 1)))
      cat_val_joined = vector(mode = "list", length = length(cat))
      names(cat_val_joined) = cat
      for(i in 1:length(cat_val)){
        cat_val_joined[[cat_val[[i]][1] ]] = c(cat_val_joined[[cat_val[[i]][1] ]],cat_val[[i]][2] )
      }

      data_cur = data
      for(i in 1:length(cat_val_joined)){
        data_cur = dplyr::filter(data_cur, !!as.symbol(names(cat_val_joined)[i]) %in% cat_val_joined[[i]])
      }

    }
  }
  # If union == False, use intersection on filters.
  else if(!union){
    data_cur = data

    if (!(no_filters == 0)){
      for(i in 1:no_filters){
        z = cat_val[[i]]
        data_cur = dplyr::filter(data_cur, !!as.symbol(z[1]) == z[2])
      }
    }
  }
  else{
    print('Invalid union variaible must be TRUE/FALSE')
    return()
  }


  return(data_cur)
}

#' Convert raw information (individual students) into pathways
#'
#' This function is to convert raw student information into flows of students through the pipeline.
#'  E.g. in raw each row is a unique student and each column contains information about the student such as grade or ethnicity.
#'  The function assumes each column is a variable we want to calculate flows for. Therefore, omit columns you don't need before using this function.
#' @param raw raw data where each row is a unique student.
#'
#' @return pathways, the number of students for each unique pathway.
#' @export
#'
raw_to_pathway = function(raw){
  flows = raw |> dplyr::group_by_all() |> dplyr::count()
  names(flows)[length(names(flows))] = 'No_students'
  return(flows)
}


#' Remove columns from pathways
#'
#'A function to remove columns from the pathway tibble, merging now identical rows and summing their No_students.
#'
#' @param pathways data frame containing pathways.
#' @param rm_columns The columns names in pathways that are to be removed.
#' @param keep_columns The columns names in pathways that are to be kept.
#' @param convert_na Logical (TRUE/FALSE) for whether we convert NA.
#' @param count_column Default = 'No_students'. The name of the column containing the number of students.
#'
#' @return pathways with columns removed and merged counts.
#' @export
#'
pathway_remove_columns <- function(pathways,
                                   rm_columns=NA,
                                   keep_columns = NA,
                                   convert_na = T,
                                   count_column = 'No_students'
                                   ){

  # A) check whether we have rm_ or keep_ columns and if keep convert to remove.
  if(is.na(rm_columns[1]) && is.na(keep_columns[1])){
    stop('Must specify either rm_columns or keep_columns!')
  }
  if(!is.na(rm_columns[1]) && !is.na(keep_columns[1])){
    stop('Must specify ONLY one of rm_columns or keep_columns!')
  }
  if(is.na(rm_columns[1]) && !is.na(keep_columns[1])){
    # Check keep columns in names of pathways.
    column_indices = match(keep_columns, names(pathways))
    if(any(is.na(column_indices))){
      issue_columns = rm_columns[which(is.na(column_indices))]
      stop(paste0('Error! The following keep_columns are not names of columns in pathways: ', paste0(issue_columns,collapse = ', '), '.', collapse=''))
    }
    columns = names(pathways)[-length(names(pathways))] # remove No_student from available col names.
    rm_columns = columns[!columns %in% keep_columns]
  }

  # 1) check the columns belong to pathways.
  column_indices = match(rm_columns, names(pathways))
  if(any(is.na(column_indices))){
    issue_columns = rm_columns[which(is.na(column_indices))]
    print(paste0('Error! The following rm_columns are not names of columns in pathways: ', paste0(issue_columns,collapse = ', '), '.', collapse=''))
    return()
  }

  # 1b) Sway NA with placeholder.
  if(convert_na){
    columns = names(pathways)[names(pathways)!= count_column]
    p=as.data.frame(pathways) ; p = remove.factors(p)
    p[,columns][is.na(p[,columns])] = 'Student not in stage'
    # pathways = tibble::tibble(p)
  }

  # 2) Remove the columns from pathways
  pathways_new = pathways[,-column_indices]

  # 3) Merge matching rows.
  by_elements = names(pathways_new)[names(pathways_new)!= count_column]
  aggregate_formula = paste0(count_column,'~', paste0(by_elements,collapse = '*'), collapse='')
  pathways_new = stats::aggregate(stats::as.formula(aggregate_formula),data=pathways_new,FUN=sum)

  return(pathways_new)
}


#' Composition of pathways by columns
#'
#' A function to get the summary of each column in pathways. I.e the Number of students in each stage/describer and how their split.
#'
#' @param pathways data frame containing pathways.
#' @param convert_na Logical (TRUE/FALSE) for whether we convert NA.
#'
#' @export
#'
pathway_summary_columns <- function(pathways, convert_na = T){
  columns = names(pathways)[names(pathways)!= 'No_students']

  #Convert NA values to student not in stage.
  if(convert_na){
    p=as.data.frame(pathways) ; p = remove.factors(p)
    p[,columns][is.na(p[,columns])] = 'Student not in stage'
    pathways = tibble::tibble(p)
  }

  output = list()
  # Loop over all columns creating their summary and adding to the output list.
  for( i in 1:length(columns)){
    column_cur = columns[i]
    column_cur

    aggregate_formula = paste0('No_students~',column_cur, collapse='')
    summary_column = stats::aggregate(stats::as.formula(aggregate_formula),data=pathways,FUN=sum, na.rm = FALSE)
    summary_column

    # Add a total row and percent columns (of total pop and of in stage)
    total_student = sum(summary_column[,2])
    # -  % of population
    percent = round(as.numeric(summary_column[,2])/ total_student*100,2)
    summary_column[,3] = percent ; names(summary_column)[3] = '% (population)'
    # - % of stage
    if(convert_na && 'Student not in stage' %in% summary_column[,1]){
      total_student_stage = sum(summary_column[,2]) - summary_column[summary_column[,1] =='Student not in stage' ,2]
      percent = rep(NA, nrow(summary_column))
      percent[summary_column[,1] !='Student not in stage'] = round(summary_column[summary_column[,1] !='Student not in stage',2]/total_student_stage *100,2)
      summary_column[,4] = percent ; names(summary_column)[4] = '% (stage)'
      # - Total row
      summary_column[nrow(summary_column)+1,] = c('Total',total_student,'100%',NA )
    }
    else{
      # - Total row
      summary_column[nrow(summary_column)+1,] = c('Total',total_student,'100%' )

    }




    output[[i]] = summary_column
  }
  names(output) = columns
  return(output)
}

#' A function to get the summary of the describers at each stage of the pipeline.
#'
#' @param pathways data frame containing pathways.
#' @param describers A character vector of column names in pathways which we want to describe the stages (such as FSM, Sex, IDACI, etc)
#' @param stages A character vector of column names in pathways we split by the columns given in describers. (columns such as KS1, KS2, etc)
#'
#' @export
#'
pathway_summary_describers_at_stages <- function(pathways, describers, stages){

  # 1) check the describers and stages correspond to columns of pathways.
  describer_indices = match(describers, names(pathways))
  stage_indices = match(stages, names(pathways))
  if(any(is.na(describer_indices)) || any(is.na(stage_indices))){
    issue_columns = c(describers[which(is.na(describer_indices))], stages[which(is.na(stage_indices))])
    mes = paste0('Error! The following describers/stages are not names of columns in pathways: ', paste0(issue_columns,collapse = ', '), '.', collapse='')
    stop(mes)
  }

  # 2) strip pathways to only the columns of interest.
  all_columns = names(pathways)[-length(names(pathways))]
  wanted = c(describers, stages)
  not_needed = all_columns[!all_columns %in% wanted]
  if(length(not_needed > 0)){
    pathways_stripped = pathway_remove_columns(pathways,not_needed)
  }
  else{pathways_stripped = pathways}
  stripped_names = names(pathways_stripped)[-length(names(pathways_stripped))]

  # 3) Create the output.
  output = list()
  # Loop over describers.
  for(i in 1:length(describers)){
    describer_cur = describers[i]
    describer_cur
    describer_list = list()

    #Loop over stages.
    for(j in 1:length(stages)){
      stage_cur = stages[j]
      stage_cur
      path_cur = pathway_remove_columns(pathways_stripped,stripped_names[!stripped_names %in% c(describer_cur, stage_cur)])

      #Convert path_cur to a table of rows describer_cur, columns stage_cur
      tab = matrix(NA, nrow = length(unique(path_cur[,1])), ncol = length(unique(path_cur[,2])))
      rownames(tab) = unique(path_cur[,1]) ; colnames(tab) = unique(path_cur[,2])
      path_cur[,1] = factor(path_cur[,1]) ; path_cur[,2] = factor(path_cur[,2])
      indices = cbind(as.numeric(path_cur[,1]), as.numeric(path_cur[,2]))
      for(kk in 1:nrow(path_cur)){
        tab[indices[kk,1],indices[kk,2]] = path_cur[kk,3]
      }
      # Add total crow/olumns?
      tab = rbind(tab, colSums(tab)) ; tab = cbind(tab, rowSums(tab))

      #Save to list
      describer_list[[j]] = tab

    }
    names(describer_list) = stages
    output[[i]] = describer_list
  }
  names(output) = describers
  return(output)
}

#' A function to get the summary of the describers at each stage of the pipeline.
#'
#' @param pathways data frame containing pathways.
#' @param describers A character vector of column names in pathways which we want to describe the stages (such as FSM, Sex, IDACI, etc)
#' @param stages A character vector of column names in pathways we split by the columns given in describers. (columns such as KS1, KS2, etc)
#' @param do_SDC Flag (TRUE/FALSE) for whether statistical disclosure is performed.
#' @param round_to number to round to
#' @param issue_level number, where values with fewer students have their value omitted (set to "-").
#'
#' @export
#'
pathway_composition_tables <- function(pathways,
                                       describers,
                                       stages,
                                       do_SDC = FALSE,
                                       round_to = 10,
                                       issue_level = 5){

  # 1) check the describers and stages correspond to columns of pathways.
  describer_indices = match(describers, names(pathways))
  stage_indices = match(stages, names(pathways))
  if(any(is.na(describer_indices)) || any(is.na(stage_indices))){
    issue_columns = c(describers[which(is.na(describer_indices))], stages[which(is.na(stage_indices))])
    mes = paste0('Error! The following describers/stages are not names of columns in pathways: ', paste0(issue_columns,collapse = ', '), '.', collapse='')
    stop(mes)
  }

  # 2) strip pathways to only the columns of interest.
  all_columns = names(pathways)[-length(names(pathways))]
  wanted = c(describers, stages)
  not_needed = all_columns[!all_columns %in% wanted]
  if(length(not_needed) > 0){
    pathways_stripped = pathway_remove_columns(pathways,not_needed)
  } else{pathways_stripped = pathways}
  stripped_names = names(pathways_stripped)[-length(names(pathways_stripped))]

  # 3) Create the output.
  output = list()
  # Loop over stages
  for(i in 1:length(stages)){
    stage_cur = stages[i]
    stage_cur

    stage_table = NULL
    #Loop over describers
    for(j in 1:length(describers)){
      describer_cur = describers[j]
      describer_cur
      path_cur = pathway_remove_columns(pathways_stripped,stripped_names[!stripped_names %in% c(describer_cur, stage_cur)])
      path_cur = path_cur[c(describer_cur, stage_cur, 'No_students')]
      #Convert path_cur to a table of rows describer_cur, columns stage_cur
      tab = matrix(NA, nrow = length(unique(path_cur[,1])), ncol = length(unique(path_cur[,2])))
      rownames(tab) = unique(path_cur[,1]) ; colnames(tab) = unique(path_cur[,2])
      path_cur[,1] = factor(path_cur[,1]) ; path_cur[,2] = factor(path_cur[,2])
      indices = cbind(as.numeric(path_cur[,1]), as.numeric(path_cur[,2]))
      for(kk in 1:nrow(path_cur)){
        tab[indices[kk,1],indices[kk,2]] = path_cur[kk,3]
      }
      # Add total crow/olumns?
      tab = rbind(tab, colSums(tab)) ; tab = cbind(tab, rowSums(tab))

      #Save to list
      name_for_tab = colnames(tab) ; name_for_tab[length(name_for_tab)] = 'Total'
      tab_new = tab[-nrow(tab),] |> data.frame()
      tab_new = data.frame(describer = describer_cur, level = rownames(tab_new), tab_new)
      names(tab_new) = c('describer', 'level', name_for_tab)
      rownames(tab_new) = NULL
      stage_table = rbind(stage_table, tab_new)

    }

    # Apply SDC
    if(do_SDC){
      for(ii in 3:ncol(stage_table)){
        stage_table[,ii] = stage_table[,ii] |>  apply_SDC(round_to = round_to, issue_level = issue_level)
      }

    }
    output[[i]] = stage_table
  }
  names(output) = stages
  return(output)
}


#' A function to get the summary of the describers at each stage of the pipeline.
#'
#' @param pathways data frame containing pathways.
#' @param describers A character vector of column names in pathways which we want to describe the stages (such as FSM, Sex, IDACI, etc)
#' @param stages A character vector of column names in pathways we split by the columns given in describers. (columns such as KS1, KS2, etc), note here that the order of stages need to be in chronological order.
#'
#' @export
#'
pathway_stage_transition_by_describer <- function(pathways, describers, stages){
  pathways = pathways |> as.data.frame() |>remove.factors()
  # 1) Get all transitions between stages.
  transitions = NULL
  for(i in 1:(length(stages)-1)){
    pre = pathways[stages[i]] |> as.vector() |> unlist()
    post = pathways[stages[i+1]] |> as.vector() |> unlist()
    transitions_cur = paste0(stages[i], ': ', pre, ' -> ', stages[i+1], ': ', post) |> unique()
  }

}


