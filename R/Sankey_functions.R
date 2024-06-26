#' A function that converts pathways to sankey diagram information.
#'
#' Note that you need to remove "describer" columns prior to using this function. In other words each column in pathways is viewed as a stage (KS1, KS2, etc) in the sankey diagram. Hence the order of columns is also important.
#'
#' @param pathways pathways
#' @param links_as_pos links_as_pos
#' @param remove_na remove_na Flag (TRUE/FALSE) for whether NA values are removed.
#' @param ... arguments to `SDC_to_links()`.
#' @return links
#' @export
#'
pathway_to_links <- function(pathways,
                             links_as_pos = TRUE,
                             remove_na=T,
                             ...){
  if(remove_na==F){
    #Join colname to pathways elements to make sure there's no repeated values across columns. For example an A at GCSE and an A at a level.
    p=as.data.frame(pathways)
    for(i in 1:(ncol(p)-1)){
      p[,i] = paste(names(p)[i],as.vector(p[,i]), sep=': ')
    }
    pathways=p

    #Create all_links to store the source/sink/count combinations.
    names = as.vector(unique(unlist(pathways[,1:(ncol(pathways)-1)])))
    nodes = data.frame(names)
    all_links = expand.grid(names,names)
    all_links = cbind(all_links, rep(NA,nrow(all_links)))



    #loop over all source/sink pairs and sum the total number of students.
    for(i in 1:nrow(all_links)){
      total_students = 0
      for(j in 1:(ncol(pathways)-1)){
        match_source = which(as.vector(pathways[,j]) == as.vector(all_links[i,1]))
        match_sink = which(as.vector(pathways[,j+1]) == as.vector(all_links[i,2]))
        indices = intersect(match_source,match_sink)
        total_students = total_students + sum(pathways[indices,ncol(pathways)])
      }
      all_links[i,3] = total_students
    }

    #remove rows with 0 students.
    all_links = all_links[which(all_links[,3] !=0 ),]
    colnames(all_links) = c('source','target', 'value')
    rownames(all_links) = 1:nrow(all_links)
    all_links = data.frame(all_links)
    all_links = remove.factors(all_links)


    #if links_as_pos convert links to be node number -1
    if(links_as_pos){
      pos = 0
      for(name in names){
        all_links[all_links == name] <- pos
        pos=pos+1
      }
      #convert source/target from string to number
      all_links[,1] = as.numeric(all_links[,1])
      all_links[,2] = as.numeric(all_links[,2])
    }

    sankey = list(nodes = nodes, links = all_links)
    return(sankey)
  }else{
    #Join colname to pathways elements to make sure there's no repeated values across columns. For example an A at GCSE and an A at a level. Do not change if the value is NA.
    p=as.data.frame(pathways)
    for(i in 1:(ncol(p)-1)){
      # which values are NA
      paste(names(p)[i],levels(p[,i]), sep=': ')
      levels(p[,i]) = paste(names(p)[i],levels(p[,i]), sep=': ')
    }
    pathways=p
  }

  #Create all_links to store the source/sink/count combinations. (strange method using levels for order then removing any levels that are not present in the data (due to previous filtering))
  names = as.vector(unlist(lapply(pathways[,1:(ncol(pathways)-1)], levels)))
  names2 = as.vector(unique(unlist(pathways[,1:(ncol(pathways)-1)])))
  names = names[names %in% names2]
  names = names[!is.na(names)]

  # names = as.vector(unique(unlist(pathways[,1:(ncol(pathways)-1)])))
  # names = names[!is.na(names)]

  nodes = data.frame(names)
  nodes <- data.frame(lapply(nodes, as.character), stringsAsFactors=FALSE) #remove pesky factors.

  all_links = expand.grid(names,names)
  all_links = cbind(all_links, rep(NA,nrow(all_links)))
  all_links <- data.frame(lapply(all_links, as.character), stringsAsFactors=FALSE) #remove pesky factors.

  #loop over all source/sink pairs and sum the total number of students.
  for(i in 1:nrow(all_links)){
    # print(paste(c('i=', i), collapse=''))
    all_links[i,]
    total_students = 0
    pathways_columns = c(strsplit(all_links[i,1],':')[[1]][1], strsplit(all_links[i,2],':')[[1]][1] )
    pos_in_pathway_colunms = match(pathways_columns,names(pathways))

    # 1) source column is before or the same as sink column. So no flow leave as NA.
    if(pos_in_pathway_colunms[1] >= pos_in_pathway_colunms[2]){
      next
    }
    # 2) source column exactly one before sink column. Flow may exist.
    else if(pos_in_pathway_colunms[1] +1 == pos_in_pathway_colunms[2]){
      match_source = which(as.vector(pathways[,pos_in_pathway_colunms[1]]) == as.vector(all_links[i,1]))
      match_sink = which(as.vector(pathways[,pos_in_pathway_colunms[2]]) == as.vector(all_links[i,2]))
      indices = intersect(match_source,match_sink)
      total_students = total_students + sum(pathways[indices,ncol(pathways)])
    }
    # 3) souce column is more than one before sink column. Flow may exist but require NA for all values between columns
    else if(pos_in_pathway_colunms[1] +1 < pos_in_pathway_colunms[2]){
      # browser()
      match_source = which(as.vector(pathways[,pos_in_pathway_colunms[1]]) == as.vector(all_links[i,1]))
      match_sink = which(as.vector(pathways[,pos_in_pathway_colunms[2]]) == as.vector(all_links[i,2]))
      indices = intersect(match_source,match_sink)
      #Need to check between = NA.
      for(ii in (pos_in_pathway_colunms[1] +1):(pos_in_pathway_colunms[2]-1)){
        indices = indices[is.na(pathways[indices,ii])]
        if(length(indices) == 0){
          break
        }
      }
      if(length(indices) == 0){
        next
      }
      total_students = total_students + sum(pathways[indices,ncol(pathways)])
    }

    all_links[i,3] = total_students
  }


  #remove rows with 0 students.
  all_links = all_links[which(all_links[,3] !=0 ),]
  colnames(all_links) = c('source','target', 'value')
  rownames(all_links) = 1:nrow(all_links)
  all_links = data.frame(all_links)
  all_links = remove.factors(all_links)

  #if links_as_pos convert links to be node number -1
  if(links_as_pos){
    pos = 0
    for(name in names){
      all_links[all_links == name] <- pos
      pos=pos+1
    }
    #convert source/target from string to number
    all_links[,1] = as.numeric(all_links[,1])
    all_links[,2] = as.numeric(all_links[,2])
  }

  ### Apply SDC ----
  # all_links$SDC <- all_links |> SDC_to_links(...)
  ### (END) Apply SDC

  ### Add node and link descriptions -----
  #1) Add details to nodes.
  state = rep(NA,nrow(nodes))
  for(i in 1:nrow(nodes)){
    #Create the state start with the node name.
    mes = as.vector(nodes[i,1])

    #Add total students in and out for the node.
    all_out = sum(as.numeric(all_links[all_links[,1] == (i-1),3]))
    all_in = sum(as.numeric(all_links[all_links[,2] == (i-1),3]))
    if (all_in > 0){
      mes = c(mes, '\nTotal students in = ',all_in, '.')
    }
    if(all_out> 0){
      mes = c(mes, '\nTotal students out = ',all_out,'.')
    }
    if(all_out> 0 && all_in> 0){
      mes = c(mes, '\nContinuation = ',round(all_out/all_in*100,1),'%.')
    }

    # Node inrelation to the stage.
    stage = strsplit(nodes[i,1], ':')[[1]][1]
    stage_nodes = which(grepl(stage, nodes[,1])) - 1
    stage_out = sum(as.numeric(all_links[all_links[,1] %in% stage_nodes,3]))
    stage_in = sum(as.numeric(all_links[all_links[,2] %in% stage_nodes,3]))
    mes = c(mes, '\n\nAs % of ', stage , ' stage.')

    if (stage_in > 0){
      mes = c(mes, '\n\tIn = ',round(all_in/stage_in*100,1), '%.')
    }
    if(stage_out> 0){
      mes = c(mes, '\n\tOut = ',round(all_out/stage_out*100,1),'%.')
    }


    mes = paste(mes,collapse='')
    state[i] = mes
  }
  nodes = data.frame(nodes,description = state)

  # 2) Add details to links
  details = rep(NA,nrow(all_links))
  for(i in 1:nrow(all_links)){
    link_cur = all_links[i,]
    link_cur

    #Set defaul message to blank,
    mes=''
    #Add detail of the source/sink and the value of the flow.
    flow = as.numeric(link_cur[3])
    flow
    mes = c(mes,  'Flow: (', as.vector(nodes[as.numeric(link_cur[1]+1),1]), ') -> (', as.vector(nodes[as.numeric(link_cur[2]+1),1]), ') = ',flow)

    #Add detail of the percent of the flow out of source.
    flow_leave_source = sum(as.numeric(all_links[all_links[,1]  == as.numeric(link_cur[1]), 3]))
    mes = c(mes, '\n\n As percent of students leaving ', nodes[as.numeric(link_cur[1]+1),1], ' = ', round(flow/flow_leave_source*100,1),'%.')

    #Add detail of the percent of the flow into of sink.
    flow_into_sink = sum(as.numeric(all_links[all_links[,2]  == as.numeric(link_cur[2]), 3]))
    mes = c(mes, '\n\n As percent of students entering ', nodes[as.numeric(link_cur[2]+1),1], ' = ', round(flow/flow_into_sink*100,1),'%.')

    mes = paste(mes,collapse='')
    details[i] = mes
  }
  name = details
  all_links = data.frame(all_links,description = name)

  ### (END) Add node and link descriptions -----

  sankey = list(nodes = nodes, links = all_links)


  return(sankey)
}

#' Statistical disclosure to sankey
#'
#' @param links links of a sankey diagram
#' @param round_to number to round to
#' @param issue_level number, where links with fewer students have their value omitted (set to "Below issue_level").
#'
#' @return vector containing the links after performing SDC
#' @export
#'
SDC_to_links <- function(links, round_to = 5, issue_level = 5){
  new_links = links$value |> as.numeric()
  new_links[new_links <= issue_level] = paste0('Below ', issue_level)
  new_links[new_links != paste0('Below ', issue_level)] = new_links[new_links != paste0('Below ', issue_level)] |>
    as.numeric() |>
    plyr::round_any(round_to)

  new_links
}

#' Create sankey diagram of the filtered data.
#'
#' @param pathways student pathways with number of students.
#' @param flow_columns Column names (or index) of the 'stages' in pathways that we want to create a sankey diagram of.
#' @param filters a character vector describing the filters to apply to the data. Of the format `XX: YY` where XX is the stage and YY the node, such as 'FSM: Yes'.
#' @param union Flag (TRUE/FALSE) for whether we want the union of filters (only across single stages)
#' @param fontFamily font family for the node text labels.
#' @param fontSize numeric font size in pixels for the node text labels.
#' @param do_SDC Flag (TRUE/FALSE) for whether you want to do SDC via the `SDC_to_links()` function.
#' @param ... arguments to `SDC_to_links()` or `networkD3::sankeyNetwork()`.
#'
#' @return sankey diagram
#' @export
#'
sankey_filtered <- function(pathways,
                            flow_columns,
                            filters = '',
                            union=F,
                            fontFamily = NULL,
                            fontSize = 7,
                            do_SDC = FALSE,
                            ...){
  # A) Check whether flow columns are text or indices.
  if(!is.numeric(flow_columns)){
    # Flow columns is not numeric assume text, need to find the indices.
    flow_columns = match(flow_columns,names(pathways))
    if(any(is.na(flow_columns))){
      stop('Inputted text flow columns. At least one does not match names of pathways.')
    }
  }

  # 1) Get the filtered data and create links and nodes.
  if(filters[1] == '' || is.null(filters)){
    pathways_filtered = pathways
  }
  else{
    pathways_filtered =filter_data(pathways,filters, union=union)

  }

  # 2) Convert Pathwats to sankey data.
  Links_filtered = pathway_to_links(pathways_filtered[,c(flow_columns,ncol(pathways))],...)
  all_links = Links_filtered$links ; nodes = Links_filtered$nodes


  # 4) Create sankey diagram.
  p <- networkD3::sankeyNetwork(Links = all_links,
                                Nodes = nodes,
                                Source = "source",
                                Target = "target",
                                Value = "value",
                                NodeID = "names",
                                sinksRight = F,
                                iterations= 0,
                                fontFamily = fontFamily,
                                fontSize = fontSize,
                                ...) |>
    add_node_hover_text(hovertext = nodes$description) |>
    add_link_hover_text(hovertext = all_links$description)


  return(p)
}

#' Create traced sankey diagram
#'
#' @inheritParams sankey_filtered
#' @param split_by a character vector describing the filters to split the data by. Of the format `XX: YY` where XX is the stage and YY the node, such as 'FSM: Yes'.
#' @param colors a character vector of length three `c(colA, colB, colC)` where colA is the link colour for traced students, colB is the link colour for other students and colC is the node colour.
#'
#' @return traced sankey diagram
#' @export
#'
sankey_trace <- function(pathways,
                         flow_columns,
                         split_by,
                         union=F,
                         colors = c("red", "gray", "antiquewhite"),
                         fontFamily = NULL,
                         fontSize = 7
                         ){

  #Get the links for the full pathways and 'traced' pathways.
  Links_full = pathway_to_links(pathways[,c(flow_columns,ncol(pathways))])
  pathways_traced =filter_data(pathways,split_by,union=union)
  Links_traced = pathway_to_links(pathways_traced[,c(flow_columns,ncol(pathways))])


  #1) Use full to create all possible nodes in our combined Sankey.
  nodes = Links_full$nodes
  group = rep('my_unique_group',nrow(nodes))
  nodes = data.frame(nodes,group)
  nodes

  #2) Work out the node positions of the subset of flows (traced) compared to full.
  node_compare = rbind(0:(length(nodes[,1])-1), match(nodes[,1],Links_traced$nodes[,1]) -1)
  colnames(node_compare) = as.vector(nodes[,1])
  rownames(node_compare) = c('Full', 'subset')
  node_compare

  # 3) Convert the subset of flows with the original nodes/convert the links. Into the variaible. links_subset.
  links_subset = data.frame(Links_traced$links) ; copy = links_subset
  for(i in 0:(length(Links_traced$nodes[,1])-1)){
    links_subset[,c(1,2)][copy[,c(1,2)] == i] <- node_compare[1,which(node_compare[2,] == i)]
  }
  group = rep('type_a',nrow(links_subset))
  links_subset = data.frame(links_subset,group)
  links_subset

  # 4) Create links for the full flows - subset of flows
  links_notsubset = NULL
  for(i in 1:nrow(Links_full$links)){
    #Get the current link
    cur = Links_full$links[i,]
    full_source_sink = cur[1:2]
    sub_source_sink = node_compare[2,which(node_compare[1,] %in% full_source_sink)]
    subset_flow = which(Links_traced$links[,1] == sub_source_sink[1] & Links_traced$links[,2] == sub_source_sink[2])
    # Check if the link exists in the subset of flows. If it does not add original else split.
    # if(any(is.na(sub_source_sink))){
    if(length(subset_flow) == 0){
      links_notsubset = rbind(links_notsubset, cur)
    }
    else{ # node pair exist in both full and subset of flows.
      sub_link = Links_traced$links[subset_flow,]
      new_link = cur
      new_link[3] = as.numeric(cur[3]) - as.numeric(sub_link[3])
      links_notsubset = rbind(links_notsubset,new_link)
    }
  }
  group = rep('type_b',nrow(links_notsubset))
  links_notsubset = data.frame(links_notsubset,group)
  links_notsubset

  # 5) Compare into a single links dataframe.
  all_links = rbind(links_subset,links_notsubset)

  # 6) Update hover information for the links.
  details = rep(NA,nrow(all_links))
  for(i in 1:nrow(all_links)){
    link_cur = all_links[i,]

    #Set defaul message to blank,
    mes=''
    # Add information of what group the flow is for.
    if(link_cur$group == 'type_a'){
      mes = c(mes,'Flow for students in: ', split_by, '.')

    }
    else{
      mes = c(mes,'Flow for students not in: ', split_by, '.')
    }
    #Add detail of the source/sink and the value of the flow.
    mes = c(mes,  '\nFlow: (', as.vector(nodes[as.numeric(link_cur[1]+1),1]), ') -> (', as.vector(nodes[as.numeric(link_cur[2]+1),1]), ') = ',as.vector(link_cur[3]))
    #Add detail of the percentage of the total flow between the source and sink.
    total_flow = sum(as.numeric(all_links[all_links[,1] == as.numeric(link_cur[1]) & all_links[,2] == as.numeric(link_cur[2]),][,3]))
    mes = c(mes, ' (',round(as.numeric(link_cur[3])/total_flow*100, digits=1),'%)')

    mes = paste(mes,collapse='')
    details[i] = mes
  }
  name = details
  all_links$description = name

  # 7) Update hover information for the nodes.
  state = rep(NA,nrow(nodes))
  for(i in 1:nrow(nodes)){
    #Create the state start with the node name.
    mes = as.vector(nodes[i,1])

    #Add total students in and out for the node.
    all_out = sum(as.numeric(all_links[all_links[,1] == (i-1),3]))
    all_in = sum(as.numeric(all_links[all_links[,2] == (i-1),3]))
    if (all_in > 0){
      mes = c(mes, '\nTotal students in = ',all_in, '.')
    }
    if(all_out> 0){
      mes = c(mes, '\nTotal students out = ',all_out,'.')
    }
    if(all_out> 0 && all_in> 0){
      mes = c(mes, '\nContinuation = ',round(all_out/all_in*100,1),'%.')
    }

    # Add details about the filter/split.
    red_out = sum(as.numeric(all_links[all_links[,1] == (i-1) & all_links[,4] == 'type_a',3]))
    red_in = sum(as.numeric(all_links[all_links[,2] == (i-1) & all_links[,4] == 'type_a',3]))
    mes = c(mes,'\n\n # students group by ',split_by,'. \n shown as (in : not in) \n')
    if(red_in >0){
      mes = c(mes, '\nIn = ', red_in, ' : ', all_in-red_in,'.')
      mes = c(mes, '\nPercent = ' , round(red_in/all_in*100,1), '%.')
    }
    if(red_out >0){
      mes = c(mes, '\nOut = ', red_out, ' : ', all_out-red_out,'.')
      mes = c(mes, '\nPercent = ' , round(red_out/all_out*100,1), '%.')

    }

    mes = paste(mes,collapse='')
    state[i] = mes
  }
  nodes$description = state

  # 8) prepare color scale: I give one specific color for each node.
  # my_color <- 'd3.scaleOrdinal() .domain(["type_a", "type_b", "my_unique_group"]) .range(["red", "gray", "antiquewhite"])'
  my_color <- paste0('d3.scaleOrdinal() .domain(["type_a", "type_b", "my_unique_group"]) .range([',
                     paste(shQuote(colors, type="cmd"), collapse=", "),'])' )

  all_links = all_links[all_links$value != '0',]

  # 9) Original sankey diagram
  p <- networkD3::sankeyNetwork(Links = all_links,
                                Nodes = nodes,
                                Source = "source",
                                Target = "target",
                                Value = "value",
                                NodeID = "names",
                                colourScale=my_color,
                                LinkGroup="group",
                                NodeGroup="group",
                                sinksRight = F,
                                iterations = 0,
                                fontFamily = fontFamily,
                                fontSize = fontSize) |>
    add_node_hover_text(hovertext = nodes$description) |>
    add_link_hover_text(hovertext = all_links$description)

  return(p)


}

#' Create sankey diagram from excel file or extracted data
#'
#' @param extract data frame containing the sankey information, with column  names including Source, Target and Number of Students.
#' @param fontFamily font family for the node text labels.
#' @param fontSize numeric font size in pixels for the node text labels.
#' @param filepath filepath to the .xlsx file containing the sankey diagram information. Assume the first 3 rows are to be removed but this can be altered with the `skip` input.
#' @param skip Number of rows to be skipped when loading the excel file.
#'
#' @return sankey diagram
#' @export
#'
sankey_from_extract <- function(filepath = NULL,
                                extract = NULL,
                                fontFamily = NULL,
                                skip = 3,
                                fontSize = 7){
  # Get the links and nodes from the file or extract data.frame.
  data = links_from_extract(filepath = filepath,
                     extract = extract,
                     skip = skip)

  nodes = data$nodes ; all_links = data$links

  # if group is in all_links then we have a traced plot otherwise we do not.
  if(!'group' %in% names(all_links)){
    p <- networkD3::sankeyNetwork(Links = all_links,
                                  Nodes = nodes,
                                  Source = "source",
                                  Target = "target",
                                  Value = "value",
                                  NodeID = "names",
                                  sinksRight = F,
                                  iterations= 0,
                                  fontFamily = fontFamily,
                                  fontSize = fontSize)
    # ...) |>
    # add_node_hover_text(hovertext = nodes$description) |>
    # add_link_hover_text(hovertext = all_links$description)

    return(p)
  }

  if('group' %in% names(all_links)){
    nodes$group = "my_unique_group"
    new_group = rep('type_b', nrow(all_links))
    new_group[all_links$group == 'Yes'] = 'type_a'
    all_links$group  = new_group
    colors = c("red", "gray", "antiquewhite")
    my_color <- paste0('d3.scaleOrdinal() .domain(["type_a", "type_b", "my_unique_group"]) .range([',
                       paste(shQuote(colors, type="cmd"), collapse=", "),'])' )

    all_links = all_links[all_links$value != '0',]

    p <- networkD3::sankeyNetwork(Links = all_links,
                                  Nodes = nodes,
                                  Source = "source",
                                  Target = "target",
                                  Value = "value",
                                  NodeID = "names",
                                  colourScale=my_color,
                                  LinkGroup="group",
                                  NodeGroup="group",
                                  sinksRight = F,
                                  iterations = 0,
                                  fontFamily = fontFamily,
                                  fontSize = fontSize)
    # ...) |>
    # add_node_hover_text(hovertext = nodes$description) |>
    # add_link_hover_text(hovertext = all_links$description)

    return(p)
  }

}

#' Get node and links from excel file or extracted data
#'
#' @param extract data frame containing the sankey information, with column  names including Source, Target and Number of Students.
#' @param filepath filepath to the .xlsx file containing the sankey diagram information. Assume the first 3 rows are to be removed but this can be altered with the `skip` input.
#' @param skip Number of rows to be skipped when loading the excel file.
#'
#' @return list containing nodes and links
#' @export
#'
links_from_extract <- function(filepath = NULL,
                                extract = NULL,
                                skip = 3){
  if(is.null(extract) & !is.null(filepath)){
    extract = readxl::read_xlsx(path = filepath, skip = skip)
  }
  if(is.null(extract)){
    stop('Error! issue with inputted filepath or extract.')
  }

  extract <- extract |> as.data.frame()

  ### Need to know if we are doing a traced sankey or a filtered sankey.
  ### If the extract has 3 columns then it's a filtered sankey if it has 4 columns it's a traced sankey.

  # Filtered
  if(ncol(extract) == 3){
    # A) Get the nodes
    nodes = unique(unlist(extract[c('Source', 'Target')]))
    nodes

    # B) Create all_links with correct format.
    all_links = extract
    names(all_links) = c('source','target','value')
    for(i in 1:length(nodes)){
      match1 = which(!is.na(match(extract[,1],nodes[i])))
      match2 = which(!is.na(match(extract[,2],nodes[i])))
      if(length(match1)>0){
        all_links[match1,1] = i-1
      }
      if(length(match2)>0){
        all_links[match2,2] = i-1
      }
    }
    all_links[,1] = as.numeric(all_links[,1])
    all_links[,2] = as.numeric(all_links[,2])
    nodes = data.frame(names = nodes) # turn nodes into a data frame.

    return(list(nodes = nodes, links = all_links))
  }

  # Traced
  if(ncol(extract) == 4){
    names(extract) = c('group', 'source', 'target', 'value')
    # A) Get the nodes
    nodes = unique(unlist(extract[c('source', 'target')]))
    nodes

    # B) Create all_links with correct format.
    all_links = extract[c('source', 'target', 'value')]
    for(i in 1:length(nodes)){
      match1 = which(!is.na(match(extract[['source']],nodes[i])))
      match2 = which(!is.na(match(extract[['target']],nodes[i])))
      if(length(match1)>0){
        all_links[match1,1] = i-1
      }
      if(length(match2)>0){
        all_links[match2,2] = i-1
      }
    }
    all_links[,1] = as.numeric(all_links[,1])
    all_links[,2] = as.numeric(all_links[,2])
    all_links$group = extract$group
    nodes = data.frame(names = nodes) # turn nodes into a data frame.

    return(list(nodes = nodes, links = all_links))
  }


}
