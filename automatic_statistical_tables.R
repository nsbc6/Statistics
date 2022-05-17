# ==================================================================================== #
# ==================================================================================== #
#################################### STATISTICAL TABLES ################################
# ==================================================================================== #
# ==================================================================================== #

### DATE: 1st March 2022
### LAST UPDATE: 2nd March 2022

### PENDING TO UPDATE:
#   - Possibility no categorical variables or continuous
#   - More than one categorical variable in columns
#   - Tutorial y particularidades del output
#   - Add where to save an name or the table

# ==================================================================================== #
####################################### Instructions ###################################
# ==================================================================================== #

# This script aims to build an Excel sheet with the statisticst of an amount of 
# variables we provide it. We just have to fill "Inputs" step and to have clear how we
# want table aspect.

# ==================================================================================== #
##################################### Load R packages ##################################
# ==================================================================================== #

library(openxlsx)
library(dplyr)
library(stats)
library(tidyr)
library(rlist)
library(DataExplorer)
library (purrr)
library(devtools)


# Clean the global environment to avoid variables mistakes:
#rm(list = ls())

# ==================================================================================== #
########################################## Inputs ######################################
# ==================================================================================== #

# We just need to include:
##### 0) Name of the table and where to save
stat_table_name <- "sexo_vs_vars_il10"
where_to_save <- "~/Diabetes probi/Ana_complete/Ana_complete/ANÁLISIS_DEFINITIVO_FACS_NOV2021/PARACONGRESO/"

##### 1) Dataframe (variables in columns and patients in rows)

# We provide path and file name:
#data <- read.xlsx("~/Diabetes probi/Ana_complete/Ana_complete/ANÁLISIS_DEFINITIVO_FACS_NOV2021/RESELECCIÓN_TR1/PROBI+MENARINI - Variables clínicas/probi_y_menarini_vars_NdeFACS_no_IMP.xlsx")


##### 2) Names of categorical variables in a vector

categ_vars_names <- c("Sexo","sarcopenia" )

##### 3) Names of continuous variables in a vector

cont_vars_names <- c(" CD4+IL10+ analitica en ml", "  CD8+ IL10+ analitica en ml", 
                     "Edad", "Peso" , "WAIST" ,"IMC" ,
                     "angulo","MLG_pct","MG_pct",
                     "ACT_pct","MM_pct","sarcopenia",              
                     "Score_DietaMed"
                     )


##### 4) Which variables do we want in columns in our final Excel (ex. Commonly categorical).
#    Consequently, the rest will be in rows.

vars_in_colums <- c("Sexo")


# ==================================================================================== #
###################################### Automatic part ##################################
# ==================================================================================== #


############################### CHECKPOINT ############################### 



check_names <- function(x){if (!(x %in% colnames(data))){
                           return (x)}
}

misspelling <- vector()
for (vars in c(categ_vars_names, cont_vars_names, vars_in_colums)){
  misspelling <- append(misspelling, check_names(vars))
}

#!# Clean #!#
remove(vars)
#!# 

if (!(is_empty(misspelling))){
    stop(paste(misspelling, "is not in your data. Please, check it."))
}

############################ Keep indexes and apply indexes ############################

idx_vars_in_colums <- as.vector(sapply(c(vars_in_colums),
                                   function(x) return(which(colnames(data) == x))))

idx_categ_vars <- as.vector(sapply(c(categ_vars_names),
                                   function(x) return(which(colnames(data) == x))))

idx_cont_vars <- as.vector(sapply(cont_vars_names,
                                  function(x) return(which(colnames(data) == x))))

data[,idx_vars_in_colums] <- lapply(as.data.frame(data[,idx_vars_in_colums]), factor)
data[,idx_categ_vars] <- lapply(data[,idx_categ_vars], factor)
data[,idx_cont_vars] <- lapply(data[,idx_cont_vars], as.numeric)


############################### Build the dataframe ###############################

######## Columns
st_cols <- sum(as.vector(sapply(as.data.frame(data[,idx_vars_in_colums]), nlevels))) + #Categories of variables
           length(vars_in_colums) + #p value column per variable
           4 # Informative columns


names_columns <- c("Variable name", "Missing values %", "Overall")
if (length(idx_vars_in_colums) != 1){
  kept_lev_col <- sapply(data[,idx_categ_vars], levels)
  for (vars_cat in 1:length(kept_lev_col)){
    for(categories_in_var in 1:length(kept_lev_col[[vars_cat]])){
      names_columns <- append(names_columns, paste(vars_in_colums[vars_cat], kept_lev_col[[vars_cat]][categories_in_var]))
    }
    names_columns <- append(names_columns, "p value")
  }
}else{
  kept_lev_col <- levels(data[,idx_vars_in_colums])
  for(categories_in_var in 1:length(kept_lev_col)){
    names_columns <- append(names_columns, paste(vars_in_colums, kept_lev_col[categories_in_var]))
  }
  names_columns <- append(names_columns, "p value")
}
names_columns <- append(names_columns, "Test")




######## Rows

names_categories_vars <- vector()
if (length(idx_categ_vars) > 1){
  kept_lev <- sapply(data[,idx_categ_vars], levels)
  for (vars_cat in 1:length(kept_lev)){
    for(categories_in_var in 1:length(kept_lev[[vars_cat]])){
      names_categories_vars <- append(names_categories_vars, paste(categ_vars_names[vars_cat],
                                                                   kept_lev[[vars_cat]][categories_in_var]))
    }
  }
}else{
  kept_lev <- levels(data[,idx_categ_vars])
  for(categories_in_var in 1:length(kept_lev)){
    names_categories_vars <- append(names_categories_vars, paste(categ_vars_names,
                                                                 kept_lev[categories_in_var]))
  }
}


st_rows <- length(c(names_categories_vars,cont_vars_names))



######## Buld the table
statistical_table <- as.data.frame(matrix(ncol = st_cols, nrow = st_rows))
colnames(statistical_table) <- names_columns
statistical_table[, "Variable name"] <- c(cont_vars_names, names_categories_vars)


############################### Continuous variables ###############################

######## Missing values
statistical_table[1:length(idx_cont_vars), 2] <- round(colMeans(is.na(data[,idx_cont_vars])) * 100,3)

######## Overall mean and SD
kept_means <- as.vector(sapply(data[,idx_cont_vars], function(x) return(mean(na.omit(x)))))
kept_sd <- as.vector(sapply(data[,idx_cont_vars], function(x) return(sd(na.omit(x)))))

for(conts in 1:length(idx_cont_vars)){
  statistical_table[conts, 3] <- paste0(as.character(round(kept_means[conts],3)),
                                                     " (",as.character(round(kept_sd[conts],3)), ")")
}


######## Per categories mean and SD
shap_own_function <- function(x){
  if (length(x) >= 3){
    if (!(is.na(mean(x)))){
      tmp_var<-shapiro.test(x)
      return(tmp_var$p.value)
    }else{
      return(NA)
    }
    
  }else{
    return(NA)
  }
}
# Option 1: Just one important variable

if(length(idx_vars_in_colums) == 1){
  shapi_table <- as.data.frame(matrix(ncol = length(kept_lev_col),  nrow = length(idx_cont_vars)))
  vuelta <- 1
  for(lev_1 in 1:length(kept_lev_col)){
    kept_means <- as.vector(sapply(data[which(data[,idx_vars_in_colums] == kept_lev_col[lev_1]),idx_cont_vars],
                                   function(x) return(mean(na.omit(x)))))
    kept_sd <- as.vector(sapply(data[which(data[,idx_vars_in_colums] == kept_lev_col[lev_1]),idx_cont_vars],
                                function(x) return(sd(na.omit(x)))))
    kept_shap <- as.vector(sapply(data[which(data[,idx_vars_in_colums] == kept_lev_col[lev_1]),idx_cont_vars],
                                  shap_own_function
                                 ))
    
    for(conts in 1:length(idx_cont_vars)){
      statistical_table[conts, as.numeric(3+vuelta)] <- paste0(as.character(round(kept_means[conts],3)),
                                                               " (",as.character(round(kept_sd[conts],3)), ")")
    }
    shapi_table[,vuelta] <- kept_shap
    vuelta <- vuelta + 1
  }
  
  # Normality test and p value
  tests_names <- vector()
  for(cont_vars in 1:length(idx_cont_vars)){
    if(!(is_empty(which(shapi_table[cont_vars,] < 0.05)))){
      # Non-normal
      if (length(kept_lev_col) == 2){
        tests_names <- append(tests_names, "Mann-Whitney U")
        tmp_var <- wilcox.test(data[,idx_cont_vars[cont_vars]] ~ data[,idx_vars_in_colums], paired = F)

        statistical_table[cont_vars,as.numeric(3+length(kept_lev_col)+ 1)] <- round(tmp_var$p.value, 4)
      }else{
        tests_names <- append(tests_names, "Kruskal-Wallis")
        tmp_var <- kruskal.test(data[,idx_cont_vars[cont_vars]] ~ data[,idx_vars_in_colums])
        statistical_table[cont_vars,as.numeric(3+length(kept_lev_col)+ 1)] <- round(tmp_var$p.value, 4)
      }
      # Normal 
    }else{
      if (length(kept_lev_col) == 2){
        tests_names <- append(tests_names, "T test")
        tmp_var <- t.test(data[,idx_cont_vars[cont_vars]] ~ data[,idx_vars_in_colums], paired = F)
        
        statistical_table[cont_vars,as.numeric(3+length(kept_lev_col)+ 1)] <- round(tmp_var$p.value, 4)
      }else{
        tests_names <- append(tests_names, "ANOVA")
        tmp_var <- summary(aov(data[,idx_cont_vars[cont_vars]] ~ data[,idx_vars_in_colums]))[[1]]
        statistical_table[cont_vars,as.numeric(3+length(kept_lev_col)+ 1)] <- round(tmp_var[1, "Pr(>F)"], 4)
      }
    }
  }
  
# Option 2: More than one important variable
  
}else{
  
}



############################### Categorical variables ###############################

######## Missing values
tmp_var <- round(colMeans(is.na(data[,idx_categ_vars])) * 100,3)
mv_categ <- vector()
for (nas_categ in 1:length(tmp_var)){
  mv_categ <- append(mv_categ, rep(tmp_var[nas_categ], length(kept_lev[[nas_categ]])))
}
statistical_table[as.numeric(length(idx_cont_vars)+1):nrow(statistical_table), 2] <- mv_categ

#!# Clean
remove(tmp_var, mv_categ)
#|# 


######## Overall number and %

kept_n <-vector()
kept_perc <- vector()
for(categ in 1:length(categ_vars_names)){
  for(lev in kept_lev[[categ]]){
    kept_n <- append(kept_n, length(data[which(data[,categ_vars_names[categ]] == lev), categ_vars_names[categ]]))
    kept_perc <- append(kept_perc,
                        round((length(data[which(data[,categ_vars_names[categ]] == lev), categ])/
                               length(which(!(is.na(data[, categ_vars_names[categ]]))))) * 100, 2))
  }
  
}
# Write the result
r_vuelta <- 1
for(n_categ in 1:length(kept_n)){
  statistical_table[as.numeric(length(idx_cont_vars)+r_vuelta), 3] <- paste0(as.character(round(kept_n[n_categ],3)),
                                                                             " (",as.character(kept_perc[n_categ],3), "%)")
  r_vuelta <- r_vuelta + 1
}



######## Per categories number and %


# Option 1: Just one important variable

if(length(idx_vars_in_colums) == 1){
  #Turn category
  turn_cat <- 1

  for(lev_1 in 1:length(kept_lev_col)){
    
    #Empty vector per category
    kept_n <- vector()
    kept_perc <- vector()
    for (categ in 1:length(kept_lev)){
      for (lev in kept_lev[[categ]]){
        # N fits the condition
        kept_n <- append(kept_n,
                         length(data[which(data[,vars_in_colums] == kept_lev_col[lev_1] & data[, categ_vars_names[categ]] == lev),
                                     categ_vars_names[categ]] ))
        # %
        kept_perc <- append(kept_perc,
                            #Divide to get %
                            round((length(data[which(data[,vars_in_colums] == kept_lev_col[lev_1] & data[, categ_vars_names[categ]] == lev),
                                         categ_vars_names[categ]] )/
                            length(data[which(data[,vars_in_colums] == kept_lev_col[lev_1]),
                                        categ_vars_names[categ]] )) *100 , 3)
                            )
                            
        
      }
    }
    
    # Fill the category
    # Write the result
    r_vuelta <- 1
    for(n_categ in 1:length(kept_n)){
      statistical_table[as.numeric(length(idx_cont_vars)+r_vuelta),
                        3+turn_cat] <- paste0(as.character(round(kept_n[n_categ],3)),
                                                                                 " (",as.character(kept_perc[n_categ]), "%)")
      r_vuelta <- r_vuelta + 1
    }
    
    turn_cat <- turn_cat + 1
  }
  
  #Add p value of categorical x categorical using CHISQ
  cq_res <- vector()
  for(categ in categ_vars_names){
    if (nlevels(data[,categ]) >= 2){
      tmp_var <- chisq.test(data[, vars_in_colums], data[,categ])
      cq_res <- append(cq_res, tmp_var$p.value)
    }else{
      cq_res <- append(cq_res, NA)
    }
    
  }
  p_val_categ <- vector()
  for (res in 1:length(kept_lev)){
    p_val_categ <- append(p_val_categ, rep(cq_res[res], length(kept_lev[[res]])))
  }
  statistical_table[as.numeric(length(idx_cont_vars)+1):nrow(statistical_table),
                    ncol(statistical_table)-1] <- p_val_categ
  
  
# Option 2: More than one important variable
  
}else{
  
}


############################### Add tests ###############################

tests_names <- append(tests_names, rep("Chi squared", length(p_val_categ)))
statistical_table[, ncol(statistical_table)] <- tests_names






# ==================================================================================== #
######################################## Save file #####################################
# ==================================================================================== #

write.xlsx(statistical_table, paste0(where_to_save, stat_table_name, ".xlsx"))
