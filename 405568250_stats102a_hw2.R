messy_impute <- function(messy_table, center = 'Mean', margin, ...) {
  # This function take in a messy dataframe and imputes any NA values. Imputation
  # margin as well as method can be specified as an argument. Any mean arguments
  # can be used as well as additional arguments.
  #Args:
  # messy_table: dataframe
  # center: character (either 'Mean' or 'Median')
  # margin: Single integer (1 for rowwise imputation and 2 for columnwise)
  #Return:
  # imputed dataframe of the same size
  if (!(margin %in% 1:2)) {
    stop('ERROR: Margin must be an integer 1 or 2')
  }
  if (is.data.frame(messy_table) == FALSE) {
    stop('ERROR: Messy Table input must be a data frame.')
  }
  center <- tolower(center)
  if (center == 'mean') {
    if (margin == 2) {
      index <- as.vector(which(rowSums(apply(messy_table, FUN = is.na, MARGIN = 2)) > 0))
      for (i in index) {
        hw <- messy_table %>% select(starts_with('H'))
        quiz <- messy_table %>% select(starts_with('Q'))
        imputehw <- mean(as.numeric(hw[i,]), na.rm = TRUE, ...)
        imputequiz <- mean(as.numeric(quiz[i,]), na.rm = TRUE, ...)
        for (j in 1:ncol(messy_table)) {
          if (is.na(messy_table[i,j])) {
            if (j %in% 2:6) {
              messy_table[i,j] <- imputehw
            } else {
              messy_table[i,j] <- imputequiz
              }
            }
          }
        }
      }
    if (margin == 1) {
      index <- as.vector(which(colSums(apply(messy_table, FUN = is.na, MARGIN = 2)) > 0))
      for (j in index) {
        impute2 <- mean(messy_table[,j], na.rm = TRUE, ...)
        for (i in 1:nrow(messy_table)) {
          if (is.na(messy_table[i,j])) {
            messy_table[i,j] <- impute2
          }
        }
      }
    }
  } else if (center == 'median') {
    if (margin == 2) {
      index <- as.vector(which(rowSums(apply(messy_table, FUN = is.na, MARGIN = 2)) > 0))
      for (i in index) {
        hw <- messy_table %>% select(starts_with('H'))
        quiz <- messy_table %>% select(starts_with('Q'))
        imputehw <- median(as.numeric(hw[i,]), na.rm = TRUE, ...)
        imputequiz <- median(as.numeric(quiz[i,]), na.rm = TRUE, ...)
        for (j in 1:ncol(messy_table)) {
          if (is.na(messy_table[i,j])) {
            if (j %in% 2:6) {
              messy_table[i,j] <- imputehw
            } else {
              messy_table[i,j] <- imputequiz
            }
          }
        }
      }
    }
    if (margin == 1) {
      index <- as.vector(which(colSums(apply(messy_table, FUN = is.na, MARGIN = 2)) > 0))
      for (j in index) {
        impute2 <- median(messy_table[,j], na.rm = TRUE, ...)
        for (i in 1:nrow(messy_table)) {
          if (is.na(messy_table[i,j])) {
            messy_table[i,j] <- impute2
          }
        }
      }
    }
  } else {
    stop('ERROR: Center must be mean or median')
  }
  messy_table
}




tidy_impute <- function(tidy_table, center = 'Mean', margin,...) {
  # This function take in a tidy dataframe and imputes any NA values. Imputation
  # margin as well as method can be specified as an argument. Any mean arguments
  # can be used as well as additional arguments.
  #Args:
  # tidy_table: tidy dataframe
  # center: character (either 'Mean' or 'Median')
  # margin: Single integer (1 for rowwise imputation and 2 for columnwise)
  #Return:
  # imputed tidy dataframe of the same size
  if (!(margin %in% 1:2)) {
    stop('ERROR: Margin must be an integer 1 or 2')
  }
  if (is_tibble(tidy_table) == FALSE) {
    stop('ERROR: Tidy Table input must be a tibble.')
  }
  center <- tolower(center)
  if (center == 'mean') {
    if (margin == 2) {
      index <- as.vector(which(rowSums(apply(tidy_table, FUN = is.na, MARGIN = 2)) > 0))
      for (i in index) {
        id <- as.numeric(tidy_table[i,1])
        temp <- tidy_table %>% group_by(UID, AssignmentType) %>% summarize(meanscore = mean(Score, na.rm = TRUE, ...), .groups = 'drop') %>% filter(UID == id)
        if (as.character(tidy_table[i,2]) == 'Homework') {
          tidy_table[i,4] <- as.numeric(temp[1,3])
        } else {
          tidy_table[i,4] <- as.numeric(temp[2,3])
        }
      }
    }
    if (margin == 1) {
      index <- as.vector(which(rowSums(apply(tidy_table, FUN = is.na, MARGIN = 2)) > 0))
      for (i in index) {
        assignmnt <- as.character(tidy_table[i,2])
        number <- as.character(tidy_table[i,3])
        temp <- tidy_table %>% group_by(AssignmentType, AssignmentNumber) %>% summarize(meanscore = mean(Score, na.rm = TRUE, ...), .groups = 'drop') %>% filter(AssignmentType == assignmnt & AssignmentNumber == number)
        tidy_table[i,4] <- as.numeric(temp[1,3])
      }
    }
  } else if (center == 'median') {
    if (margin == 2) {
      index <- as.vector(which(rowSums(apply(tidy_table, FUN = is.na, MARGIN = 2)) > 0))
      for (i in index) {
        id <- as.numeric(tidy_table[i,1])
        temp <- tidy_table %>% group_by(UID, AssignmentType) %>% summarize(meanscore = median(Score, na.rm = TRUE, ...), .groups = 'drop') %>% filter(UID == id)
        if (as.character(tidy_table[i,2]) == 'Homework') {
          tidy_table[i,4] <- as.numeric(temp[1,3])
        } else {
          tidy_table[i,4] <- as.numeric(temp[2,3])
        }
      }
    }
    if (margin == 1) {
      index <- as.vector(which(rowSums(apply(tidy_table, FUN = is.na, MARGIN = 2)) > 0))
      for (i in index) {
        assignmnt <- as.character(tidy_table[i,2])
        number <- as.character(tidy_table[i,3])
        temp <- tidy_table %>% group_by(AssignmentType, AssignmentNumber) %>% summarize(meanscore = median(Score, na.rm = TRUE, ...), .groups = 'drop') %>% filter(AssignmentType == assignmnt & AssignmentNumber == number)
        tidy_table[i,4] <- as.numeric(temp[1,3])
      }
    }
  } else {
    stop('ERROR: Center must be mean or median')
  }
  tidy_table
}