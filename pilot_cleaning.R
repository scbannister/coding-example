pilot_cleaning <- function(x){
  
  ## Function is written to clean up pilot dataset (input as "x")
  
  ## Function re-scores trait characteristics measures where needed, creates composite scores for
  ## these trait characteristics, restructures emotion data from wide to long format,
  ## recodes emotion data, and finally calculates emotion dimension scores from the emotion data
  
  ###################################################
  # Rescore individual difference items where needed
  ###################################################
  
  x <- x %>%
    mutate(across(10:477, as.integer)) %>%  # Change all required columns into numerical-type data
    mutate(BIS_BAS_2 = recode(BIS_BAS_2, `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1)) %>%  # Perform reverse scoring for item
    mutate(BIS_BAS_22 = recode(BIS_BAS_22, `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1)) %>% # Perform reverse scoring for item
    mutate(`Anx-SS_19` = recode(`Anx-SS_19`, `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1)) # Perform reverse scoring for item
  
  # Create new columns/variables for trait measure factors (for behavioural activation/BAS, behavioural inhibition/BIS, 
  # trait anxiety, sensation-seeking, trait empathy/IRI)
  x <- x %>%
    rowwise() %>% # This command ensures the subsequent calculations are done on a row-by-row basis (i.e., for each individual participant)
    mutate(BAS_Drive = mean(c(BIS_BAS_3, BIS_BAS_9, BIS_BAS_12, BIS_BAS_21), na.rm = TRUE)) %>% # Create BAS Drive composite score
    mutate(BAS_Fun = mean(c(BIS_BAS_5, BIS_BAS_10, BIS_BAS_15, BIS_BAS_20), na.rm = TRUE)) %>%  # Create BAS Fun composite score
    mutate(BAS_Reward = mean(c(BIS_BAS_4, BIS_BAS_7, BIS_BAS_14, BIS_BAS_18, BIS_BAS_23), na.rm = TRUE)) %>%  # Create BAS Reward composite score
    mutate(BIS = mean(c(BIS_BAS_2, BIS_BAS_8, BIS_BAS_13, BIS_BAS_16, BIS_BAS_19, BIS_BAS_22, BIS_BAS_24), na.rm = TRUE)) %>% # Create BIS composite score
    mutate(anxiety = mean(c(`Anx-SS_1`, `Anx-SS_2`, `Anx-SS_3`, `Anx-SS_4`, `Anx-SS_5`, `Anx-SS_6`, `Anx-SS_7`, `Anx-SS_8`, `Anx-SS_9`, `Anx-SS_10`), na.rm = TRUE)) %>%    # Create trait anxiety composite score
    mutate(sensation = mean(c(`Anx-SS_11`, `Anx-SS_12`, `Anx-SS_13`, `Anx-SS_14`, `Anx-SS_15`, `Anx-SS_16`, `Anx-SS_17`, `Anx-SS_18`, `Anx-SS_19`, `Anx-SS_20`), na.rm = TRUE)) %>%  # Create sensation-seeking composite score
    mutate(IRI_concern = mean(c(Empathy_1, Empathy_5, Empathy_9, Empathy_10), na.rm = TRUE)) %>%  # Create empathic concern composite score
    mutate(IRI_fantasy = mean(c(Empathy_2, Empathy_7, Empathy_11, Empathy_14), na.rm = TRUE)) %>% # Create fantasy composite score
    mutate(IRI_distress = mean(c(Empathy_3, Empathy_8, Empathy_12, Empathy_15), na.rm = TRUE)) %>%  # Create personal distress composite score
    mutate(IRI_perspective = mean(c(Empathy_4, Empathy_6, Empathy_13, Empathy_16), na.rm = TRUE)) # Create perspective taking composite score
  
  # Create overall scores for behavioural activation and trait empathy (IRI)
  x <- x %>%
    rowwise() %>% # This command ensures the subsequent calculations are done on a row-by-row basis (i.e., for each individual participant)
    mutate(BAS = mean(c(BAS_Drive, BAS_Fun, BAS_Reward), na.rm = TRUE)) %>%
    mutate(IRI = mean(c(IRI_concern, IRI_fantasy, IRI_distress, IRI_perspective), na.rm = TRUE))
  
  ######################################
  # Restructure the emotion rating data
  ######################################
  
  final_df <- NULL  # Prepare NULL object for building a new dataset/matrix object in subsequent loop
  
  # Loop iterates through each row of the pilot dataset, and performs actions
  for (k in 1:nrow(x)) {
    
    temp_row <- x[k,] # Extra kth row from dataset
    variable_names <- names(temp_row[70:477]) # extra relevant variable names for the emotion rating data (12 pieces, 32 ratings for each)
    long_form <- NULL   # Prepare NULL object for allowing the shift from wide to long data formatting of emotion rating data
    
    # Subloop (within top-level loop) iterates through each column of the kth row to reformat emotion ratings to long data
    for (j in 1:length(variable_names)) {
      value <- as.numeric(temp_row[,(j + 69)])  # get value in kth row, for (jth + 69) column (as emotion data begins from column 70)
      words <- unlist(str_split(names(temp_row[,(j + 69)]), '_'))  # get string components of column/variable name
      emotion <- substr(words[1], 1, nchar(words[1]) - 1) # Work with string components to extract music emotion condition
      temp_data <- NULL   # Create NULL object for iterative reshaping of data for eventual integration into long dataset
      temp_data$condition <- emotion   # Assign music emotion condition to "condition" variable 
      temp_data$piece <- words[1]   # Assign specific piece from music emotion condtiion (1/2/3) to "piece" variable
      temp_data <- as.data.frame(temp_data) # Change temp_data to dataframe format (from list)
      temp_data$task <- words[2]  # Assign emotion rating task (felt - "feel", or perceived - "perc") to "task" variable
      temp_data$emotion <- words[3] # Assign target emotion being rated to "emotion" variable (we add labels for these targets later, for now they are numbered)
      temp_data$rating <- value   # Add rating given by participant to "rating" variable
      long_form <- rbind(long_form, temp_data)  # Finally, we can horizontally concatenate the temp_data object with the long_form object (and we build iteratively)
    }
    
    # Now we move back to the top-level loop
    
    # Once the long emotion data is created for a participant (i.e., the kth row), we can add the demographics and trait characteristics
    
    long_form$participant <- temp_row$ID
    long_form$BAS_drive <- temp_row$BAS_Drive
    long_form$BAS_fun <- temp_row$BAS_Fun
    long_form$BAS_reward <- temp_row$BAS_Reward
    long_form$BIS <- temp_row$BIS
    long_form$BAS <- temp_row$BAS
    long_form$anxiety <- temp_row$anxiety
    long_form$sensation <- temp_row$sensation
    long_form$IRI_concern <- temp_row$IRI_concern
    long_form$IRI_fantasy <- temp_row$IRI_fantasy
    long_form$IRI_distress <- temp_row$IRI_distress
    long_form$IRI_perspective <- temp_row$IRI_perspective
    long_form$IRI <- temp_row$IRI
    
    final_df <- rbind(final_df, long_form)  # Here, we horizontally concatenate the complete long data for a single participant with the rest (building from NULL object)
  }
  
  rm(long_form, temp_data, temp_row)
  
  # Recode the emotion rating labels (changing from numerical to specific labels)
  final_df <- final_df %>%
    mutate(emotion = recode(emotion, `1` = 'alert', `2` = 'energetic', `3` = 'happy', `4` = 'satisfied', `5` = 'relaxed', `6` = 'inactive', `7` = 'bored', `8` = 'sad', `9` = 'fearful', `10` = 'surprised', `11` = 'perky', `12` = 'calm', `13` = 'sleepy', `14` = 'lethargic', `15` = 'hopeless', `16` = 'angry'))
  
  # The above lines of code mean that some familiarity and liking ratings are recoded as emotions. We can deal with this with the "FL" task value (familiarity/liking), and just relabel the emotions back to the relevant variables
  final_df <- within(final_df, emotion[task == 'FL' & emotion == 'alert'] <- 'familiarity')
  final_df <- within(final_df, emotion[task == 'FL' & emotion == 'energetic'] <- 'liking')
  
  #################################################################################################################
  # Calculate emotion dimensions (positive activation/PA, negative activation/NA, valence, arousal)
  #################################################################################################################
  
  participants <- unique(final_df$participant)  # Get vector of all participant IDs
  pieces <- unique(final_df$piece)  # Get vector of all 12 music pieces
  df_trim <- NULL   # Prepare NULL object (one of two main outputs of the function)
  df_full <- NULL   # Prepare NULL object (one of two main outputs of the function)
  
  # Loop that works iteratively through the final_df long format, for each specific participant in the pilot study
  for (k in 1:length(participants)) {
    temp_participant <- final_df[final_df$participant == participants[k],] # Get kth participant ID
    
    # Subloop within top-level loop, which iterates through each of the 12 music pieces for each specific participant
    for (j in 1:length(pieces)) {
      temp_piece <- temp_participant[temp_participant$piece == pieces[j],]  # Get jth music piece
    
      # For each piece, we need to calculate four emotion dimensions for "felt" emotion, and then for "perceived emotion"
      
      feel <- temp_piece[temp_piece$task == 'Feel',]  # Filter by "felt" emotion ratings and create "feel" object
      
      feel$pa <- with(feel, sum(rating[emotion == 'energetic' | emotion == 'perky'], na.rm = TRUE)) - with(feel, sum(rating[emotion == 'bored' | emotion == 'lethargic'], na.rm = TRUE))  # Create positive activation dimension
      
      feel$na <- with(feel, sum(rating[emotion == 'fearful' | emotion == 'angry'], na.rm = TRUE)) - with(feel, sum(rating[emotion == 'relaxed' | emotion == 'calm'], na.rm = TRUE))  # Create negative activation dimension
      
      feel$valence <- with(feel, sum(rating[emotion == 'happy' | emotion == 'satisfied'], na.rm = TRUE)) - with(feel, sum(rating[emotion == 'sad' | emotion == 'hopeless'], na.rm = TRUE))  # Create valence dimension
      
      feel$arousal <- with(feel, sum(rating[emotion == 'alert' | emotion == 'surprised'], na.rm = TRUE)) - with(feel, sum(rating[emotion == 'inactive' | emotion == 'sleepy'], na.rm = TRUE))  # Create arousal dimension
      
      perc <- temp_piece[temp_piece$task == 'Perc',]  # Filter then by "perceived" emotion ratings and create "perc" object
      
      perc$pa <- with(perc, sum(rating[emotion == 'energetic' | emotion == 'perky'], na.rm = TRUE)) - with(perc, sum(rating[emotion == 'bored' | emotion == 'lethargic'], na.rm = TRUE))  # Create positive activation dimension
      perc$na <- with(perc, sum(rating[emotion == 'fearful' | emotion == 'angry'], na.rm = TRUE)) - with(perc, sum(rating[emotion == 'relaxed' | emotion == 'calm'], na.rm = TRUE))  # Create negative activation dimension
      perc$valence <- with(perc, sum(rating[emotion == 'happy' | emotion == 'satisfied'], na.rm = TRUE)) - with(perc, sum(rating[emotion == 'sad' | emotion == 'hopeless'], na.rm = TRUE))  # Create valence dimension
      perc$arousal <- with(perc, sum(rating[emotion == 'alert' | emotion == 'surprised'], na.rm = TRUE)) - with(perc, sum(rating[emotion == 'inactive' | emotion == 'sleepy'], na.rm = TRUE))  # Create arousal dimension

      # Deal with familiarity and liking ratings too
      familiar_liking <- temp_piece[temp_piece$task == 'FL',] # Filter to focus on familiarity and liking ratings
      familiar <- familiar_liking[1,5]  # We get the familiarity rating by finding value for cell of 1st row, 5th column
      like <- familiar_liking[2,5] # We get the liking rating by finding value for cell of 2nd row, 5th column
      
      feel$familiarity <- familiar  # Assign familiarity rating to variable "familiarity"
      feel$liking <- like   # Assign liking rating to variable "liking"
      perc$familiarity <- familiar  # Assign familiarity rating to variable "familiarity"
      perc$liking <- like   # Assign liking rating to variable "liking"
      
      # Here we can horizontally concatenate the df_full object with the "feel" and "perc" objects (i.e., full data for a single participant),
      # and build iteratively to a full reformatted dataset
      df_full <- rbind(df_full, feel, perc)
      
      # But for further data analysis at level of emotion dimensions, it is also useful to create 
      # a trimmed/condensed dataframe with single row for a participant's responses to one of the 12 pieces of music
      feel <- feel %>%
        select(!(c(emotion, rating, familiarity, liking)))
      feel <- feel[1,]
      feel$familiarity <- familiar  # We need to re-add these
      feel$liking <- like # We need to re-add these
      
      perc <- perc %>%
        select(!(c(emotion, rating, familiarity, liking)))
      perc <- perc[1,]
      perc$familiarity <- familiar # We need to re-add these
      perc$liking <- like # We need to re-add these
      
      # Here we horizontally concatenate the df_trim object with condensed "feel" and "perc" objects 
      # (i.e., single row for a participant response to one of 12 music pieces)
      df_trim <- rbind(df_trim, feel, perc)
    }
  }
    # Function then returns the full and trimmed datasets for further analysis
  return(list(df_full, df_trim))
  
}