# toy_ds_project

"project creation date: May 30, 2023"

"author: Helmi"

MODULE 1: intro

    There are 3 types of cells (markdown, code, and raw)
    
    print() is for display the input string as the output
    
    For number, we don't have to use print() to display the number
    
    toupper() is to change input to all uppercase
    tolower() is to change input to all lower case
    
    max() is consume numbers, and print the max value
    min() is consume numbers, and print the lowest value
    
    read_csv  : for "," as delimeter
    read_csv2 : for ";" as delimeter
    read_tsv  : for tab as delimeter
    read_delim: for general file, take 2 arguments (file=..., delim="...")
    
    col_double       means that the data in this column is a number-type, specifically real numbers (can contain decimals)
    col_integer      means that the data in this column is a number-type, specifically integers (whole numbers)
    col_character    means that the data in this column contains text (e.g., letter or words)
    
    print(your_table, n = total_row) : for print table up to total_row
    nrow(your_table)                 : for print the number of row in your_table
    
    filter() : is to filter the selected data (for filtering number, use =. for filtering string, use ==.)
    select() : is to select the selected collumn, can be more than 1 collumn, use - to eliminate the collumn
    mutate(file, new_data = ...) : is to mutate new data in new collumn with calculation of existing collumn
    
    library(repr)
    options(repr.plot.width=8, repr.plot.height=7) ; code for adjust plot size
    
    ggplot(data = ..., aes(x = ..., y = ...)) + 
        geom_point() + 
        xlab("...") + 
        ylab("...") +
        ggtitle("...") +
        theme(text = element_text(size = 20))        ; simple chart template
        
        
MODULE 2: Reading

    absolute path: begin with /
    relative path: begin without /
    URL          :
    
    argument in read_csv/else:
    file       : file name
    delim      : specify the delimeter
    col_names  : give the collumn name if the data has no collumn name on it
    skip       : tell how many lines to skip
    
    read_csv    : ,
    read_csv2   : ;
    read_tsv    : tab
    read_excel(..., sheet = ...): excel file
    read_delim  : universal 
    
    library(DBI)
    library(RSQLite)
    library(dbplyr)  ; library for pull database files into R notebook
    
    dbConnect(RSQLite::SQLite(), '...')  #replace ... with the database relative path ; open and connect databese to R notebook
    dbListTables(...)  ; to see list of table in the connected database file
    tbl(connected_database_file, table_name)  ;display the asked table in connected database
    
    head(...) : to display few first data on the table
    count(...) : to count how many rows/observation in the table
    
    ... <- collect(...)  ; to download the data into local computer
    write_csv(..., "data/...") ; to store the downloaded data into a designated path
    
    download.file(url, destfile = "data/...") ; to download web data locally
    
    theme(axis.text.x = element_text(angle = 90, hjust = 1) ; code in ggtable to adjust and rotate the axis lable
    
    colnames(...) <- make.names(colnames(...))  ; to subtitute white space to dot in column name
    
    library(cowplot)
    plot_grid(plot1, plot2, ncol = 1) ; to plot two plot each other
    
MODULE 3: Wrangling Data

    |> : pipe operator
    
    group_by(...)                               : divide table based on the column
    summarize(... = ...(..., na.rm = TRUE))     : make a new table and new column with existing group_by column
    
    mutate() , to create a new column
    
    pivot_longer(cols = ...:...,           also can be "cols = everything()" if we want all column
                names_to = "...", 
                values_to = "...")
                
    all_temp_plot <- tidy_temp |> 
    ggplot(aes(x = Year, y = Temperature)) +       facet_wrap(): to make many plot based on category
        geom_point() + 
        facet_wrap(facets = vars(factor(Month, levels = c("Jan","Feb","Mar","Apr","May","Jun",
                                             "Jul","Aug","Sep","Oct","Nov","Dec")))) +
        xlab("Year") + 
        ylab("Temperature") +                          levels = specify the order of the plot
        theme(text = element_text(size=20))
        
    madrid_plot <- madrid_pollution |>
    ggplot(aes(x = mnth, y = max_ebe)) + 
        geom_point() +
        xlab("Year") + 
        ylab("Max Ebe") +
        facet_grid(~ year) +                                         : other options for facet_grid
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        theme(text = element_text(size=20))
        
    map_df(..., na.rm  = TRUE) do function to all column, eg. max, min, mean, etc
    
    arrange()  ; arrange the observation based on the one-column eg. value, desc(value), etc
    
    tail() to show the few first data
    
    pivot_wider(names_from = "...", values_from = "...")
    
MODULE 4: Effective Data Visualization

    Rule of thumbs
    1.	Avoid pie charts
    2.	Make sure that the color of the visualization is color blind friendly
    3.	Avoid 3D graph (confusing)
    4.	Label must be parallel
    5.	Use bar chart as necessary (usually cover most of visualization demand)
    6.	If using scatterplot, adjust the alpha if over plot happens or use another geometric shapes.
    
    A. Histogram : Visualizing the distribution of data in one dimension
    B. Point     : Visualizing the relationship between two variables in two dimensions
    C. Bar       : Comparing amounts
    D. Vline     : Drawing a straight, vertical line at a specified position on the x-axis.
    E. Hline     : Drawing a straight, horizontal line at a specified position on the y-axis.
    F. Line      : Showing a trend with respect to an independent quantity
    
    filter(table, column != ...)   ; to rempve all observation that satisfy the statement 
    filter(table, !is.na(column))  ; to rempve NA in a spesific column
    
    polio_regions_line <- polio |>
    ggplot( aes(x = yr, y = pct_vaccinated)) +          ; template for plot that has aes in geom_*
        geom_line(size = 3,aes(colour = who_region)) +
       labs(x = "Year", y = "vaccine", colour = "blue", shape = "square 
       
    labs(color = "Region of the world")  ; to add legend label based on color
    
    facet_grid(cols = vars(COLUMN_X))      ; Stacks plots horizontally based on COLUMN_X
    facet_grid(rows = vars(COLUMN_X)       : Stacks plots vertically based on COLUMN_X
    
    filter(st %in% c("CA", "WA", "OR"))    ; other way to filter based on specific row needed
    
    geom_bar(stat = "identity")            ; ggplot for geom_bar
    
    coord_flip()                      ; argument on ggplot to switch the axis on the exsisting chart
     
    semi_join(table 1, table 2)       ; give an intersection of the same collumn of table 2
    
    population <- c(39.512, 4.217, 7.615)
    state_counts |>
    mutate(n_per_capita = count / population)   ; do the calculation with value outside the given table
     
    geom_bar(stat = "identity", position = "dodge")  ; use this on ggplot so that the bar is not overlaying each other in each x
    geom_bar(stat = "identity", position = "fill")    ; use this so that the bar is stacking and the y axis turned into proportion
    
    summarize(accros(...:...), ...)   ; other way to do map function in every selected columns
    
    save plot with .svg so that we can re-scale it without losing quality
    
    geom_point(alpha = ...)   ; adjust the transparency [0,1]
    
    bar_plot <- insurance |>
    ggplot(aes(x = sex, fill = smoker)) +          ; bar chart for showing proportion
    geom_bar(position = 'fill') + 
    xlab("Sex") +
    ylab("Total") +
    labs(fill = "Does the person smoke") +
    ggtitle("Proportion of Smokers for Males and Females") 
    
    scale_fill_brewer(palette = "Set1") ; for inserting color pallete in ggplot for categotical variable
    scale_fill_distillerpalette = "Set1") ; for inserting color pallete in ggplot for numerical variable
    
   
MODULE 6: Classification
    
    mutate(table_name, collumn_name = as_factor(collumn_name)).   ; to threat the column as a categorical variable
    
    table_name|>
    slice(row_1,row_2)|>
    select(column(s)_to_determine_the_distance)|>
    dist()                                            ; to calculate the distance between two observation
    
    as.matrix().  ; to make a matrix
    
    table_name|> sampe_n(amount_or_row).   ; to pull some random observation
    
    table_name|> add_row(column_a = ..., column_b = ...)  ; to add observation
    
    
    knn_spec <- nearest_neighbor(weight_func = "rectangular", neighbors = 7) |>
      set_engine("kknn") |>
      set_mode("classification")                   ; model specification
      
    knn_fit <- knn_spec |> 
       fit(...~ ... + ..., data = ...) ; to make a simple training prediction model
       
    predict(knn_fit, new_obs)    ;to predict new observation with the prediction model
    
    
    knn_recipe <- recipe(Class ~ ., data = ...) |> ;to create recepy with all predictor except in step_rm
               step_rm(...)

    preprocessed_data <- knn_recipe |>      ; to examine the recepy, for example, to display the standarized data
                        prep() |> 
                        bake(cancer)
                        
    knn_workflow <- workflow() |>
                  add_recipe(knn_recipe) |>     ; make a workflow from the recipe and model, can actually fit the data here
                  add_model(knn_spec)
                  
    knn_fit_all <- knn_workflow |>              ; to fit the workflow with the data and predicting the obs
            fit(data = cancer)
    class_prediction_all <- predict(knn_fit_all, new_obs_all)
    
MODULE 7: Evaluation and Tuning

    fruit_data_scaled <- fruit_data |> 
        mutate(scaled_mass = scale(mass, center = TRUE), 
               scaled_width = scale(width, center = TRUE)) ; to add column consist of scaled data so that it will be comparable
               
    fruit_split <- initial_split(fruit_data, prop = 0.75 strata = ...)   ; prop: specify how many percent will go to training data 
    fruit_train <- training(fruit_split)                                 ; srata: column that we want to predict
    fruit_test <- testing(fruit_split)
    
    fruit_recipe <- recipe(fruit_name ~ mass + color_score , data = fruit_train) |>
    step_scale(all_predictors()) |>             ; to scale and center the data
    step_center(all_predictors())
    
    predict(fruit_fit , fruit_test) |>        ; predict and add the prediction collumn on the original data
      bind_cols(fruit_test)
      
    fruit_test_predictions |>
        metrics(truth = fruit_name, estimate = .pred_class)   ; to get the accuracy statistics of the prediction
        
    - A true positive  :predict = +, actual = +
    - A true negative  :predict = -, actual = -
    - A false positive :predict = +, actual = -
    - A false negative :predict = -, actual = +
    
    fruit_mat <- fruit_test_predictions |> 
      conf_mat(truth = fruit_name, estimate = .pred_class)   ; create a confusion table of the prediction
      
    fruit_vfold <- vfold_cv(fruit_train, v = 5, strata = fruit_name)  ; create a 5 fold cross validation
    
    fruit_resample_fit <- workflow() |>
      add_recipe(fruit_recipe) |>
      add_model(knn_spec) |>
      fit_resamples(resamples = fruit_vfold)        ; to run a cross-validation on each train created on vfold
      
    fruit_metrics <- collect_metrics(fruit_resample_fit)  ; to aggregate the mean and standart error of the prediction
    
    number_of_rows <- nrow(training_labels)  ; to calculate the n row in the table
      
MODULE 8: Regression

    marathon_spec <- nearest_neighbor(weight_func = "rectangular", neighbors = tune()) |> 
      set_engine("kknn") |>                                                                ; knn model for regression
      set_mode("regression")
      
      
    gridvals <- tibble(neighbors = seq(1, 81, by= 10))          ; to make sequence table that jumps by 10
   

    marathon_results <- workflow() |>
          add_recipe(marathon_recipe) |>
          add_model(marathon_spec) |>
          tune_grid(resamples = marathon_vfold, grid = gridvals) |>      fit the gridvals into the tune_grid
          collect_metrics()
          
    marathon_min <- marathon_results |>         ; to take the K neighbors with the lowest std_err
       filter(.metric == "rmse") |>
       arrange(std_err) |> 
       head(1)
       
       
    REPEAT THE WHOLE PROCESS AFTER GAINING THE BEST K-VALUE
    
    k_min <- marathon_min |>
         pull(neighbors)

    marathon_best_spec <- nearest_neighbor(weight_func = "rectangular", neighbors = k_min) |>
             set_engine("kknn") |>
             set_mode("regression")

    marathon_best_fit <- workflow() |>
             add_recipe(marathon_recipe) |>
             add_model(marathon_best_spec) |>
             fit(data = marathon_training)

    marathon_summary <- marathon_best_fit |>
              predict(marathon_testing) |>
              bind_cols(marathon_testing) |>
              metrics(truth = time_hrs, estimate = .pred)
              
              
MODULE 9: LINEAR REGRESSION
    
    lm_spec <- linear_reg() |>          : model for linear regression
      set_engine("lm") |>
      set_mode("regression")
      
    lm_predictions_test <- marathon_testing |>
    ggplot(aes(x = max, y =time_hrs)) +
        geom_point(alpha = 0.25, size = 2) +
        geom_smooth(data=marathon_training,aes(x=max,y=time_hrs),method = "lm", se = FALSE) +
        xlab("maximum") +
        ylab("time (hours)") +                        ; make a scatterplot with test data and linear line from training data
       theme(text = element_text(size = 20))
       
     
    linear regresion does the job better in linear trending and continuous data, while knn better in predicting non-linear trend data

    METHOD TO FIND THE BEST K VALUE:
    
    credit_spec_2 <- nearest_neighbor(weight_func = "rectangular", neighbors = tune()) |> 
          set_engine("kknn") |>                                                                
          set_mode("regression")

    credit_vfold <- vfold_cv(credit_training, v = 5, strata = Balance) 

    credit_recipe_2 <- recipe(Balance ~., data = credit_training)|>
                step_scale(all_predictors()) |>            
                step_center(all_predictors())

    kgrid <- tibble(neighbors = seq(1,10))

    credit_fit_2 <- workflow() |>
          add_recipe(credit_recipe_2) |>
          add_model(credit_spec_2) |>
          tune_grid(resamples = credit_vfold, grid = kgrid) |>         
          collect_metrics()     

    best_k <- credit_fit_2|>
            filter(.metric == "rmse")|>
            arrange(mean)|>
            select(neighbors)|>
            head(1)|>
            pull()
    END HERE.
    
    knn_rmspe <- predict(credit_fit_3,credit_testing)|>
            bind_cols(credit_testing)|>
            metrics(truth = Balance, estimate = .pred)|>      ; find the RMSE with training data or RMSPE with testing data
            filter(.metric == "rmse")|>
            select(.estimate)|>
            pull()
            
    answer2.2 <- ames_training|> ggpairs(column = 1:ncol(ames_training))   ; to make a trend comparison of each variable
    
MODULE 10: CLUSTERING
    
     filter(!is.na(ibu)) ; to filter out NAs in the dataset
     
     m_pairs <- pm_data |>select ("Total":"Speed")|>ggpairs() ; to perform a ggpairs comparison (it's eazier to select the column first)
    
     scaled_beer <- clean_beer |> 
     mutate(across(everything(), scale))   ; simpler way to scale the dataset in clustering context
     
     beer_cluster_k2 <- kmeans(scaled_beer, centers = 2)  ; perform clustering with self selected n-centers
     
     tidy_beer_cluster_k2 <- augment(beer_cluster_k2, scaled_beer)  ; to bind the cluster column in the scaled dataset, NEW COLUMN: .cluster
     
     beer_cluster_k2_model_stats <- glance(beer_cluster_k2)  ; to get the model statistics of the un-tidy clustering
     
     STEP-BY-STEP TO PERFORM CLUSTERING OF VARIETY VALUE OF K I (remember to scale the dataset first):
     elbow_stats <- tibble(k=seq(1,10))|>
     rowwise() |>
     mutate(poke_clusts = list(kmeans(scaled_km_data, nstart= 10, center=k)))|>
     mutate(glanced = list(glance(poke_clusts)))|>
     select(-poke_clusts)|>
     unnest(glanced)
     DONE HERE.
     
     elbow_plot <- elbow_stats |> 
     ggplot(aes(x=k, y = tot.withinss))+         ; make a total distance comparison line graph based on k value
     geom_line()+
     geom_point()+
     labs(x= "n clusters", y = "within-cluster sum of squares")
     
     
     beer_model_stats |> 
     pull(model_statistics) |>   ; pluck used to pull the list data on the table, consume n-row of observation
     pluck(1) 
     
MODULE 11: INFERENCE 1

     pop_parameters <- can_seniors|>
            summarize(pop_mean = mean(age), pop_med = median(age), pop_sd = sd(age)) ; find mean, median, sd
            
     sample_1 <- can_seniors |> 
     rep_sample_n(size = 40)          ; make a sample with self given size
     
     plot_grid(pop_dist, sample_1_dist, ncol = 2)  ; make a plotgrid (usualy for comparison between population and sample data)
     
     samples <- rep_sample_n(can_seniors, size = 40, reps = 1500) ; to get many sample with self given n

     dim(samples)       ; to get a column x row of the table
     
     sampling_distribution <- sample_estimates|>
                ggplot(aes(x=sample_mean))+
                geom_histogram(binwidth = 1)+
                xlab("Age (years) Average from each sample")+          ; plot-template for the point estimate distribution
                ggtitle("Sampling Distribution of the Sample Means")

     sampling_distribution_100 <- rep_sample_n(students_pop, size = 100, reps = 1500)|>
            group_by(replicate)|>   ; direct code from make many sample to show the distribution of the point interest
            summarise(sample_mean_100 = mean(grade))|>       
            ggplot(aes(x = sample_mean_100))+
            geom_histogram(binwidth = 0.5)+
            labs(x = "mean sample", title = "100 sample mean distribution")
            
MODULE 12: BOOTSTRAP

    
    one_sample <- can_seniors |>     ; ungroup the sample and select it
     rep_sample_n(40) |> 
     ungroup() |> # ungroup the data frame 
     select(age) # drop the replicate column 
     
    boot1000 <- one_sample |>
     rep_sample_n(size = 40, replace = TRUE, reps = 1000)   ; take 1000 bootstrap from sample
     
    boot1000_means <- boot1000|>
        group_by(replicate)|>       ; continue to find the point interest
        summarize(mean = mean(age))
        
    boot1000_means |>  ; find the lower and upper bound for given confidence percentage (95 percent here)
     select(mean) |> 
     pull() |> 
     quantile(c(0.025, 0.975))
    

