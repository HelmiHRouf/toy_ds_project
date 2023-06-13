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