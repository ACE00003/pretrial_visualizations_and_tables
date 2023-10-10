# Pretrial Visualizations and Tables
Visualizations and tables prepared historically for the Pretrial Working Group meetings.

# Initialize libraries needed for programs.
library(dplyr)
library(ggplot2)
library(gtsummary)
library(gt)

# Figure II-1 : Number of New Criminal Cases in the Data Used in this Report, 2018-2020
library(haven)
pretrial_figure_ii_1 <- read_sas("My SAS Files/JointFiles/Pretrial/pretrial_figure_ii_1.sas7bdat", NULL)

# Change "2020Q1 - 2020Q4" Title to just 2020 on the front-end on request
pretrial_figure_ii_1 <- dplyr::mutate(pretrial_figure_ii_1, Pretrial_Report_Year = case_when(
  Pretrial_Report_Year == "2020Q1-2020Q4" ~ "2020"
  , Pretrial_Report_Year == "2021Q1-2021Q4" ~ "2021"
  , Pretrial_Report_Year == "2022Q1-2022Q4" ~ "2022"
  , TRUE ~ Pretrial_Report_Year
  )
)

dev.off()
theme_set(theme_classic())
figure_ii_1 <- ggplot2::ggplot(pretrial_figure_ii_1 
        , aes(x=Pretrial_Report_Year, y=Count)) +
        geom_bar(stat="identity" # Instead of default of counting rows, command allows one to provide y values themselves.
                , aes(fill = Count, width=.5, na.rm=TRUE)
                , fill = "steelblue") +
        geom_text(aes(#size = 7,
              label = scales::comma((Count)), # Enable comma formatting in geom_text
                      group = Pretrial_Report_Year), # Use the group aesthetic to automatically adjust hjust and position for the group
              vjust = -0.4, size = 8.5, # low hjust position the text just next to the bars
              position = position_dodge(width = 0.75)
              ) +
        theme(
              title = element_text(face="bold", size=18, color="black")
              , axis.title.y=element_blank()
              , axis.ticks.x=element_blank()
              , axis.ticks.y=element_blank()
              , axis.text = element_text(size = 24, color = "black")
              , legend.title = element_blank()
              , legend.text = element_text(face="bold", size = 16, color = "black")
              , axis.title = element_text(face="bold", hjust = 0.5, size = 18, color = "black")
              ) +
        labs(title="" #Figure II-1: Number of New Criminal Cases in the Data Used in this Report, 2018Q2-2021Q1"
             , x=""
             , y="") +
        scale_y_continuous(labels = scales::comma # Add commas to thousands on numbers
                           , expand = expansion(mult = c(0, 0.1))) + # Expand space between data and axes, with first variable being multiplicative, and the second variable being additive.
        geom_blank(aes(y=1.1*..count..), stat="count") # Dynamically shrink gap between 0 and x-axis, using 1.1 multiplier to dynamically adjust to the y-axis.

figure_ii_1

# Save graphics file. Default unit is in pixels for .png files
ggsave('figure_ii_1.svg', figure_ii_1, device = "svg", width = 465, height = 225, units = 'mm', dpi = 1080)

# Table II-1 : Characteristics of Individuals in the 2018-2020 Pretrial Population

# Import base pretrial population file "Pretrial_Outcomes" from SAS to do graphics/tables.
pretrial_outcomes <- read_sas("My SAS Files/JointFiles/Pretrial/pretrial_outcomes.sas7bdat", NULL)

library(gtsummary)

## (Universal across all tables and figures)
# Change "2020Q1 - 2020Q4" Title to just 2020 on the front-end on request
pretrial_outcomes <- dplyr::mutate(pretrial_outcomes, Pretrial_Report_Year = case_when(
  Pretrial_Report_Year == "2020Q1-2020Q4" ~ "2020"
  , Pretrial_Report_Year == "2021Q1-2021Q4" ~ "2021"
  , Pretrial_Report_Year == "2022Q1-2022Q4" ~ "2022"
  , TRUE ~ Pretrial_Report_Year
  )
)

# make dataset with a few variables to summarize
pretrial_outcomes2 <- pretrial_outcomes %>% select(Pretrial_Report_Year, Age_Category, ChronicallyHomeless, COURT_Charge_Level,
                                                   gender_recode, Race_Category, Black_Female, Black_TAY, SMI,
                                                   CHARGE_VIOLENT, Nonviolent_Property
                                                   , Nonviolent_Drug, Nonviolent_DUI)

# Create subcategories for vulnerable subgroups
# pretrial_outcomes2$Black_Female <- case_when(pretrial_outcomes2$Race_Category == "Black" &
#                                               pretrial_outcomes2$gender_recode == "Female" ~ 1
#                                                , TRUE ~ 0)
# pretrial_outcomes2$Black_TAY <- case_when(pretrial_outcomes2$Race_Category == "Black" &
#                                            pretrial_outcomes2$Age_Category == "18-25" ~ 1
#                                          , TRUE ~ 0)

# Modify row names for display.
pretrial_outcomes2 <- dplyr::mutate(pretrial_outcomes2, COURT_Charge_Level = case_when(
                                                            COURT_Charge_Level == "F" ~ "Felony",
                                                            COURT_Charge_Level == "M" ~ "Misdemeanor",
                                                            TRUE ~ COURT_Charge_Level
  )
)

# Transform blank character values into true NULL values, so summary table will not show the row summary results.
pretrial_outcomes2$gender_recode <- dplyr::case_when(pretrial_outcomes2$gender_recode == "Unknown" ~ NA_character_, TRUE ~ pretrial_outcomes2$gender_recode)
pretrial_outcomes2$Race_Category <- dplyr::case_when(pretrial_outcomes2$Race_Category == "Unknown" ~ NA_character_, TRUE ~ pretrial_outcomes2$Race_Category)
pretrial_outcomes2$COURT_Charge_Level <- dplyr::case_when(pretrial_outcomes2$COURT_Charge_Level == "" ~ NA_character_, TRUE ~ pretrial_outcomes2$COURT_Charge_Level)

# Transform 

# Sort column order to effect summary table display.
pretrial_outcomes2 <- pretrial_outcomes2[c(1,5,2,6,7,8,3,9,4,13,12,11,10)]

# summarize the data
table_ii_1 <- tbl_summary(pretrial_outcomes2,
                      by = Pretrial_Report_Year, #split table by group
                      label = list(Age_Category ~ "Age Category",
                                   ChronicallyHomeless ~ "Has Experienced Chronic Homelessness",
                                   COURT_Charge_Level ~ "Highest Charge Level",
                                   gender_recode ~ "Sex",
                                   Race_Category ~ "Race/Ethnicity",
                                   SMI ~ "Diagnosed with Severe Mental Illness",
                                   CHARGE_VIOLENT ~ "Violent Charge",
                                   Nonviolent_Property ~ "Non-Violent Property Charge",
                                   Nonviolent_Drug ~ "Non-Violent Drug Charge",
                                   Nonviolent_DUI ~ "Non-Violent DUI Charge",
                                   Black_Female ~ "Black Female",
                                   Black_TAY ~ "Black TAY"
                                        ),
                      missing = "no" #don't list missing data separately
                        ) %>%
                        # modify_caption("Table II-1 : Characteristics of Individuals in the 2018Q2-2021Q1 
                                       # Population") %>%
                        # modify_column_hide(columns = ci) %>%
                        # remove_row_type(Unknown, type = "header") %>%
                        # modify_footnote(
                              #  gender_recode ~ "For 84 cases (0.02% of the sample) we did not have information on the sex of the defendant."
                        
                        #) %>%
                        # Set footnote specific to different charge types
                        modify_table_styling(
                          columns = label,
                          # rows = label == "Violent Charge",
                          # Same footnote for multiple row headers
                          rows = label %in% c("Violent Charge", "Non-Violent Drug Charge", "Non-Violent DUI Charge",
                                            "Non-Violent Property Charge"),
                          footnote = "The various charge types are not mutually exclusive to each other for a given case."
                        ) %>%
                        bold_labels() 

# Use function from gt package to save table, after converting to 
# gt object using as_gt()

gt::gtsave(as_gt(table_ii_1), file = "table_ii_1.png")

table_ii_1 %>%
  as_gt %>%
  gt::gtsave(filename = "Table_II_1.html")

tf <- tempfile(fileext = ".docx")

table_ii_1 %>%
  as_flex_table() %>%
  flextable::save_as_docx(table_ii_1, path = tf)

# Table III-1 : Individual Characteristics for Cases in which the Person was Released During the Pretrial Period vs. the Overall Pretrial Population, 2018Q2-2021Q1

# make dataset with a few variables to summarize
# pretrial_outcomes$detained_during_pretrial_period <-dplyr::case_when(
#                                                                pretrial_outcomes$pretrial_detention_caseongoing == 1 |
#                                                                        pretrial_outcomes$pretrial_detention == 1 ~ 1,
#                                                                TRUE ~ 0
#                                                                        )

# Imported sorted and transposed data in correct format.
library(haven)
table_iii_1_final <- read_sas("My SAS Files/JointFiles/Pretrial/table_iii_1_final.sas7bdat", NULL)

# Change "2020Q1 - 2020Q4" Title to just 2020 on the front-end on request by renaming columns
table_iii_1_final <- dplyr::rename(table_iii_1_final
                                   , "2020" = "2020Q1-2020Q4"
                                   , "2021" = "2021Q1-2021Q4"
                                   , "2022" = "2022Q1-2022Q4"
)

table_iii_1_final <- dplyr::select(table_iii_1_final, -"_NAME_")
table_iii_1_final <- dplyr::filter(table_iii_1_final, !(Characteristic %in% 
                                                                c('I', #'No Court Charge Level',
                                                                  #'Not Chronically Homeless', 'Not SMI',
                                                                  'Under18' # 'Unknown Gender', 'Unknown Race'
                                                                  ))) # remove data rows that will not be shown in the table.

# Filter table to temporarily hide extraneous categories not needed for the final table output.
table_iii_1_final <- dplyr::filter(table_iii_1_final, !(Characteristic %in% 
                                      c(#'F', 'M', 
                                        'No Court Charge Level', 'Not Chronically Homeless', 'Not SMI',
                                        'Unknown Gender', 'Unknown Race', 'Non-Black Female', 'Non-Black TAY',
                                        'Non-DUI Charge', 'Non-Drug Charge', 'Non-Property Charge',
                                        'Non-Violent Charge'
                                      ))) # remove data rows that will not be shown in the table

# Change display name for Characteristics.
table_iii_1_final <- dplyr::mutate(table_iii_1_final, Characteristic = case_when(
                                        Characteristic == "F" ~ "Felony",
                                        Characteristic == "M" ~ "Misdemeanor",
                                        Characteristic == "SMI" ~ "Diagnosed with Severe Mental Illness",
                                        Characteristic == "Chronically Homeless" ~ "Has Experienced Chronic Homelessness",
                                        Characteristic == "DUI Charge" ~ "Non-Violent DUI Charge",
                                        Characteristic == "Drug Charge" ~ "Non-Violent Drug Charge",
                                        Characteristic == "Property Charge" ~ "Non-Violent Property Charge",
                                        TRUE ~ Characteristic
  )
)

# Manually force reorder of row values in data set.
#table_iii_1_final <- table_iii_1_final %>% 
#  mutate(Characteristic = factor(Characteristic, levels =  c("Overall", "Female", "Male",
#                                                             "18-25", "26-39", "40-64", "65+",
#                                                             "Asian", "Black", "Hispanic", "Other Race", "White",
#                                                             "Black Female", "Black TAY",
#                                                             "Chronically Homeless", "Diagnosed with Severe Mental Illness",
#                                                             "Felony", "Misdemeanor",
#                                                             "Violent Charge", "Drug Charge", "DUI Charge", "Property Charge")))

# Manually force reorder of group values in data set.
# table_iii_1_final <- table_iii_1_final %>% 
#  mutate(Subcategory = factor(Subcategory, levels =  c("Overall", "Sex", "Age", "Race/Ethnicity", "Intersectional Groups",
#                                                             "Vulnerable Groups", "Case Charge Level",
#                                                             "Case Charge Type")))

#table_iii_1_final <-
#table_iii_1_final %>% 
#  mutate(Characteristic = forcats::fct_relevel(Characteristic, c("Overall", "Female", "Male",
#                                                     "18-25", "26-39", "40-64", "65+",
#                                                     "Asian", "Black", "Hispanic", "Other Race", "White",
#                                                     "Black Female", "Black TAY",
#                                                     "Chronically Homeless", "Diagnosed with Severe Mental Illness",
#                                                     "Felony", "Misdemeanor",
#                                                     "Violent Charge", "Drug Charge", "DUI Charge", "Property Charge"))) %>% 
#  mutate(Subcategory = forcats::fct_relevel(Subcategory, c("Overall", "Sex", "Age", "Race/Ethnicity", "Intersectional Groups",
#                                                           "Vulnerable Groups", "Case Charge Level",
#                                                           "Case Charge Type"))) %>% 
#  arrange(Subcategory) %>%
  
#  select(Subcategory, Characteristic, `2020Q1-2020Q4`, `2021Q1-2021Q4`, `2022Q1-2022Q4`) %>% 
#  group_by(Subcategory) %>%
#  gt(groupname_col = 'SubCategory', 
#     rowname_col = 'Characteristic')

# Turn table into a gt object (format)
gt_tbl <- gt(table_iii_1_final,
             #groupname_col = "Subcategory",
             rowname_col = "Characteristic" # Need to specifically name which records are tied to rows in the output table.
             # rowname_col = "charOrdered",
             #caption = NULL,
             #rownames_to_stub = FALSE,
             #auto_align = TRUE,
        #id = NULL,
        #row_group.sep = getOption("gt.row_group.sep", " - ")
) #%>%
#  row_group_order(groups = c("Overall", "Sex", "Age", "Race/Ethnicity", "Intersectional Groups",
#                             "Vulnerable Groups", "Case Charge Level",
#                             "Case Charge Type"))

# Title and Subtitle
gt_tbl <- 
        gt_tbl %>%
        #tab_header(
                #title = html("<em>Table III-1 : Individual Characteristics for Cases in which the Person was Released During the
                        #Pretrial Period vs. the Overall Pretrial Population, 2018Q2-2021Q1"), # Italics
                #subtitle = "The top ten largest are presented"
        #) %>%
        tab_options(
                heading.title.font.size = 16,
                heading.align = "center",
                table.border.top.color = "black",
                column_labels.border.bottom.color = "black",
                row.striping.include_table_body = FALSE
        ) %>%
        fmt_number(
                columns = 2:4,
                decimals = 0
        ) %>%
        cols_align(
                align = "center",
                columns = 2:4
        )
# Add black borders to the bottom of all the column labels
        #tab_style(
        #style = list(
                #cell_borders(
                        #sides = "bottom",
                        #color = "black",
                        #weight = px(3)
                #))
        #) %>%

# Create and order row groups in table
# Note: Reverse order in order implementation
gt_tbl <- 
      gt_tbl %>%
        tab_row_group(
        label = "Case Charge Type",
        rows = c("Violent Charge", "Non-Violent Drug Charge", "Non-Violent DUI Charge", "Non-Violent Property Charge")
        ) %>% 
        tab_row_group(
                label = "Highest Charge Level",
                rows = c("Misdemeanor", "Felony")
        ) %>%
        tab_row_group(
                label = "Vulnerable Groups",
                rows = c("Diagnosed with Severe Mental Illness", "Has Experienced Chronic Homelessness")
        ) %>%
        tab_row_group(
                label = "Intersectional Groups",
                rows = c("Black TAY", "Black Female")
        ) %>%
        tab_row_group(
                label = "Race/Ethnicity",
                rows = c("Hispanic", "Black", "White", "Asian", "Other Race")
        ) %>%
        tab_row_group(
                label = "Age",
                rows = c("18-25", "26-39", "40-64", "65+")
        ) %>%
        tab_row_group(
                label = "Sex",
                rows = c("Female", "Male")
        ) %>%
        tab_row_group(
                label = "",
                rows = c("Overall")
        ) %>%
        fmt_percent(
                #data,
                columns = everything(),
                rows = everything(),
                decimals = 0,
                #drop_trailing_zeros = FALSE,
                #pattern = "{x}",
                #sep_mark = ",",
                #dec_mark = ".",
                #force_sign = FALSE,
                #incl_space = FALSE,
                placement = "right",
                #locale = NULL
        ) %>%
        tab_style(
                locations = list(cells_column_labels(columns = everything()),
                                 cells_title(groups = "title"),
                                 cells_row_groups(groups = everything())
                        ),
                style     = list(cell_text(weight = "bold", size = 15))
        ) #%>%
        #opt_row_striping() %>% # strip stripes from row dividers
        #opt_table_lines(extent = c("none"))
                
gt_tbl

# Export and save file.
# Saving as PNG file results in a cropped image of an HTML table; the amount of whitespace can be set
gt_tbl %>%
        gtsave(
                "Table_III_1.png", expand = 10
                # ,path = tempdir()
        )

gt_tbl %>%
  gt::gtsave(filename = "Table_III_1.html")

# this works only on gtsummary table objects
gt_tbl %>%
  as_flex_table() %>%
  flextable::save_as_docx()

# Table III-2 : Individual Characteristics for Cases in which the Person was Released During the Pretrial Period, by Type of Release

# Import tables to stack horizontally.
library(haven)
tab_iiib_cite_released_rate <- read_sas("My SAS Files/JointFiles/Pretrial/tab_iiib_cite_released_rate.sas7bdat", NULL)
tab_iiib_other_released_rate <- read_sas("My SAS Files/JointFiles/Pretrial/tab_iiib_other_released_rate.sas7bdat", NULL)
tab_iiib_or_released_rate <- read_sas("My SAS Files/JointFiles/Pretrial/tab_iiib_or_released_rate.sas7bdat", NULL)
tab_iiib_bail_bond_released_rate <- read_sas("My SAS Files/JointFiles/Pretrial/tab_iiib_bail_bond_released_rate.sas7bdat", NULL)

# Create 4 different tables, by release types, of summarized results that are to be stitched later.
tab_iiib_bind <- cbind(tab_iiib_cite_released_rate, tab_iiib_or_released_rate, tab_iiib_bail_bond_released_rate,
      tab_iiib_other_released_rate)

tab_iiib_bind <- tab_iiib_bind[c(1,3:5,8:10,13:15,18:20)] # Need to filter by column numbers, since column names are not unique and are repeated.

tab_iiib_bind <- dplyr::filter(tab_iiib_bind, !(Characteristic %in% 
                                                                c('I', #'No Court Charge Level',
                                                                  #'Not Chronically Homeless', 'Not SMI',
                                                                  'Under18' # 'Unknown Gender', 'Unknown Race'
                                                                ))) # remove data rows that will not be shown in the table.

# Filter table to temporarily hide extraneous categories not needed for the final table output.
tab_iiib_bind <- dplyr::filter(tab_iiib_bind, !(Characteristic %in% 
                                                          c(#'F', 'M', 
                                                            'No Court Charge Level', 'Not Chronically Homeless', 'Not SMI',
                                                            'Unknown Gender', 'Unknown Race',
                                                            'Non-Black Female', 'Non-Black TAY',
                                                            'Non-DUI Charge', 'Non-Drug Charge', 'Non-Property Charge',
                                                            'Non-Violent Charge'
                                                          ))) # remove data rows that will not be shown in the table

# Change display name for Characteristics.
tab_iiib_bind <- dplyr::mutate(tab_iiib_bind, Characteristic = case_when(
                                        Characteristic == "F" ~ "Felony",
                                        Characteristic == "M" ~ "Misdemeanor",
                                        Characteristic == "SMI" ~ "Diagnosed with Severe Mental Illness",
                                        Characteristic == "Chronically Homeless" ~ "Has Experienced Chronic Homelessness",
                                        Characteristic == "DUI Charge" ~ "Non-Violent DUI Charge",
                                        Characteristic == "Drug Charge" ~ "Non-Violent Drug Charge",
                                        Characteristic == "Property Charge" ~ "Non-Violent Property Charge",
                                        TRUE ~ Characteristic
  )
)

# Change "2020Q1 - 2020Q4" Title to just 2020 on the front-end on request by renaming columns
tab_iiib_bind <- dplyr::rename(tab_iiib_bind
                                 , "2020" = "2020Q1-2020Q4"
                                 , "2021" = "2021Q1-2021Q4"
                                 , "2022" = "2022Q1-2022Q4"
)

# Turn table into a gt object (format)
gt_tbl3_2 <- gt(tab_iiib_bind,
                rowname_col = "Characteristic" # Need to specifically name which records are tied to rows in the output table.
                #groupname_col = dplyr::group_vars(data),
                #caption = NULL,
                #rownames_to_stub = FALSE,
                #auto_align = TRUE,
                #id = NULL,
                #row_group.sep = getOption("gt.row_group.sep", " - ")
)

# Title and Subtitle
gt_tbl3_2 <- 
        gt_tbl3_2 %>%
        #tab_header(
                #title = html("<em>Table III-2 : Individual Characteristics for Cases in which the Person was Released
                             #During the Pretrial Period, by Type of Release"), # Italics
                #subtitle = "The top ten largest are presented"
        #) %>%
        tab_options(
                heading.title.font.size = 16,
                heading.align = "center",
                table.border.top.color = "black",
                column_labels.border.bottom.color = "black",
                row.striping.include_table_body = FALSE
        ) %>%
        fmt_number(
                columns = 2:13,
                decimals = 0
        ) %>%
        cols_align(
                align = "center",
                columns = 2:13
        )

# Create and order row groups in table
# Note: Reverse order in order implementation
gt_tbl3_2 <- 
        gt_tbl3_2 %>% 
        tab_row_group(
          label = "Case Charge Type",
          rows = c("Violent Charge", "Non-Violent Drug Charge", "Non-Violent DUI Charge", "Non-Violent Property Charge")
        ) %>% 
        tab_row_group(
          label = "Highest Charge Level",
          rows = c("Misdemeanor", "Felony")
        ) %>%
        tab_row_group(
          label = "Vulnerable Groups",
          rows = c("Diagnosed with Severe Mental Illness", "Has Experienced Chronic Homelessness")
        ) %>%
        tab_row_group(
          label = "Intersectional Groups",
          rows = c("Black TAY", "Black Female")
        ) %>%
        tab_row_group(
                label = "Race/Ethnicity",
                rows = c("Hispanic", "Black", "White", "Asian", "Other Race")
        ) %>%
        tab_row_group(
                label = "Age",
                rows = c("18-25", "26-39", "40-64", "65+")
        ) %>%
        tab_row_group(
                label = "Sex",
                rows = c("Female", "Male")
        ) %>%
        tab_row_group(
                label = "",
                rows = c("Overall")
        ) %>%
        fmt_percent(
                #data,
                columns = everything(),
                rows = everything(),
                decimals = 0,
                #drop_trailing_zeros = FALSE,
                #pattern = "{x}",
                #sep_mark = ",",
                #dec_mark = ".",
                #force_sign = FALSE,
                #incl_space = FALSE,
                placement = "right",
                #locale = NULL
        ) %>%

# Create column spanners for categorization and labelling of column groups.
        
        tab_spanner(
                label = "Cited and Released",
                columns = c("2020", "2021", "2022")
        ) %>%
        tab_spanner(
                label = "Released, OR",
                columns = c("2020Q1-2020Q4.1", "2021Q1-2021Q4.1", "2022Q1-2022Q4.1")
        ) %>%
        tab_spanner(
                label = "Released, Bail/Bond",
                columns = c("2020Q1-2020Q4.2", "2021Q1-2021Q4.2", "2022Q1-2022Q4.2")
        ) %>%
        tab_spanner(
                label = "Released, Other",
                columns = c("2020Q1-2020Q4.3", "2021Q1-2021Q4.3", "2022Q1-2022Q4.3")
        ) %>%

        tab_style(
                locations = list(cells_column_labels(columns = everything()),
                                cells_title(groups = "title"),
                                cells_row_groups(groups = everything()),
                                cells_column_spanners(spanners = everything())
                                ),
                style     = list(cell_text(weight = "bold", size = 12))
        ) %>%
        tab_style(
                locations = list(cells_body(columns = everything()),
                                 cells_column_spanners(spanners = everything()),
                                 cells_column_labels(columns = everything())
                  ),
                style = list(cell_text(size = "small"))
        ) %>%
        cols_label(
                "2020Q1-2020Q4.1" = "2020",
                "2021Q1-2021Q4.1" = "2021",
                "2022Q1-2022Q4.1" = "2022",
                "2020Q1-2020Q4.2" = "2020",
                "2021Q1-2021Q4.2" = "2021",
                "2022Q1-2022Q4.2" = "2022",
                "2020Q1-2020Q4.3" = "2020",
                "2021Q1-2021Q4.3" = "2021",
                "2022Q1-2022Q4.3" = "2022"
        ) %>% # Change column names now that column spanners have been named using unique columns.
        # Change table width to fit large table without scroll bar.
        #tab_options(
          #table.width = pct(70)
          #table.margin.left = pct(95),
          #table.margin.right = pct(95)
        #)
          cols_width(
            #num ~ px(150),
            #ends_with("1") ~ px(70),
            starts_with("20") ~ px(55),
            #everything() ~ px(60)
          ) #%>%
        #opt_table_font(font = google_font("Fira Mono"))
        #opt_table_font(font = google_font("PT Sans Narrow"))
#opt_row_striping() %>% # strip stripes from row dividers
#opt_table_lines(extent = c("none"))

gt_tbl3_2

gt_tbl3_2 %>%
        gtsave(
                "Table_III_2.png", expand = 10
                # ,path = tempdir()
        )

gt_tbl3_2 %>%
  gt::gtsave(filename = "Table_III_2.html")

# Figure IV-1 : Median of Detention Length for Clients of Pretrial Reform Efforts, by Program

# Imported sorted and transposed data in correct format.
library(haven)
fig_iv_1_final <- read_sas("My SAS Files/JointFiles/Pretrial/fig_iv_1_final.sas7bdat", NULL)
fig_iv_1_final <- dplyr::select(fig_iv_1_final, -"_NAME_")
fig_iv_1_final <- dplyr::filter(fig_iv_1_final, !(Characteristic %in% 
                                                      c('I', 'No Court Charge Level',
                                                        'Not Chronically Homeless', 'Not SMI',
                                                        'Under18', 'Unknown Gender', 'Unknown Race'
                                                      ))) # remove data rows that will not be shown in the table.

# Change display name for Characteristics.
fig_iv_1_final <- dplyr::mutate(fig_iv_1_final, Characteristic = case_when(
  Characteristic == "PSA" ~ "PREP, Pre-Arraignment (PSA)",
  Characteristic == "CCAT" ~ "PREP, Post-Arraignment (CCAT)",
  Characteristic == "Bail_Project" ~ "The Bail Project",
  Characteristic == "Project180" ~ "PREP Services, Project 180",
  Characteristic == "Maternal_Health" ~ "ODR, Maternal Health",
  Characteristic == "MIST" ~ "ODR, MIST-CBR",
  Characteristic == "Housing" ~ "ODR Housing",
  Characteristic == "FIST" ~ "ODR, FIST-CBR",
  Characteristic == "DSH" ~ "ODR, DSH Diversion",
  TRUE ~ Characteristic
  )
)

# Turn table into a gt object (format)
fig_iv_1_table <- gt(fig_iv_1_final,
                      rowname_col = "Characteristic" # Need to specifically name which records are tied to rows in the output table.
)

# Title and Subtitle
fig_iv_1_table <- 
  fig_iv_1_table %>%
  #tab_header(
  #title = html("<em>Table III-1 : Individual Characteristics for Cases in which the Person was Released During the
  #Pretrial Period vs. the Overall Pretrial Population, 2018Q2-2021Q1"), # Italics
  #) %>%
  tab_options(
    heading.title.font.size = 16,
    heading.align = "center",
    table.border.top.color = "black",
    column_labels.border.bottom.color = "black",
    row.striping.include_table_body = FALSE
  ) %>%
  fmt_number(
    columns = 2:4,
    decimals = 0
  ) %>%
  cols_align(
    align = "center",
    columns = 2:4
  )

# Create and order row groups in table
# Note: Reverse order in order implementation
fig_iv_1_table <- 
  fig_iv_1_table %>% 
  tab_row_group(
    label = "Supportive Services Programs",
    rows = c("PREP Services, Project 180", "ODR, Maternal Health", "ODR, MIST-CBR", "ODR Housing", "ODR, FIST-CBR", "ODR, DSH Diversion")
  ) %>%
  tab_row_group(
    label = "Release Programs",
    rows = c("PREP, Pre-Arraignment (PSA)", "PREP, Post-Arraignment (CCAT)", "The Bail Project")
  ) %>%
  tab_row_group(
    label = "",
    rows = c("Overall")
  ) %>%
  tab_style(
    locations = list(cells_column_labels(columns = everything()),
                     cells_title(groups = "title"),
                     cells_row_groups(groups = everything())
    ),
    style     = list(cell_text(weight = "bold", size = 15))
  )

fig_iv_1_table

#Export and save file.
# Saving as PNG file results in a cropped
# image of an HTML table; the amount of
# whitespace can be set
fig_iv_1_table %>%
  gtsave(
    "Figure_IV_1.png", expand = 10
    # ,path = tempdir()
  )


# Tables in Section IV
# Need to use "Sect_IV_Program_Report_Year" as a different measure of release time for programs.
# make dataset with fewer variables to summarize
pretrial_outcomes4 <- pretrial_outcomes %>% select(
  # Characteristic/Reporting Variables
  Sect_IV_Program_Report_Year, Age_Category, ChronicallyHomeless,
  COURT_Charge_Level, gender_recode, Race_Category, SMI,
  # Program Variables
  Bail_Project_Release, CCAT_Assessed, CCAT_Release_Decision, DSH_Diversion_Flag,
  FIST_Flag, Maternal_Health_Flag, MIST_Flag, ODR_Housing_Flag,
  Project180_Client, PSA_Release,
  # Non-Cite Pretrial Release - needed variables
  pretrial_release_noncite
)

# Optional - Limit data selection to only those tied to specific reporting years
pretrial_outcomes4 <- dplyr::filter(pretrial_outcomes4, Sect_IV_Program_Report_Year %in% 
                                      c("2018Q2-2019Q1", "2019Q2-2020Q1", "2020Q2-2021Q1")
                                    )

# Create separate tables for each program in Section 4 tables, so they maintain mutual exclusive counts and stack up together later for the gtsummary tables.
pretrial_outcomes4_Bailproject <- filter(pretrial_outcomes4, Bail_Project_Release == 1)
pretrial_outcomes4_CCAT <- filter(pretrial_outcomes4, CCAT_Release_Decision == "Favorable - Supervised Release" |
                                    CCAT_Release_Decision == "Favorable - OR Release")
pretrial_outcomes4_DSH <- filter(pretrial_outcomes4, DSH_Diversion_Flag == 1)
pretrial_outcomes4_FIST <- filter(pretrial_outcomes4, FIST_Flag == 1)
pretrial_outcomes4_MaternalHealth <- filter(pretrial_outcomes4, Maternal_Health_Flag == 1)
pretrial_outcomes4_MIST <- filter(pretrial_outcomes4, MIST_Flag == 1)
pretrial_outcomes4_ODR_Housing <- filter(pretrial_outcomes4, ODR_Housing_Flag == 1)
pretrial_outcomes4_Proj180 <- filter(pretrial_outcomes4, Project180_Client == 1)
pretrial_outcomes4_PSA <- filter(pretrial_outcomes4, PSA_Release == 1)
pretrial_outcomes4_NonCitePretrialRelease <- filter(pretrial_outcomes4, pretrial_release_noncite == 1)

# Create variable to separately identify and summarize program data with gtsummary.
pretrial_outcomes4_Bailproject <- dplyr::mutate(pretrial_outcomes4_Bailproject, program = "The Bail Project")
pretrial_outcomes4_CCAT <- dplyr::mutate(pretrial_outcomes4_CCAT, program = "PREP, CCAT (Post-Arraignment)")
pretrial_outcomes4_DSH <- dplyr::mutate(pretrial_outcomes4_DSH, program = "ODR, DSH Diversion")
pretrial_outcomes4_FIST <- dplyr::mutate(pretrial_outcomes4_FIST, program = "ODR, FIST-CBR")
pretrial_outcomes4_MaternalHealth <- dplyr::mutate(pretrial_outcomes4_MaternalHealth, program = "ODR, Maternal Health")
pretrial_outcomes4_MIST <- dplyr::mutate(pretrial_outcomes4_MIST, program = "ODR, MIST-CBR")
pretrial_outcomes4_ODR_Housing <- dplyr::mutate(pretrial_outcomes4_ODR_Housing, program = "ODR Housing")
pretrial_outcomes4_Proj180 <- dplyr::mutate(pretrial_outcomes4_Proj180, program = "Project 180, PREP Services")
pretrial_outcomes4_PSA <- dplyr::mutate(pretrial_outcomes4_PSA, program = "PREP, PSA (Pre-Arraignment)")
pretrial_outcomes4_NonCitePretrialRelease <- dplyr::mutate(pretrial_outcomes4_NonCitePretrialRelease, program = "Non-Cite Pretrial Releases")

# Table IV-1 : Characteristics of Individuals Released Pretrial Due to PREP and TBP

theme_gtsummary_compact()

# Limit data selection to only those tied to the output table.
pretrial_outcomes4_1 <- rbind(
  pretrial_outcomes4_PSA
  , pretrial_outcomes4_CCAT
  , pretrial_outcomes4_Bailproject
  , pretrial_outcomes4_NonCitePretrialRelease
)

# Change display name for Characteristics.
pretrial_outcomes4_1 <- dplyr::mutate(pretrial_outcomes4_1, COURT_Charge_Level = case_when(
                      COURT_Charge_Level == "F" ~ "Felony",
                      COURT_Charge_Level == "M" ~ "Misdemeanor",
                      TRUE ~ COURT_Charge_Level
  )
)

# Sort column order to effect summary table display.
pretrial_outcomes4_1 <- pretrial_outcomes4_1[c(1,5,2,6,7,3,4,8:19)]

table_iv_1 <-
  pretrial_outcomes4_1 %>%
  select(Sect_IV_Program_Report_Year, gender_recode, Age_Category, Race_Category, SMI, ChronicallyHomeless,
         COURT_Charge_Level, program) %>%
  # mutate(gender_recode = paste("Gender", gender_recode)) %>%
  tbl_strata( # stratify column headings into distinct overhead categorizations.
    strata = program,
    .tbl_fun =
      ~ .x %>%
      tbl_summary(by = Sect_IV_Program_Report_Year,
                  label = list(gender_recode ~ "Sex",
                              Age_Category ~ "Age Category",
                              Race_Category ~ "Race/Ethnicity",
                              SMI ~ "Diagnosed with Severe Mental Illness",
                              ChronicallyHomeless ~ "Has Experienced Chronic Homelessness",
                              COURT_Charge_Level ~ "Highest Charge Level"
                              ),
                  statistic = all_categorical() ~ c("{p}%"), # Remove numbers and only display percentages in summary table.
                  missing = "no") #%>%
    # add_n() # add total number count
  ) %>%
  # modify_table_body(~.x %>% dplyr::relocate("Non-Cite Pretrial Releases", .after = "The Bail Project")) %>% #Modify and move column locations.
  #modify_caption("Table IV-1 : Characteristics of Individuals Released Pretrial Due to PREP and TBP") %>%
  bold_labels()

gt::gtsave(as_gt(table_iv_1), file = "table_iv_1.png")

# reset gtsummary themes
reset_gtsummary_theme()

# Table IV-2 : Individual Characteristics of Clients Enrolled in Programs that Provide Services to Individuals Released Pretrial
# Set compact theme as a workaround to fit content to page.
theme_gtsummary_compact()

# Limit data selection to only those tied to the output table.
pretrial_outcomes4_2 <- rbind(
                              pretrial_outcomes4_DSH
                              , pretrial_outcomes4_FIST
                              , pretrial_outcomes4_MIST 
                              , pretrial_outcomes4_NonCitePretrialRelease
                              )

# Change display name for Characteristics.
pretrial_outcomes4_2 <- dplyr::mutate(pretrial_outcomes4_2, COURT_Charge_Level = case_when(
                                        COURT_Charge_Level == "F" ~ "Felony",
                                        COURT_Charge_Level == "M" ~ "Misdemeanor",
                                        TRUE ~ COURT_Charge_Level
  )
)

# Sort column order to effect summary table display.
pretrial_outcomes4_2 <- pretrial_outcomes4_2[c(1,5,2,6,7,3,4,8:19)]

table_iv_2 <-
        pretrial_outcomes4_2 %>%
        select(Sect_IV_Program_Report_Year, gender_recode, Age_Category, Race_Category, SMI, ChronicallyHomeless,
               COURT_Charge_Level, program) %>%
        # mutate(stage = factor(stage, levels = c("T4", "T3", "T2", "T1"))) %>% # Reorder rows
        tbl_strata( # stratify column headings into distinct overhead categorizations.
                strata = program,
                .tbl_fun =
                        ~ .x %>%
                        tbl_summary(by = Sect_IV_Program_Report_Year,
                                    label = list(gender_recode ~ "Sex",
                                                 Age_Category ~ "Age Category",
                                                 Race_Category ~ "Race/Ethnicity",
                                                 SMI ~ "Diagnosed with Severe Mental Illness",
                                                 ChronicallyHomeless ~ "Has Experienced Chronic Homelessness",
                                                 COURT_Charge_Level ~ "Highest Charge Level"
                                    ),
                                    statistic = all_categorical() ~ c("{p}%"), # Remove numbers and only display percentages in summary table.
                                    missing = "no") #%>%
                        # add_n() # add total number count
        ) %>%
  
  # remove rows (only work on headers)
        #remove_row_type(variable = "Under18", 
                        #type = c("header", "reference", "missing")) %>%
  
        #modify_caption("Table IV-2 : Individual Characteristics of Clients Enrolled in Programs that Provide Services
                       #to Individuals Released Pretrial") %>%
        bold_labels()

gt::gtsave(as_gt(table_iv_2), file = "table_iv_2.png")

# reset gtsummary themes
reset_gtsummary_theme()

# Table IV-3 : Individual Characteristics of Clients Enrolled in Programs that Provide Services to Individuals Released Pretrial

theme_gtsummary_compact()

# Limit data selection to only those tied to the output table.
pretrial_outcomes4_3 <- rbind(
  pretrial_outcomes4_MaternalHealth
  , pretrial_outcomes4_ODR_Housing
  , pretrial_outcomes4_Proj180
  , pretrial_outcomes4_NonCitePretrialRelease
)

# Change display name for Characteristics.
pretrial_outcomes4_3 <- dplyr::mutate(pretrial_outcomes4_3, COURT_Charge_Level = case_when(
                                        COURT_Charge_Level == "F" ~ "Felony",
                                        COURT_Charge_Level == "M" ~ "Misdemeanor",
                                        TRUE ~ COURT_Charge_Level
  )
)

# Sort column order to effect summary table display.
pretrial_outcomes4_3 <- pretrial_outcomes4_3[c(1,5,2,6,7,3,4,8:19)]

table_iv_3 <-
  pretrial_outcomes4_3 %>%
  select(Sect_IV_Program_Report_Year, gender_recode, Age_Category, Race_Category, SMI, ChronicallyHomeless,
         COURT_Charge_Level, program) %>%
  # mutate(gender_recode = paste("Gender", gender_recode)) %>%
  tbl_strata( # stratify column headings into distinct overhead categorizations.
    strata = program,
    .tbl_fun =
      ~ .x %>%
      tbl_summary(by = Sect_IV_Program_Report_Year,
                  label = list(gender_recode ~ "Sex",
                               Age_Category ~ "Age Category",
                               Race_Category ~ "Race/Ethnicity",
                               SMI ~ "Diagnosed with Severe Mental Illness",
                               ChronicallyHomeless ~ "Has Experienced Chronic Homelessness",
                               COURT_Charge_Level ~ "Highest Charge Level"
                  ),
                  statistic = all_categorical() ~ c("{p}%"), # Remove numbers and only display percentages in summary table.
                  missing = "no") #%>%
    # add_n() # add total number count
  ) %>%
  #modify_caption("Table IV-3 : Individual Characteristics of Clients Enrolled in Programs that Provide Services
                       #to Individuals Released Pretrial") %>%
  bold_labels()

gt::gtsave(as_gt(table_iv_3), file = "table_iv_3.png")

# reset gtsummary themes
reset_gtsummary_theme()

# Table V-1 : Rates of Failure to Appear in Court and Rearrest for Individuals Released Pretrial, by Release Type and Charge Levels, 2018Q2-2021Q1

# Import tables to stack horizontally.
library(haven)
tab_v_1_all <- read_sas("My SAS Files/JointFiles/Pretrial/tab_v_1_all.sas7bdat", NULL)

# Filter table to only variables to be shown in output.
tab_v_1 <- tab_v_1_all[c(1,8:10,12:17)]
tab_v_1 <- dplyr::filter(tab_v_1, !(Characteristic %in% 
                                                  c('I', #'No Court Charge Level',
                                                    #'Not Chronically Homeless', 'Not SMI',
                                                    'Under18'
                                                    #'Unknown Gender', 'Unknown Race'
                                                  ))) # remove data rows that will not be shown in the table.

# Filter table to hide variables not planned to be shown in the output.
tab_v_1 <- dplyr::filter(tab_v_1, !(Characteristic %in% 
                                      c('Nonviolent', 'Other_Nonviolent', 'Black_Female', 'Black_TAY'
                                      ))) # remove data rows that will not be shown in the table

# Change row names
tab_v_1 <- dplyr::mutate(tab_v_1, Characteristic = case_when(Characteristic == "Overall" ~ "All Pretrial Releases",
                                                              Characteristic == "Cite" ~ "Cite/release",
                                                              Characteristic == "Noncite" ~ "Non-cite Releases",
                                                              Characteristic == "OR" ~ "Own Recognizance",
                                                              Characteristic == "Bail" ~ "Bail/bond",
                                                              Characteristic == "Other" ~ "Other Release Reasons",
                                                              Characteristic == "Misdemeanor" ~ "Misdemeanor",
                                                             Characteristic == "Mis_Cite" ~ "Misdemeanor - Cite/release",
                                                             Characteristic == "Mis_OR" ~ "Misdemeanor - Own Recognizance",
                                                             Characteristic == "Mis_Bail" ~ "Misdemeanor - Bail/bond",
                                                             Characteristic == "Mis_Other" ~ "Misdemeanor - Other Release Reasons",
                                                             Characteristic == "Felony" ~ "Felony",
                                                             Characteristic == "Fel_Cite" ~ "Felony - Cite/release",
                                                             Characteristic == "Fel_OR" ~ "Felony - Own Recognizance",
                                                             Characteristic == "Fel_Bail" ~ "Felony - Bail/bond",
                                                             Characteristic == "Fel_Other" ~ "Felony - Other Release Reasons",
                                                             Characteristic == "Violent" ~ "Violent Charge",
                                                             Characteristic == "Property" ~ "Non-Violent Property Charge",
                                                             Characteristic == "Drug" ~ "Non-Violent Drug Charge",
                                                             Characteristic == "DUI" ~ "Non-Violent DUI Charge",
                                                             # Characteristic == "Black_Female" ~ "Black Female",
                                                             # Characteristic == "Black_TAY" ~ "Black TAY",
                                                             Characteristic == "Mis_Violent" ~ "Misdemeanor - Violent Charge",
                                                             Characteristic == "Mis_Property" ~ "Misdemeanor - Non-Violent Property Charge",
                                                             Characteristic == "Mis_Drug" ~ "Misdemeanor - Non-Violent Drug Charge",
                                                             Characteristic == "Mis_DUI" ~ "Misdemeanor - Non-Violent DUI",
                                                             Characteristic == "Fel_Violent" ~ "Felony - Violent Charge",
                                                             Characteristic == "Fel_Property" ~ "Felony - Non-Violent Property Charge",
                                                             Characteristic == "Fel_Drug" ~ "Felony - Non-Violent Drug Charge",
                                                             Characteristic == "Fel_DUI" ~ "Felony - Non-Violent DUI",
                                                             )
                        )

# Change "2020Q1 - 2020Q4" Title to just 2020 on the front-end on request by renaming columns
# tab_v_1 <- dplyr::rename(tab_v_1
#                               , "2020" = "2020Q1-2020Q4"
#                               , "2021" = "2021Q1-2021Q4"
#                               , "2022" = "2022Q1-2022Q4"
# )

# Rearrange row order before executing in gt object and functions to preserve ordering in graphics.
# gt_tab_v_1 <- mutate(stage = factor(stage, levels = c("T4", "T3", "T2", "T1"))) %>% 
# tab_v_1 %>%
  # slice(16,2,13,14,1,15,12,9,10,8,11,7,4,5,3,6)
# tab_v_1 %>%
  # dplyr::mutate(Characteristic = forcats::fct_relevel(Characteristic, c("All Pretrial Releases", "Cite/release",
                                                               #"Non-cite releases", "Own Recognizance", "Bail/bond",
                                                               #"Other release reasons", "Misdemeanor", 
                                                               #"Misdemeanor - Cite/release", "Misdemeanor - Own Recognizance",
                                                               #"Misdemeanor - Bail/bond", "Misdemeanor - Other release reasons",
                                                               #"Felony", "Felony - Cite/release", "Felony - Own Recognizance",
                                                               #"Felony - Bail/bond", "Felony - Other release reasons"
                                                               #)))
#tab_v_1$Characteristic <- factor(tab_v_1$Characteristic,
#                                        levels = c("All Pretrial Releases", "Cite/release",
#                                                   "Non-cite releases", "Own Recognizance", "Bail/bond",
#                                                   "Other release reasons", "Misdemeanor", 
#                                                   "Misdemeanor - Cite/release", "Misdemeanor - Own Recognizance",
#                                                   "Misdemeanor - Bail/bond", "Misdemeanor - Other release reasons",
#                                                   "Felony", "Felony - Cite/release", "Felony - Own Recognizance",
#                                                   "Felony - Bail/bond", "Felony - Other release reasons"
#                                        ))

dev.off()

# Turn table into a gt object (format)
gt_tab_v_1 <- gt(tab_v_1,
                rowname_col = "Characteristic" # Need to specifically name which records are tied to rows in the output table.
                #groupname_col = dplyr::group_vars(data),
                #caption = NULL,
                #rownames_to_stub = FALSE,
                #auto_align = TRUE,
                #id = NULL,
                #row_group.sep = getOption("gt.row_group.sep", " - ")
)

# Title and Subtitle
gt_tab_v_1 <- 
  gt_tab_v_1 %>%
  #tab_header(
    #title = html("<em>Table V-1 : Rates of Failure to Appear in Court and Rearrest for Individuals Released Pretrial,
                 #by Release Type and Charge Levels, 2018Q2-2021Q1"), # Italics
    #subtitle = "The top ten largest are presented"
  #) %>%
  tab_options(
    heading.title.font.size = 16,
    heading.align = "center",
    table.border.top.color = "black",
    column_labels.border.bottom.color = "black",
    row.striping.include_table_body = FALSE
  ) %>%
  fmt_number(
    columns = 2:10,
    decimals = 0
  ) %>%
  cols_align(
    align = "center",
    columns = 2:10
  )

# Create and order row groups in table
# Note: Reverse order in order implementation
gt_tab_v_1 <- 
  gt_tab_v_1 %>% 
  #tab_row_group(
    #label = "Case Charge Level",
    #rows = c("M", "F")
  #) %>%
  tab_row_group(
    label = "Highest Charge Level and Case Charge Type",
    rows = c("Misdemeanor - Violent Charge", "Misdemeanor - Non-Violent Drug Charge", 
             "Misdemeanor - Non-Violent Property Charge", "Misdemeanor - Non-Violent DUI",
             "Felony - Violent Charge", "Felony - Non-Violent Drug Charge",
             "Felony - Non-Violent Property Charge", "Felony - Non-Violent DUI"
             )
  ) %>%
  tab_row_group(
    label = "Case Charge Type",
    rows = c("Violent Charge", "Non-Violent Drug Charge", "Non-Violent Property Charge", "Non-Violent DUI Charge")
  ) %>%
  tab_row_group(
    label = "Highest Charge Level and Pretrial Release Type - Felony",
    rows = c("Felony - Cite/release", "Felony - Own Recognizance",
             "Felony - Bail/bond", "Felony - Other Release Reasons")
  ) %>%
  tab_row_group(
    label = "Highest Charge Level and Pretrial Release Type - Misdemeanor",
    rows = c("Misdemeanor - Cite/release", "Misdemeanor - Own Recognizance", "Misdemeanor - Bail/bond",
             "Misdemeanor - Other Release Reasons")
  ) %>%
  tab_row_group(
    label = "Highest Charge Level and Pretrial Release Type",
    rows = c("Misdemeanor", "Felony")
  ) %>%
  tab_row_group(
    label = "Non-cite releases",
    rows = c("Own Recognizance", "Bail/bond", "Other Release Reasons")
  ) %>%
  tab_row_group(
    label = "Pretrial Release Type",
    rows = c("Cite/release", "Non-cite Releases" #, "Own Recognizance", "Bail/bond", "Other release reasons"
             )
  ) %>%
  tab_row_group(
    label = "",
    rows = c("All Pretrial Releases")
  ) %>%
  fmt_percent(
    #data,
    columns = 5:10,
    rows = everything(),
    decimals = 0,
    #drop_trailing_zeros = FALSE,
    #pattern = "{x}",
    #sep_mark = ",",
    #dec_mark = ".",
    #force_sign = FALSE,
    #incl_space = FALSE,
    placement = "right",
    #locale = NULL
  ) %>%
  
  # Create column spanners for categorization and labelling of column groups.
  
  tab_spanner(
    label = "Number Released",
    columns = c("Elig_2020Q1_2020Q4", "Elig_2021Q1_2021Q4", "Elig_2022Q1_2022Q4")
  ) %>%
  tab_spanner(
    label = "Failed to Appear in Court",
    columns = c("FTA_Rate_2020Q1_2020Q4", "FTA_Rate_2021Q1_2021Q4", "FTA_Rate_2022Q1_2022Q4")
  ) %>%
  tab_spanner(
    label = "Rearrested for a New Offense",
    columns = c("Rearrest_Rate_2020Q1_2020Q4", "Rearrest_Rate_2021Q1_2021Q4", "Rearrest_Rate_2022Q1_2022Q4")
  ) %>%
  
  tab_style(
    locations = list(cells_column_labels(columns = everything()),
                     cells_title(groups = "title"),
                     cells_row_groups(groups = everything()),
                     cells_column_spanners(spanners = everything())
    ),
    style     = list(cell_text(weight = "bold", size = 15))
  ) %>%
  #tab_style(
    #locations = list(row_name_col = "Other"),
    #style = list(
     # cell_text(align = "right")
    #)
  #) %>%
  tab_style(
    locations = cells_body(
      columns = 1, 
      rows = 3:4 #Characteristic %in% c("OR", "Other")
    ),
    style = list(
      cell_text(align = "right")
    )
    #locations = cells_body(rows = 2:4),
    #style = list(
      #cell_text(align = "right")
    # Increase indent to these columns to have 2 indent spaces for subcategorization.
  ) %>%
  cols_label(
    "Elig_2020Q1_2020Q4" = "2020",
    "Elig_2021Q1_2021Q4" = "2021",
    "Elig_2022Q1_2022Q4" = "2022",
    "FTA_Rate_2020Q1_2020Q4" = "2020",
    "FTA_Rate_2021Q1_2021Q4" = "2021",
    "FTA_Rate_2022Q1_2022Q4" = "2022",
    "Rearrest_Rate_2020Q1_2020Q4" = "2020",
    "Rearrest_Rate_2021Q1_2021Q4" = "2021",
    "Rearrest_Rate_2022Q1_2022Q4" = "2022"
  ) # Change column names now that column spanners have been named using unique columns.

#opt_row_striping() %>% # strip stripes from row dividers
#opt_table_lines(extent = c("none"))

gt_tab_v_1

#Export and save file.
# Saving as PNG file results in a cropped
# image of an HTML table; the amount of
# whitespace can be set
gt_tab_v_1 %>%
  gtsave(
    "Table_V_1.png", expand = 10
    # ,path = tempdir()
  )

gt_tab_v_1 %>%
  gt::gtsave(filename = "Table_V_1.html")

# Table V-2 : Rates of Failure to Appear in Court and Rearrest for Individuals Released Pretrial, by Individual Characteristics, 2018Q2-2021Q1

# Import tables to stack horizontally.
library(haven)
denom_tab_all <- read_sas("My SAS Files/JointFiles/Pretrial/denom_tab_all.sas7bdat", NULL)

# Filter table to only variables to be shown in output.
tab_v_2 <- denom_tab_all[c(1,8:16)]
tab_v_2 <- dplyr::filter(tab_v_2, !(Characteristic %in% 
                                      c('I', #'No Court Charge Level',
                                        #'Not Chronically Homeless', 'Not SMI',
                                        'Under18' #'Unknown Gender', 'Unknown Race',
                                        #'F', 'M', 'Not SMI', 'Not Chronically Homeless'
                                      ))) # remove data rows that will not be shown in the table.

# Filter table to temporarily hide extraneous categories not needed for the final table output.
tab_v_2 <- dplyr::filter(tab_v_2, !(Characteristic %in% 
                                      c('F', 'M', 'No Court Charge Level', 'Not Chronically Homeless', 'Not SMI',
                                        'Unknown Gender', 'Unknown Race', 'Not Black Female', 'Not Black TAY'
                                      ))) # remove data rows that will not be shown in the table

# Change row names
tab_v_2 <- dplyr::mutate(tab_v_2, Characteristic = case_when(Characteristic == "Overall" ~ "Overall",
                                                             Characteristic == "Male" ~ "Male",
                                                             Characteristic == "Female" ~ "Female",
                                                             Characteristic == "18-25" ~ "18-25",
                                                             Characteristic == "26-39" ~ "26-39",
                                                             Characteristic == "40-64" ~ "40-64",
                                                             Characteristic == "65+" ~ "65 and older",
                                                             Characteristic == "White" ~ "Non-Hispanic White",
                                                             Characteristic == "Black" ~ "Non-Hispanic Black",
                                                             Characteristic == "Hispanic" ~ "Hispanic",
                                                             Characteristic == "Asian" ~ "Non-Hispanic Asian",
                                                             Characteristic == "Other Race" ~ "Other",
                                                             Characteristic == "SMI" ~ "Diagnosed with Severe Mental Illness",
                                                             Characteristic == "Chronically Homeless" ~ "Has Experienced Chronic Homelessness",
                                                             TRUE ~ Characteristic
                                                             
  )
)

dev.off()


# Turn table into a gt object (format)
gt_tab_v_2 <- gt(tab_v_2,
                 rowname_col = "Characteristic" # Need to specifically name which records are tied to rows in the output table.
                 #groupname_col = dplyr::group_vars(data),
                 #caption = NULL,
                 #rownames_to_stub = FALSE,
                 #auto_align = TRUE,
                 #id = NULL,
                 #row_group.sep = getOption("gt.row_group.sep", " - ")
)

# Title and Subtitle
gt_tab_v_2 <- 
  gt_tab_v_2 %>%
  #tab_header(
    #title = html("<em>Table V-2 : Rates of Failure to Appear in Court and Rearrest for Individuals Released Pretrial,
                 #by Individual Characteristics, 2018Q2-2021Q1"), # Italics
    #subtitle = "The top ten largest are presented"
  #) %>%
  tab_options(
    heading.title.font.size = 16,
    heading.align = "center",
    table.border.top.color = "black",
    column_labels.border.bottom.color = "black",
    row.striping.include_table_body = FALSE
  ) %>%
  fmt_number(
    columns = 2:10,
    decimals = 0
  ) %>%
  cols_align(
    align = "center",
    columns = 2:10
  )

# Create and order row groups in table
# Note: Reverse order in order implementation
gt_tab_v_2 <- 
  gt_tab_v_2 %>% 
  tab_row_group(
    label = "Intersectional Groups",
    rows = c("Black Female", "Black TAY")
  ) %>%
  tab_row_group(
    label = "Vulnerable Groups",
    rows = c("Diagnosed with Severe Mental Illness", "Has Experienced Chronic Homelessness")
  ) %>%
  tab_row_group(
    label = "Race/Ethnicity",
    rows = c("Non-Hispanic White", "Non-Hispanic Black", "Hispanic", "Non-Hispanic Asian", "Other")
  ) %>%
  tab_row_group(
    label = "Age Category",
    rows = c("18-25", "26-39", "40-64", "65 and older")
  ) %>%
  tab_row_group(
    label = "Sex",
    rows = c("Male", "Female")
  ) %>%
  tab_row_group(
    label = "",
    rows = c("Overall")
  ) %>%
  fmt_percent(
    #data,
    columns = 5:10,
    rows = everything(),
    decimals = 0,
    #drop_trailing_zeros = FALSE,
    #pattern = "{x}",
    #sep_mark = ",",
    #dec_mark = ".",
    #force_sign = FALSE,
    #incl_space = FALSE,
    placement = "right",
    #locale = NULL
  ) %>%
  
# Create column spanners for categorization and labelling of column groups.
  tab_spanner(
    label = "Number Released",
    columns = c("Elig_2020Q1_2020Q4", "Elig_2021Q1_2021Q4", "Elig_2022Q1_2022Q4")
  ) %>%
  tab_spanner(
    label = "Failed to Appear in Court",
    columns = c("FTA_Rate_2020Q1_2020Q4", "FTA_Rate_2021Q1_2021Q4", "FTA_Rate_2022Q1_2022Q4")
  ) %>%
  tab_spanner(
    label = "Rearrested for a New Offense",
    columns = c("Rearrest_Rate_2020Q1_2020Q4", "Rearrest_Rate_2021Q1_2021Q4", "Rearrest_Rate_2022Q1_2022Q4")
  ) %>%
  
  tab_style(
    locations = list(cells_column_labels(columns = everything()),
                     cells_title(groups = "title"),
                     cells_row_groups(groups = everything()),
                     cells_column_spanners(spanners = everything())
    ),
    style     = list(cell_text(weight = "bold", size = 15))
  ) %>%
  cols_label(
    "Elig_2020Q1_2020Q4" = "2020",
    "Elig_2021Q1_2021Q4" = "2021",
    "Elig_2022Q1_2022Q4" = "2022",
    "FTA_Rate_2020Q1_2020Q4" = "2020",
    "FTA_Rate_2021Q1_2021Q4" = "2021",
    "FTA_Rate_2022Q1_2022Q4" = "2022",
    "Rearrest_Rate_2020Q1_2020Q4" = "2020",
    "Rearrest_Rate_2021Q1_2021Q4" = "2021",
    "Rearrest_Rate_2022Q1_2022Q4" = "2022"
  ) # Change column names now that column spanners have been named using unique columns.

gt_tab_v_2

#Export and save file.
# Saving as PNG file results in a cropped
# image of an HTML table; the amount of
# whitespace can be set
gt_tab_v_2 %>%
  gtsave(
    "Table_V_2.png", expand = 10
    # ,path = tempdir()
  )

gt_tab_v_2 %>%
  gt::gtsave(filename = "Table_V_2.html")

# Table V-3 : Failure to Appear and Rearrest Rates for Individuals Released Pretrial, by Release Program

# Import tables to stack horizontally.
library(haven)
tab_v_3_all <- read_sas("My SAS Files/JointFiles/Pretrial/tab_v_3_all.sas7bdat", NULL)

# For this table, need to merge overall Pretrial Release total numbers with program stats, because the denominators are
# different
tab_v_3_all$Total_2018Q2_2019Q1 <- case_when(tab_v_3_all$Characteristic %in% c("Overall", "Cite", "Noncite") ~ 
                                                   tab_v_3_all$Elig_2018Q2_2019Q1,
                                                 TRUE ~ tab_v_3_all$Prog_Elig_2018Q2_2019Q1
                                                   )
tab_v_3_all$Total_2019Q2_2020Q1 <- case_when(tab_v_3_all$Characteristic %in% c("Overall", "Cite", "Noncite") ~ 
                                               tab_v_3_all$Elig_2019Q2_2020Q1,
                                             TRUE ~ tab_v_3_all$Prog_Elig_2019Q2_2020Q1
)
tab_v_3_all$Total_2020Q2_2021Q1 <- case_when(tab_v_3_all$Characteristic %in% c("Overall", "Cite", "Noncite") ~ 
                                               tab_v_3_all$Elig_2020Q2_2021Q1,
                                             TRUE ~ tab_v_3_all$Prog_Elig_2020Q2_2021Q1
)

# Filter table to only variables to be shown in output.
tab_v_3 <- tab_v_3_all[c(1,27:29,21:26)]

# Change row names
tab_v_3 <- dplyr::mutate(tab_v_3, Characteristic = case_when(Characteristic == "Overall" ~ "All Pretrial Releases",
                                                             Characteristic == "Cite" ~ "Cite/Releases",
                                                             Characteristic == "Noncite" ~ "Non-Cite Pretrial Releases",
                                                             Characteristic == "PSA" ~ "PSA - Overall",
                                                             Characteristic == "PSA_Mis" ~ "PSA - Misdemeanor",
                                                             Characteristic == "PSA_Fel" ~ "PSA - Felony",
                                                             Characteristic == "CCAT" ~ "CCAT - Overall",
                                                             Characteristic == "CCAT_Mis" ~ "CCAT - Misdemeanor",
                                                             Characteristic == "CCAT_Fel" ~ "CCAT - Felony",
                                                             Characteristic == "CCAT_OR" ~ "CCAT - Own Recognizance",
                                                             Characteristic == "CCAT_SR" ~ "CCAT - Supervised Release",
                                                             Characteristic == "CCAT_Low" ~ "CCAT - Low/Moderate",
                                                             Characteristic == "CCAT_High" ~ "CCAT - Moderate/High",
                                                             Characteristic == "CCAT_Invalid" ~ "CCAT - Invalid",
                                                             Characteristic == "Bail_Project" ~ "The Bail Project - Overall",
                                                             Characteristic == "Bail_Project_Mis" ~ "The Bail Project - Misdemeanor",
                                                             Characteristic == "Bail_Project_Fel" ~ "The Bail Project - Felony"
  )
)

dev.off()


# Turn table into a gt object (format)
gt_tab_v_3 <- gt(tab_v_3,
                 rowname_col = "Characteristic" # Need to specifically name which records are tied to rows in the output table.
)

# Title and Subtitle
gt_tab_v_3 <- 
  gt_tab_v_3 %>%
  #tab_header(
  #title = html("<em>Table V-2 : Rates of Failure to Appear in Court and Rearrest for Individuals Released Pretrial,
  #by Individual Characteristics, 2018Q2-2021Q1"), # Italics
  #subtitle = "The top ten largest are presented"
  #) %>%
  tab_options(
    heading.title.font.size = 16,
    heading.align = "center",
    table.border.top.color = "black",
    column_labels.border.bottom.color = "black",
    row.striping.include_table_body = FALSE
  ) %>%
  fmt_number(
    columns = 2:10,
    decimals = 0
  ) %>%
  cols_align(
    align = "center",
    columns = 2:10
  )

# Create and order row groups in table
# Note: Reverse order in order implementation
gt_tab_v_3 <- 
  gt_tab_v_3 %>% 
  tab_row_group(
    label = "The Bail Project - Charge Level",
    rows = c("The Bail Project - Misdemeanor", "The Bail Project - Felony")
  ) %>%
  tab_row_group(
    label = "Releases Due to The Bail Project",
    rows = c("The Bail Project - Overall")
  ) %>%
  tab_row_group(
    label = "CCAT Risk Category",
    rows = c("CCAT - Low/Moderate", "CCAT - Moderate/High", "CCAT - Invalid")
  ) %>%
  tab_row_group(
    label = "CCAT - Release Type",
    rows = c("CCAT - Own Recognizance", "CCAT - Supervised Release")
  ) %>%
  tab_row_group(
    label = "CCAT - Charge Level",
    rows = c("CCAT - Misdemeanor", "CCAT - Felony")
  ) %>%
  tab_row_group(
    label = "PREP Post-arraignment Releases (CCAT)",
    rows = c("CCAT - Overall")
  ) %>%
  tab_row_group(
    label = "PSA - Charge Level",
    rows = c("PSA - Misdemeanor", "PSA - Felony")
  ) %>%
  tab_row_group(
    label = "PREP Pre-arraignment Releases (PSA)",
    rows = c("PSA - Overall")
  ) %>%
  tab_row_group(
    label = "  ",
    rows = c("Non-Cite Pretrial Releases")
  ) %>%
  tab_row_group(
    label = " ",
    rows = c("Cite/Releases")
  ) %>%
  tab_row_group(
    label = "",
    rows = c("All Pretrial Releases")
  ) %>%
  fmt_percent(
    #data,
    columns = 5:10,
    rows = everything(),
    decimals = 0,
    #drop_trailing_zeros = FALSE,
    #pattern = "{x}",
    #sep_mark = ",",
    #dec_mark = ".",
    #force_sign = FALSE,
    #incl_space = FALSE,
    placement = "right",
    #locale = NULL
  ) %>%
  
  # Create column spanners for categorization and labelling of column groups.
  tab_spanner(
    label = "Number Released",
    columns = c("Total_2018Q2_2019Q1", "Total_2019Q2_2020Q1", "Total_2020Q2_2021Q1")
  ) %>%
  tab_spanner(
    label = "Failed to Appear in Court",
    columns = c("FTA_Rate_2018Q2_2019Q1", "FTA_Rate_2019Q2_2020Q1", "FTA_Rate_2020Q2_2021Q1")
  ) %>%
  tab_spanner(
    label = "Rearrested for a New Offense",
    columns = c("Rearrest_Rate_2018Q2_2019Q1", "Rearrest_Rate_2019Q2_2020Q1", "Rearrest_Rate_2020Q2_2021Q1")
  ) %>%
  
  tab_style(
    locations = list(cells_column_labels(columns = everything()),
                     cells_title(groups = "title"),
                     cells_row_groups(groups = everything()),
                     cells_column_spanners(spanners = everything())
    ),
    style     = list(cell_text(weight = "bold", size = 15))
  ) %>%
  cols_label(
    "Total_2018Q2_2019Q1" = "2018Q2-2019Q1",
    "Total_2019Q2_2020Q1" = "2019Q2-2020Q1",
    "Total_2020Q2_2021Q1" = "2020Q2-2021Q1",
    "FTA_Rate_2018Q2_2019Q1" = "2018Q2-2019Q1",
    "FTA_Rate_2019Q2_2020Q1" = "2019Q2-2020Q1",
    "FTA_Rate_2020Q2_2021Q1" = "2020Q2-2021Q1",
    "Rearrest_Rate_2018Q2_2019Q1" = "2018Q2-2019Q1",
    "Rearrest_Rate_2019Q2_2020Q1" = "2019Q2-2020Q1",
    "Rearrest_Rate_2020Q2_2021Q1" = "2020Q2-2021Q1"
  ) # Change column names now that column spanners have been named using unique columns.

gt_tab_v_3

#Export and save file.
# Saving as PNG file results in a cropped
# image of an HTML table; the amount of
# whitespace can be set
gt_tab_v_3 %>%
  gtsave(
    "Table_V_3.png", expand = 10
    # ,path = tempdir()
  )

# Table V-4 : Failure to Appear and Rearrest Rates for Individuals Released Pretrial, by Service Program

# Import tables to stack horizontally.
library(haven)
tab_v_4_all <- read_sas("My SAS Files/JointFiles/Pretrial/tab_v_4_all.sas7bdat", NULL)


# For this table, need to merge overall Pretrial Release total numbers with program stats, because the denominators are
# different
tab_v_4_all$Total_2018Q2_2019Q1 <- case_when(tab_v_4_all$Characteristic %in% c("Overall", "Cite", "Noncite") ~ 
                                               tab_v_4_all$Elig_2018Q2_2019Q1,
                                             TRUE ~ tab_v_4_all$Prog_Elig_2018Q2_2019Q1
)
tab_v_4_all$Total_2019Q2_2020Q1 <- case_when(tab_v_4_all$Characteristic %in% c("Overall", "Cite", "Noncite") ~ 
                                               tab_v_4_all$Elig_2019Q2_2020Q1,
                                             TRUE ~ tab_v_4_all$Prog_Elig_2019Q2_2020Q1
)
tab_v_4_all$Total_2020Q2_2021Q1 <- case_when(tab_v_4_all$Characteristic %in% c("Overall", "Cite", "Noncite") ~ 
                                               tab_v_4_all$Elig_2020Q2_2021Q1,
                                             TRUE ~ tab_v_4_all$Prog_Elig_2020Q2_2021Q1
)

# Filter table to only variables to be shown in output.
tab_v_4 <- tab_v_4_all[c(1,27:29,21:26)]

# Change row names
tab_v_4 <- dplyr::mutate(tab_v_4, Characteristic = case_when(Characteristic == "Overall" ~ "All Pretrial Releases",
                                                             Characteristic == "Cite" ~ "Cite/Releases",
                                                             Characteristic == "Noncite" ~ "Non-Cite Pretrial Releases",
                                                             Characteristic == "DSH" ~ "DSH Diversion",
                                                             Characteristic == "FIST" ~ "FIST-CBR",
                                                             Characteristic == "MIST" ~ "MIST-CBR",
                                                             Characteristic == "Maternal_Health" ~ "Maternal Health",
                                                             Characteristic == "Housing" ~ "Housing",
                                                             Characteristic == "Project180" ~ "Project 180 - Overall",
                                                             Characteristic == "Project180_Mis" ~ "Project 180 - Misdemeanor",
                                                             Characteristic == "Project180_Fel" ~ "Project 180 - Felony"
)
)

dev.off()


# Turn table into a gt object (format)
gt_tab_v_4 <- gt(tab_v_4,
                 rowname_col = "Characteristic" # Need to specifically name which records are tied to rows in the output table.
)

# Title and Subtitle
gt_tab_v_4 <- 
  gt_tab_v_4 %>%
  #tab_header(
  #title = html("<em>Table V-2 : Rates of Failure to Appear in Court and Rearrest for Individuals Released Pretrial,
  #by Individual Characteristics, 2018Q2-2021Q1"), # Italics
  #subtitle = "The top ten largest are presented"
  #) %>%
  tab_options(
    heading.title.font.size = 16,
    heading.align = "center",
    table.border.top.color = "black",
    column_labels.border.bottom.color = "black",
    row.striping.include_table_body = FALSE
  ) %>%
  fmt_number(
    columns = 2:10,
    decimals = 0
  ) %>%
  cols_align(
    align = "center",
    columns = 2:10
  )

# Create and order row groups in table
# Note: Reverse order in order implementation
gt_tab_v_4 <- 
  gt_tab_v_4 %>% 
  tab_row_group(
    label = "Project 180 - Charge Level",
    rows = c("Project 180 - Misdemeanor", "Project 180 - Felony")
  ) %>%
  tab_row_group(
    label = "Project 180, PREP Services",
    rows = c("Project 180 - Overall")
  ) %>%
  tab_row_group(
    label = "ODR Programs",
    rows = c("DSH Diversion", "FIST-CBR", "MIST-CBR", "Maternal Health", "Housing")
  ) %>%
  tab_row_group(
    label = "  ",
    rows = c("Non-Cite Pretrial Releases")
  ) %>%
  tab_row_group(
    label = " ",
    rows = c("Cite/Releases")
  ) %>%
  tab_row_group(
    label = "",
    rows = c("All Pretrial Releases")
  ) %>%
  fmt_percent(
    #data,
    columns = 5:10,
    rows = everything(),
    decimals = 0,
    #drop_trailing_zeros = FALSE,
    #pattern = "{x}",
    #sep_mark = ",",
    #dec_mark = ".",
    #force_sign = FALSE,
    #incl_space = FALSE,
    placement = "right",
    #locale = NULL
  ) %>%
  
  # Create column spanners for categorization and labelling of column groups.
  tab_spanner(
    label = "Number Released",
    columns = c("Total_2018Q2_2019Q1", "Total_2019Q2_2020Q1", "Total_2020Q2_2021Q1")
  ) %>%
  tab_spanner(
    label = "Failed to Appear in Court",
    columns = c("FTA_Rate_2018Q2_2019Q1", "FTA_Rate_2019Q2_2020Q1", "FTA_Rate_2020Q2_2021Q1")
  ) %>%
  tab_spanner(
    label = "Rearrested for a New Offense",
    columns = c("Rearrest_Rate_2018Q2_2019Q1", "Rearrest_Rate_2019Q2_2020Q1", "Rearrest_Rate_2020Q2_2021Q1")
  ) %>%
  
  tab_style(
    locations = list(cells_column_labels(columns = everything()),
                     cells_title(groups = "title"),
                     cells_row_groups(groups = everything()),
                     cells_column_spanners(spanners = everything())
    ),
    style     = list(cell_text(weight = "bold", size = 15))
  ) %>%
  cols_label(
    "Total_2018Q2_2019Q1" = "2018Q2-2019Q1",
    "Total_2019Q2_2020Q1" = "2019Q2-2020Q1",
    "Total_2020Q2_2021Q1" = "2020Q2-2021Q1",
    "FTA_Rate_2018Q2_2019Q1" = "2018Q2-2019Q1",
    "FTA_Rate_2019Q2_2020Q1" = "2019Q2-2020Q1",
    "FTA_Rate_2020Q2_2021Q1" = "2020Q2-2021Q1",
    "Rearrest_Rate_2018Q2_2019Q1" = "2018Q2-2019Q1",
    "Rearrest_Rate_2019Q2_2020Q1" = "2019Q2-2020Q1",
    "Rearrest_Rate_2020Q2_2021Q1" = "2020Q2-2021Q1"
  ) # Change column names now that column spanners have been named using unique columns.

gt_tab_v_4

#Export and save file.
# Saving as PNG file results in a cropped
# image of an HTML table; the amount of
# whitespace can be set
gt_tab_v_4 %>%
  gtsave(
    "Table_V_4.png", expand = 10
    # ,path = tempdir()
  )

# Table VI-1 : Median of Detention Length for Individuals Detained Throughout the Pretrial Period, by Individual Characteristics

# Imported sorted and transposed data in correct format.
library(haven)
tab_vi_1_final <- read_sas("My SAS Files/JointFiles/Pretrial/tab_vi_1_final.sas7bdat", NULL)
tab_vi_1_final <- dplyr::select(tab_vi_1_final, -"_NAME_")

# Change "2020Q1 - 2020Q4" Title to just 2020 on the front-end on request by renaming columns
tab_vi_1_final <- dplyr::rename(tab_vi_1_final
                                   , "2020" = "2020Q1-2020Q4"
                                   , "2021" = "2021Q1-2021Q4"
                                   , "2022" = "2022Q1-2022Q4"
)

tab_vi_1_final <- dplyr::filter(tab_vi_1_final, !(Characteristic %in% 
                                                      c('I', #'No Court Charge Level',
                                                        #'Not Chronically Homeless', 'Not SMI',
                                                        'Under18' # 'Unknown Gender', 'Unknown Race'
                                                      ))) # remove data rows that will not be shown in the table.

# Filter table to temporarily hide extraneous categories not needed for the final table output.
tab_vi_1_final <- dplyr::filter(tab_vi_1_final, !(Characteristic %in% 
                                      c('No Court Charge Level', 'Not Chronically Homeless', 'Not SMI',
                                        'Unknown Gender', 'Unknown Race','Non-Black Female', 'Non-Black TAY',
                                        'Non-DUI Charge', 'Non-Drug Charge', 'Non-Property Charge',
                                        'Non-Violent Charge'
                                      ))) # remove data rows that will not be shown in the table

# Change display name for Characteristics.
tab_vi_1_final <- dplyr::mutate(tab_vi_1_final, Characteristic = case_when(
  Characteristic == "F" ~ "Felony",
  Characteristic == "M" ~ "Misdemeanor",
  Characteristic == "SMI" ~ "Diagnosed with Severe Mental Illness",
  Characteristic == "Chronically Homeless" ~ "Has Experienced Chronic Homelessness",
  Characteristic == "DUI Charge" ~ "Non-Violent DUI Charge",
  Characteristic == "Drug Charge" ~ "Non-Violent Drug Charge",
  Characteristic == "Property Charge" ~ "Non-Violent Property Charge",
  TRUE ~ Characteristic
  )
)

# Turn table into a gt object (format)
tab_vi_1_table <- gt(tab_vi_1_final,
                      rowname_col = "Characteristic" # Need to specifically name which records are tied to rows in the output table.
)

# Title and Subtitle
tab_vi_1_table <- 
  tab_vi_1_table %>%
  #tab_header(
  #title = html("<em>Table III-1 : Individual Characteristics for Cases in which the Person was Released During the
  #Pretrial Period vs. the Overall Pretrial Population, 2018Q2-2021Q1"), # Italics
  #) %>%
  tab_options(
    heading.title.font.size = 16,
    heading.align = "center",
    table.border.top.color = "black",
    column_labels.border.bottom.color = "black",
    row.striping.include_table_body = FALSE
  ) %>%
  fmt_number(
    columns = 2:4,
    decimals = 0
  ) %>%
  cols_align(
    align = "center",
    columns = 2:4
  )

# Create and order row groups in table
# Note: Reverse order in order implementation
tab_vi_1_table <- 
  tab_vi_1_table %>% 
  tab_row_group(
    label = "Case Charge Type",
    rows = c("Violent Charge", "Non-Violent Drug Charge", "Non-Violent DUI Charge", "Non-Violent Property Charge")
  ) %>% 
  tab_row_group(
    label = "Highest Charge Level",
    rows = c("Misdemeanor", "Felony")
  ) %>%
  tab_row_group(
    label = "Vulnerable Groups",
    rows = c("Diagnosed with Severe Mental Illness", "Has Experienced Chronic Homelessness")
  ) %>%
  tab_row_group(
    label = "Intersectional Groups",
    rows = c("Black TAY", "Black Female")
  ) %>%
  tab_row_group(
    label = "Race/Ethnicity",
    rows = c("Hispanic", "Black", "White", "Asian", "Other Race")
  ) %>%
  tab_row_group(
    label = "Age",
    rows = c("18-25", "26-39", "40-64", "65+")
  ) %>%
  tab_row_group(
    label = "Sex",
    rows = c("Female", "Male")
  ) %>%
  tab_row_group(
    label = "",
    rows = c("Overall")
  ) %>%
  tab_style(
    locations = list(cells_column_labels(columns = everything()),
                     cells_title(groups = "title"),
                     cells_row_groups(groups = everything())
    ),
    style     = list(cell_text(weight = "bold", size = 15))
  )

tab_vi_1_table

#Export and save file.
# Saving as PNG file results in a cropped
# image of an HTML table; the amount of
# whitespace can be set
tab_vi_1_table %>%
  gtsave(
    "Table_VI_1.png", expand = 10
    # ,path = tempdir()
  )

tab_vi_1_table %>%
  gt::gtsave(filename = "Table_VI_1.html")

# Table VI-1B : Median of Detention Length for Individuals Detained Throughout the Pretrial Period, by Individual Characteristics (Misdemeanors)

# Imported sorted and transposed data in correct format.
library(haven)
tab_vi_1B_M_final <- read_sas("My SAS Files/JointFiles/Pretrial/tab_vi_1b_m_final.sas7bdat", NULL)
tab_vi_1B_M_final <- dplyr::select(tab_vi_1B_M_final, -"_NAME_")

# Change "2020Q1 - 2020Q4" Title to just 2020 on the front-end on request by renaming columns
tab_vi_1B_M_final <- dplyr::rename(tab_vi_1B_M_final
                                , "2020" = "2020Q1-2020Q4"
                                , "2021" = "2021Q1-2021Q4"
                                , "2022" = "2022Q1-2022Q4"
)

tab_vi_1B_M_final <- dplyr::filter(tab_vi_1B_M_final, !(Characteristic %in% 
                                                    c('I', #'No Court Charge Level',
                                                      #'Not Chronically Homeless', 'Not SMI',
                                                      'Under18' # 'Unknown Gender', 'Unknown Race'
                                                    ))) # remove data rows that will not be shown in the table.

# Filter table to temporarily hide extraneous categories not needed for the final table output.
tab_vi_1B_M_final <- dplyr::filter(tab_vi_1B_M_final, !(Characteristic %in% 
                                                    c('No Court Charge Level', 'Not Chronically Homeless', 'Not SMI',
                                                      'Unknown Gender', 'Unknown Race', 'Non-Black Female', 'Non-Black TAY',
                                                      'Non-DUI Charge', 'Non-Drug Charge', 'Non-Property Charge',
                                                      'Non-Violent Charge'
                                                      , 'M'
                                                    ))) # remove data rows that will not be shown in the table

# Change display name for Characteristics.
tab_vi_1B_M_final <- dplyr::mutate(tab_vi_1B_M_final, Characteristic = case_when(
  Characteristic == "F" ~ "Felony",
  Characteristic == "M" ~ "Misdemeanor",
  Characteristic == "SMI" ~ "Diagnosed with Severe Mental Illness",
  Characteristic == "Chronically Homeless" ~ "Has Experienced Chronic Homelessness",
  Characteristic == "DUI Charge" ~ "Non-Violent DUI Charge",
  Characteristic == "Drug Charge" ~ "Non-Violent Drug Charge",
  Characteristic == "Property Charge" ~ "Non-Violent Property Charge",
  TRUE ~ Characteristic
  )
)

# Turn table into a gt object (format)
tab_vi_1B_M_table <- gt(tab_vi_1B_M_final,
                     rowname_col = "Characteristic" # Need to specifically name which records are tied to rows in the output table.
)

# Title and Subtitle
tab_vi_1B_M_table <- 
  tab_vi_1B_M_table %>%
  #tab_header(
  #title = html("<em>Table III-1 : Individual Characteristics for Cases in which the Person was Released During the
  #Pretrial Period vs. the Overall Pretrial Population, 2018Q2-2021Q1"), # Italics
  #) %>%
  tab_options(
    heading.title.font.size = 16,
    heading.align = "center",
    table.border.top.color = "black",
    column_labels.border.bottom.color = "black",
    row.striping.include_table_body = FALSE
  ) %>%
  fmt_number(
    columns = 2:4,
    decimals = 0
  ) %>%
  cols_align(
    align = "center",
    columns = 2:4
  )

# Create and order row groups in table
# Note: Reverse order in order implementation
tab_vi_1B_M_table <- 
  tab_vi_1B_M_table %>% 
  #Felony doesn't exist in this filtered group
  #tab_row_group(
    #label = "Case Charge Level",
    #rows = c("Misdemeanor", "Felony")
  #) %>%
  tab_row_group(
    label = "Case Charge Type",
    rows = c("Violent Charge", "Non-Violent Drug Charge", "Non-Violent DUI Charge", "Non-Violent Property Charge")
  ) %>% 
  tab_row_group(
    label = "Vulnerable Groups",
    rows = c("Diagnosed with Severe Mental Illness", "Has Experienced Chronic Homelessness")
  ) %>%
  tab_row_group(
    label = "Intersectional Groups",
    rows = c("Black TAY", "Black Female")
  ) %>%
  tab_row_group(
    label = "Race/Ethnicity",
    rows = c("Hispanic", "Black", "White", "Asian", "Other Race")
  ) %>%
  tab_row_group(
    label = "Age",
    rows = c("18-25", "26-39", "40-64", "65+")
  ) %>%
  tab_row_group(
    label = "Sex",
    rows = c("Female", "Male")
  ) %>%
  tab_row_group(
    label = "",
    rows = c("Overall")
  ) %>%
  tab_style(
    locations = list(cells_column_labels(columns = everything()),
                     cells_title(groups = "title"),
                     cells_row_groups(groups = everything())
    ),
    style     = list(cell_text(weight = "bold", size = 15))
  )

tab_vi_1B_M_table

#Export and save file.
# Saving as PNG file results in a cropped
# image of an HTML table; the amount of
# whitespace can be set
tab_vi_1B_M_table %>%
  gtsave(
    "Table_VI_1B_M.png", expand = 10
    # ,path = tempdir()
  )

tab_vi_1B_M_table %>%
  gt::gtsave(filename = "Table_VI_1B_M.html")

# Felonies
# Imported sorted and transposed data in correct format.
library(haven)
tab_vi_1B_F_final <- read_sas("My SAS Files/JointFiles/Pretrial/tab_vi_1b_f_final.sas7bdat", NULL)
tab_vi_1B_F_final <- dplyr::select(tab_vi_1B_F_final, -"_NAME_")

# Change "2020Q1 - 2020Q4" Title to just 2020 on the front-end on request by renaming columns
tab_vi_1B_F_final <- dplyr::rename(tab_vi_1B_F_final
                                   , "2020" = "2020Q1-2020Q4"
                                   , "2021" = "2021Q1-2021Q4"
                                   , "2022" = "2022Q1-2022Q4"
)

tab_vi_1B_F_final <- dplyr::filter(tab_vi_1B_F_final, !(Characteristic %in% 
                                                          c('I', #'No Court Charge Level',
                                                            #'Not Chronically Homeless', 'Not SMI',
                                                            'Under18' # 'Unknown Gender', 'Unknown Race'
                                                          ))) # remove data rows that will not be shown in the table.

# Filter table to temporarily hide extraneous categories not needed for the final table output.
tab_vi_1B_F_final <- dplyr::filter(tab_vi_1B_F_final, !(Characteristic %in% 
                                                          c('No Court Charge Level', 'Not Chronically Homeless', 'Not SMI',
                                                            'Unknown Gender', 'Unknown Race', 'Non-Black Female', 'Non-Black TAY',
                                                            'Non-DUI Charge', 'Non-Drug Charge', 'Non-Property Charge',
                                                            'Non-Violent Charge'
                                                            , 'F'
                                                          ))) # remove data rows that will not be shown in the table

# Change display name for Characteristics.
tab_vi_1B_F_final <- dplyr::mutate(tab_vi_1B_F_final, Characteristic = case_when(
  Characteristic == "F" ~ "Felony",
  Characteristic == "M" ~ "Misdemeanor",
  Characteristic == "SMI" ~ "Diagnosed with Severe Mental Illness",
  Characteristic == "Chronically Homeless" ~ "Has Experienced Chronic Homelessness",
  Characteristic == "DUI Charge" ~ "Non-Violent DUI Charge",
  Characteristic == "Drug Charge" ~ "Non-Violent Drug Charge",
  Characteristic == "Property Charge" ~ "Non-Violent Property Charge",
  TRUE ~ Characteristic
  )
)

# Turn table into a gt object (format)
tab_vi_1B_F_table <- gt(tab_vi_1B_F_final,
                        rowname_col = "Characteristic" # Need to specifically name which records are tied to rows in the output table.
)

# Title and Subtitle
tab_vi_1B_F_table <- 
  tab_vi_1B_F_table %>%
  #tab_header(
  #title = html("<em>Table III-1 : Individual Characteristics for Cases in which the Person was Released During the
  #Pretrial Period vs. the Overall Pretrial Population, 2018Q2-2021Q1"), # Italics
  #) %>%
  tab_options(
    heading.title.font.size = 16,
    heading.align = "center",
    table.border.top.color = "black",
    column_labels.border.bottom.color = "black",
    row.striping.include_table_body = FALSE
  ) %>%
  fmt_number(
    columns = 2:4,
    decimals = 0
  ) %>%
  cols_align(
    align = "center",
    columns = 2:4
  )

# Create and order row groups in table
# Note: Reverse order in order implementation
tab_vi_1B_F_table <- 
  tab_vi_1B_F_table %>% 
  #Felony doesn't exist in this filtered group
  #tab_row_group(
  #label = "Case Charge Level",
  #rows = c("Misdemeanor", "Felony")
  #) %>%
  tab_row_group(
    label = "Case Charge Type",
    rows = c("Violent Charge", "Non-Violent Drug Charge", "Non-Violent DUI Charge", "Non-Violent Property Charge")
  ) %>% 
  tab_row_group(
    label = "Vulnerable Groups",
    rows = c("Diagnosed with Severe Mental Illness", "Has Experienced Chronic Homelessness")
  ) %>%
  tab_row_group(
    label = "Intersectional Groups",
    rows = c("Black TAY", "Black Female")
  ) %>%
  tab_row_group(
    label = "Race/Ethnicity",
    rows = c("Hispanic", "Black", "White", "Asian", "Other Race")
  ) %>%
  tab_row_group(
    label = "Age",
    rows = c("18-25", "26-39", "40-64", "65+")
  ) %>%
  tab_row_group(
    label = "Sex",
    rows = c("Female", "Male")
  ) %>%
  tab_row_group(
    label = "",
    rows = c("Overall")
  ) %>%
  tab_style(
    locations = list(cells_column_labels(columns = everything()),
                     cells_title(groups = "title"),
                     cells_row_groups(groups = everything())
    ),
    style     = list(cell_text(weight = "bold", size = 15))
  )

tab_vi_1B_F_table

#Export and save file.
# Saving as PNG file results in a cropped
# image of an HTML table; the amount of
# whitespace can be set
tab_vi_1B_F_table %>%
  gtsave(
    "Table_VI_1B_F.png", expand = 10
    # ,path = tempdir()
  )

tab_vi_1B_F_table %>%
  gt::gtsave(filename = "Table_VI_1B_F.html")

# Table VI-2 : Median of Detention Length for Individuals Released During the Pretrial Period, by Type of Release

# Imported sorted and transposed data in correct format.
library(haven)
tab_vi_2_final <- read_sas("My SAS Files/JointFiles/Pretrial/tab_vi_2_final.sas7bdat", NULL)
tab_vi_2_final <- dplyr::select(tab_vi_2_final, -"_NAME_")

# Change "2020Q1 - 2020Q4" Title to just 2020 on the front-end on request by renaming columns
tab_vi_2_final <- dplyr::rename(tab_vi_2_final
                                   , "2020" = "2020Q1-2020Q4"
                                   , "2021" = "2021Q1-2021Q4"
                                   , "2022" = "2022Q1-2022Q4"
)

tab_vi_2_final <- dplyr::filter(tab_vi_2_final, !(Characteristic %in% 
                                                      c('I', #'No Court Charge Level',
                                                        #'Not Chronically Homeless', 'Not SMI',
                                                        'Under18' #, 'Unknown Gender', 'Unknown Race'
                                                      ))) # remove data rows that will not be shown in the table.

# Change display name for Characteristics.
tab_vi_2_final <- dplyr::mutate(tab_vi_2_final, Characteristic = case_when(
  Characteristic == "Cite" ~ "Citation",
  Characteristic == "OR" ~ "Own Recognizance",
  Characteristic == "Bail" ~ "Bail/Bond",
  TRUE ~ Characteristic
  )
)

# Turn table into a gt object (format)
tab_vi_2_table <- gt(tab_vi_2_final,
                      rowname_col = "Characteristic" # Need to specifically name which records are tied to rows in the output table.
)

# Title and Subtitle
tab_vi_2_table <- 
  tab_vi_2_table %>%
  #tab_header(
  #title = html("<em>Table III-1 : Individual Characteristics for Cases in which the Person was Released During the
  #Pretrial Period vs. the Overall Pretrial Population, 2018Q2-2021Q1"), # Italics
  #) %>%
  tab_options(
    heading.title.font.size = 16,
    heading.align = "center",
    table.border.top.color = "black",
    column_labels.border.bottom.color = "black",
    row.striping.include_table_body = FALSE
  ) %>%
  fmt_number(
    columns = 2:4,
    decimals = 0
  ) %>%
  cols_align(
    align = "center",
    columns = 2:4
  )

# Create and order row groups in table
# Note: Reverse order in order implementation
tab_vi_2_table <- 
  tab_vi_2_table %>% 
  tab_row_group(
    label = "",
    rows = c("Other")
  ) %>%
  tab_row_group(
    label = "",
    rows = c("Bail/Bond")
  ) %>%
  tab_row_group(
    label = "",
    rows = c("Own Recognizance")
  ) %>%
  tab_row_group(
    label = "",
    rows = c("Citation")
  ) %>%
  tab_row_group(
    label = "",
    rows = c("Overall")
  ) %>%
  tab_style(
    locations = list(cells_column_labels(columns = everything()),
                     cells_title(groups = "title"),
                     cells_row_groups(groups = everything())
    ),
    style     = list(cell_text(weight = "bold", size = 15))
  )

tab_vi_2_table

#Export and save file.
# Saving as PNG file results in a cropped
# image of an HTML table; the amount of
# whitespace can be set
tab_vi_2_table %>%
  gtsave(
    "Table_VI_2.png", expand = 10
    # ,path = tempdir()
  )

tab_vi_2_table %>%
  gt::gtsave(filename = "Table_VI_2.html")

# Misdemeanors Only

library(haven)
tab_vi_2B_M_final <- read_sas("My SAS Files/JointFiles/Pretrial/tab_vi_2b_m_final.sas7bdat", NULL)
tab_vi_2B_M_final <- dplyr::select(tab_vi_2B_M_final, -"_NAME_")

# Change "2020Q1 - 2020Q4" Title to just 2020 on the front-end on request by renaming columns
tab_vi_2B_M_final <- dplyr::rename(tab_vi_2B_M_final
                                , "2020" = "2020Q1-2020Q4"
                                , "2021" = "2021Q1-2021Q4"
                                , "2022" = "2022Q1-2022Q4"
)

tab_vi_2B_M_final <- dplyr::filter(tab_vi_2B_M_final, !(Characteristic %in% 
                                                    c('I', #'No Court Charge Level',
                                                      #'Not Chronically Homeless', 'Not SMI',
                                                      'Under18' #, 'Unknown Gender', 'Unknown Race'
                                                    ))) # remove data rows that will not be shown in the table.

# Change display name for Characteristics.
tab_vi_2B_M_final <- dplyr::mutate(tab_vi_2B_M_final, Characteristic = case_when(
  Characteristic == "Cite" ~ "Citation",
  Characteristic == "OR" ~ "Own Recognizance",
  Characteristic == "Bail" ~ "Bail/Bond",
  TRUE ~ Characteristic
  )
)

# Turn table into a gt object (format)
tab_vi_2B_M_table <- gt(tab_vi_2B_M_final,
                     rowname_col = "Characteristic" # Need to specifically name which records are tied to rows in the output table.
)

# Title and Subtitle
tab_vi_2B_M_table <- 
  tab_vi_2B_M_table %>%
  #tab_header(
  #title = html("<em>Table III-1 : Individual Characteristics for Cases in which the Person was Released During the
  #Pretrial Period vs. the Overall Pretrial Population, 2018Q2-2021Q1"), # Italics
  #) %>%
  tab_options(
    heading.title.font.size = 16,
    heading.align = "center",
    table.border.top.color = "black",
    column_labels.border.bottom.color = "black",
    row.striping.include_table_body = FALSE
  ) %>%
  fmt_number(
    columns = 2:4,
    decimals = 0
  ) %>%
  cols_align(
    align = "center",
    columns = 2:4
  )

# Create and order row groups in table
# Note: Reverse order in order implementation
tab_vi_2B_M_table <- 
  tab_vi_2B_M_table %>% 
  tab_row_group(
    label = "",
    rows = c("Other")
  ) %>%
  tab_row_group(
    label = "",
    rows = c("Bail/Bond")
  ) %>%
  tab_row_group(
    label = "",
    rows = c("Own Recognizance")
  ) %>%
  tab_row_group(
    label = "",
    rows = c("Citation")
  ) %>%
  tab_row_group(
    label = "",
    rows = c("Overall")
  ) %>%
  tab_style(
    locations = list(cells_column_labels(columns = everything()),
                     cells_title(groups = "title"),
                     cells_row_groups(groups = everything())
    ),
    style     = list(cell_text(weight = "bold", size = 15))
  )

tab_vi_2B_M_table

# Export and save file.
tab_vi_2B_M_table %>%
  gtsave(
    "Table_VI_2B_M.png", expand = 10
    # ,path = tempdir()
  )

tab_vi_2B_M_table %>%
  gt::gtsave(filename = "Table_VI_2B_M.html")

# Felonies Only

library(haven)
tab_vi_2B_F_final <- read_sas("My SAS Files/JointFiles/Pretrial/tab_vi_2b_f_final.sas7bdat", NULL)
tab_vi_2B_F_final <- dplyr::select(tab_vi_2B_F_final, -"_NAME_")

# Change "2020Q1 - 2020Q4" Title to just 2020 on the front-end on request by renaming columns
tab_vi_2B_F_final <- dplyr::rename(tab_vi_2B_F_final
                                   , "2020" = "2020Q1-2020Q4"
                                   , "2021" = "2021Q1-2021Q4"
                                   , "2022" = "2022Q1-2022Q4"
)

tab_vi_2B_F_final <- dplyr::filter(tab_vi_2B_F_final, !(Characteristic %in% 
                                                          c('I', #'No Court Charge Level',
                                                            #'Not Chronically Homeless', 'Not SMI',
                                                            'Under18' #, 'Unknown Gender', 'Unknown Race'
                                                          ))) # remove data rows that will not be shown in the table.

# Change display name for Characteristics.
tab_vi_2B_F_final <- dplyr::mutate(tab_vi_2B_F_final, Characteristic = case_when(
  Characteristic == "Cite" ~ "Citation",
  Characteristic == "OR" ~ "Own Recognizance",
  Characteristic == "Bail" ~ "Bail/Bond",
  TRUE ~ Characteristic
  )
)

# Turn table into a gt object (format)
tab_vi_2B_F_table <- gt(tab_vi_2B_F_final,
                        rowname_col = "Characteristic" # Need to specifically name which records are tied to rows in the output table.
)

# Title and Subtitle
tab_vi_2B_F_table <- 
  tab_vi_2B_F_table %>%
  #tab_header(
  #title = html("<em>Table III-1 : Individual Characteristics for Cases in which the Person was Released During the
  #Pretrial Period vs. the Overall Pretrial Population, 2018Q2-2021Q1"), # Italics
  #) %>%
  tab_options(
    heading.title.font.size = 16,
    heading.align = "center",
    table.border.top.color = "black",
    column_labels.border.bottom.color = "black",
    row.striping.include_table_body = FALSE
  ) %>%
  fmt_number(
    columns = 2:4,
    decimals = 0
  ) %>%
  cols_align(
    align = "center",
    columns = 2:4
  )

# Create and order row groups in table
# Note: Reverse order in order implementation
tab_vi_2B_F_table <- 
  tab_vi_2B_F_table %>% 
  tab_row_group(
    label = "",
    rows = c("Other")
  ) %>%
  tab_row_group(
    label = "",
    rows = c("Bail/Bond")
  ) %>%
  tab_row_group(
    label = "",
    rows = c("Own Recognizance")
  ) %>%
  tab_row_group(
    label = "",
    rows = c("Citation")
  ) %>%
  tab_row_group(
    label = "",
    rows = c("Overall")
  ) %>%
  tab_style(
    locations = list(cells_column_labels(columns = everything()),
                     cells_title(groups = "title"),
                     cells_row_groups(groups = everything())
    ),
    style     = list(cell_text(weight = "bold", size = 15))
  )

tab_vi_2B_F_table

#Export and save file.
# Saving as PNG file results in a cropped
# image of an HTML table; the amount of
# whitespace can be set
tab_vi_2B_F_table %>%
  gtsave(
    "Table_VI_2B_F.png", expand = 10
    # ,path = tempdir()
  )

tab_vi_2B_F_table %>%
  gt::gtsave(filename = "Table_VI_2B_F.html")
