
##################################################
# 1. SETUP & DATA IMPORT
##################################################

#Load Relevant Libraries
install.packages("pacman") #run install pacman library if missing 
library(pacman)
p_load(tidyverse,lubridate,scales,gridExtra,ggrepel,readxl,patchwork)
p_load(openxlsx,broom,ggtext,ggpubr)
p_load(showtext,patchwork,janitor,here,skimr,glue)

#Set the working directory
#You can also set your own directory if you want to run locally
setwd("C:/Users/User/OneDrive/Documents/smb_sales_analysis/data")

#run this command to confirm the correct directory.
here::here() 

#I manually copied Opportunities (0120 - 0223) sheet and sales_rep_team sheet from 
#"EGYM Wellpass - Case Study Teamlead Business Analytics.xlsx
#into a new excel sheet named "pass.xlsx" and pass_team respectively in the data/raw folder

#Prepare opportunities dataset
opportunities <- readxl::read_excel(here::here("data", "raw", "pass.xlsx")) |> 
  janitor::clean_names() |>  #clean column names
  janitor::remove_empty() |> #remove empty columns
  dplyr::mutate_at(dplyr::vars(contains("date")), ~ as.character(.)) |> # Convert any date columnlabels to character first
  dplyr::mutate_at(dplyr::vars(contains("date")), #Set proper date data type
                   ~lubridate::date(lubridate::parse_date_time2(., 
                                                                orders = c("Ymd", "mdY", "dOmY",
                                                                           "OmY", "Ym", "Y",
                                                                           "dOm", "Om", "dO")))) %>% 
  mutate(id = row_number()) %>%  # add id column as primary key
  dplyr::relocate(id, .before = 1) # move id column to the first column


#Prepare sales rep dataset
sales_rep_team <- readxl::read_excel(here::here("data", "raw", "pass_team.xlsx")) |> 
  janitor::clean_names() |>  #clean column names
  janitor::remove_empty() |> #remove empty columns
  dplyr::mutate_at(vars(contains("sales_rep")), ~ str_replace_all(.," ", "_"))




##################################################
# 2. DATA PREPARATION AND CLEANING
##################################################


# Convert columns to proper format and store them into a new pass dataframe
pass <- opportunities%>% 
  mutate_at(vars(contains(c("opportunity_owner","company"))),
            ~ str_replace_all(.," ", "_")) %>%  #replace empty space with underscore
  mutate_at(vars(contains(c("opportunity_owner","company","stage"))),
            ~ factor(.)) %>% # make factor since these are names
  mutate(last_stage_change_date = lubridate::as_date(last_stage_change_date)) #convert to date
  
  
  
  #Now, let's join both opportunities and sales rep datasets into a new combined_pass dataframe
  combined_pass <- 
  pass %>% 
  left_join(sales_rep_team, by = c("opportunity_owner" = "sales_rep")) %>% 
  dplyr::relocate(team, .after = opportunity_owner)


#Convert columns to proper format and merge SMB and Team SMB into SMB
combined_pass_clean <- 
  combined_pass %>% 
  #Merge "TEAM SMB" INTO "SMB" for uniformity
  mutate_at(vars(contains("team")),
            ~ str_replace_all(.,"TEAM SMB", "SMB")) %>% 
  mutate_at(vars(contains(c("team"))),~ factor(.)) 



#Preview the data         
combined_pass_clean %>% head()     


# Check for missing data in combined_pass_clean
colSums(is.na(combined_pass_clean))

#The following cols have missing data:
#1. last_stage_change_date
#2. sold_price_per_registered_member


#Let's analyze the count of missing values from last_stage_change_date
combined_pass_clean %>%
  filter(is.na(last_stage_change_date)) %>%
  group_by(stage) %>%
  summarise(count = n(), .groups = "drop") %>% 
  arrange(desc(count)) %>% as.data.frame()

#Insights
#The 9% missing values in last_stage_change_date affects only the following stages -
#NEW (590), Pitch Took Place (98), Contract Received (82), Contract Shipped (82), Pitch Arranged (80). 


# If a deal is in the NEW stage and hasn't been moved yet, maybe the date wasn't recorded.
#But in other active stages like Pitch Took Place etc. missing dates could mean the data wasn't entered properly.

# Now, to handle these missing values? I used data imputation to set the created_date = last_stage_change_date
#only for the stage "NEW" since the deal hasn't moved yet. 

#For other stages like Pitch Took Place etc, if the date is missing
#, i used the median time it takes to move from the previous stage to the current one.I didn't want to exclude any data.
#I will add a column 'imputed_last_stage' to track imputed values and make it easy to exclude them from our
#sales cycle analysis, if necessary but include them in the conversion rate calculations.


#1. Impute Missing Dates for "NEW" Stage
# Impute 'last_stage_change_date' for "NEW" stage using 'created_date'

#Initialize imputed_last_stage column ----------------------------------------
combined_pass_clean <- combined_pass_clean %>% 
  mutate(imputed_last_stage = FALSE) #set all values to FALSE


# 1. Impute "NEW" Stage Dates & Flag ------------------------------------------
combined_pass_clean <- combined_pass_clean %>%
  mutate(
    # 1) Temporarily flag rows needing imputation
    needs_imputation = stage == "NEW" & is.na(last_stage_change_date), 
    #    This creates a Boolean column (TRUE/FALSE) indicating which records 
    #    both have stage == "NEW" and a missing last_stage_change_date.
    
    # 2) Impute dates where needed
    last_stage_change_date = if_else(
      needs_imputation,
      created_date,
      last_stage_change_date
    ),
    #    If needs_imputation is TRUE, set last_stage_change_date = created_date.
    #    Otherwise, keep the original last_stage_change_date.
    
    # 3) Update or create the imputed_last_stage flag
    imputed_last_stage = if_else(needs_imputation, TRUE, imputed_last_stage)
    #    If we imputed this record, mark imputed_last_stage as TRUE.
    #    Otherwise, preserve the existing value of imputed_last_stage.
    
  ) %>% 
  select(-needs_imputation)# and then remove the temporary 'needs_imputation' col from the final output.

#Check:
#glimpse(combined_pass_clean)

#2. Impute Dates for Active Stages Using Median Stage Progression Time
#Please note that this is a simplified approach and may not be perfect.
#Due to time constraints, we could bring in more context or domain knowledge to refine the imputation strategy.

# Calculate median time between stages (e.g., "Pitch Arranged" → "Pitch Took Place")
stage_transition_time <- combined_pass_clean %>%
  filter(!is.na(last_stage_change_date)) %>%  # Exclude missing dates
  group_by(stage) %>%
  summarise(
    median_days_to_stage = median(
      as.numeric(difftime(last_stage_change_date, created_date, units = "days")),
      na.rm = TRUE
    )
  )

# make a new a new dataset to store the imputed values
combined_pass_imputed <- combined_pass_clean %>%
  left_join(stage_transition_time, by = "stage") %>%
  mutate(
    # Identify other stages aside "NEW" needing imputation
    needs_imputation = is.na(last_stage_change_date) & stage != "NEW",
    
    # Impute dates using median progression time
    last_stage_change_date = if_else(
      needs_imputation,
      created_date + days(round(median_days_to_stage)),
      last_stage_change_date
    ),
    
    # Update boolean flag
    imputed_last_stage = if_else(needs_imputation, TRUE, imputed_last_stage)
  ) %>% 
  select(-needs_imputation, -median_days_to_stage)





##################################################--------------
# 3a. DATA VALIDATION AFTER CLEANING
##################################################--------------

#3. Check for Remaining Missing Dates (If Needed)
combined_pass_imputed %>%
  filter(is.na(last_stage_change_date))

#No more missing_dates, all missing stages of last_stage_change_date have been imputed
#There is none to be excluded

# Check flag distribution
imputation_summary <- combined_pass_imputed %>% 
  group_by(imputed_last_stage) %>% 
  summarise(
    total_deals = n(),
    missing_dates_remaining = sum(is.na(last_stage_change_date))
  )

print(imputation_summary)
#total of 947 deals were imputed and 0 missing dates remaining


#Check for missing values in sold_price_per_registered_member
#Distribution of sold_price_per_registered_member
combined_pass_clean %>%
  group_by(stage, sold_price_per_registered_member) %>%
  summarise(count = n(), .groups = "drop") %>% 
  arrange(desc(count)) %>% as.data.frame()

#	sold_price_per_registered_member' has 7008 missing values (about 69% missing).
#That's a lot. The mean is 32.9 with low SD, so when it's present, the price is around 33,
#with min 30 and max 35. So pricing is pretty consistent, but the high missingness is due
#to Closed lost, New and Pitch arranged deals because they didn't close. That's logical.

#For now I'll note somewhere in the analysis documentation that "NA" in sold_price_per_registered_member 
#means “not applicable” 
#In further analyses for revenue or price metrics, we will simply filter to closed and ‐won scenarios.





##################################################
# 3b. EXPLORATORY DATA ANALYSIS
##################################################

#summary statistics
combined_pass_clean %>% skimr::skim() 


#Insights 1:
#SMB Dominance: 50% of companies have ≤60 employees, confirming a strong SMB presence.
# Outliers Exist: Max employee count is 155,082, while 25% of companies have ≤15 employees.
# Mean (567 employees) vs. Median (60 employees) indicates a heavy right skew as shown in the histogram below.
# Data Quality Flag: modal_employees = 0 suggests invalid entries (companies can’t have 0 employees).
# Could this be a data entry error or sole proprietors with no employees?


#Let's visualize the distribution of employees using a histogram
#Without outliers
combined_pass_clean %>% 
  filter(employees < 7000) %>% #Excluding huge outliers
  ggplot(aes(x = employees)) +
  geom_histogram(bins = 100) +
  labs(title = "Distribution of Employees",
       x = "Employees",
       y = "Count") +
  theme_minimal()

#Insights 2:
#	So most companies in the dataset are very small, but a few are massive, skewing the mean way up. 
#This tells me that the dataset has a lot of small businesses but also some very large enterprises,
#which could affect how we analyze SMB performance since SMBs are 
#typically defined by employee size, maybe under 500 or so according to IfM Bonn (German Institute for SME Research):
#SMEs have fewer than 500 employees
# https://www.ifm-bonn.org/en/definitions/uebersetzen-nach-english-kmu-definition-der-eu-kommission
#But with a median of 60, most are indeed SMBs, but the average is pulled up by those few large companies in the dataset.





##################################################
# 4. WIN/LOSS & CONVERSION RATE ANALYSIS
##################################################

#Some Quick Definitions:
#Sales win rate is a critical metric that reflects a team’s
#ability to convert opportunities into closed deals.
# It's calculated as the number of won deals divided by the total number of closed deals (won + lost).
# it's a key performance metric used to measure the effectiveness and efficiency of a sales process.

#Why It’s Important:

#1. Increased Revenue: Higher win rates lead to more revenue.
# 
#2. Better Forecasting: Reliable win rates improve sales projections.
# 
#3. Enhanced Team Performance: A clear metric motivates and aligns your team.

#Let's go on to calculate the conversion rate for the sales teams

# Flag 'Closed Won' vs. 'Closed Lost' opportunities
#rename stage names for easier usage
closed_opportunities <- combined_pass_imputed %>%
  mutate(
    deal_outcome = case_when(
      stage == "Closed Won"  ~ "Won",
      stage == "Closed Lost" ~ "Lost",
      TRUE                   ~ "Open"  # For non-closed stages
    )
  )



# Let's Compute Overall conversion rate or Win rate % for the entire dataset

overall_conversion <- closed_opportunities %>%
  filter(deal_outcome %in% c("Won", "Lost")) %>%  # Exclude open deals
  summarise(
    total_won = sum(deal_outcome == "Won"),
    total_lost = sum(deal_outcome == "Lost"),
    conversion_rate = round(total_won / (total_won + total_lost), 2)
  )

print(overall_conversion)
#Results:
#There are 5039 lost deals, 3172 won deals ,a total of 8211 closed deals.
#Conversion rate is 0.386, meaning 39% of deals are won overall.
#Comparing this against industry benchmarks, a 39% conversion rate 
#The calculated win rate of 38.6% falls below the median (45.5%) across industries according to Hubspot.
#. However, it's not significantly lower than the 40% win rate observed for the bottom 80% of performers8.is quite good.


#Citations:
# [1] https://www.beforesunset.ai/start-up/sales-win-rate
#[2] https://www.kixie.com/sales-blog/saas-win-rate-benchmark-proven-sales-strategies/
#[2] https://blog.hubspot.com/sales/sales-benchmarks-2022



# Let us compute Team-level conversion rates keeping an eye on SMB Performnce

team_conversion <- closed_opportunities %>%
  filter(deal_outcome %in% c("Won", "Lost")) %>%  # Exclude open deals
  group_by(team) %>%
  summarise(                       # Calculate team-level metrics
    won = sum(deal_outcome == "Won"),
    lost = sum(deal_outcome == "Lost"),
    .groups = "drop"
  ) %>%
  mutate(                           # Compute conversion rate and volume
    conversion_rate = round(won / (won + lost), 2),
    team_volume = won + lost
  ) %>%
  arrange(desc(conversion_rate)) %>% # Sort by conversion rate
  # Add ranking for easy interpretation
  mutate(team_rank = dense_rank(desc(conversion_rate))) %>% 
  select(team_rank, team, everything())

print("Team-Level Conversion Rates:")
print(as.data.frame(team_conversion))

#Export team_conversion_rates.xlsx into a table .../smb_sales_analysis/outputs/tables
openxlsx::write.xlsx(team_conversion, here::here("outputs", "tables", "team_conversion_rates.xlsx"))



#Results: Standard Performance SMB Team
# 56% Conversion Rate (951/1691)
#  ○ Why This Matters: Outperforms all other teams by ≥10 percentage points, validating the phone-based SMB sales model.

#Recommendation: 
# ○ Invest in SMB team training and resources to maintain high performance.
# ○ Consider expanding SMB team to capitalize on their success.  
# ○ Reverse-engineer SMB’s process for replication in other teams.
# ○ Increase lead allocation to SMB to leverage their efficiency.


##########################################################################
     #CHECK FOR STATISTICAL SIGNIFICANCE
##########################################################################
#Let's see if this difference in win rates is statistically significant-------


#Load the Infer package
pacman::p_load(infer)

# Chi-square test for team win rate differences

# First, create a contingency table of team vs. deal outcome
contingency_table <- closed_opportunities %>% 
  filter(deal_outcome %in% c("Won", "Lost")) %>%
  count(team, deal_outcome) %>%
  pivot_wider(names_from = deal_outcome, values_from = n) %>%
  column_to_rownames("team") %>%
  as.matrix()



# Perform chi-square test

# 1. Fisher's Exact Test (handles small sample sizes)
fisher_test <- fisher.test(contingency_table, simulate.p.value = TRUE)
cat("Fisher's Exact Test p-value:", fisher_test$p.value, "\n")

#Result of Fisher's Exact Test
# ○ Fisher's Exact Test p-value: 0.0004997501~, that's approxp = 0.0005
# ○ This p-value is less than 0.05, indicating significant differences in win rates between teams.

# 2. Chi-Square with Simulation
chi_sim <- chisq.test(contingency_table, simulate.p.value = TRUE, B = 10000)
cat("Chi-Square Simulated p-value:", chi_sim$p.value, "\n")


#Final Statistical Conclusion

#Both tests confirm extremely significant differences in team performance:

#Result of Chi-square Test
# ○ Chi-square: p < 0.0001 (χ² = 346.26)
# ○ Simulated Chi-square: p = 0.0001

#This means there’s less than 0.1% chance the observed win rate differences are random.




##################################################
# 5. REVENUE / DEAL SIZE ANALYSIS
##################################################

#Define Deal Size: The average revenue generated per closed deal.

# If we assume 'sold_price_per_registered_member' is the main revenue metric,
# we can look at average or total revenue by team or by outcome.

# Average revenue per closed won opportunity
avg_revenue_won <- closed_opportunities %>% 
  filter(deal_outcome == "Won" & !is.na(sold_price_per_registered_member)) %>%
  group_by(team) %>%
  summarise(
    avg_sold_price = mean(sold_price_per_registered_member),
    median_sold_price = median(sold_price_per_registered_member),
    won_deals_count = n(), .groups = "drop"
  ) %>%
  mutate(total_revenue = round(avg_sold_price, 2)* won_deals_count) %>%
  arrange(desc(avg_sold_price))

print(avg_revenue_won)

#Export average_revenue_won.xlsx for all teams into a table .../smb_sales_analysis/outputs/tables
openxlsx::write.xlsx(avg_revenue_won, here::here("outputs", "tables", "average_revenue_won.xlsx"))


#Results
#Insights & Recommendations from Revenue per Closed Won Opportunity Analysis

# 1. SMB Has the Highest Average Deal Size and Largest Volume
# SMB:
#   ○ Average Sold Price: €35 per member
#   ○ Won Deals: 951
#   ○ Median Sold Price: €35
#   ○ Total Revenue: €33,285

# This stands out for two reasons:
# 1. Highest average price (35) among all teams.
# 2. Large number of won deals, indicating both high volume and slightly higher pricing.
# 3. Highest total revenue (€33,285) among all teams.

# Takeaway: The SMB approach seems to capture slightly higher revenue per deal than other teams,
# while also converting more deals overall, making it a key revenue driver for the organization.
# 
# Bottom Line
# ○ SMB leads in both conversion rate and average deal size, making it the top‐performing team on pure revenue metrics.
# Other teams (especially Extern ones) might have lower average prices because of market segments, negotiation styles, or product differences—but this is also where targeted upsell strategies or pricing revisions could yield quick gains.


##################################################
# 6. SALES CYCLE & BOTTLENECK ANALYSIS
##################################################

#Define Sales Cycle: The time it takes to convert a lead into a closed deal.

# Sales cycle analysis can help identify bottlenecks, optimize processes, and improve conversion rates.

# Calculate Sales Cycle Days for Closed Opportunities -------------------------
closed_opportunities_clean <- closed_opportunities %>%
  mutate(
    sales_cycle_days = as.numeric(
      difftime(last_stage_change_date, created_date, units = "days")
    )
  )

# 1. Average Sales Cycle by Outcome -------------------------------------------
avg_cycle_by_outcome <- closed_opportunities_clean %>%
  filter(deal_outcome %in% c("Won", "Lost")) %>%
  group_by(deal_outcome) %>%
  summarise(
    avg_cycle_days = mean(sales_cycle_days, na.rm = TRUE),
    median_cycle_days = median(sales_cycle_days, na.rm = TRUE),
    sd_cycle_days = sd(sales_cycle_days, na.rm = TRUE),
    .groups = "drop"
  )

print("Average Sales Cycle by Outcome:")
print(avg_cycle_by_outcome)

#Export average_revenue_won.xlsx for all teams into a table .../smb_sales_analysis/outputs/tables
openxlsx::write.xlsx(avg_cycle_by_outcome, here::here("outputs", "tables", "average_cycle_by_outcome.xlsx"))



# 1. Bottleneck Identification ------------------------------------------------
# a. Most Common Final Stage for Lost Deals
lost_stage_analysis <- closed_opportunities_clean %>%
  filter(deal_outcome == "Lost") %>%
  count(stage) %>%
  mutate(percent = n / sum(n) * 100) %>%
  arrange(desc(n))

print("Lost Deal Stage Distribution:")
print(lost_stage_analysis)

#• Lost Deals
# ○ Mean (134) is much higher than the median (67), suggesting a strong right‐skew
#   some deals linger for many months before being lost, inflating the average.
# ○ Large standard deviation (181) reinforces that some lost deals drag on significantly.

# • Won Deals
# ○ Mean (99) and median (89) are closer, implying less extreme variation. 
# Most won deals cluster around 1–3 months, but there are some outliers that extend beyond that.

#Key Takeaway
#• The difference between lost and won deals’ medians (67 vs. 89 days) is relatively small,
# but the average for lost deals is pulled up by “zombie” opportunities that remain open
# too long before being lost. 
# A more proactive disqualification or pipeline cleanup might help reduce that dead weight.

# 2. Lost Deal Stage Distribution
# • 100% of lost deals end in the stage “Closed Lost stage

# • It confirms that no other final stage exists for deals that do not convert.

# Key Takeaway
# This doesn’t reveal the intermediate stage at which they were lost. It simply confirms “Closed Lost” is consistently used as the last step.

# b. Cycle Time Correlation Analysis
# Correlation between employees and sales cycle days
cor_test <- cor.test(
  closed_opportunities_clean$employees,
  closed_opportunities_clean$sales_cycle_days,
  method = "spearman" # Non-parametric due to skewed employee distribution
)

print(paste("Employee-Sales Cycle Correlation (rho):", 
            round(cor_test$estimate, 2)))

#Results: 0.33

#insights
# A positive Spearman correlation of 0.33 suggests that larger companies (more employees)
#generally have longer sales cycles.
#Not an extremely strong correlation, but still meaningful—indicating bigger organizations
#often involve more stakeholders and complexity.

# Key Takeaway
# Sales teams targeting large enterprises should anticipate longer deal times
#and plan resources accordingly (e.g., specialized enterprise reps, more touch points).


# 3. Team-Level Cycle Analysis ------------------------------------------------

#What is the significance of this analysis?

# Team-level median cycle analysis examines the median duration of sales cycles (time taken to close deals)
#across teams, comparing Won vs. Lost deals. 

# This helps identify:
# ○ Efficiency Patterns: Which teams close successful deals faster.
 
# ○ Bottlenecks: Whether prolonged cycles correlate with lost deals.

# ○ Team Performance: Differences in how teams manage opportunities.

#define team level median cycle analysis 

team_cycle_analysis <- closed_opportunities_clean %>%
  filter(deal_outcome %in% c("Won", "Lost")) %>%
  group_by(team, deal_outcome) %>%
  summarise(
    median_cycle_days = median(sales_cycle_days, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = deal_outcome,
    values_from = median_cycle_days,
    names_prefix = "cycle_"
  )

print("Team-Level Median Cycle Days:")
print(team_cycle_analysis)

#Export average_revenue_won.xlsx for all teams into a table .../smb_sales_analysis/outputs/tables
openxlsx::write.xlsx(team_cycle_analysis, here::here("outputs", "tables", "team-level_median_cycl_days.xlsx"))


#Key TakewayS:
#• SMB Team: 4 days to close a deal, significantly faster than all other teams.
#SMB excels at rapid closures (both Won and Lost), while some external teams let
# deals linger before losing them. 
#Early disqualification of unpromising deals could save resources and reduce average lost cycle times.




##################################################
# 7. Visualize Data
##################################################


# 4. Visualization ------------------------------------------------------------
library(ggplot2)

# Let's Visualize Team Performance --------------------------------------
library(ggplot2)


team_conversion %>%
  mutate(team = reorder(team, conversion_rate)) %>%
  ggplot(aes(x = team, y = conversion_rate)) +
  geom_col(
    aes(fill = ifelse(team == "SMB", "#b96305", "#056875")) # Replace with your color
  ) +
  geom_text(
    aes(label = scales::percent(conversion_rate)), 
    hjust = 1.2, size = 4.5, color = "#FFFFFF", face = "bold"
  ) +
  coord_flip() +
  labs(
    title = "Win Rates by Sales Team",
    subtitle = "SMB Team is the most effective at closing deals",
    y = "Conversion Rate", 
    x = ""
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_identity() + # Ensures the `fill` colors are used directly
  theme_WD()




# Boxplot of Cycle Days by Team

TEXT = "SMB Deals Close Quickly (Both Won & Lost)"

ggplot(closed_opportunities_clean %>% 
         filter(deal_outcome %in% c("Won", "Lost")),
       aes(x = team, y = sales_cycle_days, fill = deal_outcome)) +
  geom_boxplot() +
  # Annotation for SMB team
  annotate(
    "label",
    x = "SMB", 
    y = 300,
    label = TEXT,
    fill = "#056875",  # Match your theme color
    color = "white",
    size = 4,
    hjust = -0.1,
    vjust = 0.7
  ) +
  coord_flip() +
  labs(title = "Sales Cycle Duration by Team and Outcome",
       y = "Days in Sales Cycle",
       x = "") +
  theme_WD()

#Insights:

#SMB Deals Close Quickly (Both Won & Lost)
#The SMB boxplots (teal and red) sit far to the left, indicating very short sales cycles relative to other teams.
#This suggests a fast, high‐velocity approach for both successfully closed deals and those disqualified.

#Won Deals Generally Have Tighter Distributions
#In many teams (West, Nord, Mitte, etc.), the spread somewhat narrower,
#clustering around a moderate time range (~60–120 days).
#Fewer extreme outliers mean that when these teams do win, they typically
#close within a more predictable timeframe.

#Lost Deals Show More Variation or spread
# Red boxplots across most teams have long tails or outliers,indicating some deals are lost
# very quickly (days or a couple of weeks), while others drag on for hundreds of days.
# It’s possible that some early lost deals fail due to immediate disqualification
#(pricing, budget mismatch), while others linger from extended negotiations that don’t pan out.






##################################################
# 8. SUMMARY STATISTICS & RECOMMENDATIONS
##################################################

# 1) Conversion Rates
#    - SMB Team:  56.2%
#    - Other Teams: 34.1 %
#    - Observations: 
#Results: Standard Performance SMB Team
# 56% Conversion Rate (951/1691)
#  ○ Why This Matters: Outperforms all other teams by ≥10 percentage points, validating the phone-based SMB sales model.

#Recommendation: 
# ○ Invest in SMB team training and resources to maintain high performance.
# ○ Consider expanding SMB team to capitalize on their success.  
# ○ Reverse-engineer SMB’s process for replication in other teams.
# ○ Increase lead allocation to SMB to leverage their efficiency.


# 2) Average Deal Size
#    - SMB Team: €35, 951 won deals and a total revenue of €33,285
#    - Other Teams: €33 and €30
#    - Observations: See more details below
#Results
#Insights & Recommendations from Revenue per Closed Won Opportunity Analysis

# 1. SMB Has the Highest Average Deal Size and Largest Volume
# SMB:
#   ○ Average Sold Price: €35 per member
#   ○ Won Deals: 951
#   ○ Median Sold Price: €35
#   ○ Total Revenue: €33,285

# A tibble: 9 × 5
#  team          avg_sold_price   median_sold_price won_deals_count total_revenue
#  <fct>                <dbl>             <dbl>           <int>        <dbl>
# 1 SMB                  35                35             951         33285
# 2 Mitte                33                33             241          7953
# 3 Nord                 33                33             571         18843
# 4 Süd                  33                33             481         15873
# 5 West                 33                33             139          4587
# 6 Extern 1             30                30              76          2280
# 7 Extern 2             30                30             364         10920
# 8 Extern 3             30                30             199          5970
# 9 Extern 4             30                30             150          4500

# This stands out for two reasons:
# 1. Highest average price (35) among all teams.
# 2. Large number of won deals, indicating both high volume and slightly higher pricing.
# 3. Highest total revenue (€33,285) among all teams.

# Takeaway: The SMB approach seems to capture slightly higher revenue per deal than other teams,
# while also converting more deals overall, making it a key revenue driver for the organization.
# 
# Bottom Line
# ○ SMB leads in both conversion rate and average deal size, making it the top‐performing team on pure revenue metrics.
# Other teams (especially Extern ones) might have lower average prices because of market segments, negotiation styles, or product differences—but this is also where targeted upsell strategies or pricing revisions could yield quick gains.





# 3) Sales Cycle
#    - "SMB deals typically close faster/slower, by X days on average."

#Key TakewayS:
#• SMB Team: 4 days to close a deal, significantly faster than all other teams.
#SMB excels at rapid closures (both Won and Lost), while some external teams let
# deals linger before losing them. 
#Early disqualification of unpromising deals could save resources and reduce average lost cycle times.

#From Boxplot analysis of Sales Cycle Duration by Team and Outcome
#SMB Deals Close Quickly (Both Won & Lost)
#The SMB boxplots (teal and red) sit far to the left, indicating very short sales cycles relative to other teams.
#This suggests a fast, high‐velocity approach for both successfully closed deals and those disqualified.

#Won Deals Generally Have Tighter Distributions
#In many teams (West, Nord, Mitte, etc.), the spread somewhat narrower,
#clustering around a moderate time range (~60–120 days).
#Fewer extreme outliers mean that when these teams do win, they typically
#close within a more predictable timeframe.

#Lost Deals Show More Variation or spread
# Red boxplots across most teams have long tails or outliers,indicating some deals are lost
# very quickly (days or a couple of weeks), while others drag on for hundreds of days.
# It’s possible that some early lost deals fail due to immediate disqualification
#(pricing, budget mismatch), while others linger from extended negotiations that don’t pan out.


#• Lost Deals
# ○ Mean (134) is much higher than the median (67), suggesting a strong right‐skew
#   some deals linger for many months before being lost, inflating the average.
# ○ Large standard deviation (181) reinforces that some lost deals drag on significantly.

# • Won Deals
# ○ Mean (99) and median (89) are closer, implying less extreme variation. 
# Most won deals cluster around 1–3 months, but there are some outliers that extend beyond that.

#Key Takeaway
#• The difference between lost and won deals’ medians (67 vs. 89 days) is relatively small,
# but the average for lost deals is pulled up by “zombie” opportunities that remain open
# too long before being lost. 
# A more proactive disqualification or pipeline cleanup might help reduce that dead weight.

# 2. Lost Deal Stage Distribution
# • 100% of lost deals end in the stage “Closed Lost,” which is expected if that
# is your CRM’s single “final lost” stage.

# • It confirms that no other final stage exists for deals that do not convert.


# Potential next steps in analysis (pseudocode-like comments):
# - If needed, run logistic regression to see which factors (employees, region, etc.)
#   drive Win vs. Lost.
# - Investigate lost deals with 'stage' or 'reason' if available.
# - Identify top objections or reasons for Lost deals.

# Final printout for quick reference







