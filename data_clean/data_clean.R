library(tidyverse)
library(openxlsx)

##### Read files

path_dir <- "./data/redtomato"

pur <- read.xlsx(file.path(path_dir, "Purchase History/Pur_History_2014-2018.xlsx"), startRow = 4)
sal <- read.xlsx(file.path(path_dir, "Sales History/Sales Data 2014-2018.xlsx"), startRow = 5)

##### Summarize purchase data
pur$Itemcode %>%  trimws() %>% unique() %>% length()

apply(pur, 2, str)

pur_g <- pur %>% 
  mutate(Received.Quantity = as.numeric(Received.Quantity),
         Actual.Inventory.Cost = as.numeric(Actual.Inventory.Cost),
         `Actual.Add-on.Cost` = as.numeric(`Actual.Add-on.Cost`)) %>% 
  group_by(Itemcode) %>% 
  summarize(n_invoice = n(),
            received_quantity_tot = sum(Received.Quantity),
            actual_inv_cost_tot = sum(Actual.Inventory.Cost),
            actual_addon_cost_tot = sum(`Actual.Add-on.Cost`)) %>%
  mutate(actual_inv_cost_perunit = actual_inv_cost_tot / received_quantity_tot,
         actual_addon_cost_perunit =  actual_addon_cost_tot / received_quantity_tot,
         tot_cost_perunit = actual_inv_cost_perunit + actual_addon_cost_perunit) 
  pur_g %>% View()
  
pur_g  %>% filter( !(Itemcode %in% sal$Itemcode)) %>%  
  filter(!(received_quantity_tot == 0 | actual_inv_cost_tot == 0)) %>%
  View()

###### Merge purchase and sales
ps <- left_join(sal, pur_g, by = "Itemcode") %>%
  mutate(Itemcode = trimws(Itemcode))


###### Keep only apples and peaches
ps <- ps %>% filter(grepl("^APP|^PCH", Itemcode)) # how much aer yo upaying for eco bags, grep by apply after excluding ^APP

###### Drop one inactive row
ps <- ps %>% filter(!grepl("INACTIVE", Description))

###### Checks

# Check 1 ----------------------------------------------------------------------

# 124 rows passed out of 222,

check1 <- ps %>% select(Quantity_sold, received_quantity_tot) %>% 
  mutate(diff = Quantity_sold - received_quantity_tot,
         perc_diff = 100* diff/Quantity_sold)
table(check1$diff) 

# Check 2 ----------------------------------------------------------------------

# this check failed 
check2 <- ps %>% select(Number_of_invoices, n_invoice) %>% mutate(diff = Number_of_invoices - n_invoice,
                                                                  perc_diff = 100* diff/Number_of_invoices)
table(check2$diff) 


# Check 3 ----------------------------------------------------------------------

# Check 3 passed using avg_PRICE_each. Very small differences.
# note that avg_COST_each is RT's expenditure, but that variable failed this check

check3 <- ps %>% select(Ave_price_each, tot_cost_perunit, Gross_profit_per_muom) %>%
  mutate(ave_each = (tot_cost_perunit + Gross_profit_per_muom),
         diff_price = Ave_price_each - ave_each,
         perc_diff_price = 100*diff_price/Ave_price_each)


# Check 4 ----------------------------------------------------------------------

# Check 4 is passed! Very small difference
check4 <- ps %>% select(Line_gross_profit, received_quantity_tot, Gross_profit_per_muom) %>%
  mutate(calc_gross_profit = Line_gross_profit/received_quantity_tot,
         diff = calc_gross_profit - Gross_profit_per_muom,
         perc_diff = 100* diff/Gross_profit_per_muom)
table(check4$diff) 


##### Creating Indicators
ps <- ps %>%
  mutate(ECO_status = grepl("E", Itemcode)) %>%
  mutate(ECO_visible = (grepl("tote | polybag", Description) & ECO_status))

ps %>% filter(Description == "Apple McIntosh ECO 125s            ") %>% View()





