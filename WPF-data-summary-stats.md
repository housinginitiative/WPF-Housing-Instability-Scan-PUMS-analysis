---
title: "WPF Housing Instability"
author: "Anna Duan"
date: "2023-11-30"
output:
  html_document:
    keep_md: yes
    toc: yes
    toc_float: yes
    code_folding: hide
    number_sections: yes
    theme: "flatly"
  pdf_document:
    toc: yes
---



# Surveys  
Baseline survey: n = 5200
Survey: n = 6251

```r
survey_1 <- read.csv("/Users/annaduan/Library/CloudStorage/Box-Box/CLS Renter Survey/data/survey/Philly+Baseline+-+Phase+4_August+6,+2021_10.57.csv") %>%
  unique()

q1 <- survey_1 %>%
  head(1) %>%
  gather(key = "code", value = "question")

survey_2 <- read.csv("/Users/annaduan/Library/CloudStorage/Box-Box/CLS Renter Survey/data/survey/CLS+Renter+Survey_August+25,+2021_12.26.csv") %>%
  unique()

q2 <- survey_2 %>%
  head(1) %>%
  gather(key = "code", value = "question")

t <- full_join(q1, q2, by = "question")
```

# PUMS
PUMS: 38456 households, 117328 people

```r
pums <- read.csv("/Users/annaduan/Desktop/pums2021.csv")
```

### Philadelphia overall
#### Overall
##### Housing cost burden

```r
pums %>%
    mutate(count = 1,
      rent_burden_cat = factor(case_when(
      GRPIP < 30 ~ "Low (< 30%)",
      GRPIP >= 30 & GRPIP < 50 ~ "Moderate (30-50%)",
      GRPIP >= 50 ~ "Severe (>= 50%)"
    ))) %>%
  group_by(rent_burden_cat) %>%
  summarize(households = sum(count)) %>%
  ungroup() %>%
  mutate(pct = 100*households/sum(households)) %>%
  arrange(desc(pct)) %>%
  pander(caption = "Rent burden")
```


----------------------------------------
  rent_burden_cat    households    pct  
------------------- ------------ -------
    Low (< 30%)        32937      85.65 

 Moderate (30-50%)      3215      8.36  

  Severe (>= 50%)       2304      5.991 
----------------------------------------

Table: Rent burden

##### Overcrowding 

```r
pums %>%
  select(NP, BDSP) %>%
  mutate(count = 1,
      occupants_per_room = factor(case_when(
      NP/BDSP <= 2 ~ "<=2",
      NP/BDSP >= 2 & NP/BDSP < 6 ~ "3-5",
      NP/BDSP >= 6 & NP/BDSP < 9 ~ "6-9",
      NP/BDSP >= 9 ~ "9+"
    ))) %>%
  group_by(occupants_per_room) %>%
  summarize(households = sum(count)) %>%
  ungroup() %>%
  mutate(pct = 100*households/sum(households)) %>%
  arrange(desc(pct)) %>%
  pander(caption = "Occupants per room")
```


------------------------------------------
 occupants_per_room   households    pct   
-------------------- ------------ --------
        <=2             35826      93.16  

        3-5              1668      4.337  

         9+              916       2.382  

        6-9               46       0.1196 
------------------------------------------

Table: Occupants per room
##### Residence 1 year ago

```r
pums %>%
  select(MIG_label) %>%
  mutate(count = 1) %>%
  group_by(MIG_label) %>%
  summarize(households = sum(count)) %>%
  ungroup() %>%
  mutate(pct = 100*households/sum(households)) %>%
  arrange(desc(pct)) %>%
  pander(caption = "Moved in last year")
```


------------------------------------------------------
           MIG_label              households    pct   
-------------------------------- ------------ --------
  Yes, same house (nonmovers)       33343       86.7  

  No, different house in US or       4494      11.69  
          Puerto Rico                                 

   N/A (less than 1 year old)        391       1.017  

 No, outside US and Puerto Rico      228       0.5929 
------------------------------------------------------

Table: Moved in last year
##### Tenure

```r
pums %>%
  select(TEN_label) %>%
  mutate(count = 1) %>%
  group_by(TEN_label) %>%
  summarize(households = sum(count)) %>%
  ungroup() %>%
  mutate(pct = 100*households/sum(households)) %>%
  arrange(desc(pct)) %>%
  pander(caption = "Tenure")
```


--------------------------------------------------
          TEN_label            households    pct  
----------------------------- ------------ -------
 Owned with mortgage or loan     15458      40.2  
 (include home equity loans)                      

           Rented                13331      34.67 

    Owned free and clear          9027      23.47 

 Occupied without payment of      640       1.664 
            rent                                  
--------------------------------------------------

Table: Tenure
##### Housing quality

```r
pums %>%
  mutate(count = 1,
         housing_quality = factor(case_when(
      KIT == 1 & PLM == 1 ~ "full kitchen, full plumbing",
      KIT == 1 & PLM == 2 ~ "full kitchen, partial plumbing",
      KIT == 2 & PLM == 1 ~ "partial kitchen, full plumbing",
      KIT == 2 & PLM == 2 ~ "partial kitchen, partial plumbing"
    ))) %>%
  group_by(housing_quality) %>%
  summarize(households = sum(count)) %>%
  ungroup() %>%
  mutate(pct = 100*households/sum(households)) %>%
  arrange(desc(pct)) %>%
  pander(caption = "Housing quality")
```


------------------------------------------------------
        housing_quality           households    pct   
-------------------------------- ------------ --------
  full kitchen, full plumbing       38155      99.22  

 partial kitchen, full plumbing      183       0.4759 

    partial kitchen, partial          68       0.1768 
            plumbing                                  

 full kitchen, partial plumbing       50        0.13  
------------------------------------------------------

Table: Housing quality

#### Income 
##### Housing cost burden

```r
pums_inc <- pums %>%
  mutate(count = 1,
      income_cat = factor(case_when(
      HINCP < 35000 ~ "< 35k",
      HINCP >= 35000 & HINCP < 50000 ~ "35k - 50k",
      HINCP >= 50000 & HINCP < 100000 ~ "50k - 100k",
      HINCP >= 100000 ~ "> 100k"
    )))

pums_inc %>%
    mutate(rent_burden_cat = factor(case_when(
      GRPIP < 30 ~ "Low (< 30%)",
      GRPIP >= 30 & GRPIP < 50 ~ "Moderate (30-50%)",
      GRPIP >= 50 ~ "Severe (>= 50%)"
    ))) %>%
  group_by(income_cat, rent_burden_cat) %>%
  summarize(households = sum(count)) %>%
  ungroup() %>%
  group_by(income_cat) %>%
  mutate(pct = 100*households/sum(households)) %>%
  arrange(desc(income_cat), desc(rent_burden_cat)) %>%
  pander(caption = "Rent burden")
```


-------------------------------------------------------
 income_cat    rent_burden_cat    households     pct   
------------ ------------------- ------------ ---------
 50k - 100k    Severe (>= 50%)        99       0.8851  

 50k - 100k   Moderate (30-50%)      796        7.117  

 50k - 100k      Low (< 30%)        10290        92    

 35k - 50k     Severe (>= 50%)       190        4.166  

 35k - 50k    Moderate (30-50%)      981        21.51  

 35k - 50k       Low (< 30%)         3390       74.33  

   > 100k      Severe (>= 50%)        4        0.03095 

   > 100k     Moderate (30-50%)       78       0.6035  

   > 100k        Low (< 30%)        12843       99.37  

   < 35k       Severe (>= 50%)       2011       20.55  

   < 35k      Moderate (30-50%)      1360       13.9   

   < 35k         Low (< 30%)         6414       65.55  
-------------------------------------------------------

Table: Rent burden
##### Overcrowding

```r
pums_inc %>%
  mutate(count = 1,
    occupants_per_room = factor(case_when(
      NP/BDSP <= 2 ~ "<=2",
      NP/BDSP >= 2 & NP/BDSP < 6 ~ "3-5",
      NP/BDSP >= 6 & NP/BDSP < 9 ~ "6-9",
      NP/BDSP >= 9 ~ "9+"
    ))) %>%
  group_by(income_cat, occupants_per_room) %>%
  summarize(households = sum(count)) %>%
  ungroup() %>%
  group_by(income_cat) %>%
  mutate(pct = 100*households/sum(households)) %>%
  arrange(desc(income_cat), desc(occupants_per_room)) %>%
  pander(caption = "Occupants per room")
```


--------------------------------------------------------
 income_cat   occupants_per_room   households     pct   
------------ -------------------- ------------ ---------
 50k - 100k           9+              253        2.262  

 50k - 100k          6-9               20       0.1788  

 50k - 100k          3-5              562        5.025  

 50k - 100k          <=2             10350       92.53  

 35k - 50k            9+              134        2.938  

 35k - 50k           3-5              247        5.415  

 35k - 50k           <=2              4180       91.65  

   > 100k             9+              130        1.006  

   > 100k            6-9               20       0.1547  

   > 100k            3-5              464        3.59   

   > 100k            <=2             12311       95.25  

   < 35k              9+              399        4.078  

   < 35k             6-9               6        0.06132 

   < 35k             3-5              395        4.037  

   < 35k             <=2              8985       91.82  
--------------------------------------------------------

Table: Occupants per room
##### Residence 1 year ago

```r
pums_inc %>%
  group_by(income_cat, MIG_label) %>%
  summarize(households = sum(count)) %>%
  ungroup() %>%
  group_by(income_cat) %>%
  mutate(pct = 100*households/sum(households)) %>%
  arrange(desc(income_cat), desc(MIG_label)) %>%
  pander(caption = "Moved in last year")
```


-------------------------------------------------------------------
 income_cat             MIG_label              households    pct   
------------ -------------------------------- ------------ --------
 50k - 100k    Yes, same house (nonmovers)        9679      86.54  

 50k - 100k   No, outside US and Puerto Rico       63       0.5633 

 50k - 100k    No, different house in US or       1342        12   
                       Puerto Rico                                 

 50k - 100k     N/A (less than 1 year old)        101       0.903  

 35k - 50k     Yes, same house (nonmovers)        3887      85.22  

 35k - 50k    No, outside US and Puerto Rico       45       0.9866 

 35k - 50k     No, different house in US or       582       12.76  
                       Puerto Rico                                 

 35k - 50k      N/A (less than 1 year old)         47        1.03  

   > 100k      Yes, same house (nonmovers)       11155      86.31  

   > 100k     No, outside US and Puerto Rico       73       0.5648 

   > 100k      No, different house in US or       1526      11.81  
                       Puerto Rico                                 

   > 100k       N/A (less than 1 year old)        171       1.323  

   < 35k       Yes, same house (nonmovers)        8622      88.11  

   < 35k      No, outside US and Puerto Rico       47       0.4803 

   < 35k       No, different house in US or       1044      10.67  
                       Puerto Rico                                 

   < 35k        N/A (less than 1 year old)         72       0.7358 
-------------------------------------------------------------------

Table: Moved in last year
##### Tenure

```r
pums_inc %>%
  group_by(income_cat, TEN_label) %>%
  summarize(households = sum(count)) %>%
  ungroup() %>%
  group_by(income_cat) %>%
  mutate(pct = 100*households/sum(households)) %>%
  arrange(desc(income_cat), desc(TEN_label)) %>%
  pander(caption = "Tenure")
```


----------------------------------------------------------------
 income_cat            TEN_label            households    pct   
------------ ----------------------------- ------------ --------
 50k - 100k             Rented                 3980      35.58  

 50k - 100k   Owned with mortgage or loan      4519       40.4  
              (include home equity loans)                       

 50k - 100k      Owned free and clear          2556      22.85  

 50k - 100k   Occupied without payment of      130       1.162  
                         rent                                   

 35k - 50k              Rented                 1956      42.89  

 35k - 50k    Owned with mortgage or loan      1405       30.8  
              (include home equity loans)                       

 35k - 50k       Owned free and clear          1129      24.75  

 35k - 50k    Occupied without payment of       71       1.557  
                         rent                                   

   > 100k               Rented                 2861      22.14  

   > 100k     Owned with mortgage or loan      7378      57.08  
              (include home equity loans)                       

   > 100k        Owned free and clear          2621      20.28  

   > 100k     Occupied without payment of       65       0.5029 
                         rent                                   

   < 35k                Rented                 4534      46.34  

   < 35k      Owned with mortgage or loan      2156      22.03  
              (include home equity loans)                       

   < 35k         Owned free and clear          2721      27.81  

   < 35k      Occupied without payment of      374       3.822  
                         rent                                   
----------------------------------------------------------------

Table: Tenure

##### Housing quality

```r
pums_inc %>%
    mutate(count = 1,
         housing_quality = factor(case_when(
      KIT == 1 & PLM == 1 ~ "full kitchen, full plumbing",
      KIT == 1 & PLM == 2 ~ "full kitchen, partial plumbing",
      KIT == 2 & PLM == 1 ~ "partial kitchen, full plumbing",
      KIT == 2 & PLM == 2 ~ "partial kitchen, partial plumbing"
    ))) %>%
  group_by(income_cat, housing_quality) %>%
  summarize(households = sum(count)) %>%
  ungroup() %>%
  group_by(income_cat) %>%
  mutate(pct = 100*households/sum(households)) %>%
  arrange(desc(income_cat), desc(housing_quality)) %>%
  pander(caption = "Housing quality")
```


--------------------------------------------------------------------
 income_cat          housing_quality           households     pct   
------------ -------------------------------- ------------ ---------
 50k - 100k      partial kitchen, partial          8        0.07152 
                         plumbing                                   

 50k - 100k   partial kitchen, full plumbing       30       0.2682  

 50k - 100k   full kitchen, partial plumbing       18       0.1609  

 50k - 100k    full kitchen, full plumbing       11129       99.5   

 35k - 50k       partial kitchen, partial          14        0.307  
                         plumbing                                   

 35k - 50k    partial kitchen, full plumbing       22       0.4824  

 35k - 50k    full kitchen, partial plumbing       14        0.307  

 35k - 50k     full kitchen, full plumbing        4511       98.9   

   > 100k        partial kitchen, partial          12       0.09284 
                         plumbing                                   

   > 100k     partial kitchen, full plumbing       22       0.1702  

   > 100k      full kitchen, full plumbing       12891       99.74  

   < 35k         partial kitchen, partial          34       0.3475  
                         plumbing                                   

   < 35k      partial kitchen, full plumbing      109        1.114  

   < 35k      full kitchen, partial plumbing       18        0.184  

   < 35k       full kitchen, full plumbing        9624       98.35  
--------------------------------------------------------------------

Table: Housing quality

#### Race/ethnicity
##### Rent burden

```r
pums %>%
    mutate(rent_burden_cat = factor(case_when(
      GRPIP < 30 ~ "Low (< 30%)",
      GRPIP >= 30 & GRPIP < 50 ~ "Moderate (30-50%)",
      GRPIP >= 50 ~ "Severe (>= 50%)"
    )),
    count = 1) %>%
  group_by(HHLDRRAC1P_label, rent_burden_cat) %>%
  summarize(households = sum(count)) %>%
  ungroup() %>%
  group_by(HHLDRRAC1P_label) %>%
  mutate(pct = 100*households/sum(households)) %>%
  arrange(desc(HHLDRRAC1P_label), desc(rent_burden_cat), desc(pct)) %>%
  pander(caption = "Rent burden")
```


-----------------------------------------------------------------------
       HHLDRRAC1P_label          rent_burden_cat    households    pct  
------------------------------ ------------------- ------------ -------
         White alone             Severe (>= 50%)       864       4.556 

         White alone            Moderate (30-50%)      1359      7.165 

         White alone               Low (< 30%)        16743      88.28 

      Two or More Races          Severe (>= 50%)       139       9.115 

      Two or More Races         Moderate (30-50%)      160       10.49 

      Two or More Races            Low (< 30%)         1226      80.39 

    Some Other Race alone        Severe (>= 50%)       223       11.94 

    Some Other Race alone       Moderate (30-50%)      262       14.03 

    Some Other Race alone          Low (< 30%)         1383      74.04 

  Native Hawaiian and Other      Severe (>= 50%)        2        11.11 
    Pacific Islander alone                                             

  Native Hawaiian and Other     Moderate (30-50%)       2        11.11 
    Pacific Islander alone                                             

  Native Hawaiian and Other        Low (< 30%)          14       77.78 
    Pacific Islander alone                                             

  Black or African American      Severe (>= 50%)       889       7.119 
            alone                                                      

  Black or African American     Moderate (30-50%)      1164      9.322 
            alone                                                      

  Black or African American        Low (< 30%)        10434      83.56 
            alone                                                      

         Asian alone             Severe (>= 50%)       181       5.237 

         Asian alone            Moderate (30-50%)      249       7.205 

         Asian alone               Low (< 30%)         3026      87.56 

  American Indian and Alaska     Severe (>= 50%)        4        12.9  
 Native tribes specified; or                                           
  American Indian or Alaska                                            
 Native, not specified and no                                          
         other races                                                   

  American Indian and Alaska       Low (< 30%)          27       87.1  
 Native tribes specified; or                                           
  American Indian or Alaska                                            
 Native, not specified and no                                          
         other races                                                   

    American Indian alone        Severe (>= 50%)        2        1.98  

    American Indian alone       Moderate (30-50%)       15       14.85 

    American Indian alone          Low (< 30%)          84       83.17 

     Alaska Native alone        Moderate (30-50%)       4         100  
-----------------------------------------------------------------------

Table: Rent burden
##### Overcrowding

```r
pums %>%
  mutate(count = 1,
    occupants_per_room = factor(case_when(
      NP/BDSP <= 2 ~ "<=2",
      NP/BDSP >= 2 & NP/BDSP < 6 ~ "3-5",
      NP/BDSP >= 6 & NP/BDSP < 9 ~ "6-9",
      NP/BDSP >= 9 ~ "9+"
    ))) %>%
  group_by(HHLDRRAC1P_label, occupants_per_room) %>%
  summarize(households = sum(count)) %>%
  ungroup() %>%
  group_by(HHLDRRAC1P_label) %>%
  mutate(pct = 100*households/sum(households)) %>%
  arrange(desc(HHLDRRAC1P_label), desc(occupants_per_room)) %>%
  pander(caption = "Occupants per room")
```


--------------------------------------------------------------------------
       HHLDRRAC1P_label         occupants_per_room   households     pct   
------------------------------ -------------------- ------------ ---------
         White alone                    9+              424        2.236  

         White alone                   6-9               8        0.04218 

         White alone                   3-5              543        2.863  

         White alone                   <=2             17991       94.86  

      Two or More Races                 9+               49        3.213  

      Two or More Races                3-5              149        9.77   

      Two or More Races                <=2              1327       87.02  

    Some Other Race alone               9+               26        1.392  

    Some Other Race alone              6-9               12       0.6424  

    Some Other Race alone              3-5              193        10.33  

    Some Other Race alone              <=2              1637       87.63  

  Native Hawaiian and Other            <=2               18         100   
    Pacific Islander alone                                                

  Black or African American             9+              233        1.866  
            alone                                                         

  Black or African American            6-9               26       0.2082  
            alone                                                         

  Black or African American            3-5              547        4.381  
            alone                                                         

  Black or African American            <=2             11681       93.55  
            alone                                                         

         Asian alone                    9+              182        5.266  

         Asian alone                   3-5              236        6.829  

         Asian alone                   <=2              3038       87.91  

  American Indian and Alaska            9+               2         6.452  
 Native tribes specified; or                                              
  American Indian or Alaska                                               
 Native, not specified and no                                             
         other races                                                      

  American Indian and Alaska           <=2               29        93.55  
 Native tribes specified; or                                              
  American Indian or Alaska                                               
 Native, not specified and no                                             
         other races                                                      

    American Indian alone              <=2              101         100   

     Alaska Native alone               <=2               4          100   
--------------------------------------------------------------------------

Table: Occupants per room
##### Residence 1 year ago

```r
pums %>%
  mutate(count = 1) %>%
  group_by(HHLDRRAC1P_label, MIG_label) %>%
  summarize(households = sum(count)) %>%
  ungroup() %>%
  group_by(HHLDRRAC1P_label) %>%
  mutate(pct = 100*households/sum(households)) %>%
  arrange(desc(HHLDRRAC1P_label), desc(MIG_label)) %>%
  pander(caption = "Residence 1 year ago")
```


----------------------------------------------------------------------------
       HHLDRRAC1P_label                   MIG_label              households 
------------------------------ -------------------------------- ------------
         White alone             Yes, same house (nonmovers)       16089    

         White alone            No, outside US and Puerto Rico       94     

         White alone             No, different house in US or       2600    
                                         Puerto Rico                        

         White alone              N/A (less than 1 year old)        183     

      Two or More Races          Yes, same house (nonmovers)        1292    

      Two or More Races         No, outside US and Puerto Rico       21     

      Two or More Races          No, different house in US or       190     
                                         Puerto Rico                        

      Two or More Races           N/A (less than 1 year old)         22     

    Some Other Race alone        Yes, same house (nonmovers)        1642    

    Some Other Race alone       No, outside US and Puerto Rico       16     

    Some Other Race alone        No, different house in US or       185     
                                         Puerto Rico                        

    Some Other Race alone         N/A (less than 1 year old)         25     

  Native Hawaiian and Other      Yes, same house (nonmovers)         18     
    Pacific Islander alone                                                  

  Black or African American      Yes, same house (nonmovers)       11312    
            alone                                                           

  Black or African American     No, outside US and Puerto Rico       36     
            alone                                                           

  Black or African American      No, different house in US or       1030    
            alone                        Puerto Rico                        

  Black or African American       N/A (less than 1 year old)        109     
            alone                                                           

         Asian alone             Yes, same house (nonmovers)        2866    

         Asian alone            No, outside US and Puerto Rico       61     

         Asian alone             No, different house in US or       477     
                                         Puerto Rico                        

         Asian alone              N/A (less than 1 year old)         52     

  American Indian and Alaska     Yes, same house (nonmovers)         28     
 Native tribes specified; or                                                
  American Indian or Alaska                                                 
 Native, not specified and no                                               
         other races                                                        

  American Indian and Alaska     No, different house in US or        3      
 Native tribes specified; or             Puerto Rico                        
  American Indian or Alaska                                                 
 Native, not specified and no                                               
         other races                                                        

    American Indian alone        Yes, same house (nonmovers)         92     

    American Indian alone        No, different house in US or        9      
                                         Puerto Rico                        

     Alaska Native alone         Yes, same house (nonmovers)         4      
----------------------------------------------------------------------------

Table: Residence 1 year ago (continued below)

 
--------
  pct   
--------
 84.83  

 0.4956 

 13.71  

 0.9649 

 84.72  

 1.377  

 12.46  

 1.443  

  87.9  

 0.8565 

 9.904  

 1.338  

  100   

 90.59  

 0.2883 

 8.249  

 0.8729 

 82.93  

 1.765  

  13.8  

 1.505  

 90.32  

 9.677  

 91.09  

 8.911  

  100   
--------
##### Tenure 

```r
pums %>%
  mutate(count = 1) %>%
  group_by(HHLDRRAC1P_label, MIG_label) %>%
  summarize(households = sum(count)) %>%
  ungroup() %>%
  group_by(HHLDRRAC1P_label) %>%
  mutate(pct = 100*households/sum(households)) %>%
  arrange(desc(HHLDRRAC1P_label), desc(MIG_label)) %>%
  pander(caption = "Residence 1 year ago")
```


----------------------------------------------------------------------------
       HHLDRRAC1P_label                   MIG_label              households 
------------------------------ -------------------------------- ------------
         White alone             Yes, same house (nonmovers)       16089    

         White alone            No, outside US and Puerto Rico       94     

         White alone             No, different house in US or       2600    
                                         Puerto Rico                        

         White alone              N/A (less than 1 year old)        183     

      Two or More Races          Yes, same house (nonmovers)        1292    

      Two or More Races         No, outside US and Puerto Rico       21     

      Two or More Races          No, different house in US or       190     
                                         Puerto Rico                        

      Two or More Races           N/A (less than 1 year old)         22     

    Some Other Race alone        Yes, same house (nonmovers)        1642    

    Some Other Race alone       No, outside US and Puerto Rico       16     

    Some Other Race alone        No, different house in US or       185     
                                         Puerto Rico                        

    Some Other Race alone         N/A (less than 1 year old)         25     

  Native Hawaiian and Other      Yes, same house (nonmovers)         18     
    Pacific Islander alone                                                  

  Black or African American      Yes, same house (nonmovers)       11312    
            alone                                                           

  Black or African American     No, outside US and Puerto Rico       36     
            alone                                                           

  Black or African American      No, different house in US or       1030    
            alone                        Puerto Rico                        

  Black or African American       N/A (less than 1 year old)        109     
            alone                                                           

         Asian alone             Yes, same house (nonmovers)        2866    

         Asian alone            No, outside US and Puerto Rico       61     

         Asian alone             No, different house in US or       477     
                                         Puerto Rico                        

         Asian alone              N/A (less than 1 year old)         52     

  American Indian and Alaska     Yes, same house (nonmovers)         28     
 Native tribes specified; or                                                
  American Indian or Alaska                                                 
 Native, not specified and no                                               
         other races                                                        

  American Indian and Alaska     No, different house in US or        3      
 Native tribes specified; or             Puerto Rico                        
  American Indian or Alaska                                                 
 Native, not specified and no                                               
         other races                                                        

    American Indian alone        Yes, same house (nonmovers)         92     

    American Indian alone        No, different house in US or        9      
                                         Puerto Rico                        

     Alaska Native alone         Yes, same house (nonmovers)         4      
----------------------------------------------------------------------------

Table: Residence 1 year ago (continued below)

 
--------
  pct   
--------
 84.83  

 0.4956 

 13.71  

 0.9649 

 84.72  

 1.377  

 12.46  

 1.443  

  87.9  

 0.8565 

 9.904  

 1.338  

  100   

 90.59  

 0.2883 

 8.249  

 0.8729 

 82.93  

 1.765  

  13.8  

 1.505  

 90.32  

 9.677  

 91.09  

 8.911  

  100   
--------
##### Housing quality

```r
pums %>%
    mutate(count = 1,
         housing_quality = factor(case_when(
      KIT == 1 & PLM == 1 ~ "full kitchen, full plumbing",
      KIT == 1 & PLM == 2 ~ "full kitchen, partial plumbing",
      KIT == 2 & PLM == 1 ~ "partial kitchen, full plumbing",
      KIT == 2 & PLM == 2 ~ "partial kitchen, partial plumbing"
    ))) %>%
  group_by(HHLDRRAC1P_label, housing_quality) %>%
  summarize(households = sum(count)) %>%
  ungroup() %>%
  group_by(HHLDRRAC1P_label) %>%
  mutate(pct = 100*households/sum(households)) %>%
  arrange(desc(HHLDRRAC1P_label), desc(housing_quality)) %>%
  pander(caption = "Housing quality")
```


----------------------------------------------------------------------------
       HHLDRRAC1P_label                housing_quality           households 
------------------------------ -------------------------------- ------------
         White alone               partial kitchen, partial          14     
                                           plumbing                         

         White alone            partial kitchen, full plumbing       62     

         White alone            full kitchen, partial plumbing       5      

         White alone             full kitchen, full plumbing       18885    

      Two or More Races            partial kitchen, partial          3      
                                           plumbing                         

      Two or More Races         partial kitchen, full plumbing       13     

      Two or More Races          full kitchen, full plumbing        1509    

    Some Other Race alone          partial kitchen, partial          7      
                                           plumbing                         

    Some Other Race alone       partial kitchen, full plumbing       15     

    Some Other Race alone       full kitchen, partial plumbing       9      

    Some Other Race alone        full kitchen, full plumbing        1837    

  Native Hawaiian and Other     partial kitchen, full plumbing       3      
    Pacific Islander alone                                                  

  Native Hawaiian and Other      full kitchen, full plumbing         15     
    Pacific Islander alone                                                  

  Black or African American        partial kitchen, partial          40     
            alone                          plumbing                         

  Black or African American     partial kitchen, full plumbing       37     
            alone                                                           

  Black or African American     full kitchen, partial plumbing       30     
            alone                                                           

  Black or African American      full kitchen, full plumbing       12380    
            alone                                                           

         Asian alone               partial kitchen, partial          4      
                                           plumbing                         

         Asian alone            partial kitchen, full plumbing       53     

         Asian alone            full kitchen, partial plumbing       6      

         Asian alone             full kitchen, full plumbing        3393    

  American Indian and Alaska     full kitchen, full plumbing         31     
 Native tribes specified; or                                                
  American Indian or Alaska                                                 
 Native, not specified and no                                               
         other races                                                        

    American Indian alone        full kitchen, full plumbing        101     

     Alaska Native alone         full kitchen, full plumbing         4      
----------------------------------------------------------------------------

Table: Housing quality (continued below)

 
---------
   pct   
---------
 0.07382 

 0.3269  

 0.02636 

  99.57  

 0.1967  

 0.8525  

  98.95  

 0.3747  

  0.803  

 0.4818  

  98.34  

  16.67  

  83.33  

 0.3203  

 0.2963  

 0.2402  

  99.14  

 0.1157  

  1.534  

 0.1736  

  98.18  

   100   

   100   

   100   
---------

#### Sub-geography
#### Housing cost burden

```r
pums_puma <- pums %>%
  mutate(region = factor(case_when(
    PUMA == 03201  ~ "Far Northeast",
    PUMA == 03202  ~ "Far Northeast - West",
    PUMA == 03203  ~ "Far Northeast - East",
    PUMA == 03204  ~ "North",
    PUMA == 03205  ~ "East",
    PUMA == 03206  ~ "Northwest",
    PUMA == 03207  ~ "Central",
    PUMA == 03208  ~ "West",
    PUMA == 03209  ~ "Center City",
    PUMA == 03210  ~ "Southwest",
    PUMA == 03211  ~ "Southeast")))

pums_puma %>%
   mutate(rent_burden_cat = factor(case_when(
      GRPIP < 30 ~ "Low (< 30%)",
      GRPIP >= 30 & GRPIP < 50 ~ "Moderate (30-50%)",
      GRPIP >= 50 ~ "Severe (>= 50%)"
    )),
    count = 1) %>%
  group_by(region, rent_burden_cat) %>%
  summarize(households = sum(count)) %>%
  ungroup() %>%
  group_by(region) %>%
  mutate(pct = 100*households/sum(households)) %>%
  arrange(desc(region), desc(rent_burden_cat), desc(pct)) %>%
  pander(caption = "Rent burden")
```


---------------------------------------------------------------
        region           rent_burden_cat    households    pct  
---------------------- ------------------- ------------ -------
         West            Severe (>= 50%)       268       8.793 

         West           Moderate (30-50%)      333       10.93 

         West              Low (< 30%)         2447      80.28 

      Southwest          Severe (>= 50%)       294       8.861 

      Southwest         Moderate (30-50%)      384       11.57 

      Southwest            Low (< 30%)         2640      79.57 

      Southeast          Severe (>= 50%)       161       4.682 

      Southeast         Moderate (30-50%)      250       7.27  

      Southeast            Low (< 30%)         3028      88.05 

      Northwest          Severe (>= 50%)       184       4.061 

      Northwest         Moderate (30-50%)      349       7.702 

      Northwest            Low (< 30%)         3998      88.24 

        North            Severe (>= 50%)       178       5.218 

        North           Moderate (30-50%)      200       5.863 

        North              Low (< 30%)         3033      88.92 

 Far Northeast - West    Severe (>= 50%)       216       6.457 

 Far Northeast - West   Moderate (30-50%)      256       7.653 

 Far Northeast - West      Low (< 30%)         2873      85.89 

 Far Northeast - East    Severe (>= 50%)       165       5.567 

 Far Northeast - East   Moderate (30-50%)      233       7.861 

 Far Northeast - East      Low (< 30%)         2566      86.57 

    Far Northeast        Severe (>= 50%)       159       3.491 

    Far Northeast       Moderate (30-50%)      269       5.907 

    Far Northeast          Low (< 30%)         4126      90.6  

         East            Severe (>= 50%)       244       8.003 

         East           Moderate (30-50%)      259       8.495 

         East              Low (< 30%)         2546      83.5  

       Central           Severe (>= 50%)       194       10.32 

       Central          Moderate (30-50%)      176       9.367 

       Central             Low (< 30%)         1509      80.31 

     Center City         Severe (>= 50%)       241        4.9  

     Center City        Moderate (30-50%)      506       10.29 

     Center City           Low (< 30%)         4171      84.81 
---------------------------------------------------------------

Table: Rent burden
##### Overcrowding

```r
pums_puma %>%
  mutate(count = 1,
    occupants_per_room = factor(case_when(
      NP/BDSP <= 2 ~ "<=2",
      NP/BDSP >= 2 & NP/BDSP < 6 ~ "3-5",
      NP/BDSP >= 6 & NP/BDSP < 9 ~ "6-9",
      NP/BDSP >= 9 ~ "9+"
    ))) %>%
  group_by(region, occupants_per_room) %>%
  summarize(households = sum(count)) %>%
  ungroup() %>%
  group_by(region) %>%
  mutate(pct = 100*households/sum(households)) %>%
  arrange(desc(region), desc(occupants_per_room)) %>%
  pander(caption = "Occupants per room")
```


-----------------------------------------------------------------
        region          occupants_per_room   households    pct   
---------------------- -------------------- ------------ --------
         West                   9+               91       2.986  

         West                  6-9               14       0.4593 

         West                  3-5               90       2.953  

         West                  <=2              2853       93.6  

      Southwest                 9+              116       3.496  

      Southwest                3-5              163       4.913  

      Southwest                <=2              3039      91.59  

      Southeast                 9+               36       1.047  

      Southeast                3-5              157       4.565  

      Southeast                <=2              3246      94.39  

      Northwest                 9+               72       1.589  

      Northwest                3-5               75       1.655  

      Northwest                <=2              4384      96.76  

        North                   9+               52       1.524  

        North                  3-5              211       6.186  

        North                  <=2              3148      92.29  

 Far Northeast - West           9+               48       1.435  

 Far Northeast - West          6-9               18       0.5381 

 Far Northeast - West          3-5              276       8.251  

 Far Northeast - West          <=2              3003      89.78  

 Far Northeast - East           9+               27       0.9109 

 Far Northeast - East          3-5              162       5.466  

 Far Northeast - East          <=2              2775      93.62  

    Far Northeast               9+               47       1.032  

    Far Northeast              3-5              205       4.502  

    Far Northeast              <=2              4302      94.47  

         East                   9+               46       1.509  

         East                  6-9               6        0.1968 

         East                  3-5              193        6.33  

         East                  <=2              2804      91.96  

       Central                  9+               61       3.246  

       Central                 3-5               65       3.459  

       Central                 <=2              1753      93.29  

     Center City                9+              320       6.507  

     Center City               6-9               8        0.1627 

     Center City               3-5               71       1.444  

     Center City               <=2              4519      91.89  
-----------------------------------------------------------------

Table: Occupants per room

##### Residence 1 year ago

```r
pums_puma %>%
  mutate(count = 1) %>%
  group_by(region, MIG_label) %>%
  summarize(households = sum(count)) %>%
  ungroup() %>%
  group_by(region) %>%
  mutate(pct = 100*households/sum(households)) %>%
  arrange(desc(region), desc(MIG_label)) %>%
  pander(caption = "Residence 1 year ago")
```


-----------------------------------------------------------------------------
        region                    MIG_label              households    pct   
---------------------- -------------------------------- ------------ --------
         West            Yes, same house (nonmovers)        2697      88.48  

         West           No, outside US and Puerto Rico       18       0.5906 

         West            No, different house in US or       310       10.17  
                                 Puerto Rico                                 

         West             N/A (less than 1 year old)         23       0.7546 

      Southwest          Yes, same house (nonmovers)        2835      85.44  

      Southwest         No, outside US and Puerto Rico       41       1.236  

      Southwest          No, different house in US or       414       12.48  
                                 Puerto Rico                                 

      Southwest           N/A (less than 1 year old)         28       0.8439 

      Southeast          Yes, same house (nonmovers)        2916      84.79  

      Southeast         No, outside US and Puerto Rico       14       0.4071 

      Southeast          No, different house in US or       469       13.64  
                                 Puerto Rico                                 

      Southeast           N/A (less than 1 year old)         40       1.163  

      Northwest          Yes, same house (nonmovers)        3913      86.36  

      Northwest         No, outside US and Puerto Rico       17       0.3752 

      Northwest          No, different house in US or       551       12.16  
                                 Puerto Rico                                 

      Northwest           N/A (less than 1 year old)         50       1.104  

        North            Yes, same house (nonmovers)        3126      91.64  

        North           No, outside US and Puerto Rico       10       0.2932 

        North            No, different house in US or       250       7.329  
                                 Puerto Rico                                 

        North             N/A (less than 1 year old)         25       0.7329 

 Far Northeast - West    Yes, same house (nonmovers)        3016      90.16  

 Far Northeast - West   No, outside US and Puerto Rico       41       1.226  

 Far Northeast - West    No, different house in US or       255       7.623  
                                 Puerto Rico                                 

 Far Northeast - West     N/A (less than 1 year old)         33       0.9865 

 Far Northeast - East    Yes, same house (nonmovers)        2653      89.51  

 Far Northeast - East   No, outside US and Puerto Rico       10       0.3374 

 Far Northeast - East    No, different house in US or       266       8.974  
                                 Puerto Rico                                 

 Far Northeast - East     N/A (less than 1 year old)         35       1.181  

    Far Northeast        Yes, same house (nonmovers)        4179      91.77  

    Far Northeast       No, outside US and Puerto Rico       30       0.6588 

    Far Northeast        No, different house in US or       311       6.829  
                                 Puerto Rico                                 

    Far Northeast         N/A (less than 1 year old)         34       0.7466 

         East            Yes, same house (nonmovers)        2643      86.68  

         East           No, outside US and Puerto Rico       6        0.1968 

         East            No, different house in US or       350       11.48  
                                 Puerto Rico                                 

         East             N/A (less than 1 year old)         50        1.64  

       Central           Yes, same house (nonmovers)        1597      84.99  

       Central          No, outside US and Puerto Rico       6        0.3193 

       Central           No, different house in US or       258       13.73  
                                 Puerto Rico                                 

       Central            N/A (less than 1 year old)         18       0.958  

     Center City         Yes, same house (nonmovers)        3768      76.62  

     Center City        No, outside US and Puerto Rico       35       0.7117 

     Center City         No, different house in US or       1060      21.55  
                                 Puerto Rico                                 

     Center City          N/A (less than 1 year old)         55       1.118  
-----------------------------------------------------------------------------

Table: Residence 1 year ago

##### Tenure

```r
pums_puma %>%
  mutate(count = 1) %>%
  group_by(region, MIG_label) %>%
  summarize(households = sum(count)) %>%
  ungroup() %>%
  group_by(region) %>%
  mutate(pct = 100*households/sum(households)) %>%
  arrange(desc(region), desc(MIG_label)) %>%
  pander(caption = "Residence 1 year ago")
```


-----------------------------------------------------------------------------
        region                    MIG_label              households    pct   
---------------------- -------------------------------- ------------ --------
         West            Yes, same house (nonmovers)        2697      88.48  

         West           No, outside US and Puerto Rico       18       0.5906 

         West            No, different house in US or       310       10.17  
                                 Puerto Rico                                 

         West             N/A (less than 1 year old)         23       0.7546 

      Southwest          Yes, same house (nonmovers)        2835      85.44  

      Southwest         No, outside US and Puerto Rico       41       1.236  

      Southwest          No, different house in US or       414       12.48  
                                 Puerto Rico                                 

      Southwest           N/A (less than 1 year old)         28       0.8439 

      Southeast          Yes, same house (nonmovers)        2916      84.79  

      Southeast         No, outside US and Puerto Rico       14       0.4071 

      Southeast          No, different house in US or       469       13.64  
                                 Puerto Rico                                 

      Southeast           N/A (less than 1 year old)         40       1.163  

      Northwest          Yes, same house (nonmovers)        3913      86.36  

      Northwest         No, outside US and Puerto Rico       17       0.3752 

      Northwest          No, different house in US or       551       12.16  
                                 Puerto Rico                                 

      Northwest           N/A (less than 1 year old)         50       1.104  

        North            Yes, same house (nonmovers)        3126      91.64  

        North           No, outside US and Puerto Rico       10       0.2932 

        North            No, different house in US or       250       7.329  
                                 Puerto Rico                                 

        North             N/A (less than 1 year old)         25       0.7329 

 Far Northeast - West    Yes, same house (nonmovers)        3016      90.16  

 Far Northeast - West   No, outside US and Puerto Rico       41       1.226  

 Far Northeast - West    No, different house in US or       255       7.623  
                                 Puerto Rico                                 

 Far Northeast - West     N/A (less than 1 year old)         33       0.9865 

 Far Northeast - East    Yes, same house (nonmovers)        2653      89.51  

 Far Northeast - East   No, outside US and Puerto Rico       10       0.3374 

 Far Northeast - East    No, different house in US or       266       8.974  
                                 Puerto Rico                                 

 Far Northeast - East     N/A (less than 1 year old)         35       1.181  

    Far Northeast        Yes, same house (nonmovers)        4179      91.77  

    Far Northeast       No, outside US and Puerto Rico       30       0.6588 

    Far Northeast        No, different house in US or       311       6.829  
                                 Puerto Rico                                 

    Far Northeast         N/A (less than 1 year old)         34       0.7466 

         East            Yes, same house (nonmovers)        2643      86.68  

         East           No, outside US and Puerto Rico       6        0.1968 

         East            No, different house in US or       350       11.48  
                                 Puerto Rico                                 

         East             N/A (less than 1 year old)         50        1.64  

       Central           Yes, same house (nonmovers)        1597      84.99  

       Central          No, outside US and Puerto Rico       6        0.3193 

       Central           No, different house in US or       258       13.73  
                                 Puerto Rico                                 

       Central            N/A (less than 1 year old)         18       0.958  

     Center City         Yes, same house (nonmovers)        3768      76.62  

     Center City        No, outside US and Puerto Rico       35       0.7117 

     Center City         No, different house in US or       1060      21.55  
                                 Puerto Rico                                 

     Center City          N/A (less than 1 year old)         55       1.118  
-----------------------------------------------------------------------------

Table: Residence 1 year ago

##### Housing quality

```r
pums_puma %>%
    mutate(count = 1,
         housing_quality = factor(case_when(
      KIT == 1 & PLM == 1 ~ "full kitchen, full plumbing",
      KIT == 1 & PLM == 2 ~ "full kitchen, partial plumbing",
      KIT == 2 & PLM == 1 ~ "partial kitchen, full plumbing",
      KIT == 2 & PLM == 2 ~ "partial kitchen, partial plumbing"
    ))) %>%
  group_by(region, housing_quality) %>%
  summarize(households = sum(count)) %>%
  ungroup() %>%
  group_by(region) %>%
  mutate(pct = 100*households/sum(households)) %>%
  arrange(desc(region), desc(housing_quality)) %>%
  pander(caption = "Housing quality")
```


------------------------------------------------------------------------------
        region                 housing_quality           households     pct   
---------------------- -------------------------------- ------------ ---------
         West              partial kitchen, partial          6        0.1969  
                                   plumbing                                   

         West           partial kitchen, full plumbing       37        1.214  

         West           full kitchen, partial plumbing       2        0.06562 

         West            full kitchen, full plumbing        3003       98.52  

      Southwest            partial kitchen, partial          16       0.4822  
                                   plumbing                                   

      Southwest         partial kitchen, full plumbing       15       0.4521  

      Southwest         full kitchen, partial plumbing       13       0.3918  

      Southwest          full kitchen, full plumbing        3274       98.67  

      Southeast            partial kitchen, partial          1        0.02908 
                                   plumbing                                   

      Southeast         partial kitchen, full plumbing       20       0.5816  

      Southeast         full kitchen, partial plumbing       4        0.1163  

      Southeast          full kitchen, full plumbing        3414       99.27  

      Northwest            partial kitchen, partial          9        0.1986  
                                   plumbing                                   

      Northwest         partial kitchen, full plumbing       12       0.2648  

      Northwest         full kitchen, partial plumbing       7        0.1545  

      Northwest          full kitchen, full plumbing        4503       99.38  

        North              partial kitchen, partial          6        0.1759  
                                   plumbing                                   

        North           partial kitchen, full plumbing       15       0.4398  

        North           full kitchen, partial plumbing       1        0.02932 

        North            full kitchen, full plumbing        3389       99.36  

 Far Northeast - West      partial kitchen, partial          5        0.1495  
                                   plumbing                                   

 Far Northeast - West   partial kitchen, full plumbing       13       0.3886  

 Far Northeast - West   full kitchen, partial plumbing       4        0.1196  

 Far Northeast - West    full kitchen, full plumbing        3323       99.34  

 Far Northeast - East      partial kitchen, partial          6        0.2024  
                                   plumbing                                   

 Far Northeast - East   partial kitchen, full plumbing       21       0.7085  

 Far Northeast - East   full kitchen, partial plumbing       4         0.135  

 Far Northeast - East    full kitchen, full plumbing        2933       98.95  

    Far Northeast          partial kitchen, partial          4        0.08783 
                                   plumbing                                   

    Far Northeast       partial kitchen, full plumbing       13       0.2855  

    Far Northeast        full kitchen, full plumbing        4537       99.63  

         East              partial kitchen, partial          3        0.09839 
                                   plumbing                                   

         East           partial kitchen, full plumbing       15        0.492  

         East           full kitchen, partial plumbing       13       0.4264  

         East            full kitchen, full plumbing        3018       98.98  

       Central             partial kitchen, partial          5        0.2661  
                                   plumbing                                   

       Central          partial kitchen, full plumbing       12       0.6386  

       Central          full kitchen, partial plumbing       2        0.1064  

       Central           full kitchen, full plumbing        1860       98.99  

     Center City           partial kitchen, partial          7        0.1423  
                                   plumbing                                   

     Center City        partial kitchen, full plumbing       10       0.2033  

     Center City         full kitchen, full plumbing        4901       99.65  
------------------------------------------------------------------------------

Table: Housing quality

#### Tenure
##### Housing cost burden

```r
pums %>%
   mutate(rent_burden_cat = factor(case_when(
      GRPIP < 30 ~ "Low (< 30%)",
      GRPIP >= 30 & GRPIP < 50 ~ "Moderate (30-50%)",
      GRPIP >= 50 ~ "Severe (>= 50%)"
    )),
    count = 1) %>%
  group_by(TEN_label, rent_burden_cat) %>%
  summarize(households = sum(count)) %>%
  ungroup() %>%
  group_by(TEN_label) %>%
  mutate(pct = 100*households/sum(households)) %>%
  arrange(desc(TEN_label), desc(rent_burden_cat), desc(pct)) %>%
  pander(caption = "Rent burden")
```


----------------------------------------------------------------------
          TEN_label             rent_burden_cat    households    pct  
----------------------------- ------------------- ------------ -------
           Rented               Severe (>= 50%)       2304      17.28 

           Rented              Moderate (30-50%)      3215      24.12 

           Rented                 Low (< 30%)         7812      58.6  

 Owned with mortgage or loan      Low (< 30%)        15458       100  
 (include home equity loans)                                          

    Owned free and clear          Low (< 30%)         9027       100  

 Occupied without payment of      Low (< 30%)         640        100  
            rent                                                      
----------------------------------------------------------------------

Table: Rent burden

##### Overcrowding

```r
pums %>%
  mutate(count = 1,
    occupants_per_room = factor(case_when(
      NP/BDSP <= 2 ~ "<=2",
      NP/BDSP >= 2 & NP/BDSP < 6 ~ "3-5",
      NP/BDSP >= 6 & NP/BDSP < 9 ~ "6-9",
      NP/BDSP >= 9 ~ "9+"
    ))) %>%
  group_by(TEN_label, occupants_per_room) %>%
  summarize(households = sum(count)) %>%
  ungroup() %>%
  group_by(TEN_label) %>%
  mutate(pct = 100*households/sum(households)) %>%
  arrange(desc(TEN_label), desc(occupants_per_room)) %>%
  pander(caption = "Occupants per room")
```


-------------------------------------------------------------------------
          TEN_label            occupants_per_room   households     pct   
----------------------------- -------------------- ------------ ---------
           Rented                      9+              802        6.016  

           Rented                     6-9               34        0.255  

           Rented                     3-5              898        6.736  

           Rented                     <=2             11597       86.99  

 Owned with mortgage or loan           9+               58       0.3752  
 (include home equity loans)                                             

 Owned with mortgage or loan          6-9               12       0.07763 
 (include home equity loans)                                             

 Owned with mortgage or loan          3-5              511        3.306  
 (include home equity loans)                                             

 Owned with mortgage or loan          <=2             14877       96.24  
 (include home equity loans)                                             

    Owned free and clear               9+               43       0.4763  

    Owned free and clear              3-5              230        2.548  

    Owned free and clear              <=2              8754       96.98  

 Occupied without payment of           9+               13        2.031  
            rent                                                         

 Occupied without payment of          3-5               29        4.531  
            rent                                                         

 Occupied without payment of          <=2              598        93.44  
            rent                                                         
-------------------------------------------------------------------------

Table: Occupants per room

##### Residence 1 year ago

```r
pums %>%
  mutate(count = 1) %>%
  group_by(TEN_label, MIG_label) %>%
  summarize(households = sum(count)) %>%
  ungroup() %>%
  group_by(TEN_label) %>%
  mutate(pct = 100*households/sum(households)) %>%
  arrange(desc(TEN_label), desc(MIG_label)) %>%
  pander(caption = "Residence 1 year ago")
```


---------------------------------------------------------------------------
          TEN_label                      MIG_label              households 
----------------------------- -------------------------------- ------------
           Rented               Yes, same house (nonmovers)       10180    

           Rented              No, outside US and Puerto Rico      147     

           Rented               No, different house in US or       2865    
                                        Puerto Rico                        

           Rented                N/A (less than 1 year old)        139     

 Owned with mortgage or loan    Yes, same house (nonmovers)       14053    
 (include home equity loans)                                               

 Owned with mortgage or loan   No, outside US and Puerto Rico       48     
 (include home equity loans)                                               

 Owned with mortgage or loan    No, different house in US or       1159    
 (include home equity loans)            Puerto Rico                        

 Owned with mortgage or loan     N/A (less than 1 year old)        198     
 (include home equity loans)                                               

    Owned free and clear        Yes, same house (nonmovers)        8533    

    Owned free and clear       No, outside US and Puerto Rico       31     

    Owned free and clear        No, different house in US or       420     
                                        Puerto Rico                        

    Owned free and clear         N/A (less than 1 year old)         43     

 Occupied without payment of    Yes, same house (nonmovers)        577     
            rent                                                           

 Occupied without payment of   No, outside US and Puerto Rico       2      
            rent                                                           

 Occupied without payment of    No, different house in US or        50     
            rent                        Puerto Rico                        

 Occupied without payment of     N/A (less than 1 year old)         11     
            rent                                                           
---------------------------------------------------------------------------

Table: Residence 1 year ago (continued below)

 
--------
  pct   
--------
 76.36  

 1.103  

 21.49  

 1.043  

 90.91  

 0.3105 

 7.498  

 1.281  

 94.53  

 0.3434 

 4.653  

 0.4763 

 90.16  

 0.3125 

 7.812  

 1.719  
--------

##### Housing quality

```r
pums %>%
    mutate(count = 1,
         housing_quality = factor(case_when(
      KIT == 1 & PLM == 1 ~ "full kitchen, full plumbing",
      KIT == 1 & PLM == 2 ~ "full kitchen, partial plumbing",
      KIT == 2 & PLM == 1 ~ "partial kitchen, full plumbing",
      KIT == 2 & PLM == 2 ~ "partial kitchen, partial plumbing"
    ))) %>%
  group_by(TEN_label, housing_quality) %>%
  summarize(households = sum(count)) %>%
  ungroup() %>%
  group_by(TEN_label) %>%
  mutate(pct = 100*households/sum(households)) %>%
  arrange(desc(TEN_label), desc(housing_quality)) %>%
  pander(caption = "Housing quality")
```


---------------------------------------------------------------------------
          TEN_label                   housing_quality           households 
----------------------------- -------------------------------- ------------
           Rented                 partial kitchen, partial          22     
                                          plumbing                         

           Rented              partial kitchen, full plumbing       99     

           Rented              full kitchen, partial plumbing       17     

           Rented               full kitchen, full plumbing       13193    

 Owned with mortgage or loan      partial kitchen, partial          10     
 (include home equity loans)              plumbing                         

 Owned with mortgage or loan   partial kitchen, full plumbing       41     
 (include home equity loans)                                               

 Owned with mortgage or loan   full kitchen, partial plumbing       14     
 (include home equity loans)                                               

 Owned with mortgage or loan    full kitchen, full plumbing       15393    
 (include home equity loans)                                               

    Owned free and clear          partial kitchen, partial          18     
                                          plumbing                         

    Owned free and clear       partial kitchen, full plumbing       39     

    Owned free and clear       full kitchen, partial plumbing       13     

    Owned free and clear        full kitchen, full plumbing        8957    

 Occupied without payment of      partial kitchen, partial          18     
            rent                          plumbing                         

 Occupied without payment of   partial kitchen, full plumbing       4      
            rent                                                           

 Occupied without payment of   full kitchen, partial plumbing       6      
            rent                                                           

 Occupied without payment of    full kitchen, full plumbing        612     
            rent                                                           
---------------------------------------------------------------------------

Table: Housing quality (continued below)

 
---------
   pct   
---------
  0.165  

 0.7426  

 0.1275  

  98.96  

 0.06469 

 0.2652  

 0.09057 

  99.58  

 0.1994  

  0.432  

  0.144  

  99.22  

  2.812  

  0.625  

 0.9375  

  95.62  
---------

### Philadelphia households with young children (<6 years)
Households with children < 6 years: n = 3528
##### Housing cost burden

```r
pums_kids <- pums %>%
  filter(HUPAC_label == "With children under 6 years only")

pums_kids %>%
    mutate(count = 1,
      rent_burden_cat = factor(case_when(
      GRPIP < 30 ~ "Low (< 30%)",
      GRPIP >= 30 & GRPIP < 50 ~ "Moderate (30-50%)",
      GRPIP >= 50 ~ "Severe (>= 50%)"
    ))) %>%
  group_by(rent_burden_cat) %>%
  summarize(households = sum(count)) %>%
  ungroup() %>%
  mutate(pct = 100*households/sum(households)) %>%
  arrange(desc(pct)) %>%
  pander(caption = "Rent burden")
```


----------------------------------------
  rent_burden_cat    households    pct  
------------------- ------------ -------
    Low (< 30%)         3047      86.37 

 Moderate (30-50%)      287       8.135 

  Severe (>= 50%)       194       5.499 
----------------------------------------

Table: Rent burden

##### Overcrowding 

```r
pums_kids %>%
  select(NP, BDSP) %>%
  mutate(count = 1,
      occupants_per_room = factor(case_when(
      NP/BDSP <= 2 ~ "<=2",
      NP/BDSP >= 2 & NP/BDSP < 6 ~ "3-5",
      NP/BDSP >= 6 & NP/BDSP < 9 ~ "6-9",
      NP/BDSP >= 9 ~ "9+"
    ))) %>%
  group_by(occupants_per_room) %>%
  summarize(households = sum(count)) %>%
  ungroup() %>%
  mutate(pct = 100*households/sum(households)) %>%
  arrange(desc(pct)) %>%
  pander(caption = "Occupants per room")
```


------------------------------------------
 occupants_per_room   households    pct   
-------------------- ------------ --------
        <=2              3202      90.76  

        3-5              268       7.596  

         9+               50       1.417  

        6-9               8        0.2268 
------------------------------------------

Table: Occupants per room
##### Residence 1 year ago

```r
pums_kids %>%
  select(MIG_label) %>%
  mutate(count = 1) %>%
  group_by(MIG_label) %>%
  summarize(households = sum(count)) %>%
  ungroup() %>%
  mutate(pct = 100*households/sum(households)) %>%
  arrange(desc(pct)) %>%
  pander(caption = "Moved in last year")
```


------------------------------------------------------
           MIG_label              households    pct   
-------------------------------- ------------ --------
  Yes, same house (nonmovers)        2842      80.56  

  No, different house in US or       377       10.69  
          Puerto Rico                                 

   N/A (less than 1 year old)        286       8.107  

 No, outside US and Puerto Rico       23       0.6519 
------------------------------------------------------

Table: Moved in last year
##### Tenure

```r
pums_kids %>%
  select(TEN_label) %>%
  mutate(count = 1) %>%
  group_by(TEN_label) %>%
  summarize(households = sum(count)) %>%
  ungroup() %>%
  mutate(pct = 100*households/sum(households)) %>%
  arrange(desc(pct)) %>%
  pander(caption = "Tenure")
```


--------------------------------------------------
          TEN_label            households    pct  
----------------------------- ------------ -------
 Owned with mortgage or loan      1820      51.59 
 (include home equity loans)                      

           Rented                 1192      33.79 

    Owned free and clear          443       12.56 

 Occupied without payment of       73       2.069 
            rent                                  
--------------------------------------------------

Table: Tenure
##### Housing quality

```r
pums_kids %>%
  mutate(count = 1,
         housing_quality = factor(case_when(
      KIT == 1 & PLM == 1 ~ "full kitchen, full plumbing",
      KIT == 1 & PLM == 2 ~ "full kitchen, partial plumbing",
      KIT == 2 & PLM == 1 ~ "partial kitchen, full plumbing",
      KIT == 2 & PLM == 2 ~ "partial kitchen, partial plumbing"
    ))) %>%
  group_by(housing_quality) %>%
  summarize(households = sum(count)) %>%
  ungroup() %>%
  mutate(pct = 100*households/sum(households)) %>%
  arrange(desc(pct)) %>%
  pander(caption = "Housing quality")
```


------------------------------------------------------
        housing_quality           households    pct   
-------------------------------- ------------ --------
  full kitchen, full plumbing        3499      99.18  

 partial kitchen, full plumbing       18       0.5102 

    partial kitchen, partial          6        0.1701 
            plumbing                                  

 full kitchen, partial plumbing       5        0.1417 
------------------------------------------------------

Table: Housing quality

#### Income 
##### Housing cost burden

```r
pums_inc <- pums_kids %>%
  mutate(count = 1,
      income_cat = factor(case_when(
      HINCP < 35000 ~ "< 35k",
      HINCP >= 35000 & HINCP < 50000 ~ "35k - 50k",
      HINCP >= 50000 & HINCP < 100000 ~ "50k - 100k",
      HINCP >= 100000 ~ "> 100k"
    )))

pums_inc %>%
    mutate(rent_burden_cat = factor(case_when(
      GRPIP < 30 ~ "Low (< 30%)",
      GRPIP >= 30 & GRPIP < 50 ~ "Moderate (30-50%)",
      GRPIP >= 50 ~ "Severe (>= 50%)"
    ))) %>%
  group_by(income_cat, rent_burden_cat) %>%
  summarize(households = sum(count)) %>%
  ungroup() %>%
  group_by(income_cat) %>%
  mutate(pct = 100*households/sum(households)) %>%
  arrange(desc(income_cat), desc(rent_burden_cat)) %>%
  pander(caption = "Rent burden")
```


------------------------------------------------------
 income_cat    rent_burden_cat    households    pct   
------------ ------------------- ------------ --------
 50k - 100k    Severe (>= 50%)        12       1.247  

 50k - 100k   Moderate (30-50%)       63       6.549  

 50k - 100k      Low (< 30%)         887        92.2  

 35k - 50k     Severe (>= 50%)        12        3.39  

 35k - 50k    Moderate (30-50%)       89       25.14  

 35k - 50k       Low (< 30%)         253       71.47  

   > 100k     Moderate (30-50%)       12       0.7514 

   > 100k        Low (< 30%)         1585      99.25  

   < 35k       Severe (>= 50%)       170       27.64  

   < 35k      Moderate (30-50%)      123         20   

   < 35k         Low (< 30%)         322       52.36  
------------------------------------------------------

Table: Rent burden
##### Overcrowding

```r
pums_inc %>%
  mutate(count = 1,
    occupants_per_room = factor(case_when(
      NP/BDSP <= 2 ~ "<=2",
      NP/BDSP >= 2 & NP/BDSP < 6 ~ "3-5",
      NP/BDSP >= 6 & NP/BDSP < 9 ~ "6-9",
      NP/BDSP >= 9 ~ "9+"
    ))) %>%
  group_by(income_cat, occupants_per_room) %>%
  summarize(households = sum(count)) %>%
  ungroup() %>%
  group_by(income_cat) %>%
  mutate(pct = 100*households/sum(households)) %>%
  arrange(desc(income_cat), desc(occupants_per_room)) %>%
  pander(caption = "Occupants per room")
```


-------------------------------------------------------
 income_cat   occupants_per_room   households    pct   
------------ -------------------- ------------ --------
 50k - 100k           9+               22       2.287  

 50k - 100k          3-5               70       7.277  

 50k - 100k          <=2              870       90.44  

 35k - 50k            9+               8         2.26  

 35k - 50k           3-5               42       11.86  

 35k - 50k           <=2              304       85.88  

   > 100k             9+               8        0.5009 

   > 100k            6-9               8        0.5009 

   > 100k            3-5               97       6.074  

   > 100k            <=2              1484      92.92  

   < 35k              9+               12       1.951  

   < 35k             3-5               59       9.593  

   < 35k             <=2              544       88.46  
-------------------------------------------------------

Table: Occupants per room
##### Residence 1 year ago

```r
pums_inc %>%
  group_by(income_cat, MIG_label) %>%
  summarize(households = sum(count)) %>%
  ungroup() %>%
  group_by(income_cat) %>%
  mutate(pct = 100*households/sum(households)) %>%
  arrange(desc(income_cat), desc(MIG_label)) %>%
  pander(caption = "Moved in last year")
```


-------------------------------------------------------------------
 income_cat             MIG_label              households    pct   
------------ -------------------------------- ------------ --------
 50k - 100k    Yes, same house (nonmovers)        781       81.19  

 50k - 100k   No, outside US and Puerto Rico       11       1.143  

 50k - 100k    No, different house in US or        99       10.29  
                       Puerto Rico                                 

 50k - 100k     N/A (less than 1 year old)         71        7.38  

 35k - 50k     Yes, same house (nonmovers)        272       76.84  

 35k - 50k    No, outside US and Puerto Rico       5        1.412  

 35k - 50k     No, different house in US or        48       13.56  
                       Puerto Rico                                 

 35k - 50k      N/A (less than 1 year old)         29       8.192  

   > 100k      Yes, same house (nonmovers)        1286      80.53  

   > 100k     No, outside US and Puerto Rico       3        0.1879 

   > 100k      No, different house in US or       168       10.52  
                       Puerto Rico                                 

   > 100k       N/A (less than 1 year old)        140       8.766  

   < 35k       Yes, same house (nonmovers)        503       81.79  

   < 35k      No, outside US and Puerto Rico       4        0.6504 

   < 35k       No, different house in US or        62       10.08  
                       Puerto Rico                                 

   < 35k        N/A (less than 1 year old)         46        7.48  
-------------------------------------------------------------------

Table: Moved in last year
##### Tenure

```r
pums_inc %>%
  group_by(income_cat, TEN_label) %>%
  summarize(households = sum(count)) %>%
  ungroup() %>%
  group_by(income_cat) %>%
  mutate(pct = 100*households/sum(households)) %>%
  arrange(desc(income_cat), desc(TEN_label)) %>%
  pander(caption = "Tenure")
```


----------------------------------------------------------------
 income_cat            TEN_label            households    pct   
------------ ----------------------------- ------------ --------
 50k - 100k             Rented                 352       36.59  

 50k - 100k   Owned with mortgage or loan      456        47.4  
              (include home equity loans)                       

 50k - 100k      Owned free and clear          139       14.45  

 50k - 100k   Occupied without payment of       15       1.559  
                         rent                                   

 35k - 50k              Rented                 180       50.85  

 35k - 50k    Owned with mortgage or loan       81       22.88  
              (include home equity loans)                       

 35k - 50k       Owned free and clear           79       22.32  

 35k - 50k    Occupied without payment of       14       3.955  
                         rent                                   

   > 100k               Rented                 328       20.54  

   > 100k     Owned with mortgage or loan      1134      71.01  
              (include home equity loans)                       

   > 100k        Owned free and clear          131       8.203  

   > 100k     Occupied without payment of       4        0.2505 
                         rent                                   

   < 35k                Rented                 332       53.98  

   < 35k      Owned with mortgage or loan      149       24.23  
              (include home equity loans)                       

   < 35k         Owned free and clear           94       15.28  

   < 35k      Occupied without payment of       40       6.504  
                         rent                                   
----------------------------------------------------------------

Table: Tenure

##### Housing quality

```r
pums_inc %>%
    mutate(count = 1,
         housing_quality = factor(case_when(
      KIT == 1 & PLM == 1 ~ "full kitchen, full plumbing",
      KIT == 1 & PLM == 2 ~ "full kitchen, partial plumbing",
      KIT == 2 & PLM == 1 ~ "partial kitchen, full plumbing",
      KIT == 2 & PLM == 2 ~ "partial kitchen, partial plumbing"
    ))) %>%
  group_by(income_cat, housing_quality) %>%
  summarize(households = sum(count)) %>%
  ungroup() %>%
  group_by(income_cat) %>%
  mutate(pct = 100*households/sum(households)) %>%
  arrange(desc(income_cat), desc(housing_quality)) %>%
  pander(caption = "Housing quality")
```


-------------------------------------------------------------------
 income_cat          housing_quality           households    pct   
------------ -------------------------------- ------------ --------
 50k - 100k   partial kitchen, full plumbing       5        0.5198 

 50k - 100k   full kitchen, partial plumbing       3        0.3119 

 50k - 100k    full kitchen, full plumbing        954       99.17  

 35k - 50k    partial kitchen, full plumbing       4         1.13  

 35k - 50k    full kitchen, partial plumbing       2        0.565  

 35k - 50k     full kitchen, full plumbing        348       98.31  

   > 100k     partial kitchen, full plumbing       4        0.2505 

   > 100k      full kitchen, full plumbing        1593      99.75  

   < 35k         partial kitchen, partial          6        0.9756 
                         plumbing                                  

   < 35k      partial kitchen, full plumbing       5        0.813  

   < 35k       full kitchen, full plumbing        604       98.21  
-------------------------------------------------------------------

Table: Housing quality

#### Race/ethnicity
##### Rent burden

```r
pums_kids %>%
    mutate(rent_burden_cat = factor(case_when(
      GRPIP < 30 ~ "Low (< 30%)",
      GRPIP >= 30 & GRPIP < 50 ~ "Moderate (30-50%)",
      GRPIP >= 50 ~ "Severe (>= 50%)"
    )),
    count = 1) %>%
  group_by(HHLDRRAC1P_label, rent_burden_cat) %>%
  summarize(households = sum(count)) %>%
  ungroup() %>%
  group_by(HHLDRRAC1P_label) %>%
  mutate(pct = 100*households/sum(households)) %>%
  arrange(desc(HHLDRRAC1P_label), desc(rent_burden_cat), desc(pct)) %>%
  pander(caption = "Rent burden")
```


--------------------------------------------------------------------
     HHLDRRAC1P_label         rent_burden_cat    households    pct  
--------------------------- ------------------- ------------ -------
        White alone           Severe (>= 50%)        49       2.908 

        White alone          Moderate (30-50%)       76       4.51  

        White alone             Low (< 30%)         1560      92.58 

     Two or More Races        Severe (>= 50%)        12       7.186 

     Two or More Races       Moderate (30-50%)       16       9.581 

     Two or More Races          Low (< 30%)         139       83.23 

   Some Other Race alone      Severe (>= 50%)        28       16.09 

   Some Other Race alone     Moderate (30-50%)       45       25.86 

   Some Other Race alone        Low (< 30%)         101       58.05 

 Black or African American    Severe (>= 50%)        97       9.381 
           alone                                                    

 Black or African American   Moderate (30-50%)      129       12.48 
           alone                                                    

 Black or African American      Low (< 30%)         808       78.14 
           alone                                                    

        Asian alone           Severe (>= 50%)        8        1.709 

        Asian alone          Moderate (30-50%)       21       4.487 

        Asian alone             Low (< 30%)         439       93.8  
--------------------------------------------------------------------

Table: Rent burden
##### Overcrowding

```r
pums_kids %>%
  mutate(count = 1,
    occupants_per_room = factor(case_when(
      NP/BDSP <= 2 ~ "<=2",
      NP/BDSP >= 2 & NP/BDSP < 6 ~ "3-5",
      NP/BDSP >= 6 & NP/BDSP < 9 ~ "6-9",
      NP/BDSP >= 9 ~ "9+"
    ))) %>%
  group_by(HHLDRRAC1P_label, occupants_per_room) %>%
  summarize(households = sum(count)) %>%
  ungroup() %>%
  group_by(HHLDRRAC1P_label) %>%
  mutate(pct = 100*households/sum(households)) %>%
  arrange(desc(HHLDRRAC1P_label), desc(occupants_per_room)) %>%
  pander(caption = "Occupants per room")
```


----------------------------------------------------------------------
     HHLDRRAC1P_label        occupants_per_room   households    pct   
--------------------------- -------------------- ------------ --------
        White alone                  9+               9        0.5341 

        White alone                 6-9               8        0.4748 

        White alone                 3-5               80       4.748  

        White alone                 <=2              1588      94.24  

     Two or More Races               9+               5        2.994  

     Two or More Races              3-5               13       7.784  

     Two or More Races              <=2              149       89.22  

   Some Other Race alone             9+               4        2.299  

   Some Other Race alone            3-5               22       12.64  

   Some Other Race alone            <=2              148       85.06  

 Black or African American           9+               19       1.838  
           alone                                                      

 Black or African American          3-5               71       6.867  
           alone                                                      

 Black or African American          <=2              944        91.3  
           alone                                                      

        Asian alone                  9+               13       2.778  

        Asian alone                 3-5               82       17.52  

        Asian alone                 <=2              373        79.7  
----------------------------------------------------------------------

Table: Occupants per room
##### Residence 1 year ago

```r
pums_kids %>%
  mutate(count = 1) %>%
  group_by(HHLDRRAC1P_label, MIG_label) %>%
  summarize(households = sum(count)) %>%
  ungroup() %>%
  group_by(HHLDRRAC1P_label) %>%
  mutate(pct = 100*households/sum(households)) %>%
  arrange(desc(HHLDRRAC1P_label), desc(MIG_label)) %>%
  pander(caption = "Residence 1 year ago")
```


----------------------------------------------------------------------------------
     HHLDRRAC1P_label                  MIG_label              households    pct   
--------------------------- -------------------------------- ------------ --------
        White alone           Yes, same house (nonmovers)        1371      81.36  

        White alone          No, outside US and Puerto Rico       5        0.2967 

        White alone           No, different house in US or       161       9.555  
                                      Puerto Rico                                 

        White alone            N/A (less than 1 year old)        148       8.783  

     Two or More Races        Yes, same house (nonmovers)        137       82.04  

     Two or More Races       No, outside US and Puerto Rico       4        2.395  

     Two or More Races        No, different house in US or        18       10.78  
                                      Puerto Rico                                 

     Two or More Races         N/A (less than 1 year old)         8         4.79  

   Some Other Race alone      Yes, same house (nonmovers)        131       75.29  

   Some Other Race alone     No, outside US and Puerto Rico       4        2.299  

   Some Other Race alone      No, different house in US or        22       12.64  
                                      Puerto Rico                                 

   Some Other Race alone       N/A (less than 1 year old)         17        9.77  

 Black or African American    Yes, same house (nonmovers)        837       80.95  
           alone                                                                  

 Black or African American   No, outside US and Puerto Rico       2        0.1934 
           alone                                                                  

 Black or African American    No, different house in US or       125       12.09  
           alone                      Puerto Rico                                 

 Black or African American     N/A (less than 1 year old)         70        6.77  
           alone                                                                  

        Asian alone           Yes, same house (nonmovers)        366       78.21  

        Asian alone          No, outside US and Puerto Rico       8        1.709  

        Asian alone           No, different house in US or        51        10.9  
                                      Puerto Rico                                 

        Asian alone            N/A (less than 1 year old)         43       9.188  
----------------------------------------------------------------------------------

Table: Residence 1 year ago
##### Tenure 

```r
pums_kids %>%
  mutate(count = 1) %>%
  group_by(HHLDRRAC1P_label, MIG_label) %>%
  summarize(households = sum(count)) %>%
  ungroup() %>%
  group_by(HHLDRRAC1P_label) %>%
  mutate(pct = 100*households/sum(households)) %>%
  arrange(desc(HHLDRRAC1P_label), desc(MIG_label)) %>%
  pander(caption = "Residence 1 year ago")
```


----------------------------------------------------------------------------------
     HHLDRRAC1P_label                  MIG_label              households    pct   
--------------------------- -------------------------------- ------------ --------
        White alone           Yes, same house (nonmovers)        1371      81.36  

        White alone          No, outside US and Puerto Rico       5        0.2967 

        White alone           No, different house in US or       161       9.555  
                                      Puerto Rico                                 

        White alone            N/A (less than 1 year old)        148       8.783  

     Two or More Races        Yes, same house (nonmovers)        137       82.04  

     Two or More Races       No, outside US and Puerto Rico       4        2.395  

     Two or More Races        No, different house in US or        18       10.78  
                                      Puerto Rico                                 

     Two or More Races         N/A (less than 1 year old)         8         4.79  

   Some Other Race alone      Yes, same house (nonmovers)        131       75.29  

   Some Other Race alone     No, outside US and Puerto Rico       4        2.299  

   Some Other Race alone      No, different house in US or        22       12.64  
                                      Puerto Rico                                 

   Some Other Race alone       N/A (less than 1 year old)         17        9.77  

 Black or African American    Yes, same house (nonmovers)        837       80.95  
           alone                                                                  

 Black or African American   No, outside US and Puerto Rico       2        0.1934 
           alone                                                                  

 Black or African American    No, different house in US or       125       12.09  
           alone                      Puerto Rico                                 

 Black or African American     N/A (less than 1 year old)         70        6.77  
           alone                                                                  

        Asian alone           Yes, same house (nonmovers)        366       78.21  

        Asian alone          No, outside US and Puerto Rico       8        1.709  

        Asian alone           No, different house in US or        51        10.9  
                                      Puerto Rico                                 

        Asian alone            N/A (less than 1 year old)         43       9.188  
----------------------------------------------------------------------------------

Table: Residence 1 year ago
##### Housing quality

```r
pums_kids %>%
    mutate(count = 1,
         housing_quality = factor(case_when(
      KIT == 1 & PLM == 1 ~ "full kitchen, full plumbing",
      KIT == 1 & PLM == 2 ~ "full kitchen, partial plumbing",
      KIT == 2 & PLM == 1 ~ "partial kitchen, full plumbing",
      KIT == 2 & PLM == 2 ~ "partial kitchen, partial plumbing"
    ))) %>%
  group_by(HHLDRRAC1P_label, housing_quality) %>%
  summarize(households = sum(count)) %>%
  ungroup() %>%
  group_by(HHLDRRAC1P_label) %>%
  mutate(pct = 100*households/sum(households)) %>%
  arrange(desc(HHLDRRAC1P_label), desc(housing_quality)) %>%
  pander(caption = "Housing quality")
```


----------------------------------------------------------------------------------
     HHLDRRAC1P_label               housing_quality           households    pct   
--------------------------- -------------------------------- ------------ --------
        White alone          partial kitchen, full plumbing       6        0.3561 

        White alone           full kitchen, full plumbing        1679      99.64  

     Two or More Races        full kitchen, full plumbing        167        100   

   Some Other Race alone        partial kitchen, partial          4        2.299  
                                        plumbing                                  

   Some Other Race alone     full kitchen, partial plumbing       3        1.724  

   Some Other Race alone      full kitchen, full plumbing        167       95.98  

 Black or African American   full kitchen, partial plumbing       2        0.1934 
           alone                                                                  

 Black or African American    full kitchen, full plumbing        1032      99.81  
           alone                                                                  

        Asian alone             partial kitchen, partial          2        0.4274 
                                        plumbing                                  

        Asian alone          partial kitchen, full plumbing       12       2.564  

        Asian alone           full kitchen, full plumbing        454       97.01  
----------------------------------------------------------------------------------

Table: Housing quality

#### Sub-geography
#### Housing cost burden

```r
pums_puma <- pums_kids %>%
  mutate(region = factor(case_when(
    PUMA == 03201  ~ "Far Northeast",
    PUMA == 03202  ~ "Far Northeast - West",
    PUMA == 03203  ~ "Far Northeast - East",
    PUMA == 03204  ~ "North",
    PUMA == 03205  ~ "East",
    PUMA == 03206  ~ "Northwest",
    PUMA == 03207  ~ "Central",
    PUMA == 03208  ~ "West",
    PUMA == 03209  ~ "Center City",
    PUMA == 03210  ~ "Southwest",
    PUMA == 03211  ~ "Southeast")))

pums_puma %>%
   mutate(rent_burden_cat = factor(case_when(
      GRPIP < 30 ~ "Low (< 30%)",
      GRPIP >= 30 & GRPIP < 50 ~ "Moderate (30-50%)",
      GRPIP >= 50 ~ "Severe (>= 50%)"
    )),
    count = 1) %>%
  group_by(region, rent_burden_cat) %>%
  summarize(households = sum(count)) %>%
  ungroup() %>%
  group_by(region) %>%
  mutate(pct = 100*households/sum(households)) %>%
  arrange(desc(region), desc(rent_burden_cat), desc(pct)) %>%
  pander(caption = "Rent burden")
```


---------------------------------------------------------------
        region           rent_burden_cat    households    pct  
---------------------- ------------------- ------------ -------
         West            Severe (>= 50%)        17       7.907 

         West           Moderate (30-50%)       24       11.16 

         West              Low (< 30%)         174       80.93 

      Southwest          Severe (>= 50%)        38       13.72 

      Southwest         Moderate (30-50%)       35       12.64 

      Southwest            Low (< 30%)         204       73.65 

      Southeast          Severe (>= 50%)        3        0.838 

      Southeast         Moderate (30-50%)       25       6.983 

      Southeast            Low (< 30%)         330       92.18 

      Northwest          Severe (>= 50%)        9        2.113 

      Northwest         Moderate (30-50%)       22       5.164 

      Northwest            Low (< 30%)         395       92.72 

        North            Severe (>= 50%)        18       6.143 

        North           Moderate (30-50%)       21       7.167 

        North              Low (< 30%)         254       86.69 

 Far Northeast - West    Severe (>= 50%)        26       8.228 

 Far Northeast - West   Moderate (30-50%)       25       7.911 

 Far Northeast - West      Low (< 30%)         265       83.86 

 Far Northeast - East    Severe (>= 50%)        28       9.272 

 Far Northeast - East   Moderate (30-50%)       21       6.954 

 Far Northeast - East      Low (< 30%)         253       83.77 

    Far Northeast        Severe (>= 50%)        13       3.258 

    Far Northeast       Moderate (30-50%)       35       8.772 

    Far Northeast          Low (< 30%)         351       87.97 

         East            Severe (>= 50%)        24       7.717 

         East           Moderate (30-50%)       29       9.325 

         East              Low (< 30%)         258       82.96 

       Central           Severe (>= 50%)        11       6.587 

       Central          Moderate (30-50%)       22       13.17 

       Central             Low (< 30%)         134       80.24 

     Center City         Severe (>= 50%)        7        1.509 

     Center City        Moderate (30-50%)       28       6.034 

     Center City           Low (< 30%)         429       92.46 
---------------------------------------------------------------

Table: Rent burden
##### Overcrowding

```r
pums_puma %>%
  mutate(count = 1,
    occupants_per_room = factor(case_when(
      NP/BDSP <= 2 ~ "<=2",
      NP/BDSP >= 2 & NP/BDSP < 6 ~ "3-5",
      NP/BDSP >= 6 & NP/BDSP < 9 ~ "6-9",
      NP/BDSP >= 9 ~ "9+"
    ))) %>%
  group_by(region, occupants_per_room) %>%
  summarize(households = sum(count)) %>%
  ungroup() %>%
  group_by(region) %>%
  mutate(pct = 100*households/sum(households)) %>%
  arrange(desc(region), desc(occupants_per_room)) %>%
  pander(caption = "Occupants per room")
```


-----------------------------------------------------------------
        region          occupants_per_room   households    pct   
---------------------- -------------------- ------------ --------
         West                   9+               8        3.721  

         West                  3-5               6        2.791  

         West                  <=2              201       93.49  

      Southwest                 9+               5        1.805  

      Southwest                3-5               35       12.64  

      Southwest                <=2              237       85.56  

      Southeast                3-5               14       3.911  

      Southeast                <=2              344       96.09  

      Northwest                3-5               28       6.573  

      Northwest                <=2              398       93.43  

        North                   9+               4        1.365  

        North                  3-5               10       3.413  

        North                  <=2              279       95.22  

 Far Northeast - West           9+               17        5.38  

 Far Northeast - West          3-5               32       10.13  

 Far Northeast - West          <=2              267       84.49  

 Far Northeast - East          3-5               41       13.58  

 Far Northeast - East          <=2              261       86.42  

    Far Northeast               9+               3        0.7519 

    Far Northeast              3-5               24       6.015  

    Far Northeast              <=2              372       93.23  

         East                   9+               5        1.608  

         East                  3-5               37        11.9  

         East                  <=2              269        86.5  

       Central                  9+               8         4.79  

       Central                 3-5               15       8.982  

       Central                 <=2              144       86.23  

     Center City               6-9               8        1.724  

     Center City               3-5               26       5.603  

     Center City               <=2              430       92.67  
-----------------------------------------------------------------

Table: Occupants per room

##### Residence 1 year ago

```r
pums_puma %>%
  mutate(count = 1) %>%
  group_by(region, MIG_label) %>%
  summarize(households = sum(count)) %>%
  ungroup() %>%
  group_by(region) %>%
  mutate(pct = 100*households/sum(households)) %>%
  arrange(desc(region), desc(MIG_label)) %>%
  pander(caption = "Residence 1 year ago")
```


-----------------------------------------------------------------------------
        region                    MIG_label              households    pct   
---------------------- -------------------------------- ------------ --------
         West            Yes, same house (nonmovers)        182       84.65  

         West           No, outside US and Puerto Rico       1        0.4651 

         West            No, different house in US or        15       6.977  
                                 Puerto Rico                                 

         West             N/A (less than 1 year old)         17       7.907  

      Southwest          Yes, same house (nonmovers)        223       80.51  

      Southwest         No, outside US and Puerto Rico       6        2.166  

      Southwest          No, different house in US or        33       11.91  
                                 Puerto Rico                                 

      Southwest           N/A (less than 1 year old)         15       5.415  

      Southeast          Yes, same house (nonmovers)        296       82.68  

      Southeast          No, different house in US or        29       8.101  
                                 Puerto Rico                                 

      Southeast           N/A (less than 1 year old)         33       9.218  

      Northwest          Yes, same house (nonmovers)        340       79.81  

      Northwest         No, outside US and Puerto Rico       1        0.2347 

      Northwest          No, different house in US or        47       11.03  
                                 Puerto Rico                                 

      Northwest           N/A (less than 1 year old)         38        8.92  

        North            Yes, same house (nonmovers)        243       82.94  

        North            No, different house in US or        30       10.24  
                                 Puerto Rico                                 

        North             N/A (less than 1 year old)         20       6.826  

 Far Northeast - West    Yes, same house (nonmovers)        259       81.96  

 Far Northeast - West   No, outside US and Puerto Rico       7        2.215  

 Far Northeast - West    No, different house in US or        31        9.81  
                                 Puerto Rico                                 

 Far Northeast - West     N/A (less than 1 year old)         19       6.013  

 Far Northeast - East    Yes, same house (nonmovers)        240       79.47  

 Far Northeast - East   No, outside US and Puerto Rico       1        0.3311 

 Far Northeast - East    No, different house in US or        34       11.26  
                                 Puerto Rico                                 

 Far Northeast - East     N/A (less than 1 year old)         27        8.94  

    Far Northeast        Yes, same house (nonmovers)        340       85.21  

    Far Northeast       No, outside US and Puerto Rico       4        1.003  

    Far Northeast        No, different house in US or        35       8.772  
                                 Puerto Rico                                 

    Far Northeast         N/A (less than 1 year old)         20       5.013  

         East            Yes, same house (nonmovers)        246        79.1  

         East            No, different house in US or        31       9.968  
                                 Puerto Rico                                 

         East             N/A (less than 1 year old)         34       10.93  

       Central           Yes, same house (nonmovers)        122       73.05  

       Central          No, outside US and Puerto Rico       2        1.198  

       Central           No, different house in US or        29       17.37  
                                 Puerto Rico                                 

       Central            N/A (less than 1 year old)         14       8.383  

     Center City         Yes, same house (nonmovers)        351       75.65  

     Center City        No, outside US and Puerto Rico       1        0.2155 

     Center City         No, different house in US or        63       13.58  
                                 Puerto Rico                                 

     Center City          N/A (less than 1 year old)         49       10.56  
-----------------------------------------------------------------------------

Table: Residence 1 year ago

##### Tenure

```r
pums_puma %>%
  mutate(count = 1) %>%
  group_by(region, MIG_label) %>%
  summarize(households = sum(count)) %>%
  ungroup() %>%
  group_by(region) %>%
  mutate(pct = 100*households/sum(households)) %>%
  arrange(desc(region), desc(MIG_label)) %>%
  pander(caption = "Residence 1 year ago")
```


-----------------------------------------------------------------------------
        region                    MIG_label              households    pct   
---------------------- -------------------------------- ------------ --------
         West            Yes, same house (nonmovers)        182       84.65  

         West           No, outside US and Puerto Rico       1        0.4651 

         West            No, different house in US or        15       6.977  
                                 Puerto Rico                                 

         West             N/A (less than 1 year old)         17       7.907  

      Southwest          Yes, same house (nonmovers)        223       80.51  

      Southwest         No, outside US and Puerto Rico       6        2.166  

      Southwest          No, different house in US or        33       11.91  
                                 Puerto Rico                                 

      Southwest           N/A (less than 1 year old)         15       5.415  

      Southeast          Yes, same house (nonmovers)        296       82.68  

      Southeast          No, different house in US or        29       8.101  
                                 Puerto Rico                                 

      Southeast           N/A (less than 1 year old)         33       9.218  

      Northwest          Yes, same house (nonmovers)        340       79.81  

      Northwest         No, outside US and Puerto Rico       1        0.2347 

      Northwest          No, different house in US or        47       11.03  
                                 Puerto Rico                                 

      Northwest           N/A (less than 1 year old)         38        8.92  

        North            Yes, same house (nonmovers)        243       82.94  

        North            No, different house in US or        30       10.24  
                                 Puerto Rico                                 

        North             N/A (less than 1 year old)         20       6.826  

 Far Northeast - West    Yes, same house (nonmovers)        259       81.96  

 Far Northeast - West   No, outside US and Puerto Rico       7        2.215  

 Far Northeast - West    No, different house in US or        31        9.81  
                                 Puerto Rico                                 

 Far Northeast - West     N/A (less than 1 year old)         19       6.013  

 Far Northeast - East    Yes, same house (nonmovers)        240       79.47  

 Far Northeast - East   No, outside US and Puerto Rico       1        0.3311 

 Far Northeast - East    No, different house in US or        34       11.26  
                                 Puerto Rico                                 

 Far Northeast - East     N/A (less than 1 year old)         27        8.94  

    Far Northeast        Yes, same house (nonmovers)        340       85.21  

    Far Northeast       No, outside US and Puerto Rico       4        1.003  

    Far Northeast        No, different house in US or        35       8.772  
                                 Puerto Rico                                 

    Far Northeast         N/A (less than 1 year old)         20       5.013  

         East            Yes, same house (nonmovers)        246        79.1  

         East            No, different house in US or        31       9.968  
                                 Puerto Rico                                 

         East             N/A (less than 1 year old)         34       10.93  

       Central           Yes, same house (nonmovers)        122       73.05  

       Central          No, outside US and Puerto Rico       2        1.198  

       Central           No, different house in US or        29       17.37  
                                 Puerto Rico                                 

       Central            N/A (less than 1 year old)         14       8.383  

     Center City         Yes, same house (nonmovers)        351       75.65  

     Center City        No, outside US and Puerto Rico       1        0.2155 

     Center City         No, different house in US or        63       13.58  
                                 Puerto Rico                                 

     Center City          N/A (less than 1 year old)         49       10.56  
-----------------------------------------------------------------------------

Table: Residence 1 year ago

##### Housing quality

```r
pums_puma %>%
    mutate(count = 1,
         housing_quality = factor(case_when(
      KIT == 1 & PLM == 1 ~ "full kitchen, full plumbing",
      KIT == 1 & PLM == 2 ~ "full kitchen, partial plumbing",
      KIT == 2 & PLM == 1 ~ "partial kitchen, full plumbing",
      KIT == 2 & PLM == 2 ~ "partial kitchen, partial plumbing"
    ))) %>%
  group_by(region, housing_quality) %>%
  summarize(households = sum(count)) %>%
  ungroup() %>%
  group_by(region) %>%
  mutate(pct = 100*households/sum(households)) %>%
  arrange(desc(region), desc(housing_quality)) %>%
  pander(caption = "Housing quality")
```


-----------------------------------------------------------------------------
        region                 housing_quality           households    pct   
---------------------- -------------------------------- ------------ --------
         West           partial kitchen, full plumbing       4         1.86  

         West           full kitchen, partial plumbing       2        0.9302 

         West            full kitchen, full plumbing        209       97.21  

      Southwest            partial kitchen, partial          2        0.722  
                                   plumbing                                  

      Southwest          full kitchen, full plumbing        275       99.28  

      Southeast          full kitchen, full plumbing        358        100   

      Northwest          full kitchen, full plumbing        426        100   

        North              partial kitchen, partial          4        1.365  
                                   plumbing                                  

        North            full kitchen, full plumbing        289       98.63  

 Far Northeast - West   partial kitchen, full plumbing       3        0.9494 

 Far Northeast - West    full kitchen, full plumbing        313       99.05  

 Far Northeast - East   partial kitchen, full plumbing       9         2.98  

 Far Northeast - East   full kitchen, partial plumbing       3        0.9934 

 Far Northeast - East    full kitchen, full plumbing        290       96.03  

    Far Northeast        full kitchen, full plumbing        399        100   

         East           partial kitchen, full plumbing       2        0.6431 

         East            full kitchen, full plumbing        309       99.36  

       Central           full kitchen, full plumbing        167        100   

     Center City         full kitchen, full plumbing        464        100   
-----------------------------------------------------------------------------

Table: Housing quality

#### Tenure
##### Housing cost burden

```r
pums_kids %>%
   mutate(rent_burden_cat = factor(case_when(
      GRPIP < 30 ~ "Low (< 30%)",
      GRPIP >= 30 & GRPIP < 50 ~ "Moderate (30-50%)",
      GRPIP >= 50 ~ "Severe (>= 50%)"
    )),
    count = 1) %>%
  group_by(TEN_label, rent_burden_cat) %>%
  summarize(households = sum(count)) %>%
  ungroup() %>%
  group_by(TEN_label) %>%
  mutate(pct = 100*households/sum(households)) %>%
  arrange(desc(TEN_label), desc(rent_burden_cat), desc(pct)) %>%
  pander(caption = "Rent burden")
```


----------------------------------------------------------------------
          TEN_label             rent_burden_cat    households    pct  
----------------------------- ------------------- ------------ -------
           Rented               Severe (>= 50%)       194       16.28 

           Rented              Moderate (30-50%)      287       24.08 

           Rented                 Low (< 30%)         711       59.65 

 Owned with mortgage or loan      Low (< 30%)         1820       100  
 (include home equity loans)                                          

    Owned free and clear          Low (< 30%)         443        100  

 Occupied without payment of      Low (< 30%)          73        100  
            rent                                                      
----------------------------------------------------------------------

Table: Rent burden

##### Overcrowding

```r
pums_kids %>%
  mutate(count = 1,
    occupants_per_room = factor(case_when(
      NP/BDSP <= 2 ~ "<=2",
      NP/BDSP >= 2 & NP/BDSP < 6 ~ "3-5",
      NP/BDSP >= 6 & NP/BDSP < 9 ~ "6-9",
      NP/BDSP >= 9 ~ "9+"
    ))) %>%
  group_by(TEN_label, occupants_per_room) %>%
  summarize(households = sum(count)) %>%
  ungroup() %>%
  group_by(TEN_label) %>%
  mutate(pct = 100*households/sum(households)) %>%
  arrange(desc(TEN_label), desc(occupants_per_room)) %>%
  pander(caption = "Occupants per room")
```


------------------------------------------------------------------------
          TEN_label            occupants_per_room   households    pct   
----------------------------- -------------------- ------------ --------
           Rented                      9+               36        3.02  

           Rented                     6-9               8        0.6711 

           Rented                     3-5              167       14.01  

           Rented                     <=2              981        82.3  

 Owned with mortgage or loan           9+               8        0.4396 
 (include home equity loans)                                            

 Owned with mortgage or loan          3-5               57       3.132  
 (include home equity loans)                                            

 Owned with mortgage or loan          <=2              1755      96.43  
 (include home equity loans)                                            

    Owned free and clear              3-5               36       8.126  

    Owned free and clear              <=2              407       91.87  

 Occupied without payment of           9+               6        8.219  
            rent                                                        

 Occupied without payment of          3-5               8        10.96  
            rent                                                        

 Occupied without payment of          <=2               59       80.82  
            rent                                                        
------------------------------------------------------------------------

Table: Occupants per room

##### Residence 1 year ago

```r
pums_kids %>%
  mutate(count = 1) %>%
  group_by(TEN_label, MIG_label) %>%
  summarize(households = sum(count)) %>%
  ungroup() %>%
  group_by(TEN_label) %>%
  mutate(pct = 100*households/sum(households)) %>%
  arrange(desc(TEN_label), desc(MIG_label)) %>%
  pander(caption = "Residence 1 year ago")
```


---------------------------------------------------------------------------
          TEN_label                      MIG_label              households 
----------------------------- -------------------------------- ------------
           Rented               Yes, same house (nonmovers)        864     

           Rented              No, outside US and Puerto Rico       17     

           Rented               No, different house in US or       216     
                                        Puerto Rico                        

           Rented                N/A (less than 1 year old)         95     

 Owned with mortgage or loan    Yes, same house (nonmovers)        1531    
 (include home equity loans)                                               

 Owned with mortgage or loan   No, outside US and Puerto Rico       5      
 (include home equity loans)                                               

 Owned with mortgage or loan    No, different house in US or       134     
 (include home equity loans)            Puerto Rico                        

 Owned with mortgage or loan     N/A (less than 1 year old)        150     
 (include home equity loans)                                               

    Owned free and clear        Yes, same house (nonmovers)        392     

    Owned free and clear       No, outside US and Puerto Rico       1      

    Owned free and clear        No, different house in US or        19     
                                        Puerto Rico                        

    Owned free and clear         N/A (less than 1 year old)         31     

 Occupied without payment of    Yes, same house (nonmovers)         55     
            rent                                                           

 Occupied without payment of    No, different house in US or        8      
            rent                        Puerto Rico                        

 Occupied without payment of     N/A (less than 1 year old)         10     
            rent                                                           
---------------------------------------------------------------------------

Table: Residence 1 year ago (continued below)

 
--------
  pct   
--------
 72.48  

 1.426  

 18.12  

  7.97  

 84.12  

 0.2747 

 7.363  

 8.242  

 88.49  

 0.2257 

 4.289  

 6.998  

 75.34  

 10.96  

  13.7  
--------

##### Housing quality

```r
pums_kids %>%
    mutate(count = 1,
         housing_quality = factor(case_when(
      KIT == 1 & PLM == 1 ~ "full kitchen, full plumbing",
      KIT == 1 & PLM == 2 ~ "full kitchen, partial plumbing",
      KIT == 2 & PLM == 1 ~ "partial kitchen, full plumbing",
      KIT == 2 & PLM == 2 ~ "partial kitchen, partial plumbing"
    ))) %>%
  group_by(TEN_label, housing_quality) %>%
  summarize(households = sum(count)) %>%
  ungroup() %>%
  group_by(TEN_label) %>%
  mutate(pct = 100*households/sum(households)) %>%
  arrange(desc(TEN_label), desc(housing_quality)) %>%
  pander(caption = "Housing quality")
```


---------------------------------------------------------------------------
          TEN_label                   housing_quality           households 
----------------------------- -------------------------------- ------------
           Rented              partial kitchen, full plumbing       6      

           Rented              full kitchen, partial plumbing       5      

           Rented               full kitchen, full plumbing        1181    

 Owned with mortgage or loan   partial kitchen, full plumbing       7      
 (include home equity loans)                                               

 Owned with mortgage or loan    full kitchen, full plumbing        1813    
 (include home equity loans)                                               

    Owned free and clear       partial kitchen, full plumbing       5      

    Owned free and clear        full kitchen, full plumbing        438     

 Occupied without payment of      partial kitchen, partial          6      
            rent                          plumbing                         

 Occupied without payment of    full kitchen, full plumbing         67     
            rent                                                           
---------------------------------------------------------------------------

Table: Housing quality (continued below)

 
--------
  pct   
--------
 0.5034 

 0.4195 

 99.08  

 0.3846 

 99.62  

 1.129  

 98.87  

 8.219  

 91.78  
--------
