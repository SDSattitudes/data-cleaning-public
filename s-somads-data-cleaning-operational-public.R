#################################
# S-SOMAS Data Cleaning Wrapper #
#################################
# 13 June 2025                  #
# Eexternal distribution okay   #
#################################

# Workflow for S-SOMAS

#############################
# 1.	Read in the data file.

library("tidyverse")
library("janitor")
library("stringdist")
library("ids")
set.seed(9999) # change me

# Constants 
construct_names_string <- c("acad_sc", "attain", "expectancy",
                            "goals", "int_enj", "utility", 
                            "cost", "difficulty") # no agg_cost for DS
global_names_string <- c("g_beliefs", "global_")

### Change filename as needed for reading in new data from Qualtrics CSV files
data_from_qualtrics_filename <- "path/to/your/data/here.csv"

ssomads_raw <- read.csv(
  file = data_from_qualtrics_filename,
  header = FALSE,
  skip = 7, # change from S-SOMAS cleaning to avoid the first row being filled with values like {"ImportId":"QID139_144"}
  colClasses = "character", # The use of colClasses = "character" and type_convert() is important so that "" entries (empty strings) are correctly coded as NA values. 
  col.names = data_from_qualtrics_filename %>%
    read.csv(header = FALSE) %>%
    slice(1)) %>% 
  type_convert() %>% # Rename the items coming out of qualtrics 
  rename(Attain_1 = Attain_1,
         Attain_2 = Attain_2,
         Attain_3 = Attain_3,
         Attain_4 = Attain_7,
         Attain_5 = Attain_9,
         Attain_6 = Attain_10,
         Attain_7 = Attain_12,
         Attain_8 = Attain_16,
         AcadSC_1 = ASC_5,
         AcadSC_2 = ASC21_201M,
         AcadSC_3 = AcadSC_7M,
         AcadSC_4 = ASC_23M,
         AcadSC_5 = AcadSC21_492,
         AcadSC_6 = AcadSC_2,
         AcadSC_7 = AcadSC21_50,
         AcadSC_8 = AcadSC21_41,
         Cost_1 = Cost21_12DS,
         Cost_2 = CB_3,
         Cost_3 = CostBen_4M,
         Cost_4 = Cost21_3DS,
         Cost_5 = CostBen_5DS,
         Cost_6 = CostBen_6DS,
         Difficulty_1 = Difficulty21_11M,
         Difficulty_2 = Diff_1,
         Difficulty_3 = Difficulty21_12M,
         Difficulty_4 = Diff_8,
         Difficulty_5 = Difficulty_2DS,
         Difficulty_6 = Difficulty_3M,
         Difficulty_7 = Difficulty_4DS,
         IntEnj_1 = IE_1,
         IntEnj_2 = IE_2,
         IntEnj_3 = IE_3,
         IntEnj_4 = IntEnj_2DS,
         IntEnj_5 = IntEnj_4M,
         IntEnj_6 = IE_4,
         IntEnj_7 = IntEnj_5M,
         IntEnj_8 = IntEnj_8DS,
         IntEnj_9 = IE_7,
         IntEnj_10 = IE_8,
         Goals_1 = GO_1,
         Goals_2 = GO_4,
         Goals_3 = GO_6,
         Goals_4 = GO_9,
         Goals_5 = EGO_1DS,
         Goals_6 = EGO_5DS,
         Goals_7 = EGO21_40DS,
         Utility_1 = Utility_4DS,
         Utility_2 = UT_1,
         Utility_3 = Utility_8DS,
         Utility_4 = Utility_7M,
         Utility_5 = UT_5,
         Utility_6 = Utility_2DS,
         Utility_7 = Utility_5DS,
         Utility_8 = UT_7,
         Expectancy_1 = Expectancy_1M,
         Expectancy_2 = Expt_2,
         Expectancy_3 = Expt_6,
         Expectancy_4 = Expt_7,
         Expectancy_5 = Expt_8,
         Expectancy_6 = Expt_9,
         Expectancy_7 = Expt_10,
         Expectancy_8 = Expt_11,
         Expectancy_9 = Expt_New1,
         ) %>% 
  clean_names() %>% 
  mutate(email = str_to_lower(email),
         first = str_to_lower(first),
         last = str_to_lower(last)) %>% 
  rename(
    expected_grade = grade_expect,
    # need to deal with lang and lang_5_text
    class_level = stud_type,
    class_level_9_text = stud_type_9_text,
    gpahs_1_1_1 = high_gpa_num_1_1_1,
    gpahs_1_2_1 = high_gpa_num_1_2_1,
    gpa_college_1_1_1 = col_gpa_num_1_1_1,
    gpa_college_1_2_1 = col_gpa_num_1_2_1,
    ds_before = d_sb4,
    stat_before = sta_tb4, # closest to stat_before
    cs_before = c_sb4,
    why_take = maj_reason,
    why_take_8_text = maj_reason_7_text,
    gender_5_text = gender_3_text,
    major1_2 = major1_1_2,
    major2_2 = major2_1_2,
    minor1_2 = minor1_1_2,
    minor2_2 = minor2_1_2,
    live_meet = q1,
  )

##########################################
# 2.	Remove students who do not consent. Report the number of rows dropped. 
# 3.	Standardize email address (serves as unique identifier for students).
# a.	Currently toupper() str_to_lower() is used. Create a new variable. 
#
# There appear to be MANY (about 50% on S-SOMAS Pilot 4) respondents who click 
# into the survey and then immediately close out of it. These are characterized 
# by an "" (in the raw data) or <NA> (in the type_convert data) for Consent and 
# other items (e.g., Global_5) in the raw data. There IS QueryString information 
# for these rows, but they still did not meaningfully engage with the survey in 
# any way.
#
# The resulting file is not appropriate for sending participating student names
# to instructors for credit purposes because we have dropped students who do not
# consent but who would deserve credit in their class.
# 
# We also rename items and deal with some typos and the like here.

ssomads_init <- ssomads_raw %>% 
  drop_na(consent) %>% # Removing students who did not consent
  dplyr::filter(consent == "Yes: I agree to participate AND am 18 years or older") %>% 
  separate(email, sep = "@", into = c("email_user", "email_host"), remove = FALSE) %>% 
  mutate(
    pre_post = case_when(is.na(survey_instance) ~ NA,
                         survey_instance == 1 ~ "pre",
                         survey_instance == 2 ~ "post"),
    across(attain_1:expectancy_9, # Construct Items
           ~as.numeric(recode(., 
                              "Strongly Disagree" = 1, 
                              "Disagree" = 2,
                              "Somewhat Disagree" = 3,
                              "Neither Agree Nor Disagree" = 4,
                              "Somewhat Agree" = 5,
                              "Agree" = 6,
                              "Strongly Agree" = 7))),
    gpa_college_1_2_1 = case_when(
      gpa_college_1_2_1 == "?" ~ "4",
      gpa_college_1_2_1 == "3.63" ~ "4",
      gpa_college_1_2_1 == "3.6" ~ "4",
      TRUE ~ gpa_college_1_2_1
    ),
    gpahs_1_1_1 = as.numeric(gpahs_1_1_1),
    gpahs_1_2_1 = as.numeric(gpahs_1_2_1),
    gpa_college_1_1_1 = as.numeric(gpa_college_1_1_1),
    gpa_college_1_2_1 = as.numeric(gpa_college_1_2_1),
    percent_na = rowMeans(is.na(.)),
    percent_na_construct = 
      rowMeans(is.na(select(., attain_1:expectancy_9))),  
    # percent_na_globals = 
    #   rowMeans(is.na(select(., g_beliefs_1:global_5)))
  ) %>% 
  # mutate(across(c(gpahs_1_1_1, 
  #                 gpahs_1_2_1, 
  #                 gpa_college_1_1_1, 
  #                 gpa_college_1_2_1), 
  #               ~as.numeric)) %>% 
  # replace_na( # no NAs for the QS values
  #   list(
  #     name_first = "MISSING-first", 
  #     name_last = "MISSING-last",
  #     id = "MISSING-id",
  #     course_prefix_num = "MISSING-course_prefix",
  #     course_name = "MISSING-course_name"
  #   )) %>% # check to see if we can be better
  rowwise %>% 
  mutate(
    survey_instance = case_when(
      is.na(survey_instance) && pre_post == "pre" ~ 1,
      is.na(survey_instance) && pre_post == "post" ~ 2,
      TRUE ~ survey_instance
    )) %>% 
  ungroup

  
# Match 3) email_user (separate by whether it's a unique email_user or not)
masderID_emailuser <- ssomads_init %>%  
  distinct(institution, last, first, email, email_user) %>%
  group_by(institution, last, first, email_user) %>%
  mutate(numrep = max(n())) %>%
  filter(numrep > 1) %>%
  select(-numrep) %>% 
  ungroup %>% 
  arrange(last, first, institution, email, email_user)
masderID_emailuser %>% nrow # 160 cases have the same email_user but different emails

# Match 4) If one is gmail and the other isn't, then just match on Inst, Last, First
masderID_gmail <- ssomads_init %>% 
  select(institution, last, first, email, email_host) %>% 
  left_join(ssomads_init %>%  
              distinct(institution, last, first, email) %>%
              group_by(institution, last, first) %>%
              mutate(numrep = max(n())) %>%
              filter(numrep > 1) %>%
              select(-numrep) %>% 
              ungroup %>% 
              rename(email2 = email),
            by = c("institution", "last", "first"), relationship = "many-to-many") %>% 
  filter(email2 != email) %>% 
  mutate(studentID = case_when(
    {email_host == "gmail.com"} & {str_sub(email2, -9) == "gmail.com"} ~ email, # both are gmail
    email_host == "gmail.com" ~ email2,
    str_sub(email2, -9) == "gmail.com" ~ email,
    TRUE ~ NA)) %>% 
  drop_na(studentID) %>% 
  filter(studentID != email) %>% 
  select(institution, last, first, studentID, email) %>% 
  arrange(institution, last, first)
masderID_gmail %>% nrow  # 54 fixes

# Identify a unique studentID per respondent based on sequential matching rules
ssomads_matched <- ssomads_init %>%
  
  # Match 1) Unique emails
  left_join(ssomads_init %>% 
              select(email) %>% 
              group_by(email) %>% 
              mutate(reps = n()) %>% 
              filter(reps == 1) %>% 
              mutate(studentID = email, studentID_reason = "email unique"),
            by = "email") %>% 
  
  # Match 2) Duplicate emails
  left_join(ssomads_init %>% 
              select(email) %>% 
              group_by(email) %>% 
              mutate(reps = n()) %>% 
              filter(reps > 1) %>% 
              distinct(email) %>%
              mutate(studentID_new = email, studentID_reason_new = "email non-unique"),
            by = "email") %>% 
  mutate(studentID = ifelse(!is.na(studentID_new), studentID_new, studentID),
         studentID_reason = ifelse(!is.na(studentID_reason_new),
                                   studentID_reason_new,
                                   studentID_reason)) %>%
  select(-c(studentID_new, studentID_reason_new)) %>%
  
  # Match 3) Same email_user, inst, last, first (using masderID_emailuser from above)
  left_join(masderID_emailuser %>%
              left_join(masderID_emailuser %>%
                          distinct(last, first, institution, email_user, .keep_all = TRUE) %>%
                          rename(studentID = email),
                        by = c("institution", "last", "first", "email_user")) %>%
              filter(email != studentID) %>% 
              select(last, first, institution, email, studentID) %>% 
              rename(studentID_new = studentID) %>% 
              mutate(studentID_reason_new = "email_user + Inst/Last/First"),
            by = c("institution", "last", "first", "email")) %>% 
  mutate(studentID = ifelse(!is.na(studentID_new), studentID_new, studentID),
         studentID_reason = ifelse(!is.na(studentID_reason_new),
                                   studentID_reason_new,
                                   studentID_reason)) %>%
  select(-c(studentID_new, studentID_reason_new)) %>% 
  
  # Match 4) gmail account(s) (from masderID_gmail above)
  left_join(masderID_gmail %>%
              rename(studentID_new = studentID) %>%
              mutate(studentID_reason_new = "gmail + Inst/Last/First"),
            by = c("institution", "last", "first", "email"), relationship = "many-to-many") %>%
  mutate(studentID = ifelse(!is.na(studentID_new), studentID_new, studentID),
         studentID_reason = ifelse(!is.na(studentID_reason_new),
                                   studentID_reason_new,
                                   studentID_reason)) %>%
  select(-c(studentID_new, studentID_reason_new)) %>% 
  
  mutate(studentID_reason = factor(studentID_reason, levels = c("email unique",
                                                                "email non-unique",
                                                                "email_user + Inst/Last/First",
                                                                "gmail + Inst/Last/First",
                                                                "email_user string_dist")))



ssomads_takebest <- 
  ssomads_matched %>% 
  unique %>% 
  #### EXPECTED GRADE ####
left_join(
  ssomads_matched %>% 
    filter(pre_post == "pre") %>% 
    drop_na(expected_grade) %>% 
    group_by(studentID) %>% 
    slice_min(end_date) %>% 
    mutate(expected_grade_pre = expected_grade) %>% 
    select(studentID, pre_post, expected_grade_pre)
) %>% 
  left_join(
    ssomads_matched %>% 
      filter(pre_post == "post") %>% 
      drop_na(expected_grade) %>% 
      group_by(studentID) %>% 
      slice_max(end_date) %>% 
      mutate(expected_grade_post = expected_grade) %>% 
      select(studentID, pre_post, expected_grade_post)
  ) %>% #nrow
  mutate(expected_grade_new = 
           case_when(pre_post == "pre" ~ expected_grade_pre,
                     pre_post == "post" ~ expected_grade_post,
                     TRUE ~ "???")) %>% 
  select(-expected_grade_pre, -expected_grade_post) %>% 
  replace_na(list(expected_grade_new = "No Response")) %>% 
  #### FLUENCY (language) #### 
  left_join(
    ssomads_matched %>% 
      mutate(# Set up indicator variables for languages
        lang_english = str_detect(lang, "English"),
        lang_spanish = str_detect(lang, "Spanish"),
        lang_french = str_detect(lang, "French"),
        lang_chinese = str_detect(lang, "Chinese"),
        lang_other = str_detect(lang, "Other"),
      ) %>% 
      group_by(studentID) %>% 
      drop_na(lang) %>% 
      slice_min(end_date) %>% # we probably care most at the beginning of the semester
      select(studentID, lang_english, lang_spanish, lang_french, lang_chinese, lang_other)
  ) %>% 
  #### CLASS LEVEL ####
left_join(
  ssomads_matched %>% 
    drop_na(class_level) %>% 
    group_by(studentID) %>% 
    slice_min(end_date) %>% 
    mutate(class_level_new = class_level,
           class_level_9_text_new = class_level_9_text) %>% 
    select(studentID, class_level_new, class_level_9_text_new)
) %>% 
  replace_na(list(class_level_new = "No Response",
                  class_level_9_text_new = "No Response")) %>%
  #### SECTION ####
left_join(
  ssomads_matched %>% 
    drop_na(section) %>% 
    group_by(studentID, pre_post) %>% 
    slice_max(end_date) %>% 
    mutate(section_new = section) %>%  
    select(studentID, pre_post, section_new)
) %>% 
  #### GENDER ####
left_join(
  ssomads_matched %>% 
    drop_na(gender) %>% 
    group_by(studentID) %>% 
    filter(end_date == max(end_date)) %>% 
    rename(gender_new = gender,
           gender_5_text_new = gender_5_text) %>% 
    select(studentID, gender_new, gender_5_text_new)) %>% 
  replace_na(list(gender_new = "No Response")) %>% 
  #### RACE ####
left_join(
  ssomads_matched %>% 
    mutate(# Set up indicator variables for races
      race_white = str_detect(race, "White:"),
      race_black = str_detect(race, "Black or African-American:"),
      race_asian = str_detect(race, "Asian:"),
      race_other = str_detect(race, "Some other race or origin:"),
      race_hisp = str_detect(race, "Hispanic, Latino or Spanish origin:"),
      race_noans = str_detect(race, "Prefer not to answer"),
      race_indian = str_detect(race, "American Indian or Alaskan Native:"),
      race_nhpi = str_detect(race, "Native Hawaiian or Pacific Islander:"),
      race_mina = str_detect(race, "Middle Eastern or North African:"),
      num_race = 
        race_white + race_black + race_asian + race_other + 
        race_hisp + race_indian + race_nhpi + race_mina,
      # Define a single race variable based on indicators
      race_summary = case_when(
        is.na(race) ~ "No Response",
        num_race == 2 ~ "Biracial",
        num_race > 2 ~ "Multiracial (3+)",
        race_white ~ "White",
        race_black ~ "Black",
        race_asian ~ "Asian",
        race_other ~ "Other",
        race_hisp ~ "Hispanic",
        race_indian ~ "Other",  # Add to other due to small sample size
        race_nhpi ~ "Other", # Add to other due to small sample size
        race_mina ~ "MINA", # Sample size is larger than noans (added after Pilot 4)
        race_noans ~ "Prefer Not to Answer",
        TRUE ~ "???"),) %>%
    group_by(studentID) %>% 
    drop_na(race) %>% 
    slice_max(end_date) %>% 
    select(studentID, num_race, race_white, race_black, race_asian,
           race_other, race_hisp, race_noans, race_indian, race_nhpi,
           race_mina, race_summary)
) %>% 
  #### FIRST GEN ####
left_join(
  ssomads_matched %>% 
    select(studentID, first_gen) %>% 
    drop_na(first_gen) %>% 
    group_by(studentID) %>% 
    mutate(first_gen_new = ifelse(any(first_gen == "Yes"), "Yes", "No")) %>% 
    slice(1) %>% 
    select(studentID, first_gen_new)
) %>% 
  replace_na(list(first_gen_new = "No Response")) %>% 
  #### AGE ####
left_join(
  ssomads_matched %>% 
    filter(pre_post == "pre") %>% 
    drop_na(age) %>% 
    group_by(studentID) %>% 
    slice_min(end_date) %>% 
    mutate(age_pre = age) %>% 
    select(studentID, pre_post, age_pre)
) %>% 
  left_join(
    ssomads_matched %>% 
      filter(pre_post == "post") %>% 
      drop_na(age) %>% 
      group_by(studentID) %>% 
      slice_max(end_date) %>% 
      mutate(age_post = age) %>% 
      select(studentID, pre_post, age_post)
  ) %>% 
  mutate(age_new = 
           case_when(pre_post == "pre" ~ age_pre,
                     pre_post == "post" ~ age_post,
                     TRUE ~ NA)) %>% # should never happen because we got all of the NA pre_post dealt with now
  select(!age_pre, !age_post) %>% 
  mutate(age30 = case_when(
    age_new > 29 ~ "30+",
    age_new <= 29 ~ as.character(age_new),
    TRUE ~ "No Response",
  )) %>% 
  select(-age_pre, -age_post) %>% 
  #### GPA (HS) ####
left_join(
  ssomads_matched %>% 
    select(studentID, gpahs_1_1_1, gpahs_1_2_1, end_date) %>% 
    mutate(
      gpaHS = case_when(gpahs_1_2_1 == 100 ~ gpahs_1_1_1 * 4.0 / 100,
                        gpahs_1_1_1 > 4.0 & gpahs_1_1_1 <= 6.0 ~ 4.0,
                        gpahs_1_1_1 > 6.0 ~ NA,
                        gpahs_1_1_1 == 0 ~ NA,
                        TRUE ~ gpahs_1_1_1), 
    ) %>% 
    group_by(studentID) %>% 
    filter(end_date == max(end_date)) %>% # take the latest HS GPA to match instructor report code
    select(studentID, gpaHS)
) %>% 
  #### GPA (College) ####
left_join(
  ssomads_matched %>% 
    select(studentID, gpa_college_1_1_1, gpa_college_1_2_1, end_date) %>% 
    mutate(
      gpaCol = case_when(gpa_college_1_1_1 > 5 ~ NA,
                         gpa_college_1_1_1 > 4 & gpa_college_1_1_1 <= 5 ~ 4,
                         gpa_college_1_1_1 == 0 ~ NA,
                         TRUE ~ gpa_college_1_1_1)
    ) %>% 
    group_by(studentID) %>% 
    filter(end_date == max(end_date)) %>% # take the latest College GPA to match instructor report code
    select(studentID, gpaCol)
) %>% 
  # We think students might misinterpret the post question, so we'll use pre only where possible
  # What to do about post-only students? We believe them.
  # This whole section needs to preserve the success_* block of items
  #### WHY TAKE ####
left_join(
  ssomads_matched %>% 
    drop_na(why_take) %>% 
    group_by(studentID) %>% 
    slice_min(end_date) %>% 
    mutate(why_take_new = why_take,
           why_take_8_text_new = why_take_8_text,
           why_take_major_required = str_detect(why_take, "It is required by my major"),
           why_take_minor_required = str_detect(why_take, "It is required by my minor"),
           why_take_major_elective = str_detect(why_take, "It is an elective for my major"),
           why_take_minor_elective = str_detect(why_take, "It is an elective for my minor"),
           why_take_prep_gradprof  = str_detect(why_take, "It helps me prepare for my graduate or professional degree"),
           why_take_gen_ed         = str_detect(why_take, "If fulfills a general education requirement"),
           why_take_credits        = str_detect(why_take, "It is not part of my major or minor or general education program, but it gives me additional credits needed to graduate"),
           why_take_other          = str_detect(why_take, "Other:"),
           num_why_take_reasons    = 
             why_take_major_required + why_take_minor_required + 
             why_take_major_elective + why_take_minor_elective + 
             why_take_prep_gradprof  + why_take_gen_ed +
             why_take_credits        + why_take_other,
           why_take_summary = case_when(
             num_why_take_reasons > 1 ~ "Multiple reasons",
             why_take_major_required ~ "Required Major",
             why_take_minor_required ~ "Required Minor",
             why_take_major_elective ~ "Elective Major",
             why_take_minor_elective ~ "Elective Minor",
             why_take_prep_gradprof  ~ "Prep GradProf",
             why_take_gen_ed         ~ "Gen Ed",
             why_take_credits        ~ "Credits",
             why_take_other          ~ "Other",
             TRUE ~ "???",)
    ) %>% 
    select(studentID, why_take_new, why_take_8_text_new, why_take_major_required,
           why_take_minor_required, why_take_major_elective, 
           why_take_minor_elective, why_take_prep_gradprof, why_take_gen_ed,
           why_take_credits, why_take_other, num_why_take_reasons, 
           why_take_summary,)
) %>% 
  replace_na(list(why_take_summary = "No Response")) %>% 
  left_join(
    ssomads_matched %>% 
      mutate(percent_na_major_minor = 
               rowMeans(is.na(select(., major1_1:minor2_2)))) %>% 
      group_by(studentID) %>% 
      slice_min(percent_na_major_minor, with_ties = TRUE) %>% 
      arrange(end_date) %>%
      slice(1) %>%
      mutate(major1_1_new = major1_1, 
             major1_2_new = major1_2,
             major2_1_new = major2_1, 
             major2_2_new = major2_2, 
             minor1_1_new = minor1_1, 
             minor1_2_new = minor1_2, 
             minor2_1_new = minor2_1, 
             minor2_2_new = minor2_2) %>% 
      mutate(across(major1_1_new:minor2_2_new, ~replace_na(., "No Response"))) %>% 
      select(studentID, percent_na_major_minor, 
             major1_1_new, major1_2_new, major2_1_new, major2_2_new, 
             minor1_1_new, minor1_2_new, minor2_1_new, minor2_2_new)) %>% 
  #### prev stat ####
left_join(
  ssomads_matched %>% 
    select(studentID, pre_post, end_date, stat_before) %>% 
    drop_na(stat_before) %>% 
    group_by(studentID) %>%
    arrange(end_date) %>% 
    mutate(valid_pre = ifelse(any(pre_post == "pre"), "Yes", "No")) %>% 
    mutate(prevStat = case_when(valid_pre == "No" ~ stat_before,
                                valid_pre == "Yes" ~ ifelse(any(stat_before == "Yes" & pre_post == "pre"), "Yes", "No"),
                                pre_post == "pre" & any(stat_before == "Yes") ~ "Yes",
                                TRUE ~ "No")) %>% #,
    slice_min(end_date) %>% 
    select(studentID, prevStat) 
) %>% 
  #### prev DS ####
left_join(
  ssomads_matched %>% 
    select(studentID, pre_post, end_date, ds_before) %>% 
    drop_na(ds_before) %>% 
    group_by(studentID) %>%
    arrange(end_date) %>% 
    mutate(valid_pre = ifelse(any(pre_post == "pre"), "Yes", "No")) %>% 
    mutate(prevDataSci = case_when(valid_pre == "No" ~ ds_before,
                                   valid_pre == "Yes" ~ ifelse(any(ds_before == "Yes" & pre_post == "pre"), "Yes", "No"),
                                   pre_post == "pre" & any(ds_before == "Yes") ~ "Yes",
                                   TRUE ~ "No")) %>% #,
    slice_min(end_date) %>% 
    select(studentID, prevDataSci) 
) %>% 
  #### prev CS ####
left_join(
  ssomads_matched %>% 
    select(studentID, pre_post, end_date, cs_before) %>% 
    drop_na(cs_before) %>% 
    group_by(studentID) %>%
    arrange(end_date) %>% 
    mutate(valid_pre = ifelse(any(pre_post == "pre"), "Yes", "No")) %>% 
    mutate(prevCompSci = case_when(valid_pre == "No" ~ cs_before,
                                   valid_pre == "Yes" ~ ifelse(any(cs_before == "Yes" & pre_post == "pre"), "Yes", "No"),
                                   pre_post == "pre" & any(cs_before == "Yes") ~ "Yes",
                                   TRUE ~ "No")) %>% #,
    slice_min(end_date) %>% 
    select(studentID, prevCompSci) 
) %>% 
  #### other CS ####
left_join(
  ssomads_matched %>% 
    select(studentID, end_date, cs_other) %>% 
    drop_na(cs_other) %>% 
    mutate(cs_other_new = cs_other) %>% 
    group_by(studentID) %>%
    slice_min(end_date) %>% 
    select(studentID, cs_other_new) 
) %>% 
  replace_na(list(prevStat = "No Response",
                  prevDataSci = "No Response",
                  prevCompSci = "No Response",
                  cs_other_new = "No Response")) %>% 
  #### PREV/SUCCESS STAT ####
left_join(
  {.} %>% # we need prevStat from the previous left_join - insufficient to use the ssomads_matched dataframe
    filter(prevStat == "No") %>% 
    filter(prevStat == stat_before) %>% 
    group_by(studentID) %>% 
    slice_min(end_date) %>% 
    mutate(stat_before_new_no = stat_before,
           stat_where_new_no = stat_where,) %>% 
    select(studentID,
           stat_before_new_no, stat_where_new_no,) 
) %>% 
  left_join(
    {.} %>% # we need prevStat from the double previous left_join - insufficient to use the ssomads_matched dataframe
      filter(prevStat == "Yes") %>% 
      filter(prevStat == stat_before) %>% 
      group_by(studentID) %>% 
      slice_min(end_date) %>% 
      mutate(stat_before_new_yes = stat_before,
             stat_where_new_yes = stat_where,) %>% 
      select(studentID,
             stat_before_new_yes, stat_where_new_yes,) 
  ) %>% 
  mutate(stat_before_new = 
           case_when(prevStat == "Yes" ~ stat_before_new_yes,
                     prevStat == "No" ~ stat_before_new_no,
                     TRUE ~ "No Response"),
         stat_where_new = 
           case_when(prevStat == "Yes" ~ stat_where_new_yes,
                     prevStat == "No" ~ stat_where_new_no,
                     TRUE ~ "No Response"),
  ) %>% 
  select(-c(stat_before_new_no, stat_where_new_no, 
            stat_before_new_yes, stat_where_new_yes,)) %>% 

#### PREV/SUCCESS DS ####
left_join(
  {.} %>% # we need prevDataSci from the previous left_join - insufficient to use the ssomads_matched dataframe
    filter(prevDataSci == "No") %>% 
    filter(prevDataSci == ds_before) %>% 
    group_by(studentID) %>% 
    slice_min(end_date) %>% 
    mutate(ds_before_new_no = ds_before,
           ds_where_new_no = ds_where,) %>% 
    select(studentID,
           ds_before_new_no, ds_where_new_no,) 
) %>% 
  left_join(
    {.} %>% # we need prevDataSci from the double previous left_join - insufficient to use the ssomads_matched dataframe
      filter(prevDataSci == "Yes") %>% 
      filter(prevDataSci == ds_before) %>% 
      group_by(studentID) %>% 
      slice_min(end_date) %>% 
      mutate(ds_before_new_yes = ds_before,
             ds_where_new_yes = ds_where,) %>% 
      select(studentID,
             ds_before_new_yes, ds_where_new_yes,) 
  ) %>% 
  mutate(ds_before_new = 
           case_when(prevDataSci == "Yes" ~ ds_before_new_yes,
                     prevDataSci == "No" ~ ds_before_new_no,
                     TRUE ~ "No Response"),
         ds_where_new = 
           case_when(prevDataSci == "Yes" ~ ds_where_new_yes,
                     prevDataSci == "No" ~ ds_where_new_no,
                     TRUE ~ "No Response"),
  ) %>% 
  select(-c(ds_before_new_no, ds_where_new_no, 
            ds_before_new_yes, ds_where_new_yes,)) %>% 


#### PREV/SUCCESS CS ####
left_join(
  {.} %>% # we need prevCompSci from the previous left_join - insufficient to use the ssomacs_matched dataframe
    filter(prevCompSci == "No") %>% 
    filter(prevCompSci == cs_before) %>% 
    group_by(studentID) %>% 
    slice_min(end_date) %>% 
    mutate(cs_before_new_no = cs_before,
           cs_where_new_no = cs_where,) %>% 
    select(studentID,
           cs_before_new_no, cs_where_new_no,) 
) %>% 
  left_join(
    {.} %>% # we need prevCompSci from the double previous left_join - insufficient to use the ssomacs_matched dataframe
      filter(prevCompSci == "Yes") %>% 
      filter(prevCompSci == cs_before) %>% 
      group_by(studentID) %>% 
      slice_min(end_date) %>% 
      mutate(cs_before_new_yes = cs_before,
             cs_where_new_yes = cs_where,) %>% 
      select(studentID,
             cs_before_new_yes, cs_where_new_yes,) 
  ) %>% 
  mutate(cs_before_new = 
           case_when(prevCompSci == "Yes" ~ cs_before_new_yes,
                     prevCompSci == "No" ~ cs_before_new_no,
                     TRUE ~ "No Response"),
         cs_where_new = 
           case_when(prevCompSci == "Yes" ~ cs_where_new_yes,
                     prevCompSci == "No" ~ cs_where_new_no,
                     TRUE ~ "No Response"),
  ) %>% 
  select(-c(cs_before_new_no, cs_where_new_no, 
            cs_before_new_yes, cs_where_new_yes,)) 


ssomads_non_likert_keep <-
  ssomads_takebest %>%
  select(studentID, consent, # definitely keep
         name_first:section, # query string variables 
#         term, year,
#         name_first, name_last, institution, id, course_name, course_prefix_num, survey_instance, # QS variables, but we created section_new to account for students who changed sections
         pre_post:cs_where_new) %>% # variables we created
  select(-percent_na,
         -percent_na_construct,
         -reps,
         -studentID_reason) %>% 
  unique

#masderXdw_constructs <-
ssomads_constructs_all <-
  ssomads_takebest %>% 
  filter(percent_na_construct < 1) %>%
  select(studentID, pre_post, percent_na_construct, end_date, 
         duration_in_seconds,
         starts_with(construct_names_string)) %>% 
  group_by(studentID, pre_post) %>% 
  slice_min(percent_na_construct, with_ties = TRUE) %>% 
  unique


ssomads_constructs_1pp <- 
  ssomads_constructs_all %>% 
  filter(pre_post == "pre") %>% 
  slice_min(end_date) %>% 
  bind_rows(
    ssomads_constructs_all %>% 
      filter(pre_post == "post") %>% 
      slice_max(end_date)
  ) %>% #%>% dim #
  mutate(surveytime_seconds_constructs = duration_in_seconds) %>% 
  select(-end_date, -duration_in_seconds) %>% 
  unique

ssomads_tsr_1pp <- 
  ssomads_takebest %>% 
  select(studentID, end_date, pre_post, live_meet:tsr_community_1) %>% 
  filter(str_detect(live_meet, "Yes, this class had live meetings")) %>% 
  mutate(percent_na_tsr = 
           rowMeans(is.na(select(., tsr_q_class:tsr_community))),) %>% 
  filter(percent_na_tsr < 1) %>% 
  group_by(studentID) %>% 
  slice_min(percent_na_tsr, with_ties = TRUE) %>% 
  slice_max(end_date) %>% 
  bind_rows(
    ssomads_takebest %>% 
      select(studentID, end_date, pre_post, live_meet:tsr_community_1) %>% 
      filter(str_detect(live_meet, "No, this class did not have live meetings")) %>% 
      mutate(percent_na_tsr_1 = 
               rowMeans(is.na(select(., tsr_q_outside_1:tsr_community_1))),) %>% 
      filter(percent_na_tsr_1 < 1) %>% 
      group_by(studentID) %>% 
      slice_min(percent_na_tsr_1, with_ties = TRUE) %>% 
      slice_max(end_date)
  ) %>% 
  group_by(studentID) %>% 
  slice_max(end_date) %>%  # address anyone who has a potential duplicate
  select(-end_date) %>% 
  unique


ssomads_joined <-
  ssomads_non_likert_keep %>% 
  left_join(ssomads_constructs_1pp) %>% 
  left_join(ssomads_tsr_1pp) %>% 
  unique #%>% dim # [1] 242 145

# We now address instances of students completing the survey multiple times across different semesters...
# ... but this shouldn't matter for DS because of when we collected data. 
# We retain the code because it should work all the same and this is for future proofing.
ssomads <- 
  ssomads_joined %>% 
  group_by(studentID) %>% 
  mutate(student_rep_num = n()) %>% 
  left_join(
    ssomads_joined %>% 
      mutate(term = factor(term, levels = c("Winter", "Spring", "Summer", "Fall"))) %>% 
      select(studentID, year, term, survey_instance) %>% 
      group_by(studentID, term, year) %>% 
      mutate(term_rep_num = row_number()) %>% #ungroup %>% 
      group_by(studentID) %>% 
      arrange(studentID, year, term) %>% 
      mutate(num_terms_real = sum(term_rep_num == 1)) %>% 
      filter(term_rep_num == 1) %>% # subsets the data, so we turned this into joins
      mutate(term_num = row_number()) %>% 
      mutate(term_order = case_when(
        term_num == 1 & num_terms_real == 1 ~ "only",
        term_num == 1 ~ "first",
        term_num == num_terms_real ~ "last",
        TRUE ~ "other"
      )) %>%
      ungroup %>% 
      select(studentID, year, term, term_num, term_order)
  ) %>% 
  left_join(
    ssomads_joined %>% 
      select(studentID, year, term, pre_post, course_prefix_num, name_first, name_last, institution, id) %>% 
      group_by(studentID, year, term, course_prefix_num, name_first, name_last, institution, id) %>% 
      mutate(rep_per_qs_set = n())
  ) %>% 
  group_by(studentID, year, term) %>% 
  mutate(temp_n = n(),
         match_within_semester = ifelse(temp_n == 1, "unmatched", "matched")) %>% 
  group_by(studentID, year, term, pre_post) %>% 
  mutate(survey_response_superceded = case_when(
    match_within_semester == "matched" & rep_per_qs_set == 1 ~ "Isolated course/instructor",
    TRUE ~ "No superceding match within year-term"
  )) %>% 
  ungroup %>% 
  select(-temp_n, -term_num, -rep_per_qs_set) 

ssomads %>%
  write_csv(file = "data/confidential-identified/S-SOMADS/Spring 2024/ssomads-final-candidate-1.csv") # change me


ssomads %>% 
  # Drop students with more than 3 (i.e., 4 in the operational data) whose responses are not their first matched pair
  # This next filter works for the operational but might need to be tweaked for additional semesters (need to check for matching)
  filter(!({student_rep_num > 3} & {term_order %in% c("last", "other")})) %>% # KEEP
  # Drop the unmatched response for students who have exactly 3 responses
  filter(!({student_rep_num == 3} & 
             {match_within_semester == "unmatched"})) %>% # KEEP
  # Drop the errant match for students who have exactly 3 responses within one semester
  filter(!({student_rep_num == 3} & 
             {match_within_semester == "matched"} & 
             str_detect(survey_response_superceded, "Isolated"))) %>% # KEEP
  # Drop the latter response for students who only completed the survey once in two different semesters
  filter(!({student_rep_num == 2} & 
             {term_order == "last"})) %>% 
  group_by(studentID) %>%
  mutate(rand_id = random_id(use_openssl = FALSE)) %>% # FALSE ensures that the random IDs are generated with R's RNG so they are reproducible
  ungroup %>% 
  select(rand_id, pre_post, starts_with(construct_names_string)) %>%
  write_csv(file = "data/confidential-identified/S-SOMADS/Spring 2024/ssomads-construct-only-final-candidate-1.csv") # change me
