#Data loading
#Prepared by Qu Ying validate by zhou jifang on 2024.6.18
#version 1.0
#set up the working directory
setwd("D:/常州一院第一批次淋巴瘤数据")
#setwd("D:/常州一院第一批次淋巴瘤数据/qy6.15”)
#set up chinese language environment
Sys.setlocale(category="LC_ALL",locale="chinese")

#Load needed package
library(readxl)
library(tidyr)
library(dplyr)
library("lubridate",exclude =c("date","intersect","setdiff"))
library(lubridate)
library(data.table)
library(tidyverse)

#Import raw dataset from respective files
basic_info<-fread("患者基本信息.csv",encoding ="UTF-8")
#Determine the number of unique patients
length(unique(basic_info$cardno))
#Determine the number of organizations using organization codes/names
length(unique(basic_info$reg_organcode))
# nobs = ，npts= ，organcode=
discharge <- fread("西医病案首页-主表.csv",encoding ="UTF-8")
#nobs=,npts=，organcode=
discharge_operation <- fread("西医病案首页-手术.csv",encoding ="UTF-8")
#nobs= ,npts=
death_file<-fread("出院死亡记录表.csv",encoding ="UTF-8")
#nobs=  ,npts=
procedure<-fread("有创操作记录.csv",encoding ="UTF-8")
#nobs= ,npts=
advice_1<- fread("医嘱-1.csv",encoding ="UTF-8")
#nobs=23492713,npts=21086
advice_2<-fread("医嘱-2.csv",encoding ="UTF-8")
#nobs=26087083.npts=37141
advice_3<-fread("医嘱-3.csv",encoding ="UTF-8")
#nobs= ,npts=
advice_4<-fread("医嘱-4.csv",encoding ="UTF-8")
#nobs=
inspection_record<-fread("检查记录表.csv",encoding ="UTF-8")
lab_file_1<-fread("检验结果1.csv",encoding="UTF-8")
lab_file_2<-fread("检验结果2.csv",encoding="UTF-8")
lab_file_3<-fread("检验结果3.csv",encoding="UTF-8")
lab_file_4<-fread("检验结果4.csv",encoding="UTF-8")
lab_file_5<-fread("检验结果5.csv",encoding="UTF-8")
lab_file_6<-fread("检验结果6.csv",encoding="UTF-8")
lab_file_7_1_1<-fread("检验结果7-1-1.csv",encoding="UTF-8")
lab_file_7_1_2<-fread("检验结果7-1-2.csv",encoding="UTF-8")
lab_fi1e_7_1_3<-fread("检验结果7-1-3.csv",encoding="UTF-8")
lab_file_7_1_4<-fread("检验结果7-1-4.csv",encoding="UTF-8")
lab_file_7_1_5<-fread("检验结果7-1-5.csV",encoding="UTF-8")
lab_file_7_1_6<-fread("检验结果7-1-6.csv",encoding ="UTF-8")
lab_file_7_2<-fread("检验结果7-2.csv",encoding ="UTF-8")
lab_file_8<-fread("检验结果-8.csv",encoding ="UTF-8")
opt_file<-fread("就诊记录表.csv",encoding ="UTF-8")
#Nobs=   ,npts=   ,organcode=
prescription_file<- fread("西药处方主表.csv",encoding="UTF-8")
prescription_main<- fread("西药处方从表.csv",encoding="UTF-8")
#Nobs=
admission_file<- fread("入院记录表.csv",encoding="UTF-8")


#Eligible lymphoma case identification qy 0612
#Finding eligible lymphoma cases from ipt&opt files
#Corresponding protocol G-csF version-1
#qy 2024_6 12#validated by jifang zhou on the 0618
#Eligible lymphoma cases defined by either
#1.discharge 2.opt_file diagnoses at least 30 days apart
#extract all lymphoma from discharge
lymphoma_ipt_dx<-discharge %>%
  filter(grepl("淋巴癌|非白血病B-CLL变异|淋巴样肉芽肿病|賈样真菌病|賈样肉芽肿|T细胞增殖性疾病|血管内皮癌病|恶性免疫增生性疾病",discharge$hm_dis_name)|
           grepl("c82|c83|c84|c85|c86|c88",discharge$hm_dis_code)|
           grepl("淋巴癌|非白血病B-CLL变异|淋巴样肉芽肿病|賈样真菌病|賈样肉芽肿|T细胞增殖性疾病|血管内皮癌病|恶性免疫增生性疾病",discharge$m_dis_name_1)|
           grepl("c82|c83|c84|c85|c86|c88",discharge$m_dis_code_1)|
           grepl("淋巴癌|非白血病B-CLL变异|淋巴样肉芽肿病|賈样真菌病|賈样肉芽肿|T细胞增殖性疾病|血管内皮癌病|恶性免疫增生性疾病",discharge$m_dis_name_2)|
           grepl("c82|c83|c84|c85|c86|c88",discharge$m_dis_code_2)|
           grepl("淋巴癌|非白血病B-CLL变异|淋巴样肉芽肿病|賈样真菌病|賈样肉芽肿|T细胞增殖性疾病|血管内皮癌病|恶性免疫增生性疾病",discharge$m_dis_name_3)|
           grepl("c82|c83|c84|c85|c86|c88",discharge$m_dis_code_3)|
           grepl("淋巴癌|非白血病B-CLL变异|淋巴样肉芽肿病|賈样真菌病|賈样肉芽肿|T细胞增殖性疾病|血管内皮癌病|恶性免疫增生性疾病",discharge$m_dis_name_4)|
           grepl("c82|c83|c84|c85|c86|c88",discharge$m_dis_code_4)|
           grepl("淋巴癌|非白血病B-CLL变异|淋巴样肉芽肿病|賈样真菌病|賈样肉芽肿|T细胞增殖性疾病|血管内皮癌病|恶性免疫增生性疾病",discharge$m_dis_name_5)|
           grepl("c82|c83|c84|c85|c86|c88",discharge$m_dis_code_5)|
           grepl("淋巴癌|非白血病B-CLL变异|淋巴样肉芽肿病|賈样真菌病|賈样肉芽肿|T细胞增殖性疾病|血管内皮癌病|恶性免疫增生性疾病",discharge$m_dis_name_6)|
           grepl("c82|c83|c84|c85|c86|c88",discharge$m_dis_code_6)|
           grepl("淋巴癌|非白血病B-CLL变异|淋巴样肉芽肿病|賈样真菌病|賈样肉芽肿|T细胞增殖性疾病|血管内皮癌病|恶性免疫增生性疾病",discharge$m_dis_name_7)|
           grepl("c82|c83|c84|c85|c86|c88",discharge$m_dis_code_7)|
           grepl("淋巴癌|非白血病B-CLL变异|淋巴样肉芽肿病|賈样真菌病|賈样肉芽肿|T细胞增殖性疾病|血管内皮癌病|恶性免疫增生性疾病",discharge$m_dis_name_8)|
           grepl("c82|c83|c84|c85|c86|c88",discharge$m_dis_code_8)|
           grepl("淋巴癌|非白血病B-CLL变异|淋巴样肉芽肿病|賈样真菌病|賈样肉芽肿|T细胞增殖性疾病|血管内皮癌病|恶性免疫增生性疾病",discharge$m_dis_name_9)|
           grepl("c82|c83|c84|c85|c86|c88",discharge$m_dis_code_9))
length(unique(lymphoma_ipt_dx$cardno))
length(unique(lymphoma_ipt_dx$organ_code))
#Nobs=  ,npts=   ,organcode=
#Produce lymphoma case list with initial eligible diagnosis from discharge files
lymphoma_ipt_list<-lymphoma_ipt_dx%>%
  mutate(birthyear=year(birthday), in_hp_dt = as.Date(in_hp_dt))%>%
  group_by(cardno)%>%
  summarise_at(vars(sex,birthyear,in_hp_dt), min)
#Nobs= ,ipt_index_year from  
rm(discharge)
##extract all lymphoma from opt_file
lymphoma_opt_dx <- opt_file %>%  
  filter(grep("淋巴癌|非白血病B-CLL变异|淋巴样肉芽肿病|賈样真菌病|賈样肉芽肿|T细胞增殖性疾病|血管内皮癌病|恶性免疫增生性疾病",opt_file$hm_dis_name)|
           grepl("c82|c83|c84|c85|c86|c88",opt_file$md_dis_code)|
           grep("淋巴癌|非白血病B-CLL变异|淋巴样肉芽肿病|賈样真菌病|賈样肉芽肿|T细胞增殖性疾病|血管内皮癌病|恶性免疫增生性疾病",opt_file$dis_name_1)|
           grepl("c82|c83|c84|c85|c86|c88",opt_file$dis_code_1)|
           grep("淋巴癌|非白血病B-CLL变异|淋巴样肉芽肿病|賈样真菌病|賈样肉芽肿|T细胞增殖性疾病|血管内皮癌病|恶性免疫增生性疾病",opt_file$dis_name_2)|
           grepl("c82|c83|c84|c85|c86|c88",opt_file$dis_code_2)|
           grep("淋巴癌|非白血病B-CLL变异|淋巴样肉芽肿病|賈样真菌病|賈样肉芽肿|T细胞增殖性疾病|血管内皮癌病|恶性免疫增生性疾病",opt_file$dis_name_3)|
           grepl("c82|c83|c84|c85|c86|c88",opt_file$dis_code_3)|
           grep("淋巴癌|非白血病B-CLL变异|淋巴样肉芽肿病|賈样真菌病|賈样肉芽肿|T细胞增殖性疾病|血管内皮癌病|恶性免疫增生性疾病",opt_file$dis_name_4)|
           grepl("c82|c83|c84|c85|c86|c88",opt_file$dis_code_4)|
           grep("淋巴癌|非白血病B-CLL变异|淋巴样肉芽肿病|賈样真菌病|賈样肉芽肿|T细胞增殖性疾病|血管内皮癌病|恶性免疫增生性疾病",opt_file$dis_name_5)|
           grepl("c82|c83|c84|c85|c86|c88",opt_file$dis_code_5)|
           grep("淋巴癌|非白血病B-CLL变异|淋巴样肉芽肿病|賈样真菌病|賈样肉芽肿|T细胞增殖性疾病|血管内皮癌病|恶性免疫增生性疾病",opt_file$dis_name_6)|
           grepl("c82|c83|c84|c85|c86|c88",opt_file$dis_code_6)|
           grep("淋巴癌|非白血病B-CLL变异|淋巴样肉芽肿病|賈样真菌病|賈样肉芽肿|T细胞增殖性疾病|血管内皮癌病|恶性免疫增生性疾病",opt_file$dis_name_7)|
           grepl("c82|c83|c84|c85|c86|c88",opt_file$dis_code_7)|
           grep("淋巴癌|非白血病B-CLL变异|淋巴样肉芽肿病|賈样真菌病|賈样肉芽肿|T细胞增殖性疾病|血管内皮癌病|恶性免疫增生性疾病",opt_file$dis_name_8)|
           grepl("c82|c83|c84|c85|c86|c88",opt_file$dis_code_8))
length(unique(lymphoma_opt_dx$card_no))
length(unique(lymphoma_opt_dx$organ_code))
#Nobs=,npts= ,organcode=
rm(opt_file)
#Produce lymphoma case list with initial eligible diagnosis from opt_files
lymphoma_opt_list<-lymphoma_opt_dx%>%
  mutate(opt_dt=as.Date(see_doc_dt))%>%
  rename(cardno=card_no)%>%
  group_by(cardno)%>%
  summarise_at(vars(opt_dt),c(min.max))
#Nobs=  ,ipt_index_year form 

#OPT minimal and maximal dates to be >=30days apart
lymphoma_opt_list$opt_dx_lag<-as.numeric(difftime(lymphoma_opt_list$fn2,lymphoma_opt_list$fn1,units = "days"))
#Nobs= 
#Restrict to 30 days aprt
lymphoma_opt_list<-lymphoma_opt_list[lymphoma_opt_list$opt_dx_1ag>=30,]
#Nobs=
#combine diagnosislists from opt and ipt, then get the unique patient respective dates
id_list<-unique(rbind(lymphoma_ipt_list[,1],lymphoma_opt_list[,1]))
#Nobs=

#Merge demographic information from ipt_list
id_list_ipt_info<-id_list%>%
left_join(lymphoma_ipt_list,by="cardno")

#Merge demographic information from ipt_list
lymphoma_opt_list<-lymphoma_opt_list[,c(1,2)]%>%
rename(opt_index_date=fn1)
id_list_ipt_opt_info<-id_list_ipt_info%>%
  left_join(lymphoma_opt_list,by="cardno")
#Add supplemental information on sex and birth yearfrom basic info
head(basic_info)
lymphoma_basic_demo<-basic_info%>%
  mutate(birthyear =year(birthday))%%
  group_by(cardno)%>%
  summarise_at(vars(sex,birthyear),min)%>%
  rename(sex_basic_info=sex)%>%
  rename(birthyear_basic_info=birthyear)
#Nobs=
#Merge back basic demographic info to patient id list
id_list_ipt_opt_basic_info<-id_list_ipt_opt_info%>%
  left_join(lymphoma_basic_demo,by="cardno")

#Convert sex info from basic demo files
id_list_ipt_opt_basic_info$sex_basic_info[id_list_ipt_opt_basic_info$sex_basic_info=="男"]<-1
id_list_ipt_opt_basic_info$sex_basic_info[id_list_ipt_opt_basic_info$sex_basic_info=="女"]<-2
#Impute missing sex and birth year from ipt files using basic information
id_list_ipt_opt_basic_info$sex<-if_else(grepl("1|2",id_list_ipt_opt_basic_info$sex),
                                        id_list_ipt_opt_basic_info$sex,id_list_ipt_opt_basic_info$sex_basic_info)
id_list_ipt_opt_basic_info$birthyear<-if_else(id_list_ipt_opt_basic_info$birthyear<=1924|
                                                id_list_ipt_opt_basic_info$birthyear>=2006,
                                              id_list_ipt_opt_basic_info$birthyear_basic_info,
                                              id_list_ipt_opt_basic_info$birthyear)
id_list_demo_info<-id_list_ipt_opt_basic_info[,c("cardno","sex","birthyear")]
#Extarct index admission date for lymphoma to id list with demo information
lymphoma_ipt_list_need<-lymphoma_ipt_list[,c("cardno","in_hp_dt")]
id_list_demo_ipt_index<-id_list_demo_info%>%
  left_join(lymphoma_ipt_list_need,by="cardno")
#Extarct index opt date for lymphoma to id list with demo information
id_list_demo_ipt_opt_index<-id_list_demo_ipt_index%>%
  left_join(lymphoma_opt_list,by="cardno")
#Determine the earliest encounter date for lymphoma as index diagnosis date
id_list_demo_ipt_opt_index$index_dx_date<-pmin(id_list_demo_ipt_opt_index$in_hp_dt,
                                               id_list_demo_ipt_opt_index$opt_index_date,
                                               na.rm = TRUE)
table(is.na(id_list_demo_ipt_opt_index$birthyear))
#save eligible lyphoma lists
write.csv(id_list_demo_ipt_opt_index,file ="lymphoma_dx_list_1z0618.csv",row.nameS=FALSE)











  

