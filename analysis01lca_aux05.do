*created on 20240312
*stream line the analysis
*lca + targeting & victimization 
*copy &past from aux04 to strealine the code 
*net install speccurve, from("https://raw.githubusercontent.com/martin-andresen/speccurve/master")
*updated on 20240507: 1st RR 
*lca results are from lca_only.do

*cd "C:\Users\Donghui\SynologyDrive\抖音项目\dofie"   //desktop
*cd "D:\SynologyDrive\抖音项目\dofie"  // office 
*cd "/Users/priscillawang/SynologyDrive/抖音项目/dofie"  //mac 
cd "C:\Users\donghuiwang\SynologyDrive\抖音项目\dofie"  // um desktop 
*----------------------
/*internet access
import excel "C:\Users\Donghui\SynologyDrive\抖音项目\data\地级市-互联网普及率（2011-2022年）\地级市-互联网普及率.xlsx", sheet("原始数据") firstrow clear 
 rename (年份 行政区划代码 每百人互联网宽带用户_常住人口口径 地区 ) (year gbcode access prefecture)
 sort gbcode year 
 keep if year==2015
 keep gbcode year access prefecture 
save access, replace 
*/

*use datause-jin, clear 
*use lca5, clear 
use lca4_actonly, clear 
/*110000 
destring 统计用区划代码,replace 
gen gbcode=int(统计用区划代码/1000000)  //62

merge m:1  gbcode using access 
*/
*SES, Demographic 

*1 年龄 age
gen age = int(2022-A2__1__open+(12-A2__2__open)/12)
tab age
gen ageg = 1
replace ageg = 2 if age > 55 & age <61
replace ageg = 3 if age > 60 & age <66
replace ageg = 4 if age > 65 & age <71
replace ageg = 5 if age > 70 & age <76
replace ageg = 6 if age > 75
codebook ageg
tab ageg,gen(ageg)
la var ageg1 "55 or younger"
la var ageg2 "56~60"
la var ageg3 "61~65"
la var ageg4 "66~70"
la var ageg5 "71~75"
la var ageg6 "76 or older"

g agesq=age*age 

*another way to categorize

recode age (50/59 =1) (60/69 =2 ) (70/79 =3 ) (80/max =4), gen(agecat)




*2 性别 sex
gen female =(A1==2)

*3 地域 area
tab province
recode province (1 2 6 7 9 11 15 = 4) (3 8 10 12 13 14 = 2) (16/18 = 1) (4 5 = 3),gen(region)
label define region 1"West" 2"Middle" 3"Northeast" 4"east", modify 
label values region region 
codebook region

tab region, gen(region)
la var region2 "Middle"
la var region3 "Northeast"
la var region4 "East"



*4 教育 edu
codebook A3
gen edu = 1
replace edu = 2 if A3==4
replace edu = 3 if A3==5 | A3 == 6
replace edu = 4 if A3>6
label define edu 1"primary and below " 2"middle" 3"high school" 4"college and above" , modify 
label values edu edu
codebook edu
tab edu, gen(edu)

la var edu2 "Middle school"
la var edu3 "High school"
la var edu4 "College"

g hsabove= (A3 >4)
lab var hsabove "High school and above"


g midabove = (A3 >3)
lab var midabove "Middle school and above"


*5 婚姻 mar
codebook A5
gen mar = 1
replace mar = 0 if A5 >3
label define mar 0"不在婚" 1"在婚" //搭伴居住处理为在婚（事实婚姻）
label values mar mar
codebook mar

g married = (A5 <4)

*living alone
*drop alone 
g alone=(A6__1==1)
la var alone "living alone"


*6 户口 hukou /rural living 
codebook A8
gen hukou = 0
replace hukou = 1 if A8 == 1 | A8 == 3
label define hukou 0"非农业户口" 1"农业户口"
label values hukou hukou
codebook hukou

g ruralhukou=(A8 ==1 | A8==3)
la var ruralhukou "rural hukou"

* if living in rural area 
gen ruralliving=(Q7==2)

*8 就业 work
codebook H1
recode H1 (1 4 = 0)(2 3 = 1),gen(haswork)
label define haswork 0"目前没有工作" 1"目前有工作"
label values haswork haswork
codebook haswork
la var haswork "has work"

*9 个人收入 income
codebook I2__1__open, detail
clonevar income = I2__1__open
la var income "income"
tab income
gen inc = log(income*10000+1)
tab inc
lab var inc "income (log transformed)"

bysort province: egen rankincome=rank(I2__1__open), track

*income by category 
recode I2__1__open (0/1=1 "10k and below") (1.1/5 =2 "10k-50k") (5.1/10=3 "50k-100k") (10.2/max=4 "above 100k"), gen(incomecat)



*addtional housing 
gen otherhousing=(I13==1)
la var otherhousing "has additional housing"

codebook D9
gen d9 = 1
replace d9 = 0 if D9 == 2
codebook d9

gen informed =(D9==1)

*sources of information 
egen infor_family = anymatch(D10__1 D10__2 D10__3 D10__4 D10__5) , values(1)
egen infor_gov=anymatch(D10__6 D10__7) , values(1)
egen infor_own=anymatch(D10__8 D10__9 D10__10), values(1)

*occupation
*retired: last job occupation Ha3

g       occu=.
replace occu =Ha_3 if H1 == 1 
replace occu =Hb_3 if H1 == 2
replace occu =Hc_3 if H1 ==3
replace occu =8  if H1== 4  //never worked collapse with 8
recode occu (7/8=7)

lab def occu 1 "Agricultural"  2"Managerial"  3"Professional" 4 "Clerk" 5 "Service" 6 "Manufacture" 7 "Others" , modify 
lab values occu occu 

tab occu, gen(occup)
la var occup1 "Agricultural"
la var occup2 "Managerial"
la var occup3 "Professional"
la var occup4 "Clerk"
la var occup5 "Service"
la var occup6 "Manufacture"
la var occup7 "Others"

g manaprof=(occup2==1 |occup3==1)
la var manaprof "Managerial or Professional"


g manaprofck=(occup2==1 |occup3==1| occup4 ==1)
la var manaprofck "Managerial,Professional, or clerk"

*subjective ses rank 

g middleabv=(G7<=3)
la var middleabv "subjective social status : middle and above"

*---------------------------------
*health 
*self-rated health 
codebook A9
recode A9 (1=5) (2=4) (3=3) (4=2) (5=1),gen(healthcondition)
codebook healthcondition
gen goodhealh = (A9 ==1 | A9 ==2)

*daily activity Living 
egen dal= anycount(A10_1-A10_7), val(2 3)
la var dal "daily activity living limiations "

*mental health 
codebook F1__1-F1__10
*g F1__5new=5-F1__5
*g F1__8new=5-F1__8

alpha F1__1-F1__10, reverse(F1__5 F1__8) std item gen(mental)  // alpha 0.6897
*alpha F1__1-F1__10, reverse(F1__5 F1__8)  item gen(mentalnostd)
*alpha F1__1 F1__2 F1__3 F1__4 F1__5new F1__6 F1__7 F1__8new F1__9 F1__10,  item std gen(mental3) //
*twoway scatter (mental mentalnostd) (mental mental3) 
la var mental "bad mental health conditions"

*competence 
codebook F2__1-F2__10
alpha F2__1-F2__10, reverse(F2__4 F2__5 F2__7 F2__8) std item gen(competence) // 0.6103
la var competence "no competence"

*addiction 
codebook F5__1-F5__10
alpha F5__1-F5__10, std item gen(noaddiction) // 0.7997

*----------------------------------------------------
*online fraud type 

codebook D5  //诈骗
gen fraud = 1
replace fraud = 0 if D5>3
tabulate fraud  //诈骗

egen loss=anycount(D7_*), values(1)
recode loss (0=0)(1/5=1)
la var loss "loss to any frauds"  // 129
tab loss if fraud ==1  //6.63


/*
tab _Best_Index, gen(type)
la var type1 "Entertainment users"
la var type2 "Risky users"
la var type3 "Beginners"
la var type4 "Skilled users"
la var type5 "Instrumental users"

la def type  1  "Entertainment users"  2 "Risky users" 3 "Beginners" 4 "Skilled users" 5 "Instrumental users", modify 
la val _Best_Index type 
 */
 
 
/*risky behaviors 
*风险行为  1 = risky behavior , 0 not 

recode  D8__2 D8__3 D8__4 D8__5 D8__6 D8__8 (1/2=1) (3/4=0), gen(password QR verification wifi identity download)

gen link=(D1_1<3)
la var link "打开可疑链接"

la var password "在所有网站使用同一密码"
la var QR "扫来源不明的二维码"
la var verification "告知他人验证码"
la var wifi "使用公共网络进行交易"
la var identity "在网上告知自己真实信息"
la var download "下载来源可疑的应用程序"

*/

*quick look at descriptives 
table1, vars(income conts \ edu cat \ occu cat \ female bin \ haswork bin \ age conts \ ruralhukou bin \ married bin \ alone bin \ goodhealh bin\ region cat)


#delimit ;
table1, vars(QRcode cat \ information cat \ shopping cat \ food cat \ payment cat \ car cat \ ticket cat \ health cat \ investment cat \
             messaging cat \ audio cat \ read cat \ vedio cat \ game cat \ study cat\ work cat \ read_entertainment cat \ read_news cat \ read_learn cat \ 
			  link cat \ password cat \ QR cat \ verification cat\ identity cat\  wifi cat \ download cat) saving(activity.xls, replace)
			  ;
			  
#delimit cr



*================================


*regression analysis 





*stability of the predictor 

*1. Demographic, SES to fraud 
*global ctrl " ageg2 ageg3 ageg4 ageg5 ageg6   female   edu2 edu3 edu4  income  occup2 occup3 occup4 occup5 occup6 occup7"
*global ctrl "ageg2 ageg3 ageg4 ageg5 ageg6   female hsabove income manaprof ruralhukou married alone   area2 area3 area4"
*global ctrl "ageg2 ageg3 ageg4 ageg5 ageg6 midabove inc female manaprof ruralhukou married alone region2 region3 region4"


*global ctrl " ageg2 ageg3 ageg4 ageg5 ageg6   female   edu2 edu3 edu4  income  occup2 occup3 occup4 occup5 occup6 occup7  married alone ruralhukou  area2 area3 area4 "
*global ctrl "ageg2 ageg3 ageg4 ageg5 ageg6   female   married alone ruralhukou  area2 area3 area4"
global ctrl "income   i.edu  i.occu female  haswork age agesq   ruralhukou married alone goodhealh   region2 region3 region4"
global type "type1 type2  type4 type5"


*null  effect  
logit fraud income  i.edu  i.occu
estimates store m0_fraud

logit loss  income i.edu  i.occu if fraud==1 
estimates store m0_loss

*do the same by adding controls

logit  fraud   $ctrl ,coeflegend
estimates store m1_fraud


logit   loss  $ctrl   if fraud ==1 
estimates store m1_loss

coefplot m1_fraud m1_loss, keep(income *.edu *.occu ) 
esttab m0_fraud   m0_loss m1_fraud m1_loss using "m01.rtf" , se(%9.2f)  noomitted la replace 


*alternative specification 
global ctrl "income   ib(last).edu  ib(2).occu female  haswork age agesq   ruralhukou married alone goodhealh   region2 region3 region4"

logit  fraud   $ctrl 
estimates store aux_m1_fraud

logit   loss  $ctrl   if fraud ==1 
estimates store aux_m1_loss


esttab   aux_m1_fraud  aux_m1_loss using "auxm01.rtf" , se(%9.2f)  noomitted la replace 






*2. activity 

/*tab plot of the results 

*ssc install tabplot

label define fraud 0 "Not targeted" 1"Targeted"
label values fraud fraud

label define loss 0 "Do not have monetary loss" 1"Monetary loss"
label values loss loss

tabplot fraud _Best_Index , percent(fraud) showval  
tabplot loss _Best_Index if fraud==1, percent(loss) showval  


*ssc install catplot
ssc install spineplot 

spineplot fraud _Best_Index , percent 
spineplot loss _Best_Index if fraud==1
*/
logit fraud ib(3)._Best_Index $ctrl  
estimates store m2_fraud 

*average marginal effects
 
margins, dydx(i._Best_Index ) post 
estimate store fraud_ame 



logit loss ib(3)._Best_Index  $ctrl  if fraud ==1 
estimates store m2_loss
margins , dydx(i._Best_Index ) post 
estimate store loss_ame 

esttab m2_fraud m2_loss using "m2.rtf" , se(%9.2f) noomitted la replace //okay 

esttab fraud_ame loss_ame using  "m2_ame.rtf", b(%9.2f) se(%9.2f) la replace 


/*additional analysis 
global act "messaging audio  vedio game study read_entertainment  read_news read_learn  link password QR verification  wifi download"

logit fraud $act $ctrl
estimates store m2_fraud_aux 


logit loss $act $ctrl if fraud==1 
estimates store m2_loss_aux 

esttab m2_fraud_aux m2_loss_aux using "m2_aux.rtf" , eform ci(%9.2f) noomitted la replace //okay 

*/
*multinomial predict class membership 

mlogit _Best_Index $ctrl , base(3)
mlogit _Best_Index $ctrl , base(4)
mlogit _Best_Index $ctrl , base(1)
