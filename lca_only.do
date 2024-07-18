*analysis lca 
*version coplied from analysis01lca_aux01
*created on 20240227
*task : redo the lca  anaysis 


cd "C:\Users\Donghui\SynologyDrive\抖音项目\dofie"  // put 上网总数据.dta to the same folder as do file to call stata plugin

use "上网总数据.dta" , clear

**----lca----**
*①1=是 2=否

codebook B10__1-B10__10

gen QRcode = 1
replace QRcode = 2 if B10__1 == 0
la var QRcode "scan QR code"

gen information = 1
replace information = 2 if B10__2 == 0
la var information "search information"

gen shopping = 1
replace shopping = 2 if B10__3 == 0
la var shopping "online shopping"

gen food = 1
replace food = 2 if B10__4 == 0
la var food "order food"

gen payment = 1
replace payment = 2 if B10__5 == 0
la var payment "online payment"

gen car = 1
replace car = 2 if B10__6 == 0
la var car "call taxi"


gen ticket = 1
replace ticket = 2 if B10__7 == 0
la var ticket "purchase ticket"


gen health = 1
replace health = 2 if B10__8== 0
la var health "health managment"

gen investment = 1
replace investment = 2 if B10__9== 0
la var investment "online inverstment "

*more than 30 mins on average 
gen messaging = 1
replace messaging = 2 if B11__1 <2 
la var messaging "messaging"

gen audio = 1
replace audio = 2 if B11__2<2
la var audio "listen to music/podcast"

gen read_entertainment = 1
replace read_entertainment = 2 if B11__3 <2

gen read_news = 1
replace read_news = 2 if B11__4 <2

gen read_learn = 1
replace read_learn = 2 if B11__5<2


gen vedio = 1
replace vedio = 2 if B11__6 <2

gen game = 1
replace game = 2 if B11__7 <2


gen study = 1
replace study = 2 if B11__8 <2


gen work = 1
replace work = 2 if B11__9 <2

*文章阅读 :if ever spend more than 30 mins on any of the reading activities 
g read = 1 if read_entertainment ==1 | read_news ==1 | read_learn ==1 
replace read =2 if read ==. 

 
*risky behaviors 

*风险行为  1 = risky behavior , 2 not 

gen     link= 1 if D1_1 <=2 
replace link= 2 if D1_1 >2
la var link "open unsolicited link"


codebook D8__2
gen     password = 1
replace password = 2 if D8__2>2
la var password "use the same password for every site"

codebook D8__3
gen     QR = 1
replace QR = 2 if D8__3>2
la var QR "open unsolicited QR code"

gen     verification =1 
replace verification = 2 if D8__4 >2 
la var verification "tell others verification code"

codebook D8__5
gen wifi = 1
replace wifi = 2 if D8__5>2
la var wifi "use public wifi for transaction"

gen identity= 1 
replace identity= 2 if D8__6>2 
la var identity "reveal true identity to online strangers"

gen download = 1
replace download = 2 if D8__8>2
la var download "Download applications from doubious sites"


*quick look at descriptives 
#delimit ;
table1, vars(QRcode cat \ information cat \ shopping cat \ food cat \ payment cat \ car cat \ ticket cat \ health cat \ investment cat \
             messaging cat \ audio cat \ read cat \ vedio cat \ game cat \ study cat\ work cat \ read_entertainment cat \ read_news cat \ read_learn cat \ 
			  link cat \ password cat \ QR cat \ verification cat\ identity cat\  wifi cat \ download cat) ;
			  
#delimit cr
*****************end of variable generation********************************* 

*lca 

* n class :2- 20
forvalues i=2/20 {
display "===> `i' class"
discard
doLCA QRcode information shopping food payment car  ticket health investment messaging audio  vedio game study read_entertainment  read_news read_learn      link password QR verification  wifi download , 	///
      nclass(`i') 			///
	  seed(100000) 			///
	  seeddraws(100000) 	///
	  categories(2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)  ///
	  criterion(0.000001)  ///
	  rhoprior(1.0)
	  	  
scalar entropy_`i'=r(EntropyRsqd)
scalar gsq_`i'=r(Gsquared)
scalar aic_`i'=r(aic)
scalar bic_`i'=r(bic)
scalar adjBIC_`i'=r(AdjustedBIC) 
*scalar ll_`i'=r(loglikelihood)
}



* create matrix to store model fit statistics 
numlist "2/20"
local stats "Gsquared AIC BIC adjBIC entropy"
local n :word count `r(numlist)'
mat stats=J(`n',5,-99)
mat colnames stats=`stats'
mat rownames stats=`r(numlist)'
mat list stats


forvalues i=2/20{
mat stats[`i'-1,1]= gsq_`i'
mat stats[`i'-1,2]= aic_`i'
mat stats[`i'-1,3]= bic_`i'
mat stats[`i'-1,4]= adjBIC_`i'
mat stats[`i'-1,5]= entropy_`i'
}

mat list stats, format (%9.3f)


svmat stats, names (col)
gen nclass=_n+1 if AIC !=.

 set scheme cleanplots 
   twoway (connected Gsquared nclass, lp(dash_dot) m(oh))    ///
          (connected AIC nclass, lp(dash_dot_dot) m(oh))     ///
             (connected BIC nclass, lp(solid))               ///
             (connected adjBIC nclass, lp(dash) m(0)) ,      ///
			 xlab(2(1)20) xtitle(Number of Latent Classes)    ///
			 title("Fit Statistics") 
graph export fit.png, as(png) replace 

*final : 5 class for now 

discard 
doLCA QRcode information shopping food payment car  ticket health investment messaging audio  vedio game study read_entertainment  read_news read_learn  link password QR verification  wifi download , 	///
      nclass(4) 			///
	  seed(100000) 			///
	  seeddraws(100000) 	///
	  categories(2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)  ///
	  criterion(0.000001)  ///
	  rhoprior(1.0)

return list 
mat C=r(rho)
mat gamma = r(gamma)
mat list gamma ,format(%9.3f) 


*------------------
*selected numbers of variables 
*information shopping payment health inverstment messaging read_entertainment read_news vedio game link password QR verification wifi identity download

*lca 

* n class :2- 15
forvalues i=2/15 {
display "===> `i' class"
discard
doLCA  information shopping  payment  investment messaging read_entertainment read_news  vedio game    link password QR verification  wifi identity download , 	///
      nclass(`i') 			///
	  seed(100000) 			///
	  seeddraws(100000) 	///
	  categories(2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 )  ///
	  criterion(0.000001)  ///
	  rhoprior(1.0)
	  	  
scalar entropy_`i'=r(EntropyRsqd)
scalar gsq_`i'=r(Gsquared)
scalar aic_`i'=r(aic)
scalar bic_`i'=r(bic)
scalar adjBIC_`i'=r(AdjustedBIC) 
*scalar ll_`i'=r(loglikelihood)
}



* create matrix to store model fit statistics 
numlist "2/15"
local stats "Gsquared AIC BIC adjBIC entropy"
local n :word count `r(numlist)'
mat stats=J(`n',5,-99)
mat colnames stats=`stats'
mat rownames stats=`r(numlist)'
mat list stats


forvalues i=2/15{
mat stats[`i'-1,1]= gsq_`i'
mat stats[`i'-1,2]= aic_`i'
mat stats[`i'-1,3]= bic_`i'
mat stats[`i'-1,4]= adjBIC_`i'
mat stats[`i'-1,5]= entropy_`i'
}

mat list stats, format (%9.3f)


svmat stats, names (col)
gen nclass=_n+1 if AIC !=.

 set scheme cleanplots 
   twoway (connected Gsquared nclass, lp(dash_dot) m(oh))    ///
          (connected AIC nclass, lp(dash_dot_dot) m(oh))     ///
             (connected BIC nclass, lp(solid))               ///
             (connected adjBIC nclass, lp(dash) m(0)) ,      ///
			 xlab(2(1)15) xtitle(Number of Latent Classes)    ///
			 title("Fit Statistics") 
graph export fit.png, as(png) replace 

*four classes 
discard 
doLCA information shopping  payment  investment messaging read_entertainment read_news  vedio game    link password QR verification  wifi identity download , 	///
      nclass(4) 			///
	  seed(100000) 			///
	  seeddraws(100000) 	///
	  categories(2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)  ///
	  criterion(0.000001)  ///
	  rhoprior(1.0)

return list 
mat C=r(rho)
mat gamma = r(gamma)
mat list gamma ,format(%9.3f) 

tab _Best_Index, gen(type)
la var type1 "Risky users"
la var type2 "Beginners"
la var type3 "Instrumental users"
la var type4 "Skilled users"


la define index  1 "Risky users" 2 "Beginners" 3 "Instrumental users" 4 "Skilled users" , modify 

la val _Best_Index index

save lca4, replace 


*================================================================
*version 3: use only online actvities and risky online activities :14 activities 

* n class :2- 20
forvalues i=2/10 {
display "===> `i' class"
discard
doLCA messaging audio  vedio game study read_entertainment  read_news read_learn      link password QR verification  wifi download , 	///
      nclass(`i') 			///
	  seed(100000) 			///
	  seeddraws(100000) 	///
	  categories(2 2 2 2 2 2 2 2 2 2 2 2 2 2)  ///
	  criterion(0.000001)  ///
	  rhoprior(1.0)
	  	  
scalar entropy_`i'=r(EntropyRsqd)
scalar gsq_`i'=r(Gsquared)
scalar aic_`i'=r(aic)
scalar bic_`i'=r(bic)
scalar adjBIC_`i'=r(AdjustedBIC) 
*scalar ll_`i'=r(loglikelihood)
}



* create matrix to store model fit statistics 
numlist "2/10"
local stats "Gsquared AIC BIC adjBIC entropy"
local n :word count `r(numlist)'
mat stats=J(`n',5,-99)
mat colnames stats=`stats'
mat rownames stats=`r(numlist)'
mat list stats


forvalues i=2/10{
mat stats[`i'-1,1]= gsq_`i'
mat stats[`i'-1,2]= aic_`i'
mat stats[`i'-1,3]= bic_`i'
mat stats[`i'-1,4]= adjBIC_`i'
mat stats[`i'-1,5]= entropy_`i'
}

mat list stats, format (%9.3f)


svmat stats, names (col)
gen nclass=_n+1 if AIC !=.

 set scheme cleanplots 
   twoway (connected Gsquared nclass, lp(dash_dot) m(oh))    ///
          (connected AIC nclass, lp(dash_dot_dot) m(oh))     ///
             (connected BIC nclass, lp(solid))               ///
             (connected adjBIC nclass, lp(dash) m(0)) ,      ///
			 xlab(2(1)10) xtitle(Number of Latent Classes)    ///
			 title("Fit Statistics") 

graph export fit_lca4.png, as(png) replace 
			 
*-------------------------------------			 
*four class 
discard
doLCA messaging audio  vedio game study read_entertainment  read_news read_learn  link password QR verification  wifi download , 	///
      nclass(4) 			///
	  seed(100000) 			///
	  seeddraws(100000) 	///
	  categories(2 2 2 2 2 2 2 2 2 2 2 2 2 2)  ///
	  criterion(0.000001)  ///
	  rhoprior(1.0)

return list 
mat C=r(rho)
mat gamma = r(gamma)
mat list gamma ,format(%9.3f) 



tab _Best_Index, gen(type)
la var type1 "Risky users"
la var type2 "Entertainment users"
la var type3 "Basic users"
la var type4 "Advanced users"


la define index  1 "Risky users" 2 "Entertainment users" 3 "Basic users" 4 "Advanced users" , modify 

la val _Best_Index index



*heatmap for item response probability 
mat list C 
mat rho = C[1..14, 1..4]
mat list rho 


ssc install heatplot
ssc install colrspace, replace

#delimit ;
heatplot rho,values(format(%4.3f) size(vsmall)) 
 color(hcl diverging,  intensity(.6))  legend(off) 
 
 xlabel(1 `" "Risky users"  "(16.1%)"  "' 
        2 `" "Entertainment users"          "(8.7%)" "'
		3`"  "Basic users"             "(53.7%)" "'
		4`"  "Advanced users"          "(21.5%)"  "' )

 ylabel ( 
			1 "Duration: Messaging"
			2 "Duration: Audio"
			3 "Duration: Vedio"
			4 "Duration: Game"
			5 "Duration: Study"			
			6 "Duration: Read leisure"
			7 "Duration: Read news"
			8 "Duration: Read learn"
			9 "Risky: Link"
			10 "Risky: Password"
			11 "Risky: QR code"
			12 "Risky: Verification"  
			13 "Risky: Public wifi"
			14 "Risky: Download"  , labsize(vsmall))
 ;			
#delimit cr


graph save "Graph" "rho_lca4.gph" , replace 
graph export "rho_lca4.png", as(png)  replace



*aux analysis: 5 latent class 
discard
doLCA messaging audio  vedio game study read_entertainment  read_news read_learn  link password QR verification  wifi download , 	///
      nclass(5) 			///
	  seed(100000) 			///
	  seeddraws(100000) 	///
	  categories(2 2 2 2 2 2 2 2 2 2 2 2 2 2)  ///
	  criterion(0.000001)  ///
	  rhoprior(1.0)

return list 
mat C=r(rho)
mat gamma = r(gamma)
mat list gamma ,format(%9.3f) 


save lca4_actonly, replace 

