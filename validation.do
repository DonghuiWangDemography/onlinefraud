*validation through split-sample
*created on 20240714
*the final lca results are in lca_only.do file


*cd "/Users/priscillawang/SynologyDrive/抖音项目/dofie"  //mac 
cd "C:\Users\donghuiwang\SynologyDrive\抖音项目\dofie" // UM office desktop 
*step 1: variable generation: the same as lca_only.do 

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

*---------end of variable generation --------------

*step2, randomly split sample into two halves, Subsample A (the "calibration" data set) and Subsample B (the"validation" data set).
splitsample, generate(svar)
save prepare_validation.dta, replace  


*calibration on sample 1
use prepare_validation.dta,clear
keep if svar==1


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

/*
stats[19,5]
Gsquared	AIC	BIC	adjBIC	entropy
2  16884.007	16978.007	17244.583	17095.258	0.730
3  15983.597	16125.597	16528.296	16302.721	0.816
4  15306.759	15496.759	16035.583	15733.756	0.759
5  14902.472	15140.472	15815.419	15437.341	0.763
6  14721.900	15007.900	15818.971	15364.642	0.753
7  14434.548	14768.548	15715.743	15185.164	0.769
8  14318.677	14700.677	15783.996	15177.165	0.754
9  14128.635	14558.635	15778.077	15094.996	0.749
10  14050.423	14528.423	15883.990	15124.657	0.789
11  13881.634	14407.634	15899.324	15063.741	0.772
12  13778.523	14352.523	15980.337	15068.503	0.764
13  13701.640	14323.640	16087.578	15099.493	0.771
14  13656.835	14326.835	16226.897	15162.561	0.777
15  13579.572	14297.572	16333.758	15193.170	0.775
16  13532.400	14298.400	16470.710	15253.872	0.773
17  13467.328	14281.328	16589.762	15296.673	0.790
18  13424.617	14286.617	16731.174	15361.834	0.774
19  13371.991	14281.991	16862.672	15417.081	0.785
20  13344.904	14302.904	17019.709	15497.867	0.797

*/


*four class 

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
mat list gamma  
mat list C


*modify rho so that it can be fit as restrictions 
mat C1= C[1..14, 1..4]'
mat C2=C[15..28, 1..4]'

mat list C1
mat list C2


/*

gamma[1,4]
       Class1     Class2     Class3     Class4
r1  .21063111  .08652509  .49111853  .21172527


C1[4,14]
	     messaging11	audio11	vedio11	game11	study11	read_ente~11	read_news11	read_learn11	link11	password11	QR11	verificat~11	wifi11	download11
Class1	.41999668	.09361539	.56641771	.09756194	.06558049	.06086967	.13966644	.02895335	.29901706	.59980437	.27305822	.36656631	.52413413	.40220738
Class2	.48345891	.32254877	.95081001	.65509977	.01550101	.33780478	.67128177	.02334785	.00123343	.71771463	.00112737	.00116259	.00311685	.01408494
Class3	.6296639	.09430479	.60168245	.04655486	.0531112	.06904713	.14502033	.01663339	.07264634	.36995151	.0294008	.00988575	.09796202	.0720249
Class4	.84109876	.41634037	.8485356	.50811197	.35077972	.40666072	.63381798	.46859594	.27265607	.35257253	.19863478	.1398499	.32797756	.32016091

*/


*fit 4 class model to sample B 

clear 
use prepare_validation.dta,clear
keep if svar==2

*rho restrict :  0= fixed, 1=freely estimated, 2=form part of an equivalence set 
discard
doLCA messaging audio  vedio game study read_entertainment  read_news read_learn  link password QR verification  wifi download , 	///
      nclass(4) 			///
	   maxiter(5000)   ///
	  categories(2 2 2 2 2 2 2 2 2 2 2 2 2 2)  ///
	  gammastart (.21063111  .08652509  .49111853  .21172527) ///
	  gammarestrict(1 0 0 0) ///
	  rhostart (0.41999668	0.09361539	0.56641771	0.09756194	0.06558049	0.06086967	0.13966644	0.02895335	0.29901706	0.59980437	0.27305822	0.36656631	0.52413413	0.40220738  ///
	            0.48345891	0.32254877	0.95081001	0.65509977	0.01550101	0.33780478	0.67128177	0.02334785	0.00123343	0.71771463	0.00112737	0.00116259	0.00311685	0.01408494  ///
	            0.6296639	0.09430479	0.60168245	0.04655486	0.0531112	0.06904713	0.14502033	0.01663339	0.07264634	0.36995151	0.0294008	0.00988575	0.09796202	0.0720249   ///
	            0.84109876	0.41634037	0.8485356	0.50811197	0.35077972	0.40666072	0.63381798	0.46859594	0.27265607	0.35257253	0.19863478	0.1398499	0.32797756	0.32016091  ///
                0.58000332	0.90638461	0.43358229	0.90243806	0.93441951	0.93913033	0.86033356	0.97104665	0.70098294	0.40019563	0.72694178	0.63343369	0.47586587	0.59779262 ///
                0.51654109	0.67745123	0.04918999	0.34490023	0.98449899	0.66219522	0.32871823	0.97665215	0.99876657	0.28228537	0.99887263	0.99883741	0.99688315	0.98591506 ///
                0.3703361	0.90569521	0.39831755	0.95344514	0.9468888	0.93095287	0.85497967	0.98336661	0.92735366	0.63004849	0.9705992	0.99011425	0.90203798	0.9279751  ///
                0.15890124	0.58365963	0.1514644	0.49188803	0.64922028	0.59333928	0.36618202	0.53140406	0.72734393	0.64742747	0.80136522	0.8601501	0.67202244	0.67983909 )  ///
	  rhorestrict ( 0	0	0	0	0	0	0	0	0	0	0	0	0	0  ///
					0	0	0	0	0	0	0	0	0	0	0	0	0	0  ///
					0	0	0	0	0	0	0	0	0	0	0	0	0	0  ///
					1	1	1	1	1	1	1	1	1	1	1	1	1	1  ///
					1	1	1	1	1	1	1	1	1	1	1	1	1	1  ///
					1	1	1	1	1	1	1	1	1	1	1	1	1	1  ///
					1	1	1	1	1	1	1	1	1	1	1	1	1	1  ///
					1	1	1	1	1	1	1	1	1	1	1	1	1	1 ) ///
	  criterion(0.000001)  		///
	  rhoprior(1.0)	  
return list 

scalar m1= r(loglikelihood)
scalar df1=  r(df) 
scalar g1= r(Gsquared) 
	  
/*

Log-likelihood = -14265.925
G-squared = 3531.3604
AIC = 3559.3604
BIC = 3638.7595
CAIC = 3652.7595
Adjusted BIC = 3594.2798
Entropy Raw = 1050.0119
Entropy R-sqd = .64705336
Degrees of freedom = 16369
*/


*fit the same model, but no restrictions 

discard
doLCA messaging audio  vedio game study read_entertainment  read_news read_learn  link password QR verification  wifi download , 	///
      nclass(4) 			///
	  seed(100000) 			///
	  seeddraws(100000) 	///
	  categories(2 2 2 2 2 2 2 2 2 2 2 2 2 2)  ///
	  criterion(0.000001)  ///
	  rhoprior(1.0)

scalar m2 = r(loglikelihood)
scalar df2= r(df) 
scalar g2= r(Gsquared) 
 

di df1-df2

di "chi2(45) = " -2*(m1-m2)
di "Prob > chi2 = "chi2tail(45, -2*(m1-m2))

di -561.05+765.73

	  
/*

Log-likelihood = -14223.12
G-squared = 3445.7493
AIC = 3563.7493
BIC = 3898.3596
CAIC = 3957.3596
Adjusted BIC = 3710.9094
Entropy Raw = 867.52788
Entropy R-sqd = .70839279
Degrees of freedom = 16324
*/	  
	  