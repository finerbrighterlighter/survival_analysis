use "/Users/hteza/Desktop/Class/RACE626/Assignment_2_SA/Assignment II/merged data for KT project.dta"
d
sort subjid datevisit1

** decalre this dataset if for survival analysis
* multiple event -> censoring
* one subject multiple record , by subjid
* orgin is the time when subject becomes at risk of graft failure, so date of operation
* failure event is graft faiure ( gf =2 )

stset datevisit1, id(subjid) origin(time dateop1) failure(gf==2) scale(30.5)

* list subjid if _st==0 

list subjid dateop1 datevisit1 _st _t gf if subjid==3125636 
list subjid dateop1 datevisit1 _st _t gf if subjid==3422842

sts list, fail at ( 24 60 )
*sts graph, fail

stsum
strate

**************

* Univariate

* age gp

sts test agegr
*sts graph, fail by (agegr)

* sex receive

sts test sexr
*sts graph, fail by (sexr)

* abo group

sts test bloodgrr
*sts graph, fail by (bloodgrr)

* sex of the donor

sts test typedonor
sts graph, fail by (typedonor)

* hla matching

sts test hlamatch 
*sts graph, fail by (hlamatch)
stsum, by(hlamatch)
stcox i.hlamatch, nolog

* hiv receive

tab hiv

* hbs antigen

sts test hbs
*sts graph, fail by (hbs)

* hbs antibody

sts test athbsr
*sts graph, fail by (athbsr)

* hcv antibody

sts test anthcv
*sts graph, fail by (anthcv)
stsum, by(anthcv)
stcox ib(2).anthcv, nolog

* modee of dialysis

sts test mode
*sts graph, fail by (mode)

* Dibetes

sts test diagdm
*sts graph, fail by (diagdm)

* hypertension

sts test hyper
*sts graph, fail by (hyper)
stsum, by(hyper)
stcox i.hyper, nolog

* high chol

sts test highchol
*sts graph, fail by (highchol)
stsum, by(highchol)
stcox i.highchol, nolog

* cyclosporin

sts test csa
*sts graph, fail by (csa)
stsum, by(csa)
stcox i.csa, nolog

* prednisolone

sts test pred
*sts graph, fail by (pred)

* mmf

sts test mmf
*sts graph, fail by (mmf)

* cardil

sts test cardil
*sts graph, fail by (cardil)

**********

** Multivariate

* Step 0

stcox i.hlamatch if hlamatch!=. & anthcv!=. & hyper!=. & highchol!=. & csa!=.
stcox i.anthcv if hlamatch!=. & anthcv!=. & hyper!=. & highchol!=. & csa!=.
stcox i.hyper if hlamatch!=. & anthcv!=. & hyper!=. & highchol!=. & csa!=.
stcox i.highchol if hlamatch!=. & anthcv!=. & hyper!=. & highchol!=. & csa!=.
stcox i.csa if hlamatch!=. & anthcv!=. & hyper!=. & highchol!=. & csa!=.

* Step 1

qui : stcox i.hyper if hlamatch!=. & anthcv!=. & hyper!=. & highchol!=. & csa!=.
est store A

qui : stcox i.hyper i.hlamatch if hlamatch!=. & anthcv!=. & hyper!=. & highchol!=. & csa!=.
est store B 
lrtest A B

qui : stcox i.hyper i.anthcv if hlamatch!=. & anthcv!=. & hyper!=. & highchol!=. & csa!=.
est store C
lrtest A C

qui : stcox i.hyper i.highchol if hlamatch!=. & anthcv!=. & hyper!=. & highchol!=. & csa!=.
est store D
lrtest A D

qui : stcox i.hyper i.csa if hlamatch!=. & anthcv!=. & hyper!=. & highchol!=. & csa!=.
est store E
lrtest A E

* step 2

qui : stcox i.hyper i.csa i.hlamatch if hlamatch!=. & anthcv!=. & hyper!=. & highchol!=. & csa!=.
est store B1
lrtest E B1

qui : stcox i.hyper i.csa i.anthcv if hlamatch!=. & anthcv!=. & hyper!=. & highchol!=. & csa!=.
est store C1
lrtest E C1

qui : stcox i.hyper i.csa i.highchol if hlamatch!=. & anthcv!=. & hyper!=. & highchol!=. & csa!=.
est store D1 
lrtest E D1

* Step 3

qui : stcox i.hyper i.csa i.highchol i.hlamatch if hlamatch!=. & anthcv!=. & hyper!=. & highchol!=. & csa!=.
est store B2

qui : stcox i.hyper i.csa i.highchol i.hlamatch  i.anthcv if hlamatch!=. & anthcv!=. & hyper!=. & highchol!=. & csa!=.
est store C2
lrtest B2 C2

* final model

stcox i.hyper i.csa i.highchol i.hlamatch if hlamatch!=. & anthcv!=. & hyper!=. & highchol!=. & csa!=.


* Assumptions 
*1. Plotting scaled schoenfeld residual versus function time 

estat phtest,plot(1.hyper) log

estat phtest,plot(2.csa) log

estat phtest,plot(1.highchol) log
                                           
estat phtest,plot(2.hlamatch) log
estat phtest,plot(3.hlamatch) log

*2. log log plot

stphplot,by(hyper)
stphplot,by(csa)
stphplot,by(highchol)
stphplot,by(hlamatch)

*3. Statistical test

qui: stcox i.hyper i.csa i.highchol i.hlamatch if hlamatch!=. & anthcv!=. & hyper!=. & highchol!=. & csa!=., nolog
estat phtest,d

*4. Assess goodness of fit

predict cs,csnell
stset cs,failure(gf==2)
sts gen surfunc=s
list cs _st _d _t _t0 surfunc in 1/10
gen Ht=-ln(surfunc)
list cs _st _d _t _t0 surfunc Ht in 1/10
twoway (line Ht cs, sort lcolor(dkgreen)) (line cs cs, sort lpattern(dash))

*5. Identify outliers by deviance
qui: stcox i.hyper i.csa i.highchol i.hlamatch if hlamatch!=. & anthcv!=. & hyper!=. & highchol!=. & csa!=., nolog
predict dv,deviance
predict xb,xb
scatter dv xb,yline(0) sort mlabel(subjid)

*6. Detect influence outliers cases on coefficient
xi: stcox i.hyper i.csa i.highchol i.hlamatch if hlamatch!=. & anthcv!=. & hyper!=. & highchol!=. & csa!=., esr(esr*)
mat list e(V)
mkmat esr1 esr2 esr3 esr4 esr5 esr6 ,matrix(esr)
mat V=e(V)
mat dfbeta=esr*V
svmat dfbeta,names(s)
label var s1 "Hypertension-positive"
label var s2 "no-cyclosporin"
label var s3 "missing-cyclosporin"
label var s4 "high-cholestrol"
label var s5 "hla-match-one-haplotype"
label var s6 "hla-mismatch"

sum s1,d
disp r(mean)+(3*r(sd))
disp r(mean)-(3*r(sd))
scatter s1 _t,yline(-.04935087 0 .04935087) mlabel(subjid)

sum s2,d
disp r(mean)+(3*r(sd))
disp r(mean)-(3*r(sd))
scatter s2 _t,yline(-.05814241 0 .05814241) mlabel(subjid)

sum s4,d
disp r(mean)+(3*r(sd))
disp r(mean)-(3*r(sd))
scatter s4 _t,yline(-.04154308 0 .04154308) mlabel(subjid)

sum s5,d
disp r(mean)+(3*r(sd))
disp r(mean)-(3*r(sd))
scatter s5 _t,yline(-.15670351 0 .15670351) mlabel(subjid)

sum s6,d
disp r(mean)+(3*r(sd))
disp r(mean)-(3*r(sd))
scatter s6 _t,yline(-.15883871 0 .15883871) mlabel(subjid)
