library(rpart)
library(caret)
library(gmodels)
library(C50)
library(irr)
library(party)
library(wSVM)
set.seed(1701)
str(Consumer_Complaints)
levels(Consumer_Complaints$Product)
#This line will show all levels that the factor above has. It seems to be OK.
levels(Consumer_Complaints$Sub.product)
#This line also will show all levels that the factor above has. However, this seems to be not OK. 
table(Consumer_Complaints$Sub.product=="")
#This seems to be concerning! But looking at the actual spreadsheet seems to make this not so much of an issue
#In fact, it seems to be a positively optional row.
#As a result, imputation seems to not be needed. But perhaps coding a separate column, detailing whether or not
#this column is occupied or not will be useful:
Consumer_Complaints$SubProdExist<-0
Consumer_Complaints$SubProdExist[which(Consumer_Complaints$Sub.product=="")]<-1
levels(Consumer_Complaints$Issue)
#In general, these seem good, but again, we have "" listed here.
which(Consumer_Complaints$Issue=="")
#Thankfully, this time around, we only see 6 different ones without an issue. Let's see what else these have.
#Maybe we can input the issue from what else they have around them.
Consumer_Complaints[44023,]
Consumer_Complaints[45748,]
Consumer_Complaints[85397,]
Consumer_Complaints[86963,]
Consumer_Complaints[94243,]
Consumer_Complaints[99431,]
#Unfortunately, there is no subissue with any of these. Instead, let's again make a separate category for it.
Consumer_Complaints$IssueExist<-0
Consumer_Complaints$IssueExist[which(Consumer_Complaints$Issue=="")]<-1
#I see a pattern here, with the SubIssue. Let's make sure, though.
levels(Consumer_Complaints$Sub.issue)
table(Consumer_Complaints$Sub.issue=="")
#This one has "" as being the most common subissue by far. Let's make another separate column, and encode it
#as we've done before.
Consumer_Complaints$SubIssueExist<-0
Consumer_Complaints$SubIssueExist[which(Consumer_Complaints$Sub.issue=="")]<-1
levels(Consumer_Complaints$State)
table(Consumer_Complaints$State)
#A very common one is "" again. Same deal.
Consumer_Complaints$StateExist<-0
Consumer_Complaints$StateExist[which(Consumer_Complaints$State=="")]<-1
#The ZIP code category seems fine. It is encoded as an Int. We'll need to change that to a factor, as we
#don't want it to draw any false conclusions about the ZIP codes being continous.
Consumer_Complaints$ZIP.code<-as.factor(Consumer_Complaints$ZIP.code)
levels(Consumer_Complaints$Submitted.via)
levels(Consumer_Complaints$Date.received)
#These look fine.
levels(Consumer_Complaints$Date.sent.to.company)
levels(Consumer_Complaints$Company)
#These as well.
levels(Consumer_Complaints$Company.response)
levels(Consumer_Complaints$Timely.response.)
#Same.
levels(Consumer_Complaints$Consumer.disputed.)
table(Consumer_Complaints$Consumer.disputed.)
#Finally, let's make a column just for this one as well. There are about 32k ""s.
Consumer_Complaints$DisputedExist<-0
Consumer_Complaints$DisputedExist[which(Consumer_Complaints$Consumer.disputed.=="")]<-1
#Now we will sample the data, partitioning it.
positions<-sample(length(Consumer_Complaints), size=198591, replace=TRUE)
training<-Consumer_Complaints[positions]
positions.verification<-sample(positions, size=99296)
testing<-Consumer_Complaints[positions.verification,]
verification<-Consumer_Complaints[-positions.verification,]
#Now for the model building exercise
#model<-rpart(Company.response~Product+Sub.product+NewIssue+Sub.issue+State+ZIP.code+Submitted.via+Date.received+Date.sent.to.company+Timely.response.+Consumer.disputed.+SubProdExist+IssueExist+StateExist+DisputedExist,data=training, method="class")
#Oh my. It seemed like it was working, but then generated an error indicating that this creates an absolutely 
#massive model.Maybe reducing the complexity of the variables would help this. We'll have to re-do the 
#partitioning later, once we've modified and purified the data further.

Revised_Complaints <- data.frame(matrix(ncol = 0, nrow = 397183))
Revised_Complaints$ID<-Consumer_Complaints$Complaint.ID
#Product is already down to 11 levels-- further reduction probably isn't needed.
Revised_Complaints$Product<-Consumer_Complaints$Product
#SubProduct is substantially larger. Let's reduce that.
Revised_Complaints$SubProduct<-as.character(Consumer_Complaints$Sub.product)
Revised_Complaints$SubProduct[which(Consumer_Complaints$Sub.product=="Auto")]<-"Consumer"
Revised_Complaints$SubProduct[which(Consumer_Complaints$Sub.product=="Medical")]<-"Consumer"
Revised_Complaints$SubProduct[which(Consumer_Complaints$Sub.product=="Other (phone, health club, etc.)")]<-"Consumer"
Revised_Complaints$SubProduct[which(Consumer_Complaints$Sub.product=="Transit card")]<-"Consumer"
Revised_Complaints$SubProduct[which(Consumer_Complaints$Sub.product=="Title loan")]<-"Consumer"
Revised_Complaints$SubProduct[which(Consumer_Complaints$Sub.product=="Vehicle lease")]<-"Consumer"
Revised_Complaints$SubProduct[which(Consumer_Complaints$Sub.product=="Vehicle loan")]<-"Consumer"
Revised_Complaints$SubProduct[which(Consumer_Complaints$Sub.product=="Domestic (US) money transfer")]<-"Consumer"
Revised_Complaints$SubProduct[which(Consumer_Complaints$Sub.product=="Foreign currency exchange")]<-"Consumer"
Revised_Complaints$SubProduct[which(Consumer_Complaints$Sub.product=="International money transfer" )]<-"Consumer"
Revised_Complaints$SubProduct[which(Consumer_Complaints$Sub.product=="Money order")]<-"Consumer"
Revised_Complaints$SubProduct[which(Consumer_Complaints$Sub.product=="Traveler's/Cashier's checks" )]<-"Consumer"
Revised_Complaints$SubProduct[which(Consumer_Complaints$Sub.product=="Personal line of credit"  )]<-"Consumer"
Revised_Complaints$SubProduct[which(Consumer_Complaints$Sub.product=="Other bank product/service")]<-"Consumer"


Revised_Complaints$SubProduct[which(Consumer_Complaints$Sub.product=="Conventional fixed mortgage")]<-"Mortgage"
Revised_Complaints$SubProduct[which(Consumer_Complaints$Sub.product=="Conventional adjustable mortgage (ARM)")]<-"Mortgage"
Revised_Complaints$SubProduct[which(Consumer_Complaints$Sub.product=="FHA mortgage")]<-"Mortgage"
Revised_Complaints$SubProduct[which(Consumer_Complaints$Sub.product=="Home equity loan or line of credit" )]<-"Mortgage"
Revised_Complaints$SubProduct[which(Consumer_Complaints$Sub.product=="Mortgage")]<-"Mortgage"
Revised_Complaints$SubProduct[which(Consumer_Complaints$Sub.product=="Other mortgage")]<-"Mortgage"
Revised_Complaints$SubProduct[which(Consumer_Complaints$Sub.product=="Reverse mortgage" )]<-"Mortgage"
Revised_Complaints$SubProduct[which(Consumer_Complaints$Sub.product=="Second mortgage" )]<-"Mortgage"
Revised_Complaints$SubProduct[which(Consumer_Complaints$Sub.product=="VA mortgage")]<-"Mortgage"

Revised_Complaints$SubProduct[which(Consumer_Complaints$Sub.product=="Cashing a check without an account")]<-"Savings/Checking/Cards"
Revised_Complaints$SubProduct[which(Consumer_Complaints$Sub.product=="Check cashing")]<-"Savings/Checking/Cards"
Revised_Complaints$SubProduct[which(Consumer_Complaints$Sub.product=="(CD) Certificate of deposit")]<-"Savings/Checking/Cards"
Revised_Complaints$SubProduct[which(Consumer_Complaints$Sub.product=="Checking account")]<-"Savings/Checking/Cards"
Revised_Complaints$SubProduct[which(Consumer_Complaints$Sub.product=="General purpose card" )]<-"Savings/Checking/Cards"
Revised_Complaints$SubProduct[which(Consumer_Complaints$Sub.product=="Gift or merchant card" )]<-"Savings/Checking/Cards"
Revised_Complaints$SubProduct[which(Consumer_Complaints$Sub.product=="ID prepaid card")]<-"Savings/Checking/Cards"
Revised_Complaints$SubProduct[which(Consumer_Complaints$Sub.product=="Mobile wallet")]<-"Savings/Checking/Cards"
Revised_Complaints$SubProduct[which(Consumer_Complaints$Sub.product=="Payroll card" )]<-"Savings/Checking/Cards"
Revised_Complaints$SubProduct[which(Consumer_Complaints$Sub.product=="Savings account")]<-"Savings/Checking/Cards"
Revised_Complaints$SubProduct[which(Consumer_Complaints$Sub.product=="Credit card")]<-"Savings/Checking/Cards"

Revised_Complaints$SubProduct[which(Consumer_Complaints$Sub.product=="Government benefit payment card")]<-"Poor Services"
Revised_Complaints$SubProduct[which(Consumer_Complaints$Sub.product=="Credit repair")]<-"Poor Services"
Revised_Complaints$SubProduct[which(Consumer_Complaints$Sub.product=="Electronic Benefit Transfer / EBT card")]<-"Poor Services"
Revised_Complaints$SubProduct[which(Consumer_Complaints$Sub.product=="Installment loan")]<-"Poor Services"
Revised_Complaints$SubProduct[which(Consumer_Complaints$Sub.product=="Other special purpose card")]<-"Poor Services"
Revised_Complaints$SubProduct[which(Consumer_Complaints$Sub.product=="Payday loan" )]<-"Poor Services"
Revised_Complaints$SubProduct[which(Consumer_Complaints$Sub.product=="Pawn loan" )]<-"Poor Services"
Revised_Complaints$SubProduct[which(Consumer_Complaints$Sub.product=="Refund anticipation check" )]<-"Poor Services"
Revised_Complaints$SubProduct[which(Consumer_Complaints$Sub.product=="Pawn loan" )]<-"Poor Services"
Revised_Complaints$SubProduct[which(Consumer_Complaints$Sub.product=="Debt settlement" )]<-"Poor Services"

Revised_Complaints$SubProduct[which(Consumer_Complaints$Sub.product=="Federal student loan" )]<-"Student Loans"
Revised_Complaints$SubProduct[which(Consumer_Complaints$Sub.product=="Non-federal student loan" )]<-"Student Loans"

Revised_Complaints$SubProduct[which(Consumer_Complaints$Sub.product=="" )]<-"None"
#Nice! We've reduced the number of SubProducts from 46 down to 6. Most fit in well within their categories,
#but some are a little more iffy. If need be, we can expand the number of categories and try and fit it 
#better later.
Revised_Complaints$SubProduct<-as.factor(Revised_Complaints$SubProduct)

#This next one has 96 different levels! Let's get to it.
Revised_Complaints$Issue<-as.character(Consumer_Complaints$Issue)
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Balance transfer fee")]<-"Fees"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Cash advance fee")]<-"Fees"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Charged fees or interest I didn't expect")]<-"Fees"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Excessive fees")]<-"Fees"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Fees")]<-"Fees"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Late fee" )]<-"Fees"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Other fee" )]<-"Fees"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Overlimit fee" )]<-"Fees"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Unexpected/Other fees")]<-"Fees"

Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Applied for loan/did not receive money" )]<-"Errors/Illegality"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Can't contact lender"  )]<-"Errors/Illegality"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Can't stop charges to bank account" )]<-"Errors/Illegality"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Charged bank acct wrong day or amt" )]<-"Errors/Illegality"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Cont'd attempts collect debt not owed"  )]<-"Errors/Illegality"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Collection debt dispute" )]<-"Errors/Illegality"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Collection practices" )]<-"Errors/Illegality"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Communication tactics" )]<-"Errors/Illegality"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Customer service / Customer relations" )]<-"Errors/Illegality"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Dealing with my lender or servicer" )]<-"Errors/Illegality"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Customer service/Customer relations" )]<-"Errors/Illegality"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="False statements or representation" )]<-"Errors/Illegality"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Fraud or scam" )]<-"Errors/Illegality"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Improper contact or sharing of info" )]<-"Errors/Illegality"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Improper use of my credit report" )]<-"Errors/Illegality"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Incorrect/missing disclosures or info" )]<-"Errors/Illegality"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Lender damaged or destroyed property" )]<-"Errors/Illegality"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Lender damaged or destroyed vehicle" )]<-"Errors/Illegality"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Lost or stolen check" )]<-"Errors/Illegality"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Lost or stolen money order" )]<-"Errors/Illegality"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Money was not available when promised" )]<-"Errors/Illegality"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Other service issues" )]<-"Errors/Illegality"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Taking/threatening an illegal action"  )]<-"Errors/Illegality"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Transaction issue" )]<-"Errors/Illegality"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Unable to get credit report/credit score" )]<-"Errors/Illegality"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Unauthorized transactions/trans. issues" )]<-"Errors/Illegality"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Unsolicited issuance of credit card" )]<-"Errors/Illegality"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Wrong amount charged or received")]<-"Errors/Illegality"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Incorrect information on credit report")]<-"Errors/Illegality"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Identity theft / Fraud / Embezzlement")]<-"Errors/Illegality"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Received a loan I didn't apply for")]<-"Errors/Illegality"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Incorrect exchange rate")]<-"Errors/Illegality"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Payment to acct not credited")]<-"Errors/Illegality"

Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Arbitration")]<-"Delinquency"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Bankruptcy")]<-"Delinquency"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Can't repay my loan" )]<-"Delinquency"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Delinquent account")]<-"Delinquency"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Lender repossessed or sold the vehicle")]<-"Delinquency"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Lender sold the property")]<-"Delinquency"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Loan modification,collection,foreclosure")]<-"Delinquency"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Problems caused by my funds being low")]<-"Delinquency"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Problems when you are unable to pay")]<-"Delinquency"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Settlement process and costs")]<-"Delinquency"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Forbearance / Workout plans")]<-"Delinquency"


Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Account opening, closing, or management")]<-"Accounts/Account Functions"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Account terms and changes")]<-"Accounts/Account Functions"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Adding money")]<-"Accounts/Account Functions"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Balance transfer")]<-"Accounts/Account Functions"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Billing disputes")]<-"Accounts/Account Functions"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Billing statement")]<-"Accounts/Account Functions"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Closing/Cancelling account")]<-"Accounts/Account Functions"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Convenience checks")]<-"Accounts/Account Functions"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Credit card protection / Debt protection")]<-"Accounts/Account Functions"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Credit monitoring or identity protection")]<-"Accounts/Account Functions"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Deposits and withdrawals")]<-"Accounts/Account Functions"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Disclosures")]<-"Accounts/Account Functions"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Making/receiving payments, sending money")]<-"Accounts/Account Functions"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Managing, opening, or closing account")]<-"Accounts/Account Functions"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Other transaction issues" )]<-"Accounts/Account Functions"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Overdraft, savings or rewards features")]<-"Accounts/Account Functions"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Privacy")]<-"Accounts/Account Functions"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Rewards")]<-"Accounts/Account Functions"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Sale of account")]<-"Accounts/Account Functions"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Using a debit or ATM card" )]<-"Accounts/Account Functions"

Revised_Complaints$Issue[which(Consumer_Complaints$Issue== "Application, originator, mortgage broker")]<-"Loans"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Application processing delay")]<-"Loans"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="APR or interest rate" )]<-"Loans"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Credit determination")]<-"Loans"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Credit decision / Underwriting" )]<-"Loans"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Credit line increase/decrease")]<-"Loans"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Getting a loan" )]<-"Loans"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Loan servicing, payments, escrow account")]<-"Loans"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Managing the line of credit")]<-"Loans"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Managing the loan or lease")]<-"Loans"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Payoff process" )]<-"Loans"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Repaying your loan" )]<-"Loans"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Shopping for a line of credit" )]<-"Loans"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Shopping for a loan or lease"  )]<-"Loans"
Revised_Complaints$Issue[which(Consumer_Complaints$Issue=="Taking out the loan or lease" )]<-"Loans"

Revised_Complaints$Issue<-as.factor(Revised_Complaints$Issue)

#The ZIP Code category is interesting, but ultimately probably duplicative of the State category.
#The State category is also somewhat problematic-- there are 63 levels, but how can we condense them? One option would be
#to keep them as they are, as it is reasonable to assume that the laws around Financial Regulation are standardized 
#at the state level. At the same time, it is also reasonable to assume that the laws around Financial Regulation are
#standardized at the Federal Circuit level. This would allow us to reduce the number of categories substantially as well,
#even if it isn't a perfect sort of standardization.

#Additionally, we will encode the military levels of this category in the same one as DC-- both are under Federal 
#jurisdiction.

Revised_Complaints$Circuit<-as.character(Consumer_Complaints$State)

Revised_Complaints$Circuit[which(Consumer_Complaints$State=="ME" )]<-"1"
Revised_Complaints$Circuit[which(Consumer_Complaints$State=="MA" )]<-"1"
Revised_Complaints$Circuit[which(Consumer_Complaints$State=="NH" )]<-"1"
Revised_Complaints$Circuit[which(Consumer_Complaints$State=="PR" )]<-"1"
Revised_Complaints$Circuit[which(Consumer_Complaints$State=="RI" )]<-"1"

Revised_Complaints$Circuit[which(Consumer_Complaints$State=="CT" )]<-"2"
Revised_Complaints$Circuit[which(Consumer_Complaints$State=="NY" )]<-"2"
Revised_Complaints$Circuit[which(Consumer_Complaints$State=="VT" )]<-"2"

Revised_Complaints$Circuit[which(Consumer_Complaints$State=="DE" )]<-"3"
Revised_Complaints$Circuit[which(Consumer_Complaints$State=="NJ" )]<-"3"
Revised_Complaints$Circuit[which(Consumer_Complaints$State=="PA" )]<-"3"
Revised_Complaints$Circuit[which(Consumer_Complaints$State=="VI" )]<-"3"

Revised_Complaints$Circuit[which(Consumer_Complaints$State=="MD" )]<-"4"
Revised_Complaints$Circuit[which(Consumer_Complaints$State=="NC" )]<-"4"
Revised_Complaints$Circuit[which(Consumer_Complaints$State=="SC" )]<-"4"
Revised_Complaints$Circuit[which(Consumer_Complaints$State=="VA" )]<-"4"
Revised_Complaints$Circuit[which(Consumer_Complaints$State=="WV" )]<-"4"

Revised_Complaints$Circuit[which(Consumer_Complaints$State=="LA" )]<-"5"
Revised_Complaints$Circuit[which(Consumer_Complaints$State=="MS" )]<-"5"
Revised_Complaints$Circuit[which(Consumer_Complaints$State=="TX" )]<-"5"

Revised_Complaints$Circuit[which(Consumer_Complaints$State=="KY" )]<-"6"
Revised_Complaints$Circuit[which(Consumer_Complaints$State=="MI" )]<-"6"
Revised_Complaints$Circuit[which(Consumer_Complaints$State=="OH" )]<-"6"
Revised_Complaints$Circuit[which(Consumer_Complaints$State=="TN" )]<-"6"

Revised_Complaints$Circuit[which(Consumer_Complaints$State=="IL" )]<-"7"
Revised_Complaints$Circuit[which(Consumer_Complaints$State=="IN" )]<-"7"
Revised_Complaints$Circuit[which(Consumer_Complaints$State=="WI" )]<-"7"

Revised_Complaints$Circuit[which(Consumer_Complaints$State=="AR" )]<-"8"
Revised_Complaints$Circuit[which(Consumer_Complaints$State=="IA" )]<-"8"
Revised_Complaints$Circuit[which(Consumer_Complaints$State=="MO" )]<-"8"
Revised_Complaints$Circuit[which(Consumer_Complaints$State=="NE" )]<-"8"
Revised_Complaints$Circuit[which(Consumer_Complaints$State=="SD" )]<-"8"
Revised_Complaints$Circuit[which(Consumer_Complaints$State=="MN" )]<-"8"
Revised_Complaints$Circuit[which(Consumer_Complaints$State=="ND" )]<-"8"


Revised_Complaints$Circuit[which(Consumer_Complaints$State=="AK" )]<-"9"
Revised_Complaints$Circuit[which(Consumer_Complaints$State=="AZ" )]<-"9"
Revised_Complaints$Circuit[which(Consumer_Complaints$State=="CA" )]<-"9"
Revised_Complaints$Circuit[which(Consumer_Complaints$State=="GU" )]<-"9"
Revised_Complaints$Circuit[which(Consumer_Complaints$State=="HI" )]<-"9"
Revised_Complaints$Circuit[which(Consumer_Complaints$State=="ID" )]<-"9"
Revised_Complaints$Circuit[which(Consumer_Complaints$State=="MT" )]<-"9"
Revised_Complaints$Circuit[which(Consumer_Complaints$State=="NV" )]<-"9"
Revised_Complaints$Circuit[which(Consumer_Complaints$State=="MP" )]<-"9"
Revised_Complaints$Circuit[which(Consumer_Complaints$State=="OR" )]<-"9"
Revised_Complaints$Circuit[which(Consumer_Complaints$State=="WA" )]<-"9"

Revised_Complaints$Circuit[which(Consumer_Complaints$State=="WY" )]<-"10"
Revised_Complaints$Circuit[which(Consumer_Complaints$State=="UT" )]<-"10"
Revised_Complaints$Circuit[which(Consumer_Complaints$State=="CO" )]<-"10"
Revised_Complaints$Circuit[which(Consumer_Complaints$State=="NM" )]<-"10"
Revised_Complaints$Circuit[which(Consumer_Complaints$State=="KS" )]<-"10"
Revised_Complaints$Circuit[which(Consumer_Complaints$State=="OK" )]<-"10"

Revised_Complaints$Circuit[which(Consumer_Complaints$State=="AL" )]<-"11"
Revised_Complaints$Circuit[which(Consumer_Complaints$State=="FL" )]<-"11"
Revised_Complaints$Circuit[which(Consumer_Complaints$State=="GA" )]<-"11"

Revised_Complaints$Circuit[which(Consumer_Complaints$State=="AA" )]<-"Fed"
Revised_Complaints$Circuit[which(Consumer_Complaints$State=="AE" )]<-"Fed"
Revised_Complaints$Circuit[which(Consumer_Complaints$State=="AP" )]<-"Fed"
Revised_Complaints$Circuit[which(Consumer_Complaints$State=="DC" )]<-"Fed"

Revised_Complaints$Circuit[which(Consumer_Complaints$State=="" )]<-"None"

Revised_Complaints$Circuit[which(Consumer_Complaints$State=="FM" )]<-"Free Association"
Revised_Complaints$Circuit[which(Consumer_Complaints$State=="MH" )]<-"Free Association"
Revised_Complaints$Circuit[which(Consumer_Complaints$State=="PW" )]<-"Free Association"

Revised_Complaints$Circuit[which(Consumer_Complaints$State=="AS" )]<-"Samoa"

Revised_Complaints$Circuit<-as.factor(Revised_Complaints$Circuit)

Revised_Complaints$Submitted<-Consumer_Complaints$Submitted.via
#The above needs no changes-- it is already 6 levels.

#The date received might have some useful information, but with each encoded as they are, it will be difficult to
#gain much insight from it. Besides, there's a good indication that the ID numbers go up as the time goes on, so 
#we implicitly have some measurement of time within the data already. Putting in the precise date might be duplicative
#with no advantage. If we are strapped for area for improvement later, we might do something with it. For now, it will
#be omitted.

#Company as a field is also very complicated. Maybe having a certain word in the name makes it more likely to have a 
#negative or positive outcome. Regardless, we will omit this for the time being as well.


#Company.response is the category we're after, and Timely.response/Consumer.disputed are already 2 and 3 levels 
#respectively. This means we're ready to re-divide our data and go ahead and make a model.

Revised_Complaints$Response<-Consumer_Complaints$Company.response
Revised_Complaints$Timely<-Consumer_Complaints$Timely.response.
Revised_Complaints$Disputed<-Consumer_Complaints$Consumer.disputed.

Revised_Complaints$Response<-as.character(Revised_Complaints$Response)

Revised_Complaints$Response[which(Consumer_Complaints$Company.response=="Closed with monetary relief" )]<-"Positive"
Revised_Complaints$Response[which(Consumer_Complaints$Company.response=="Closed with non-monetary relief" )]<-"Positive"
Revised_Complaints$Response[which(Consumer_Complaints$Company.response=="Closed with relief" )]<-"Positive"

Revised_Complaints$Response[which(Consumer_Complaints$Company.response=="Closed without relief" )]<-"Negative"
Revised_Complaints$Response[which(Consumer_Complaints$Company.response=="In progress" )]<-"Negative"
Revised_Complaints$Response[which(Consumer_Complaints$Company.response=="Untimely response" )]<-"Negative"
Revised_Complaints$Response[which(Consumer_Complaints$Company.response=="Closed" )]<-"Negative"

Revised_Complaints_Positive<-Revised_Complaints[which(Revised_Complaints$Response=="Positive"),]
Revised_Complaints_Negative<-Revised_Complaints[which(Revised_Complaints$Response=="Negative"),]

Revised_Complaints<-rbind(Revised_Complaints_Positive,Revised_Complaints_Negative)
#Revised_Complaints$Response[which(Consumer_Complaints$Company.response=="Closed with explanation" )]<-"Negative"

Revised_Complaints$Response<-as.factor(Revised_Complaints$Response)



#Now we will implement cross-validation.

folds<-createFolds(Revised_Complaints$Response, k =10)

Second.Folds<-lapply(folds,sample, 1000)
Second.Training.Folds<-Second.Folds
Second.Training.Folds$Fold01<-Second.Training.Folds$Fold01[1:900]
Second.Training.Folds$Fold02<-Second.Training.Folds$Fold02[1:900]
Second.Training.Folds$Fold03<-Second.Training.Folds$Fold03[1:900]
Second.Training.Folds$Fold04<-Second.Training.Folds$Fold04[1:900]
Second.Training.Folds$Fold05<-Second.Training.Folds$Fold05[1:900]
Second.Training.Folds$Fold06<-Second.Training.Folds$Fold06[1:900]
Second.Training.Folds$Fold07<-Second.Training.Folds$Fold07[1:900]
Second.Training.Folds$Fold08<-Second.Training.Folds$Fold08[1:900]
Second.Training.Folds$Fold09<-Second.Training.Folds$Fold09[1:900]
Second.Training.Folds$Fold10<-Second.Training.Folds$Fold10[1:900]

Second.Testing.Folds<-Second.Folds
Second.Testing.Folds$Fold01<-Second.Testing.Folds$Fold01[901:1000]
Second.Testing.Folds$Fold02<-Second.Testing.Folds$Fold02[901:1000]
Second.Testing.Folds$Fold03<-Second.Testing.Folds$Fold03[901:1000]
Second.Testing.Folds$Fold04<-Second.Testing.Folds$Fold04[901:1000]
Second.Testing.Folds$Fold05<-Second.Testing.Folds$Fold05[901:1000]
Second.Testing.Folds$Fold06<-Second.Testing.Folds$Fold06[901:1000]
Second.Testing.Folds$Fold07<-Second.Testing.Folds$Fold07[901:1000]
Second.Testing.Folds$Fold08<-Second.Testing.Folds$Fold08[901:1000]
Second.Testing.Folds$Fold09<-Second.Testing.Folds$Fold09[901:1000]
Second.Testing.Folds$Fold10<-Second.Testing.Folds$Fold10[901:1000]

myFunction<-function(x,y)
{
  STrain<-Revised_Complaints[x,]
  STest<-Revised_Complaints[y,]
  SModel<-cforest(Response ~ Product+SubProduct+Issue+Circuit+Submitted+Timely+Disputed,data = STrain)
  SPrediction<-predict(SModel, STest, OOB=TRUE, type = "response")
  ErrorRate<-Error.rate(SPrediction, STest$Response)
  return(1-ErrorRate)
}





cv_results<-lapply(Second.Training.Folds,Second.Testing.Folds, myFunction)
cv_results

#Response~Product+SubProduct+Issue+Circuit+Submitted+Timely+Disputed,data=training, method="class")

#Prediction<-predict(model, testing, type="class")
#Verification.Prediction<-predict(model, verification, type="class")

#CrossTable(x=testing$Response, y=Prediction, dnn=c("Actual","Prediction"))

#CrossTable(verification$Response, Verification.Prediction, prop.chisq = FALSE, prop.c=FALSE, prop.r=FALSE, dnn=c("Actual","Prediction"))