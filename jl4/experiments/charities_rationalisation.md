# Charities Rationalisation / Attack Plan
---

## **Updated consolidated tables (Charities (Jersey) Law 2014 \+ R\&O 59/2018, 60/2018, 144/2019, 102/2020, 27/2025)**

**Key:**  
**A** \= Structural/interpretive provisions  
**B** \= Deontic rules (conduct † procedure) ruleType \= CONDUCT | PROCEDURE  
**C** \= Register-event triggers (state-transitions)  
---

### **Section A – Structural / Interpretive layer**

| Provision (instrument) | What it seeds (examples) | Why it is *structural* | Citations |
| ----- | ----- | ----- | ----- |
| Arts 1-2 **Law 2014** | Core terms: *charitable purpose, governor, misconduct* | Pure definitions, no modal force |  |
| Arts 3-4 & Sched 1 **Law 2014** | Creates office of *Charity Commissioner* | Institutional scaffold |  |
| Arts 38-43 & Sched 2 **Law 2014** | Service rules, order-making power, tribunal machinery | Administrative plumbing |  |
| Arts 1-2 **R\&O 59/2018** | Adds definitions *connected person, core financial information, governor payment* | Extends glossary used by later rules |  |
| Art 1 **R\&O 60/2018** | Adds definitions *donation, relevant professional, soliciting* | Supports restricted-section funding condition |  |
| Art 6(2) (Law) | CharitablePurpose enum \= 13 statutory heads | Used by Art 5 charity-test & application form |  |
| **Reg 1** (Core Financial Info Regs 2018\) | CoreFinancialField \= { income, expenditure, openingAssets, closingAssets, otherAssetsList } | Imported by Art 11(2)(e) & Annual Return rules |  |
| Art 2(10) “misconduct (includes mismanagement or misapplication of charity property)” | MisconductType \= { MISMANAGEMENT, MISAPPLICATION } | Lets DSL validation and analytics classify grounds for suspension/disqualification; avoids free-text searches later. | |

---

### **Section B – Deontic rules layer**

| ruleId | ruleType | Modal | Actor(s) | Trigger / condition | Duty / Power / Prohibition | Default sanction / consequence | Citations |
| ----- | ----- | ----- | ----- | ----- | ----- | ----- | ----- |
| **B-AR-01** | CONDUCT | OBLIGES | Registered charity | End of each return-year | File Annual Return **within 2 months** | Late → Commissioner EMPOWERS to issue Required Steps Notice → possible deregistration |  |
| **B-AR-02** | CONDUCT | OBLIGES | Commissioner | On receipt of registration data / Annual Return | Publish extra data columns (financials, governor-payments, public-benefit etc.) | Tribunal appeal |  |
| **B-RS-01** | CONDUCT | PROHIBITS | Restricted-section charity | Continuous while in restricted section | Solicit donations from the public | Breach → refusal of restricted status or enforcement under Art 27 |  |
| **B-FID-01** | CONDUCT | OBLIGES | Governor | Continuous | Act in best interests of charity & beneficiaries | Suspension / removal; offence |  |
| **B-REP-01** | CONDUCT | OBLIGES | Governor / senior employee | Conviction for offence against vulnerable person **or** any unspent conviction | Report to Commissioner ASAP | Suspension / Required Steps Notice / deregistration |  |
| **B-INF-01** | PROCEDURE | EMPOWERS | Commissioner | Reasonable cause | Demand info / attendance (Art 26\) | Non-compliance offence |  |
| **B-RSN-01** | PROCEDURE | EMPOWERS | Commissioner | Grounds in Art 27(1) | Issue Required Steps Notice specifying action \+ deadline (logged) | Non-compliance → deregistration |  |
| **B-APP-01** | PROCEDURE | OBLIGES | Commissioner | When making appealable decision | Serve notice **with reasons \+ last appeal date** | Decision vulnerable if notice absent |  |
| **B-APP-02** | PROCEDURE | OBLIGES | Appellant (charity / applicant) | Decision notice served | Lodge appeal **within 28 days** (charity) | Appeal struck out if late (tribunal may extend) |  |
| **B-APP-03** | PROCEDURE | OBLIGES | Third party | Decision notice served | Lodge appeal **within 56 days** | Same |  |
| B-APP-00 | CONDUCT | OBLIGES | *Applicant entity* | Making an application (Art 11(1)–(2)) | Provide: constitution \+ purposes \+ public-benefit stmt \+ core financial info | Application incomplete ⇒ Commissioner may REFUSE (B-REF-01) |  |
| **B-REF-01** | PROCEDURE | EMPOWERS | Commissioner | Application fails charity-test **or** lacks required docs | **Refuse registration** and issue “reasons & deadline” notice (R\&O 102/2020 Art 2\) | Applicant may appeal ≤ 28 d |  |
| **B-CT-01** | CONDUCT | OBLIGES | *Registered charity* | Continuous | Keep purposes exclusively charitable; deliver public-benefit | Breach ⇒ EMPOWERS Commissioner to deregister (Art 16\) |  |
| **B-PB-01** | PROCEDURE | OBLIGES | Commissioner | Assessing charity-test (Art 7\) | Weigh factors in Art 7(2)-(5) | Decision vulnerable on appeal if factors ignored |  |
| **B-NAME-01** | CONDUCT | PROHIBITS | Any person | Carrying on activities with word “charity”, “charitable” etc. so as to mislead (Arts 21-23) | Use of protected word without entitlement | Criminal offence (Level 3 fine); Commissioner may seek injunction |  |
| **B-MIS-01** | CONDUCT | PROHIBITS | Any person | Statement / document to Commissioner (Art 31\) | Knowingly or recklessly false / misleading | Criminal offence |  |
| **B-GOV-02** | CONDUCT | OBLIGES | Governor | Any *reportable matter* in Art 19(1)(a-h) **incl.** new Order 2025 | Report to Commissioner “as soon as practicable” | Failure signals unfitness; triggers suspension / disqualification powers |  |
| B-GOV-03 *(already present, refined)* | PROCEDURE | EMPOWERS | Commissioner | “Person is unfit” or misconduct found or Art 19 event reported / discovered | Suspend or fully disqualify governor; must state (a) grounds (enum), (b) period, (c) any conditions (Art 20(2)(c)) | Acting while suspended/disqualified \= offence (B-GOV-05) | |
| **B-AR-03** | CONDUCT | OBLIGES | Registered charity | Annual Return cycle (Art 13(7)-(10) **\+** Timing Order 2019 **\+** Core & Add. Info Orders) | Include: core financial info, most-recent accounts, governor-payment breakdowns, public-benefit narrative, *and* send **within 2 months** of year-end | Late or incomplete ⇒ lateFlag=True; enables Required Steps Notice |  |
| **B-APP-03** | PROCEDURE | OBLIGES | Tribunal | Appeal received late but justice favours extension (R\&O 102/2020 Art 7\) | May extend 28/56-day limit | – |  |
| B-GOV-04 *(new)* | PROCEDURE | EMPOWERS | Commissioner | Suspension/disqualification already in force (Art 20(4)) | Vary or revoke order at any time; must serve notice on charity & governor | – | |
| **B-GOV-05** *(new)* | CONDUCT | PROHIBITS | Suspended or disqualified person | During period of suspension/disqualification (Art 20(6)) | Act as governor of any registered charity | Criminal offence; charity risks deregistration | |
| **B-GOV-06** *(new)* | CONDUCT | OBLIGES | Registered charity | A governor becomes suspended or disqualified | Ensure suspended person **does not** act; update Register particulars via ordinary change-notice | Failure evidences mismanagement; enables Required Steps Notice | |

---

### **Section C – Register Events / State-Transitions**

| eventType | Trigger provision | Who initiates | Core data fields written | Effect on Register entry | Citations |
| ----- | ----- | ----- | ----- | ----- | ----- |
| RegisterCharity | Art 11 | Commissioner | name, number, constitution, section=general | New active entry created |  |
| MoveToRestricted | Art 9 \+ R\&O 60/2018 Art 2 | Commissioner on R1 approval | section=restricted; fundingCondition=true | Entry relocated; reduced public fields |  |
| ChangeName | Art 12 | Charity (Form N1) / Commissioner | newRegisteredName, oldNames\[\] | Updates name column; historic name retained |  |
| AnnualReturnLogged | Art 13 \+ R\&O 59/2018 | Charity | financials, paymentsTotals, publicBenefit, lateFlag(bool) | Updates yearly snapshot; flips late if after deadline |  |
| RequiredStepsNoticeLogged | Art 27(4) | Commissioner | noticeId, issueDate, steps\[\], deadline | Notice reference stored under Art 8(3)(k) |  |
| VoluntaryDeregistration | Art 15 | Charity (Form D1) | deregistrationDate, reason | Entry moved to *historic*; number retired |  |
| CompulsoryDeregistration | Art 16 | Commissioner | deregistrationDate, grounds, retrospective? | Same; may be back-dated |  |
| RegistrationRefused | Commissioner decision (B-REF-01) | refusalReason, date, appealDeadline | Starts 28-day appeal timer; no Register entry created |  |  |
| GovernorSuspended / GovernorDisqualified (already listed) now **cross-linked** to reportableMatterId when suspension follows unreported conviction | Art 20 decision | governorId, status, reason, period | Flags on charity & public list |  |  |
| GovernorMisconductFound | Art 18 duty breach or Art 2(10) finding | governorId, misconductType (enum), findingDate | Pivots analytics between pure “fitness” suspensions (bankruptcy, conviction) and substantive mismanagement findings. |  |  |
| GovernorOrderVaried *(covers vary or revoke)* | Art 20(4)-(5) | orderId, newStatus (VARIED/REVOKED), newPeriod, newConditions | Allows front-end to update ban timers and lets appeals see history of modifications. |  |  |
