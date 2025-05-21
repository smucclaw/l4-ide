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

*(Table truncated to principal rules; further rules—misleading fundraising offences, name-change duties, etc.—follow the same pattern and can be appended as needed.)*

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

---
