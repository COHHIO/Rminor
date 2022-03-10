utilization_notes <- list(bed = "Bed Utilization is the percentage of a project's available beds being populated by individual clients.",
                          unit = "Unit Utilization is the percentage of a project's available units being populated by households. A household can be a single individual or multiple clients presenting together for housing.",
                          calc = "Bed Utilization = bed nights* served / total possible bed nights** in a month.
Unit Utilization = unit nights* served / total possible unit nights in a
month.

* A bed night is a single night in a bed.
* A unit night is a single night in a unit.
** Total possible bed/unit nights = number of beds/units a project has multiplied by how many days are in the given month.

Example A: Client A enters a shelter on May 1 and exits on May 5. They spent four nights in the shelter, so that was 4 bed nights from that client alone in the month of May for that shelter.

Example B: PSH Project A served 10 people every single night in the month of June. Each client was served 30 bed nights during that month, and since there were 10 clients, that PSH project served a total of 300 bed nights for the month of June.

Example C: PSH Project B has 5 beds. That project's total possible bed
nights for the month of April (which has 30 days in it) is 30 x 5, which is 150.

Example D: Using what we know from Example B of PSH Project A's total bed nights for the month of June, let's calculate what their bed utilization was for that month. They have 11 beds and June has 30 days so since 11 × 30 = 330 possible bed nights. Their bed utilization is bed nights (300) divided by possible bed nights (330), which is: 91%!")

qpr_note <- list(served_county = "The horizontal lines represent the average scores of Heads of Household who were served in the County in a ES, TH, SH, or Outreach project during the reporting period and who were scored. If a Head of Household was served in a County outside the Balance of State or if that data was missing, they are not being counted. When there are multiple project entries for the same client, this only counts the most recent entry. When there are multiple scores, this only counts the most recent score. There should not be more than 1 score on the same day, but if there are it is counting the highest score.",
                 housed_county = "The triangle represents the average score of each household entering into a permanent housing project in a County during the reporting period. This will necessarily leave out households coming from Domestic Violence shelters since they are not scored. Any Heads of Household who entered a permanent housing project without a score will be counted as having a score of 0.",
                 dq_community_need = "It is very important that your Duplicate Entry Exits and your Household Data Quality tabs are totally clear for this report to be accurate. It is also important that your VI-SPDAT scores are ON THE HEAD OF HOUSEHOLD'S RECORD. Any scores recorded on non-HoHs will not be counted here.  Also if a HoH is missing their County data or they were served in a County outside the Ohio Balance of State, they will also not show here.")