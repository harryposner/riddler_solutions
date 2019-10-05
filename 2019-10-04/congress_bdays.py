import time
from collections import defaultdict
from datetime import datetime

import requests

CONGRESS = 115
COURTESY_DELAY = 1  # seconds

all_mems_url_fmt = "https://memberguide.gpo.gov/Congressional.svc/GetMembers/{}"
all_members = requests.get(all_mems_url_fmt.format(CONGRESS)).json()

rep_ids = [m["MemberId"] for m in all_members if m["MemberTypeId"] == "RP"]

rep_url_fmt = "https://memberguide.gpo.gov/Congressional.svc/GetMember/{}"
representatives = []
for rep_id in rep_ids:
    time.sleep(COURTESY_DELAY)
    representatives.append(requests.get(rep_url_fmt.format(rep_id)).json())

len([x for x in representatives if x["BirthDate"] is None])

same_bdays = defaultdict(list)
for rep in representatives:
    if rep["FirstName"] == "Vacant" and rep["LastName"] == "Vacant":
        continue
    # These reps' birthdays aren't on the GPO website
    if rep["FirstName"] == "Troy" and rep["LastName"] == "Balderson":
        rep["BirthDate"] = "1/16/1962"
    if rep["FirstName"] == "Kevin" and rep["LastName"] == "Hern":
        rep["BirthDate"] = "12/4/1961"
    if rep["FirstName"] == "Joseph" and rep["LastName"] == "Morelle":
        rep["BirthDate"] = "4/29/1957"
    if rep["FirstName"] == "Mary" and rep["LastName"] == "Scanlon":
        rep["BirthDate"] = "8/30/1959"
    if rep["FirstName"] == "Susan" and rep["LastName"] == "Wild":
        rep["BirthDate"] = "6/7/1957"
    full_bday = datetime.strptime(rep["BirthDate"], "%m/%d/%Y").date()
    bday = (full_bday.month, full_bday.day)
    same_bdays[bday].append(rep)

print("The representatives who share birthdays are:")
for bday, reps in sorted(same_bdays.items(), key=lambda x: len(x[1])):
    if len(reps) == 1:
        continue
    print(bday)
    for representative in reps:
        print("{Name} ({PartyId} - {StateId})".format(**representative))
    print()
