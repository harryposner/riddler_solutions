# Problem statement

[Original article](https://fivethirtyeight.com/features/can-you-escape-this-enchanted-maze/)

There are 48 contiguous states in the United States. Each of these states shares
at least one border with another state. You are planning a road trip in order to
visit as many states as possible. The only restriction is that you can only
cross a border from one state to another one time. You can visit a state any
number of times, as long as you do not enter or leave it from a border you have
already crossed.

How many states can you visit without crossing the same border twice?


# My solution

This path visits each of the lower 48 exactly once:

ME - NH - VT - MA - RI - CT - NY - NJ - PA - DE - MD - VA - NC - SC - GA - FL -
AL - MS - TN - KY - WV - OH - MI - IN - IL - WI - MN - IA - MO - AR - LA - TX -
OK - KS - NE - SD - ND - MT - WY - CO - NM - AZ - UT - ID - NV - CA - OR - WA


But since this is a road trip, we probably want to do a loop.  It's impossible
to visit each of the lower 48 and end in the same state where you started, since
you'd have to cross the Maine-New Hampshire border twice.  This circuit skips
Maine and visits New York twice:

MA - NH - VT - **NY** - PA - WV - KY - OH - MI - IN - IL - WI - MN - IA - MO -
AR - OK - KS - NE - SD - ND - MT - WY - CO - UT - NV - ID - WA - OR - CA - AZ -
NM - TX - LA - MS - TN - AL - FL - GA - SC - NC - VA - MD - DE - NJ - **NY** -
CT - RI - MA
