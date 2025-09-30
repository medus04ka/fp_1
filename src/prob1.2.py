import re
from decimal import *

def displaymatch(match):
    if match is None:
        return 0
   
    return len(match.group(1))
    
                                  
length = 0
maxlen = 0
d = 0
getcontext().prec = 2000

for i in range(1,1000):
    fraction = 1/Decimal(i)
    pattern = re.search(r"^[0-9]\.[0-9]*([0-9]{7,}?)(\1+)[0-9]*?$", str(fraction))
    
    length = displaymatch(pattern)

    if length > maxlen:
        maxlen = length
        d = i

print(d)