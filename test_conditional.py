import reader
import conditional
from optparse import OptionParser
import os

r = reader.ReaderSolc()
parser = OptionParser()
(options, args) = parser.parse_args()
##r.load("sample-solidity/if-then-else.sol")
if len(args) != 1:
    print("one argument required for the input file name.  abort.")
    exit()
filepath = args[0]
if not os.path.exists(filepath):
    print(f"{filepath} doesn't exist.  abort.")
    exit()
r.load(filepath)

c = conditional.Conditional()
c.setJson(r.toJson())
out = c.findConditionalsFromJson()

for i in range(len(out)):
    res = c.conditionalClauseToSatFormulas(out[i])
    for elem in res:
        print (elem)
        print (c.sat(elem))
