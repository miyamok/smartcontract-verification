import reader
import conditional

r = reader.ReaderSolc()
##r.load("sample-solidity/if-then-else.sol")
r.load("sample-solidity/simple-if.sol")

c = conditional.Conditional()
c.setJson(r.toJson())
out = c.findConditionalsFromJson()
print(out[0].keys())
print(out[1].keys())
print(out[2].keys())
for i in range(len(out)):
    ##res = c.conditionToFormula(out[i]['condition'])
    res = c.conditionalClauseToSatFormulas(out[i])
    for elem in res:
        print (elem)
