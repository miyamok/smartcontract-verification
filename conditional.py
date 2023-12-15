from z3 import *
import re

class Conditional:

    def setJson(self, json):
        self.json = json

    def findConditionalsFromJson(self):
        return [x for x in self.findConditionalsFromJsonAux(self.json) if x is not []]
    
    def findConditionalsFromJsonAux(self, json):
        if type(json) == list:
            return self.findConditionalsFromList(json)
        elif type(json) == dict:
            # print(json.keys())
            return self.findConditionalsFromDict(json)
        else:
            return []
    
    def findConditionalsFromDict(self, d):
        ret = []
        if 'nodeType' in d.keys() and d['nodeType'] == 'IfStatement':
            ret.append(self.conditionalClauseToSpecification(d))
        ## 'IfStatement' for if-then-else, and 'Conditional' for the ternary b ? t : f.
        ## If nodeType is 'IfStatement', this node carries 'condition', 'trueBody', and optionally 'falseBody'.
        ## If nodeType is 'Conditional', this node carries 'condition', 'trueExpression', and 'falseExpression'.
        else:
            ret += self.findConditionalsFromJsonAux([v for k, v in d.items()])
        # for k, v in d.items():
        #     if k == 'condition':
        #         ret.append(self.conditionalClauseToSpecification(v))
        #     else:
        #         ret += self.findConditionalsFromJsonAux(v)
        return ret
        # return [self.conditionalClauseToSpecification(v) if k == 'condition'
        #          else [ x for x in self.findConditionalsFromJsonAux(v) if x is not None]
        #          for k, v in d.items() ]
    
    def findConditionalsFromList(self, ls):
        return [ item for sublist in [self.findConditionalsFromJsonAux(l) for l in ls] for item in sublist if item != []]

    def conditionalClauseToSpecification(self, d):
        d['condition']
        return { key: d[key] for key in list({'condition', 'trueBody', 'falseBody', 'src'} & set(d.keys())) }
    
    def typeAndNameToAtomicTerm(self, t, n):
        if t == 'uint256':
            return Int(n)

    def expressionToTerm(self, c):
        if c['nodeType'] == 'Identifier':
            n = c['name']
            t = c['typeDescriptions']['typeString']
            return self.typeAndNameToAtomicTerm(t, n)
        elif c['nodeType'] == 'Literal':
            t = c['typeDescriptions']['typeString']
            ##print(c['typeDescriptions'])
            if re.match('int_const\s\d+', t):
                return c['value']
            else:
                print('new')
                print(t)
                print(c)
                return 0
        elif c['nodeType'] == 'UnaryOperation':
            sub = self.expressionToTerm(c['subExpression'])
            op = c['operator']
            if op == '!':
                return Not(sub)
            else:
                print ('unknown operator: ' + op + ' at src ' + c['src'])
                exit()

        elif c['nodeType'] == 'BinaryOperation':
            lft = self.expressionToTerm(c['leftExpression'])
            rht = self.expressionToTerm(c['rightExpression'])
            op = c['operator']
            if op == '%':
                return lft % rht
            elif op == '+':
                return lft + rht
            elif op == '-':
                return lft - rht
            elif op == '*':
                return lft * rht
            elif op == '/':
                return lft / rht
            elif op == '==':
                return lft == rht
            elif op == '&&':
                # print(lft)
                # print(rht)
                return And(lft, rht)
            elif op == '||':
                return Or(lft, rht)
            elif op == '==':
                return lft == rht
            elif op == '<':
                return lft < rht
            elif op == '>':
                return lft > rht
            elif op == '<=':
                ## check if <= is a correct one in solc AST
                return lft <= rht
            elif op == '>=':
                ## check if >= is a correct one in solc AST
                return lft >= rht
            else:
                print('unknown operator ' + op + ' at src ' + c['src'])
                exit()
        elif c['nodeType'] == 'TupleExpression':
            t = c['typeDescriptions']['typeString']
            components = [ self.expressionToTerm(e) for e in c['components'] ]
            if t == 'bool':
                return components[0]
            else:
                print('not yet supported 1')
                ##print(t)
                # print(c)
                exit()
        elif c['nodeType'] == 'Conditional':
            ## Ternary conditional expression
            #print(c['condition'].keys())
            #print(c['condition'])
            condExpr = self.expressionToTerm(c['condition'])
            trueExpr = self.expressionToTerm(c['trueExpression'])
            falseExpr = self.expressionToTerm(c['falseExpression'])
            return Or(And(condExpr, trueExpr), And(Not(condExpr), falseExpr))
        else:
            print('not yet supported 2')
            t = c['typeDescriptions']['typeString']
            print(t)
            print(c['nodeType'])
            print(c)
            exit()

    def conditionalClauseToFormula(self, c):
        condition = c['condition']
        # if 'trueBody' in condition.keys():
        condExpr = self.expressionToTerm(condition)
        # trueBody = c['trueBody']
        # if 'falseBody' in c.keys():
        #     falseBody = c['falseBody']
        return condExpr

    def conditionalClauseToSatFormulas(self, c):
        condition = c['condition']
        # if 'trueBody' in condition.keys():
        condExpr = self.expressionToTerm(condition)
        # trueBody = c['trueBody']
        # if 'falseBody' in c.keys():
        #     falseBody = c['falseBody']
        return [condExpr, Not(condExpr)]

    # def conditionToFormula(self, c):
    #     #print(c.items())
    #     #print(c.keys())
    #     #print(c['nodeType'])
    #     if c['nodeType'] in ['Identifier', 'Literal', 'TupleExpression']:
    #         return self.expressionToTerm(c)
    #     op = c['operator']
    #     nt = c['nodeType']
    #     if nt == 'UnaryOperation':
    #         sub = self.conditionToFormula(c['subExpression'])
    #         if op == '!':
    #             return Not(sub)
    #         else:
    #             print ('unknown operator: ' + op + ' at src ' + c['src'])
    #             exit()
    #     elif nt == 'BinaryOperation':
    #         lft = self.conditionToFormula(c['leftExpression'])
    #         rht = self.conditionToFormula(c['rightExpression'])
    #         if op == '&&':
    #             # print(lft)
    #             # print(rht)
    #             return And(lft, rht)
    #         elif op == '||':
    #             return Or(lft, rht)
    #         elif op == '==':
    #             return lft == rht
    #         elif op == '<':
    #             return lft < rht
    #         elif op == '>':
    #             return lft > rht
    #         elif op == '<=':
    #             ## check if <= is a correct one in solc AST
    #             return lft <= rht
    #         elif op == '>=':
    #             ## check if >= is a correct one in solc AST
    #             return lft >= rht
    #         else:
    #             print ('unknown operator: ' + op + ' at src ' + c['src'])
    #             exit()
    #     else:
    #         print ('unknown nodeType: ' + nt + ' at src ' + c['src'])
    #         exit()

    def booleanExpressionToFormula(self, e):
        pass

    def sat(self, e):
        s = Solver()
        s.add(e)
        return s.check()
