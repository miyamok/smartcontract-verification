// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

contract IfElse {
    function foo(uint x) public pure returns (uint) {
        if (x < 10) {
            return 0;
        } else if (x < 20) {
            return 1;
        } else {
            return 2;
        }
    }

    function ternary(uint _x) public pure returns (uint) {
        // if (_x < 10) {
        //     return 1;
        // }
        // return 2;

        // shorthand way to write if / else statement
        // the "?" operator is called the ternary operator
        return _x < 10 ? 1 : 2;
    }

    function bar(uint x, uint y) public pure returns (uint) {
        if (x > 0) {
            if (y > 5) {
                return 10;
            } else if (y < 10) {
                return 10;
            } else {
                return 0;
            }
        } else if (x < 0) {
            if (y>5) {
                return 10;
            } else if (y<10) {
                return 5;
            } else {
                return 0;
            }
        } else {
            return 0;
        }
    }
}

/*
{'children': [{'name': 'solidity',
               'type': 'PragmaDirective',
               'value': '^0.8.20'},
              {'baseContracts': [],
               'kind': 'contract',
               'name': 'IfElse',
               'subNodes': [{'body': {'statements': [{'FalseBody': {'FalseBody': {'statements': [{'number': '2',
                                                                                                  'subdenomination': None,
                                                                                                  'type': 'NumberLiteral'}],
                                                                                  'type': 'Block'},
                                                                    'TrueBody': {'statements': [{'number': '1',
                                                                                                 'subdenomination': None,
                                                                                                 'type': 'NumberLiteral'}],
                                                                                 'type': 'Block'},
                                                                    'condition': {'left': {'name': 'x',
                                                                                           'type': 'Identifier'},
                                                                                  'operator': '<',
                                                                                  'right': {'number': '20',
                                                                                            'subdenomination': None,
                                                                                            'type': 'NumberLiteral'},
                                                                                  'type': 'BinaryOperation'},
                                                                    'type': 'IfStatement'},
                                                      'TrueBody': {'statements': [{'number': '0',
                                                                                   'subdenomination': None,
                                                                                   'type': 'NumberLiteral'}],
                                                                   'type': 'Block'},
                                                      'condition': {'left': {'name': 'x',
                                                                             'type': 'Identifier'},
                                                                    'operator': '<',
                                                                    'right': {'number': '10',
                                                                              'subdenomination': None,
                                                                              'type': 'NumberLiteral'},
                                                                    'type': 'BinaryOperation'},
                                                      'type': 'IfStatement'}],
                                      'type': 'Block'},
                             'isConstructor': False,
                             'isFallback': False,
                             'isReceive': False,
                             'modifiers': [],
                             'name': 'foo',
                             'parameters': {'parameters': [{'isIndexed': False,
                                                            'isStateVar': False,
                                                            'name': 'x',
                                                            'storageLocation': None,
                                                            'type': 'Parameter',
                                                            'typeName': {'name': 'uint',
                                                                         'type': 'ElementaryTypeName'}}],
                                            'type': 'ParameterList'},
                             'returnParameters': {'parameters': [{'isIndexed': False,
                                                                  'isStateVar': False,
                                                                  'name': None,
                                                                  'storageLocation': None,
                                                                  'type': 'Parameter',
                                                                  'typeName': {'name': 'uint',
                                                                               'type': 'ElementaryTypeName'}}],
                                                  'type': 'ParameterList'},
                             'stateMutability': 'pure',
                             'type': 'FunctionDefinition',
                             'visibility': 'public'},
                            {'body': {'statements': [{'FalseExpression': {'number': '2',
                                                                          'subdenomination': None,
                                                                          'type': 'NumberLiteral'},
                                                      'TrueExpression': {'number': '1',
                                                                         'subdenomination': None,
                                                                         'type': 'NumberLiteral'},
                                                      'condition': {'left': {'name': '_x',
                                                                             'type': 'Identifier'},
                                                                    'operator': '<',
                                                                    'right': {'number': '10',
                                                                              'subdenomination': None,
                                                                              'type': 'NumberLiteral'},
                                                                    'type': 'BinaryOperation'},
                                                      'type': 'Conditional'}],
                                      'type': 'Block'},
                             'isConstructor': False,
                             'isFallback': False,
                             'isReceive': False,
                             'modifiers': [],
                             'name': 'ternary',
                             'parameters': {'parameters': [{'isIndexed': False,
                                                            'isStateVar': False,
                                                            'name': '_x',
                                                            'storageLocation': None,
                                                            'type': 'Parameter',
                                                            'typeName': {'name': 'uint',
                                                                         'type': 'ElementaryTypeName'}}],
                                            'type': 'ParameterList'},
                             'returnParameters': {'parameters': [{'isIndexed': False,
                                                                  'isStateVar': False,
                                                                  'name': None,
                                                                  'storageLocation': None,
                                                                  'type': 'Parameter',
                                                                  'typeName': {'name': 'uint',
                                                                               'type': 'ElementaryTypeName'}}],
                                                  'type': 'ParameterList'},
                             'stateMutability': 'pure',
                             'type': 'FunctionDefinition',
                             'visibility': 'public'}],
               'type': 'ContractDefinition'}],
 'type': 'SourceUnit'}
*/