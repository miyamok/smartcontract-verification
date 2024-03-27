// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

contract Counter {
    uint public count = f(1)==1 ? 4 : 0;
    event FunctionCalled();
    uint[] public counters;

    error CustomError();
    function f(uint8 x) public returns (uint16 z) {
        uint y=x++;
        while (++y>0) {
            y -= 1;
            //x += y;
        }
        z=x+1;
        counters[x] += z;
        z+=x;
    }

    // Function to get the current count
    function get() public view returns (uint) {
        return count;
    }

    // Function to increment count by 1
    function inc() public {
        count += 1;
    }

    // Function to decrement count by 1
    function dec() public {
        // This function will fail if count = 0
        count -= 1;
    }
}

// taken from https://solidity-by-example.org/first-app/

/*
{'children': [{'name': 'solidity',
               'type': 'PragmaDirective',
               'value': '^0.8.20'},
              {'baseContracts': [],
               'kind': 'contract',
               'name': 'Counter',
               'subNodes': [{'initialValue': None,
                             'type': 'StateVariableDeclaration',
                             'variables': [{'expression': None,
                                            'isDeclaredConst': False,
                                            'isIndexed': False,
                                            'isStateVar': True,
                                            'name': 'count',
                                            'type': 'VariableDeclaration',
                                            'typeName': {'name': 'uint',
                                                         'type': 'ElementaryTypeName'},
                                            'visibility': 'public'}]},
                            {'body': {'statements': [{'name': 'count',
                                                      'type': 'Identifier'}],
                                      'type': 'Block'},
                             'isConstructor': False,
                             'isFallback': False,
                             'isReceive': False,
                             'modifiers': [],
                             'name': 'get',
                             'parameters': {'parameters': [],
                                            'type': 'ParameterList'},
                             'returnParameters': {'parameters': [{'isIndexed': False,
                                                                  'isStateVar': False,
                                                                  'name': None,
                                                                  'storageLocation': None,
                                                                  'type': 'Parameter',
                                                                  'typeName': {'name': 'uint',
                                                                               'type': 'ElementaryTypeName'}}],
                                                  'type': 'ParameterList'},
                             'stateMutability': 'view',
                             'type': 'FunctionDefinition',
                             'visibility': 'public'},
                            {'body': {'statements': [{'expression': {'left': {'name': 'count',
                                                                              'type': 'Identifier'},
                                                                     'operator': '+=',
                                                                     'right': {'number': '1',
                                                                               'subdenomination': None,
                                                                               'type': 'NumberLiteral'},
                                                                     'type': 'BinaryOperation'},
                                                      'type': 'ExpressionStatement'}],
                                      'type': 'Block'},
                             'isConstructor': False,
                             'isFallback': False,
                             'isReceive': False,
                             'modifiers': [],
                             'name': 'inc',
                             'parameters': {'parameters': [],
                                            'type': 'ParameterList'},
                             'returnParameters': [],
                             'stateMutability': None,
                             'type': 'FunctionDefinition',
                             'visibility': 'public'},
                            {'body': {'statements': [{'expression': {'left': {'name': 'count',
                                                                              'type': 'Identifier'},
                                                                     'operator': '-=',
                                                                     'right': {'number': '1',
                                                                               'subdenomination': None,
                                                                               'type': 'NumberLiteral'},
                                                                     'type': 'BinaryOperation'},
                                                      'type': 'ExpressionStatement'}],
                                      'type': 'Block'},
                             'isConstructor': False,
                             'isFallback': False,
                             'isReceive': False,
                             'modifiers': [],
                             'name': 'dec',
                             'parameters': {'parameters': [],
                                            'type': 'ParameterList'},
                             'returnParameters': [],
                             'stateMutability': None,
                             'type': 'FunctionDefinition',
                             'visibility': 'public'}],
               'type': 'ContractDefinition'}],
 'type': 'SourceUnit'}
 */