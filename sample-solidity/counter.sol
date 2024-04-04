// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

contract Simpl {
    function id() public returns (uint) {
        uint x;
        x = 3;
        return x+1;
    }
}

contract Counter {
    uint public count = 0;
    event FunctionCalled();
    uint[] public counters;

    error CustomError();

    function f0(uint x) public returns (uint) {
        uint y = ++x + x; // x is used, hence there should be no warning.
        return y;
    }

    function f1(uint x) public returns (uint) {
        uint y = x++ + x; // x is not used after incrementing itself.
        return y;
    }

    function f2(uint x) public returns (uint) {
        uint y = x + ++x;
        return y;
    }

    function f3(uint x) public returns (uint) {
        uint y = x + x++;
        return y;
    }

    function f4(uint8 x) public returns (uint) {
        uint16 z;
        uint8 y = x++ + x; // here x is updated and will never be used later. 
        /* while (++y>0) {
            y -= 1;
            //x += y;
        } */
        z=y;
        counters[y] = 1;
        z+=y; // z is updated and won't be used.
        return y;
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