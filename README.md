This note is a preliminary memo for the project.

# Smartcontract Verification
The objective of this project is to develop software to practice formal verification of smart contracts.
Due to the immutability of blockchain, the correctness of smart contract code is a crucial issue to secure crypto assets, because a flaw of a source code makes the smart contract vulnerable. 
 It may cause a serious financial loss which in general cannot be recoverd, once it has happened.
Formal verification allows us to check the smart contract code before its deployment.  As a result, if the implementation is secure it mathematically proves the correctness of the smart contract, otherwise it points out what (potential) problems are there.

The final goal, the correctness of smartcontract, consists of pieces of correctness conditions, which for example includes:
+ No overflow, no underflow in arithmetic
+ No division by zero
+ No access to arrays exceeding the bounds
+ No unreachable code
+ No transfers with insufficient balance
+ No reentrancy
+ No selfdestruct reacheability
<!-- + guaranteeing <code>assert</code> and <code>require</code> -->

## Features of the Solidity compiler
The solidity compiler <code>solc</code> offers helpful features for preventing potential problems of smart contracts.
Byte code compiled by <code>solc</code> version 0.8.4 or above equips the overflow checking, that is, when there was a runtime arithmetic overflow, it is automatically detected and the execution of the contract gets reverted.  In contract to it, the older <code>solc</code> did not offer this feature, and hence the runtime overflow could lead an unrecoverable failure such as a permanent loss of assets.

# Abstract syntax tree by <code>solc</code>
The official solidity compiler <code>solc</code> offers the option <code>--ast-compact-json</code> to output the abstract syntax tree in the JSON format.
Our verification system relies on this feature of <code>solc</code>.

## Conditionals
Solidity has two kinds of conditionals, namely, <code>if</code>-statement and the ternary expression <code>b ? t : f</code>.
In the <code>then</code> clause, the verification process carries on assuming the condition holds, while the negation of the condition is taken in the verification of the <code>else</code> clause.
One target of the verification is detecting an unreachable code segment.

## Functions

A function definition comes inside a contract definition and it involves specifications for its name, parameters, return values, modifiers, and its body.
```
function f(uint8 x, uint8 y) public pure returns (uint16) {
    uint16 ret;
    // ...
    // ...
    return ret;
}
```
The above function has the name <code>f</code>, the parameter list contains two parameters <code>uint8 x, uint8 y</code>, the return type list contains <code>uint16</code>, which is a singleton list for this particular example, and the modifier <code>public pure</code> which means that this function is publically accessible and doesn't access storage data, neither reading nor writing.
Alternatively the return variable list can be used instead of the type list, specifying 
```
function f(uint8 x, uint8 y) public pure returns (uint16 z) {
    z = x+y;
}
```
The variable <code>z</code> of type <code>uint16</code> is supporsed to have a return value, assigned through the execution of the body of the function, <code>x+y</code> for this case.  Explicit <code>return</code> has a priority over the specified retuen variable name <code>z</code>. 
```
function f(uint8 x, uint8 y) public pure returns (uint16 z) {
    z = 16;
    return x+y;
}
```
The result of <code>f(4, 3)</code> from the above definition is </code>7</code>, not <code>16</code>.
This priority is same for functions returning several values with an incomplete variable name specification.
<code>solc</code> (I tested on remix solc 0.8.24) compiles code containing the following function definition.
```
function add(uint x_, uint y_) internal pure returns (uint z, uint) {
    return (x_ + y_, 10);
}
```
Note that the first element of the return values comes with a variable name but the second one is without a variable name.
This function returns <code>(8, 10)</code> for <code>add(3, 5)</code>, while <code>z</code> should have the default value <code>0</code>.
In case <code>return</code> is missing as follows, <code>solc</code> gives a warinng message but it anyway compiles,
```
function add(uint x_, uint y_) internal pure returns (uint z, uint) {
    z = x_ + y_;
}
```
and the function call <code>add(3, 1)</code> gives <code>(4, 0)</code>, filling the default value <code>0</code> of uint for the second return value.
### Examples
Assume a conditional statement (namely, if-then-else) with a boolean expression <code>b</code> (namely, <code>if (b) { ... }</code>), then the execusion reaches the <code>else</code> clause if not <code>b</code> holds.
If either <code>b</code> or not <code>b</code> is unsatisfiable, the <code>then</code> clause or the <code>else</code> clause is never executed, namely, is unreachable.
The verifier issues a warning message respecting such an unreachable code.

In case a conditional statement is nested, the verification gets more complex.  Assume the following code.
```
if (b1) {
  f1();
  if (b2) {
    g1();
  } else {
    g2();
  }
} else {
  f2();
}
```
The reachability of <code>g2()</code> is checked due to the satisfiability of <code>b1 && !b2</code>.
Another example involving the ternary expression is
```
if (b1 && (x == (b2 ? y : z)) {
  f1();
} else {
  f2();
}
```
The reachability of <code>y</code> is checked by the satisfiability of <code>b1 && b2</code>, and the one of <code>z</code> is by <code>b1 && !b2</code>
On the other hand, the reachability of <code>f1()</code> is checked due to the satisfiability of <code>b1 && ((b2 && x==y) || (!b2 && x==z))</code>, and by its negation, the one of <code>f2()</code> is checked.

## Arrays
Solidity offers both statically sized arrays and dynamically sized arrays.

### Examples
Consider the following code, assuming solidity version is 0.8.20 or older.
```
uint256[3] nums;
uint256 x = 3;

nums[0] = 20;
nums[1] = 50;
nums[2] = 121;
nums[x] = 61;
```
The last substitution is out of bound, as the size of the array <code>nums</code> is 3, hence it causes a run-time error.
This problem should be statically detected, and it is feasible due to known program analysis techniques.

## Struct
Variables ranging over array and struct has a modifier such as memory, storage, or calldata, in order to indicate how those data are kept.
Data on memory are not permanent, and changes are lost, as an example below, when the execution got out from the function scope.
```
function processData(account a) external {
    Data memory d = getData(a);
    d.processed = true;
}
```
A concrete case is found in the tutorial by Alchemy University (https://university.alchemy.com/overview/solidity), Section 3, Reference Types, Structs, around 20:00 in the video lecture.
The problem shown in the lecture video is reproduced by modifying <code>storage</code> to <code>memory</code> at the line 34 of Example.sol in https://github.com/alchemyplatform/learn-solidity-presentations/blob/main/7-structs/examples/0-playing-with-structs/src/Example.sol

# Tips

## Static Verification Feature of Solidity Compiler solc

solc has its own SMTChecker feature for compile time verification.
```
% solc test.sol --model-checker-targets all --model-checker-timeout 1000 --model-checker-solvers z3  --model-checker-engine chc
```
In order to enable this feature, one use Linux for dynamic library loading (for z3 etc) or otherwise one has to re-compile the solc compiler with static library linking.
Cf. https://github.com/ethereum/solidity/issues/14014

## hardhat
The following setup procedure is successsful
```
% mkdir myProject && cd myProject
% npm install --save-dev hardhat
% npm install @nomiclabs/hardhat-waffle ethereum-waffle chai @nomiclabs/hardhat-ethers ethers@5.7.2 dotenv
% npm init -y
% npx hardhat
```
The version of ethers is specified to be 5.7.2, because the latest version has an issue for smart contract deployment. (cf. https://ethereum.stackexchange.com/questions/144451/typeerror-cannot-read-properties-of-undefined-reading-jsonrpcprovider)
The above installation works if @5.7.2 is missing.
One should think about changing the ethers version in case there is a trouble such as below during the deployment.
```
% npx hardhat run scripts/deploy.js
TypeError: Cannot read properties of undefined (reading 'JsonRpcProvider')
    at main (/tmp/myProject/scripts/deploy.js:10:41)

## The corresponding line of deploy.js:
## const provider = new ethers.providers.JsonRpcProvider(url);
```

The following is an unsuccessful procedure (to figure out why)
```
% mkdir myProject && cd myProject
% npm init -y
% npm install --save-dev hardhat
% npm install @nomiclabs/hardhat-waffle ethereum-waffle chai @nomiclabs/hardhat-ethers ethers@5.7.2 dotenv
% npx hardhat
```

## chai.js@^5.0.0 with hardhat

In case there is an error such as follows,
```
% npx hardhat test
Downloading compiler 0.8.4
Compiled 1 Solidity file successfully (evm target: istanbul).
An unexpected error occurred:

Error [ERR_REQUIRE_ESM]: require() of ES Module /tmp/node_modules/chai/chai.js from /tmp/myProject/test/sample-test.js not supported.
Instead change the require of chai.js in /tmp/myProject/test/sample-test.js to a dynamic import() which is available in all CommonJS modules.
    at Object.<anonymous> (/tmp/myProject/test/sample-test.js:2:28) {
  code: 'ERR_REQUIRE_ESM'
}
```
downgrading chai.js to version 4 is a cure.
```
% npm install chai@4.3.7
```
Simply following the suggested solution by the error message, using import instead of require, does make another error.
```
TypeError: Cannot read properties of undefined (reading 'equal')
      at Context.<anonymous> (test/sample-test.js:21:12)
```
It is further to investigate why the suggestion doesn't work.

chai.js@5 is still new and seems not very stable.  A discussion is found at https://github.com/chaijs/chai/issues/1561 .

# To do
- Explaining basic logic and the satisfiability problem
- Write about <code>require</code> and <code>assert</code>.
- Ocaml project for a Solidity parser, https://gitlab.com/o-labs/solidity-parser-ocaml
