# Solidity Verification
The objective of this project is to develop software to practice formal verification of Solidity smart contracts.
Due to the immutability of blockchain, the correctness of smart contract code is a crucial issue to secure crypto assets, because a flaw of a source code makes the smart contract vulnerable. 
It may cause a serious financial loss which in general cannot be recoverd, once it has happened.
Formal verification allows us to check the smart contract code before its deployment.  As a result, if the implementation is secure it mathematically proves the correctness of the smart contract, otherwise it points out what (potential) problems are there.

The final goal, the correctness of smartcontract, consists of pieces of correctness conditions, which for example includes:
+ No overflow, no underflow in arithmetic
+ No division by zero
+ No access to arrays exceeding the bounds
+ No unreachable code
+ No transfers with insufficient balance
+ No reentrancy vulnerability
+ No selfdestruct reacheability
<!-- + guaranteeing <code>assert</code> and <code>require</code> -->

# Usage
Haskell (GHC 9.2.8 checked to work), cabal (3.6.2.0 checked to work), Solidity compiler (solc 0.8.23 checked to work) are required.
Download the source code from the repository or use git as follows
```
$ git clone https://github.com/miyamok/smartcontract-verification/
```
then move to the directory <code>smartcontract-verification</code> and issue
```
$ cabal run solidity-verification PATH/TO/YOUR/SOLIDITYPROGRAM.sol
```

# Implemented features

## Definition-use analysis
Definition-use analysis is to find from a definition its use.
Here a definition means a value assignment to a variable, and a use means any operation referring to the assigned value.
For example, in the following pseudo-code,
```
String greeting = "Hello, world"
print greeting
```
The first line is a definition of <code>greeting</code> and its use is found in the following line.

In case of Solidity, this analysis is useful to find a particular type of bug due to the memory/storage distinction.

### Example
Here we discuss an example solidity program [structs.sol](sample-solidity/structs.sol).
It is a smart contract for an online shop where the functionalities dealing with orders and payments are implemented.

The state variable <code>orders</code> is array of <code>Order</code>.  It is a parsistent data and meant to record orders.

The function <code>payment</code> has a local variable <code>order</code> of struct type <code>Order</code>.
As this is a <code>storage</code> variable, the initial assignment makes <code>order</code> point to <code>orders[key]</code>, and future modification to <code>order</code> actually modified the state variable <code>orders</code>.
The following lines check that
+ the caller of this function is indeed the buyer,
+ the caller of this function pays exactly the price of the order, and
+ the status of the order indicates that the payment is still awaited.

The last step of the function is the assignment.
As described above, the local variable <code>order</code> is pointing to <code>orders[key]</code>, and <code>orders[key].status</code> gets the new value <code>OrderStatus.Paid</code>.
This change is persistent, and in the future one can see that the payment has been completed.

Another function <code>paymentForgetful</code> has a problem, although it looks similar as <code>payment</code>.
The difference is that the local variable <code>order</code> is declared as a <code>memory</code> variable, that means its initial value <code>orders[key]</code> is copied and future change via this local variable <code>order</code> affects this copy.  At the end of this function, the assignment changes this copy, which is obviously diverged from the state variable, and this change is lost when the transaction is over.  In the future, the order status still indicates unpaid even though the buyer has already paid!

Our tool gives a warning message concerning the local memory variable <code>order</code> as follows.
```
$ cabal run solidity-verification sample-solidity/structs.sol
In function paymentForgetful the following variables are not read after an assignment
"order" declared at line 44
```
This suggests that there is a potential issue, as there is a meaningless assigment.

# Semantics
In this section, we describe what issues will be clarified as a result of studying semantics of Solidity.

Solidity's syntax looks similar as Java and Javascript, however the following simple arithmetical example shows that Solidity's semantics is different from the others.
```
int x=0;
int y=x + ++x;
return y;
```
While in Javascript (where we should write var instead of int) the return value is 1, in Solidity it is 2.
In Javascript the evaluation goes from the left hand side of "+" to the right hand side.  Hence the left hand side of "+" is 0 and the right hand side is 1, then finally the value of y is 1.
In Solidity the evaluation goes in the opposite direction.  The right hand side of "+" is 1, and then the left hand side is 1, therefore the final value of y is 2.

Although the above coding practice is not at all recommended, it is true that semantics is a delicate issue in formal verification.
Studying formal semantics of Solidity is a crucial step to develop reliable formal verification methods.

Let's see arithmetical examples a bit more, which are understood as a consequence of the above observation.
```
int x=0;
x += x++;
int y=0;
y += ++y;
```
While in Javascript (again, use var instead of int in Javascript) the values of x and y become 0 and 1, in Solidity the values of x and y become 1 and 2, respectively.
Although the meaning of <code>a += b</code> is <code>a = a + b</code>, that is same in the both languages, the computation order makes the results different.
```
int x=0;
x -= ++x;
```
The result x in Solidity is 0, while it is -1 in Javascript.

The tuple allows the following code in Solidity.
```
uint x = 0;
uint y;
uint z;
(y,z) = (x, ++x);
```
The values of y and z are respectively 0 and 1.  The evaluation goes left to right in the case of tuple, and function arguments work in the same way.

There is another issue concerning the memory/storage distinction.
Assume <code>Person</code> is a sturuct already defined, and there is a state variable <code>vip</code> of type <code>Person</code>.
```
Person memory p = vip;
```
Here, <code>p</code> gets the copy of data kept by <code>vip</code>.  On the other hand, in the followng code, <code>p</code> gets a storage location of the state variable <code>vip</code>.
```
Person storage p = vip;
```
Although there can be several different formal semantics of Solidity language, one possible interpretation of the above example is that the right hand side of the assignment, <code>vip</code>, denotes something different between the two.
In the first example, <code>vip</code> denotes the copy of the data which we can refer by <code>vip</code>, and in the second, <code>vip</code> denotes the storage location of itself.
<!-- # To do -->
<!--
- Explaining basic logic and the satisfiability problem
- Write about <code>require</code> and <code>assert</code>.
- Ocaml project for a Solidity parser, https://gitlab.com/o-labs/solidity-parser-ocaml
-->

# Miscellaneous memorandum

The rest of this page is a memorandum for the project.

## Features of the Solidity compiler
The solidity compiler <code>solc</code> offers helpful features for preventing potential problems of smart contracts.
Byte code compiled by <code>solc</code> version 0.8.4 or above equips the overflow checking, that is, when there was a runtime arithmetic overflow, it is automatically detected and the execution of the contract gets reverted.  In contract to it, the older <code>solc</code> did not offer this feature, and hence the runtime overflow could lead an unrecoverable failure such as a permanent loss of assets.

## Abstract syntax tree by <code>solc</code>
The official solidity compiler <code>solc</code> offers the option <code>--ast-compact-json</code> to output the abstract syntax tree in the JSON format.
Our verification system relies on this feature of <code>solc</code>.
The description of the solidity AST is available as https://solidity-ast.netlify.app/ .

### LValue

In Solidity AST we often see information concerning LValue, that is for instance <code>isLValue</code> and <code>lValueRequested</code>.
An expression is LValue if and only if it is a variable or something that can be assigned to.

For example, assume we have an expression <code>something.property</code> in Solidity.
In case it is an enum, the <code>isLValue</code> of <code>something</code> is falase.  On the other hand if it is a variable of struct type, the <code>isLValue</code> is true.

- https://www.gnu.org/software/c-intro-and-ref/manual/html_node/Lvalues.html
- https://docs.soliditylang.org/en/latest/types.html#compound-and-increment-decrement-operators
- https://www.learncpp.com/cpp-tutorial/value-categories-lvalues-and-rvalues/

### Conditionals
Solidity has two kinds of conditionals, namely, <code>if</code>-statement and the ternary expression <code>b ? t : f</code>.
In the <code>then</code> clause, the verification process carries on assuming the condition holds, while the negation of the condition is taken in the verification of the <code>else</code> clause.
One target of the verification is detecting an unreachable code segment.

### Functions

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
and the function call <code>add(3, 1)</code> gives <code>(4, 0)</code>, filling the default value <code>0</code> of uint for the second return value.  The same happens when the execution did not come to <code>return</code> even if there is <code>return</code> in the function body.  (a presence of conditional makes such a case probable.)

#### Examples
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

### Arrays
Solidity offers both statically sized arrays and dynamically sized arrays.

#### Examples
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

### Struct
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

### <code>return</code>

The <code>nodeType</code> of <code>return</code> in the AST output of solc is <code>"Return"</code>, while in the grammer the corresponding rule is return-statement (cf. https://docs.soliditylang.org/en/latest/grammar.html#a4.SolidityParser.returnStatement).
The AST output is in principle following the grammer definition, just using the CamelCase instead of the hyphen-separation, but it's not always the case and should be checked through the actual output AST from solc.

### assert, require, revert

assert and require are always function, so the nodeType of those statements are ExpressionStatement.
revert in Solidity ^0.8.0 is either a function or a statement.  In case it comes with a string argument such as
```
revert "Error.  Abort.";
```
it is a function call and the nodeType of this statement is ExpressionStatement.
If revert takes an Error object,
```
revert MyCustomError("Error, Aborted.", 121731);
```
the nodeType of this statement is RevertStatement.

## Proxy contract

<code>delegatecall</code> is a function available for Contract.  It is to call a function of some contract on chain, delegating to the contract the parameters, such as msg.sender, the context etc.  A typical use case of this feature is to give a possibility of upgrading of smart contracts.  In the simplest scenario, Instead of a single smart contract, one creates two contracts, they are so-called the logic contract and the proxy contract.  The logic contract is in charge of the implementation of a business logic which could be upgraded in the future.  The proxy contract has a state variable which keeps the address of the implementation contract, and it invokes the corresponding business logic via <code>delegatecall</code>.  Upgrading of the business logic is done in two steps: deploying the new logic contract and changing the state variable of the proxy contract so that it points to the new contract.

### Question
What happens if the implementation has been changed after the moment one called the proxy and before the moment the contract is actually executed.  If this change of the implementation is not detected, the contract can run differently from what is expected at the time to initiate the transaction, that arise a security concern.

## Tips

### Static Verification Feature of Solidity Compiler solc

solc has its own SMTChecker feature for compile time verification.
```
% solc test.sol --model-checker-targets all --model-checker-timeout 1000 --model-checker-solvers z3  --model-checker-engine chc
```
In order to enable this feature particularly by z3, one use Linux for dynamic library loading (libz3.so) or otherwise one has to re-compile the solc compiler with static library linking.
Cf. https://github.com/ethereum/solidity/issues/14014

The option --model-checker-print-query is used to show the smtlib2 model of the contract, eg. by
```
solc overflow.sol --model-checker-solvers smtlib2 --model-checker-targets overflow --model-checker-print-query
```

### hardhat
The following setup procedure is successsful
```
% mkdir myProject && cd myProject
% npm install --save-dev --legacy-peer-deps hardhat
% npm install --legacy-peer-deps @nomiclabs/hardhat-waffle ethereum-waffle chai @nomiclabs/hardhat-ethers ethers@5.7.2 dotenv
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

### chai.js@^5.0.0 with hardhat

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
