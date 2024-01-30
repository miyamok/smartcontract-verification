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

# Abstract syntax tree by <code>solc</code>
The official solidity compiler <code>solc</code> offers the option <code>--ast-compact-json</code> to output the abstract syntax tree in the JSON format.
Our verification system relies on this feature of <code>solc</code>.

## Conditionals
Solidity has two kinds of conditionals, namely, <code>if</code>-statement and the ternary expression <code>b ? t : f</code>.
In the <code>then</code> clause, the verification process carries on assuming the condition holds, while the negation of the condition is taken in the verification of the <code>else</code> clause.
One target of the verification is detecting an unreachable code segment.

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
The last substitution is out of bound, as the size of the array <code>nums</code> is 3.
This problem should be statically detected, and it is feasible due to known program analysis techniques.

# To do
- Explaining basic logic and the satisfiability problem
- Write about <code>require</code> and <code>assert</code>.
- Ocaml project for a Solidity parser, https://gitlab.com/o-labs/solidity-parser-ocaml
