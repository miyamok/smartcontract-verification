# Smartcontract Verification
The objective of this project is to develop software to practice formal verification of smart contracts.
Due to the immutability of blockchain, the correctness of smart contract code is a crucial issue to secure crypto assets, because a flaw of a smart contract code makes itself vulnerable and may cause a serious financial loss which in general cannot be recoverd, once it has happened.
Formal verification allows us to check the smart contract code before its deployment.  As a result, if the implementation is secure it mathematically proves the correctness of the smart contract, otherwise it points out what (potential) problems are there.

The final goal, the correctness of smartcontract, consists of concrete pieces of correctness conditions, which for example includes:
+ No overflow, no underflow in arithmetic
+ No division by zero
+ No access to arrays exceeding the bounds
+ No transfers with insufficient balance
+ No reentrancy
+ No selfdestruct reacheability
<!-- + guaranteeing <code>assert</code> and <code>require</code> -->
