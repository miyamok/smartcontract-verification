// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

contract SimpleIf {

    function foo(uint x) public pure returns (uint) {
        uint y = 100;

        if (!(x==10)) {
            // What to check is the satisfiability of
            // 1. Not(x==10)
            // 2. Not(Not(x==10))
            return 1;
        } else {
            y += 100;
        }
        // Here the true case of the above conditional has return inside,
        // hence we assume (Not (Not (x==10))) in the rest of analysis.
        if (!(x==10)) {
            return 5;
        }

        // Here the true case of the above conditional has return inside,
        // hence we assume (Not (Not (x==10))) in the rest of analysis.
        if (x > 10 ? x % 2 == 1 : x % 3 == 0) {
            return 2;
        }

        if (x < 10 && (x % 2 == 0 ? x%3==0 : x % 5==1)) {
            return 3;
        }

        //  Obviously nonsense condition, as it is always true
        if (x < 10 || x >= 10) {
            return 4;
        }
        return x;

    }
}
