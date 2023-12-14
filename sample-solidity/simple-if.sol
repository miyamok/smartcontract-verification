// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

contract SimpleIf {

    function foo(uint x) public pure returns (uint) {

        if (!(x==10)) {
            return 1;
        }

        if (x > 10 ? x % 2 == 1 : x % 3 == 0) {
            return 2;
        }

        if (x < 10 && (x % 2 == 0 ? x%3==0 : x % 5==1)) {
            return 3;
        }

        return 0;

    }
}
