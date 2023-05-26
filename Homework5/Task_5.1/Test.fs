open Computers
open FsUnit
open NUnit.Framework

[<Test>]
let test1() =
    let net = Net([
        Computer(Windows, [1], 1);
        Computer(Windows, [0; 2], 1);
        Computer(Windows, [1; 3], 1);
        Computer(Windows, [2], 1)
    ])
    net.InfectComputer 1
    net.Infect()
    net.Infect()
    List.contains false (List.map (fun (c: Computer) -> c.isInfected) net.computers)
        |> should equal false
        
[<Test>]
let test2() =
    let net = Net([
        Computer(Windows, [1; 2], 1);
        Computer(Linux, [0; 3; 4], 1);
        Computer(MacOS, [0; 5; 6], 1);
        Computer(Windows, [1], 1);
        Computer(Windows, [1], 1);
        Computer(Linux, [2], 1);
        Computer(Windows, [2], 1)
    ])
    net.InfectComputer 0
    net.Infect()
    net.Infect()
    List.contains false (List.map (fun (c: Computer) -> c.isInfected) net.computers)
        |> should equal false
        
[<Test>]
let test3() =
    let net = Net([
        Computer(Windows, [1; 2], 1);
        Computer(Linux, [0; 3; 4], 1);
        Computer(MacOS, [0; 5; 6], 1);
        Computer(Windows, [1], 1);
        Computer(Windows, [1], 1);
        Computer(Linux, [2], 1);
        Computer(Windows, [2], 1)
    ])
    net.InfectComputer 1
    net.Infect()
    net.Infect()
    net.Infect()
    List.contains false (List.map (fun (c: Computer) -> c.isInfected) net.computers)
        |> should equal false
        
[<Test>]
let test4() =
    let net = Net([
        Computer(Windows, [1; 2], 0);
        Computer(Linux, [0; 3; 4], 0);
        Computer(MacOS, [0; 5; 6], 0);
        Computer(Windows, [1], 0);
        Computer(Windows, [1], 0);
        Computer(Linux, [2], 0);
        Computer(Windows, [2], 0)
    ])
    net.InfectComputer 1
    net.Infect()
    net.Infect()
    net.Infect()
    List.contains true (List.map (fun (c: Computer) -> c.isInfected) net.computers)
        |> should equal false