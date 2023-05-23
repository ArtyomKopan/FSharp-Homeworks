module Computers

open System

type OS = Windows | Linux | MacOS

type Computer(os: OS, neighbors: int list, ?probability: float) =
    let mutable mIsInfected = false
    
    member c.isInfected = mIsInfected
    
    member c.os = os
    
    member c.probability =
        match probability with
        | None -> match os with
                  | Windows -> 0.7
                  | Linux -> 0.3
                  | MacOS -> 0.2
        | Some p -> p
        
    member c.InfectComputer() = mIsInfected <- true
    
    member c.neighbors = neighbors
    
    
type Net(computers: Computer list) =
    let mutable mComputers: Computer list = computers
    
    let mutable mNewInfectedComputers: Computer list = []
    
    let randomNumberGenerator = Random()
    
    member n.computers = mComputers
    
    member n.newInfectedComputers = mNewInfectedComputers
    
    member n.AddInfectedComputer comp = mNewInfectedComputers <- comp :: mNewInfectedComputers
    
    member n.updateComputers newComputersList = mComputers <- newComputersList
    
    member n.InfectComputer ?number =
        let p = n.randomNumberGenerator.NextDouble()
        match number with
        | None ->
            let randomNumber = n.randomNumberGenerator.Next(0, List.length n.computers - 1)
            if (p <= mComputers[randomNumber].probability) then
                mComputers[randomNumber].InfectComputer()
        | Some num ->
            if (p <= mComputers[num].probability) then
                mComputers[num].InfectComputer()
            
    member n.Infect() =
        n.updateComputers (
            List.map (fun (comp: Computer) ->
                let currentIsInfected = comp.isInfected &&
                                        not (List.contains comp n.newInfectedComputers)
                match currentIsInfected with
                | false -> comp
                | true ->
                    let randomNumber = n.randomNumberGenerator.NextDouble()
                    if (randomNumber <= comp.probability) then
                        List.map (fun i ->
                                      if (not n.computers[i].isInfected) then
                                          mComputers[i].InfectComputer()
                                          n.AddInfectedComputer mComputers[i]
                                      i)
                                      comp.neighbors |> ignore
                        
                    comp
                )
                mComputers
            )
        mNewInfectedComputers <- []
        n.ShowNetState()
            
    member n.ShowNetState() =
        printf "__________________________\n"
        let length = List.length n.computers - 1
        List.map (fun i -> printfn $"{i} {n.computers[i].isInfected}")
                 (List.ofSeq (seq {0..length})) |> ignore
        