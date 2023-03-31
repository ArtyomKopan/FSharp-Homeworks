module Phone.Test

open NUnit.Framework
open Phone.Backend
open FsUnit

[<Test>]
let ``init phone list test``() =
    let phoneList = initPhoneList()
    phoneList |> should equal Map.empty<string, string>
    
let addTwoRecords() =
    let phoneList = initPhoneList()
    let phoneList' = phoneList |> addPhone "Artyom" "123"
    let phoneList'' = phoneList' |> addPhone "Noname" "234"
    phoneList''

[<Test>]
let ``add record test``() =
    let phoneList = initPhoneList()
    let phoneList' = phoneList |> addPhone "Artyom" "123"
    let phoneList'' = phoneList' |> addPhone "Noname" "234"
    let newMap = Map.ofList [("Artyom", "123"); ("Noname", "234")]
    phoneList'' |> should equal newMap
    
[<Test>]
let ``find phone test``() =
    let phoneList = addTwoRecords()
    let phone = phoneList |> getPhoneByName "Artyom"
    phone.Value |> should equal "123"
    
[<Test>]
let ``find name test``() =
    let phoneList = addTwoRecords()
    let name = phoneList |> getNameByPhone "234"
    name.Value |> should equal "Noname"
    
[<Test>]
let ``get all records test``() =
    let phoneList = addTwoRecords()
    getAllRecords phoneList |> should equal [("Artyom", "123"); ("Noname", "234")]
    
    