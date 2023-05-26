module Phone.Frontend

open System
open Phone.Backend
open System.IO

let readPhonesFromFile file phoneList =
    // file содержит строки вида "name phone"
    let allRecords = Array.toList (File.ReadAllLines(file)) 
    let newPhones = allRecords |>
                    List.map (fun s -> ((s.Split " ")[0], (s.Split " ")[1]))
    let oldPhones = phoneList |> getAllRecords
    Map.ofList (oldPhones @ newPhones)
    
let writePhonesToFile file phoneList =
    let records = phoneList |> getAllRecords |>
                  List.map (fun (key, value) -> key + " " + value)
    File.WriteAllLines(file, records)
    
let printAllPhones phoneList =
    let records = phoneList |> getAllRecords |>
                  List.map (fun (key, value) -> key + " " + value)
    records |> List.map (fun s ->
        printfn $"{s}"
        s)
    
let main() =
    printfn "Вам доступны следующие команды:
             Exit -- выйти
             Add <name> <phone> -- добавить имя и телефон
             ReadFromFile <filename> -- считать контакты из файла
             WriteToFile <filename> -- записать все контакты в файл
             FindPhone <name> -- найти телефон по имени
             FindName <phone> -- найти имя по телефону
             PrintAllPhones -- напечатать все имена и телефоны в консоль"
    
    let phoneList = initPhoneList()
    
    let rec executeCommand phoneList =
        printf ">>> "
        let args = Console.ReadLine().Split " "
        let command = args[0].ToLower()
        match command with
            | "exit" -> true
            | "add" ->
                let name = args[1]
                let phone = args[2]
                executeCommand (addPhone name phone phoneList)
            | "readfromfile" ->
                let file = args[1]
                executeCommand (readPhonesFromFile file phoneList)
            | "writetofile" ->
                let file = args[1]
                writePhonesToFile file phoneList
                executeCommand phoneList
            | "findphone" ->
                let name = args[1]
                let phone = getPhoneByName name phoneList
                if phone.IsSome then
                   printfn $"{phone.Value}"
                else
                    printfn "Такого имени нет в списке контактов!"
                executeCommand phoneList
            | "findname" ->
                let phone = args[1]
                let name = getNameByPhone phone phoneList
                if name.IsSome then
                    printfn $"{name.Value}"
                else
                    printfn "Такого телефона нет в списке контактов!"
                executeCommand phoneList
            | "printallphones" ->
                printAllPhones phoneList |> ignore
                executeCommand phoneList
            | _ ->
                printfn "Команда некорректна!"
                executeCommand phoneList
    
    executeCommand phoneList
                
main() |> ignore
