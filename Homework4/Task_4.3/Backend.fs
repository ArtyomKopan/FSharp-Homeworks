module Phone.Backend

let initPhoneList() =
    Map.empty<string, string>

let addPhone name phone phoneList =
    Map.add name phone phoneList
    
let getPhoneByName name phoneList =
    Map.tryFind name phoneList
    
let getNameByPhone phone phoneList =
    Map.tryFindKey (fun _k v -> v = phone) phoneList
    
let getAllRecords phoneList =
    Map.toList phoneList
    
    


