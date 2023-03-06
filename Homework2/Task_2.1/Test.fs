open Task2
open FsCheck
    
let even_numbers_test list =
    even_numbers_1 list = even_numbers_2 list &&
    even_numbers_1 list = even_numbers_3 list &&
    even_numbers_2 list = even_numbers_3 list

Check.Quick even_numbers_test