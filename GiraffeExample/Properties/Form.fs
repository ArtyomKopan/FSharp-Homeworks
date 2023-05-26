module GiraffeExample.Properties.Form

open Giraffe
open Microsoft.AspNetCore.Http

let inputFormHandler (next : HttpFunc) (ctx : HttpContext) : HttpFuncResult =
    match ctx.Request.Method with
    | "GET" ->
        let html =
            sprintf "
                <html>
                <body>
                    <h2>Input Form</h2>
                    <form method='post' action='/submit'>
                        <label for='name'>Name:</label>
                        <input type='text' id='name' name='name'><br><br>
                        <label for='email'>Email:</label>
                        <input type='text' id='email' name='email'><br><br>
                        <input type='submit' value='Submit'>
                    </form>
                </body>
                </html>"
        text html next ctx
    | "POST" ->
        let name = ctx.Request.Form["name"]
        let email = ctx.Request.Form["email"]
        let response =
            sprintf $"
                <html>
                <body>
                    <h2>Submitted Form</h2>
                    <p>Name: %s{name}</p>
                    <p>Email: %s{email}</p>
                </body>
                </html>"
        text response next ctx
    | _ -> failwith "todo"

let app : HttpHandler =
    choose [
        POST >=> route "/submit" >=> inputFormHandler
        GET >=> route "/" >=> inputFormHandler
    ]
