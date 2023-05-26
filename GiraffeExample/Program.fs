open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.DependencyInjection

open GiraffeExample.Properties.Blog
open Giraffe

(* Web App Configuration *)

let webApp = 
    let blogDb = BlogDb()

    let serviceTree = {
        getBlogDb = fun() -> blogDb
    }
    
    let time() = System.DateTime.Now.ToString()

    choose [
        route "/" >=> text "Hello, world!"
        route "/foo" >=> text "Foo"
        route "/warbler" >=> warbler (fun _ -> text (time()))
        subRoute "/posts" 
            (choose [
                route "" >=> GET >=> warbler (fun _ -> 
                    (getPostsHttpHandler serviceTree))
                route "/create" 
                    >=> POST 
                    >=> warbler (fun _ -> 
                        (createPostHttpHandler serviceTree))
            ])
    ]

(* Infrastructure Configuration *)

let configureApp (app : IApplicationBuilder) =
    app.UseGiraffe webApp

let configureServices (services : IServiceCollection) =
    services.AddGiraffe() |> ignore

[<EntryPoint>]
let main _ =
    Host.CreateDefaultBuilder()
        .ConfigureWebHostDefaults(
            fun webHostBuilder ->
                webHostBuilder
                    .Configure(configureApp)
                    .ConfigureServices(configureServices)
                    |> ignore)
        .Build()
        .Run()
    0