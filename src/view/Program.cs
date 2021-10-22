using System;
using System.Net.Http;
using Microsoft.AspNetCore.Components.WebAssembly.Hosting;
using Microsoft.Extensions.DependencyInjection;
using DIPolWeb;
using DIPolWeb.FetchData;
using DIPolWeb.Services;
using DIPolWeb.Services.Implementations;


var builder = WebAssemblyHostBuilder.CreateDefault(args);
builder.RootComponents.Add<App>("#app");

builder.Services.AddScoped(_ => new HttpClient { BaseAddress = new Uri(builder.HostEnvironment.BaseAddress) });
builder.Services.AddSingleton<ISearchSpecificationProvider>(new NasaAdsLikeSpecificationProvider());
builder.Services.AddSingleton<IMjdConverter>(new SimpleMjdConverter());

await builder.Build().RunAsync();
