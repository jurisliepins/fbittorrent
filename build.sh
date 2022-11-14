dotnet tool restore
dotnet paket restore

# TODO: Use FAKE build.fsx once path too long issue has been fixes.
dotnet restore
dotnet clean
dotnet build