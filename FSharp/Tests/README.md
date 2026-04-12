# Tests in F#


## Expecto .NET

`FSharp/Tests/Test.fsproj` contains a suite of 66 tests covering all the main functionality of the splay tree.

run from this folder:
```bash
dotnet run
```

or from the repository root:
```bash
dotnet run --project FSharp/Tests/Test.fsproj -- --summary
```



## Fable build


```bash
cd FSharp/Tests
```

```bash
dotnet tool restore
npm install
```

```bash
npm run buildJS
```

```bash
npm run bundleJS
```

```bash
npm run vite
```


## Test fable output with TSC

```bash
npm run compileTS
```


## Build original TypeScript output

from root of repository:

```bash
npm run build
```

## Benchmarks

```bash
npm run bench
```