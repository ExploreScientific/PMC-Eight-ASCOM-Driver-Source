# Build Instructions

These instructions build the Explore Scientific PMC-Eight ASCOM driver DLL and the Windows setup executable from this repository.

## Required Build Tools

- Visual Studio 2019 or later with MSBuild and Visual Basic/.NET Framework targeting support.
- .NET Framework 4.5.2 targeting pack.
- ASCOM Platform 6 Developer Components.
- Inno Setup 6.

## Build Driver DLL

From a PowerShell prompt:

```powershell
& "C:\Program Files (x86)\Microsoft Visual Studio\2019\Enterprise\MSBuild\Current\Bin\MSBuild.exe" `
  ".\PMC8_DRV1.sln" `
  /t:Rebuild `
  /p:Configuration=Release `
  /p:Platform="Any CPU" `
  /p:SignAssembly=false `
  /p:RegisterForComInterop=false `
  /p:SkipAscomRegistration=true `
  /m
```

The compiled driver DLL is created at:

```text
bin\Release\ASCOM.ES_PMC8.Telescope.dll
```

The `RegisterForComInterop=false` and `SkipAscomRegistration=true` properties are used for packaging builds. The setup executable performs registration on the target system.

## Build Setup Executable

After the Release DLL is built:

```powershell
& "C:\Program Files (x86)\Inno Setup 6\ISCC.exe" ".\installer\ExploreScientific-PMC-Eight-ASCOM-Driver.iss"
```

The setup executable is created at:

```text
dist\ExploreScientific-PMC-Eight-ASCOM-Driver-6.0.0.3-Setup.exe
```

## Installer Payload

The installer script uses a repo-local payload directory under:

```text
installer\payload\c_\ES_PMC8_Utilities
```

That payload was extracted from the current public Explore Scientific setup executable so the rebuilt installer carries forward the required support utilities.

The ASCOM installer payload currently retains this support firmware binary:

```text
20A02.1.8.3.bt.binary
```

Current public PMC-Eight firmware releases are hosted separately in the official Explore Scientific firmware release repository:

```text
https://github.com/ExploreScientific/PMC-Eight-Firmware/releases/latest
```

The UFCT executable and Propellent support files in the payload are taken from the current public Explore Scientific Shopify UFCT ZIP package.
