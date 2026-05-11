# Release Notes

## 6.0.0.2

Initial Explore Scientific public GitHub release packaging for the PMC-Eight ASCOM driver.

This release provides:

- Rebuilt `ASCOM.ES_PMC8.Telescope.dll` from the current source tree.
- A repeatable Inno Setup installer script using repo-local paths.
- The same PMC-Eight support utility payload carried by the current installer package.
- Updated the bundled UFCT payload to match the current public Explore Scientific UFCT ZIP package.
- Updated bundled PMC-Eight firmware binary from `20A01.4.4.binary` to `20A02.1.8.3.bt.binary`.
- Removed installer source-file payload; source is distributed through GitHub instead of being copied into customer installations.
- Clean release build path that does not require administrator rights during compilation.

Installer asset:

```text
ExploreScientific-PMC-Eight-ASCOM-Driver-6.0.0.2-Setup.exe
```

Driver assembly version:

```text
6.0.0.2
```
