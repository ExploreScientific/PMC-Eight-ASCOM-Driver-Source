# Release Notes

## 6.0.0.3

Syncs Wes McDonald's WiFi/TCP pacing fix from the public `wifi-fix` branch.

This release provides:

- Re-enabled a measured 25 ms post-reply delay in the WiFi command path to avoid dropped commands when polling over TCP.
- Driver assembly version bump from `6.0.0.2` to `6.0.0.3`.
- Installer metadata bump from `6.0.0.2` to `6.0.0.3`.

Installer asset:

```text
ExploreScientific-PMC-Eight-ASCOM-Driver-6.0.0.3-Setup.exe
```

Driver assembly version:

```text
6.0.0.3
```

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
