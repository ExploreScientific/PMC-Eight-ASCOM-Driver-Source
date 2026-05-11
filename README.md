# Explore Scientific PMC-Eight ASCOM Driver

Official public source and release repository for the Explore Scientific PMC-Eight ASCOM telescope driver for Windows.

Use this driver when controlling an Explore Scientific PMC-Eight mount from Windows astronomy software that supports the ASCOM Platform, including planetarium, imaging, guiding, and observatory-control applications.

## Current Release

The current public setup package is published from this repository's GitHub Releases page:

https://github.com/ExploreScientific/PMC-Eight-ASCOM-Driver/releases/latest

Download the setup executable from the latest release assets:

`ExploreScientific-PMC-Eight-ASCOM-Driver-6.0.0.2-Setup.exe`

## Requirements

- Windows 10 or Windows 11 recommended.
- ASCOM Platform 6.2 or later is required before installing the driver.
- Administrator permission is required when running the setup program because the installer registers the ASCOM driver with Windows COM and the ASCOM chooser.

Download the ASCOM Platform from:

https://ascom-standards.org/Downloads/Index.htm

## Installation

1. Install the ASCOM Platform first.
2. Download the latest PMC-Eight ASCOM setup executable from this repository's Releases page.
3. Run the setup executable as an administrator.
4. Open your ASCOM-compatible astronomy application and select the Explore Scientific PMC-Eight telescope driver.
5. Configure the connection method for your mount: serial, WiFi, or Bluetooth serial COM port, depending on your PMC-Eight controller configuration.

## Included Utilities

The setup program includes the same support utilities carried by the current Explore Scientific installer package:

- PMC-Eight ASCOM telescope driver DLL.
- Current public PMC-Eight Universal Firmware Configuration Tool 1.3 package files.
- Propellent firmware programming utility files required by UFCT.
- Configure PMC8 for Home Network Connection utility.
- PMC-Eight firmware binary `20A02.1.8.3.bt.binary`, matching the current public firmware package.
- PMC-Eight utility documentation and release notes.

## Build

See [BUILD.md](BUILD.md) for repeatable local build and installer instructions.

The setup program installs the driver and support utilities only. Source code is distributed through this repository rather than installed by the setup program.

## Support

Community and customer discussion:

https://espmc-eight.groups.io/g/MAIN

Explore Scientific:

https://explorescientific.com/
