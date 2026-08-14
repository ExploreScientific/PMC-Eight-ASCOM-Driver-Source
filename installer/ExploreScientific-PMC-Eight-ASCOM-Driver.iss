;
; Explore Scientific PMC-Eight ASCOM driver installer.
; This script is intentionally repo-local and uses relative paths so release
; builds are repeatable without developer-specific folders.
;

#define DriverVersion "6.0.0.3"
#define RepoRoot ".."
#define UtilityPayload "payload\c_\ES_PMC8_Utilities"

[Setup]
AppID={{658d079e-3064-459d-b488-db63c2329b6a}
AppName=Explore Scientific PMC-Eight ASCOM Telescope Driver
AppVerName=Explore Scientific PMC-Eight ASCOM Telescope Driver {#DriverVersion}
AppVersion={#DriverVersion}
AppPublisher=Explore Scientific, LLC
AppPublisherURL=https://explorescientific.com/
AppSupportURL=https://espmc-eight.groups.io/g/MAIN
AppUpdatesURL=https://github.com/ExploreScientific/PMC-Eight-ASCOM-Driver-Source/releases
VersionInfoVersion={#DriverVersion}
VersionInfoCompany=Explore Scientific, LLC
VersionInfoDescription=Explore Scientific PMC-Eight ASCOM Telescope Driver Setup
VersionInfoProductName=Explore Scientific PMC-Eight ASCOM Telescope Driver
VersionInfoProductVersion={#DriverVersion}
MinVersion=6.1sp1
DefaultDirName="{commoncf}\ASCOM\Telescope"
DisableDirPage=yes
DisableProgramGroupPage=yes
OutputDir="..\dist"
OutputBaseFilename="ExploreScientific-PMC-Eight-ASCOM-Driver-{#DriverVersion}-Setup"
Compression=lzma
SolidCompression=yes
WizardImageFile="C:\Program Files (x86)\ASCOM\Platform 6 Developer Components\Installer Generator\Resources\WizardImage.bmp"
LicenseFile="C:\Program Files (x86)\ASCOM\Platform 6 Developer Components\Installer Generator\Resources\CreativeCommons.txt"
UninstallFilesDir="{commoncf}\ASCOM\Uninstall\Telescope\ASCOM_ES_PMC8_Driver"

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Dirs]
Name: "{commoncf}\ASCOM\Uninstall\Telescope\ASCOM_ES_PMC8_Driver"
Name: "C:\ES_PMC8_Utilities"

[Files]
Source: "{#RepoRoot}\bin\Release\ASCOM.ES_PMC8.Telescope.dll"; DestDir: "{app}"; Flags: ignoreversion
Source: "{#RepoRoot}\Read_Me.txt"; DestDir: "{app}"; Flags: isreadme ignoreversion
Source: "{#RepoRoot}\Read_Me.txt"; DestDir: "C:\ES_PMC8_Utilities"; Flags: ignoreversion
Source: "{#RepoRoot}\CUMULATIVE RELEASE NOTES.pdf"; DestDir: "C:\ES_PMC8_Utilities"; DestName: "Cumulative Release Notes.pdf"; Flags: ignoreversion
Source: "{#RepoRoot}\PMC-Eight 20A01.4.5 Firmware Features.pdf"; DestDir: "C:\ES_PMC8_Utilities"; Flags: ignoreversion
Source: "{#UtilityPayload}\PMC-Eight Universal Firmware Configuration Tool 1.3.exe"; DestDir: "C:\ES_PMC8_Utilities"; Flags: ignoreversion
Source: "{#UtilityPayload}\Propellent.exe"; DestDir: "C:\ES_PMC8_Utilities"; Flags: ignoreversion
Source: "{#UtilityPayload}\Propellent.dll"; DestDir: "C:\ES_PMC8_Utilities"; Flags: ignoreversion
Source: "{#UtilityPayload}\Configure PMC8 for Home Network Connection.exe"; DestDir: "C:\ES_PMC8_Utilities"; Flags: ignoreversion
Source: "{#UtilityPayload}\20A02.1.8.3.bt.binary"; DestDir: "C:\ES_PMC8_Utilities"; Flags: ignoreversion
Source: "{#UtilityPayload}\PMC-Eight Mount Setup Quick Guide 1.pdf"; DestDir: "C:\ES_PMC8_Utilities"; Flags: ignoreversion
Source: "{#UtilityPayload}\Setting_up_a_new_PMC8_mount.pdf"; DestDir: "C:\ES_PMC8_Utilities"; Flags: ignoreversion

[Run]
Filename: "{dotnet4032}\regasm.exe"; Parameters: "/codebase ""{app}\ASCOM.ES_PMC8.Telescope.dll"""; Flags: runhidden 32bit
Filename: "{dotnet4064}\regasm.exe"; Parameters: "/codebase ""{app}\ASCOM.ES_PMC8.Telescope.dll"""; Flags: runhidden 64bit; Check: IsWin64

[UninstallRun]
Filename: "{dotnet4032}\regasm.exe"; Parameters: "-u ""{app}\ASCOM.ES_PMC8.Telescope.dll"""; Flags: runhidden 32bit; RunOnceId: "Unregister32"
Filename: "{dotnet4064}\regasm.exe"; Parameters: "-u ""{app}\ASCOM.ES_PMC8.Telescope.dll"""; Flags: runhidden 64bit; Check: IsWin64; RunOnceId: "Unregister64"

[Code]
const
   REQUIRED_PLATFORM_VERSION = 6.2;

function PlatformVersion(): Double;
var
   PlatVerString : String;
begin
   Result := 0.0;
   try
      if RegQueryStringValue(HKEY_LOCAL_MACHINE_32, 'Software\ASCOM','PlatformVersion', PlatVerString) then
      begin
         Result := StrToFloat(PlatVerString);
      end;
   except
      ShowExceptionMessage;
      Result:= -1.0;
   end;
end;

function InitializeSetup(): Boolean;
var
   PlatformVersionNumber : double;
begin
   Result := FALSE;
   PlatformVersionNumber := PlatformVersion();
   If PlatformVersionNumber >= REQUIRED_PLATFORM_VERSION then
      Result := TRUE
   else
      if PlatformVersionNumber = 0.0 then
         MsgBox('No ASCOM Platform is installed. Please install Platform ' + Format('%3.1f', [REQUIRED_PLATFORM_VERSION]) + ' or later from https://www.ascom-standards.org', mbCriticalError, MB_OK)
      else
         MsgBox('ASCOM Platform ' + Format('%3.1f', [REQUIRED_PLATFORM_VERSION]) + ' or later is required, but Platform '+ Format('%3.1f', [PlatformVersionNumber]) + ' is installed. Please install the latest Platform before continuing; you will find it at https://www.ascom-standards.org', mbCriticalError, MB_OK);
end;

procedure CurStepChanged(CurStep: TSetupStep);
var
  ResultCode: Integer;
  UninstallExe: String;
  UninstallRegistry: String;
begin
  if (CurStep = ssInstall) then
  begin
    UninstallRegistry := ExpandConstant('Software\Microsoft\Windows\CurrentVersion\Uninstall\{#SetupSetting("AppId")}' + '_is1');
    if RegQueryStringValue(HKLM, UninstallRegistry, 'UninstallString', UninstallExe) then
    begin
      MsgBox('Setup will now remove the previous version.', mbInformation, MB_OK);
      Exec(RemoveQuotes(UninstallExe), ' /SILENT', '', SW_SHOWNORMAL, ewWaitUntilTerminated, ResultCode);
      Sleep(1000);
    end
  end;
end;
