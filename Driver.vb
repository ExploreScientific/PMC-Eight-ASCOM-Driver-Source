'tabs=4
' --------------------------------------------------------------------------------
'
' ASCOM Telescope driver for ES_PMC-Eight
'
' Description:	This is the main telescope driver for the Explore Scientific
'				Precision Motion Controller Eight (PMC-Eight). This driver
'               is copyright 2013-2021 Explore Scientific, LLC.
'
' Implements:	ASCOM Telescope interface version: 6.0
' Author:		(GRH) Gerald R. Hubbell <jrh@explorescientific.com>
'               Vice President Engineering, Explore Scientific, LLC.
'
' Edit Log:
'
' Date			Who	Vers	Description
' -----------	---	-----	---------------------------------------------------------------------
' 26-AUG-2015	GRH	1.0.0	Initial edit, from Telescope template
' 30-AUG-2015   GRH 1.0.1   Added code for connect routine
' 19-MAR-2016   GRH 1.0.2   Code added for connecting to serial port
' 19-MAY-2016   GRH 1.0.3   Added calculations for RightAscension and Declination
' 21-MAY-2016   GRH 1.0.4   Added calculations for converting RA,DEC,ALT,AZ to motor
'                           counts.
' 24-MAY-2016   GRH 1.0.5   Added and Tested base functionality for Slewing, Moving,
'                           and Syncing controller. Tested Serial port connection
' 06-JUN-2016   GRH 1.0.6   Added code to implement functionality and Tested Wireless
'                           TCP/IP connection to controller, Further performance testing
' 10-JUN-2016   GRH 1.0.7   Corrected minor issues with driver. Implemented AbortSlew
'                           turned on tracking after slew, other misc fixes.
'                           Added Home functioinality
' 12-JUN-2016   GRH 1.0.8   Added PierSide functionality and calculation and SetPark
'                           functionality. Tested functionality SideOfPier
' 28-JUN-2016   GRH 1.0.9   Continued to test and fix coordinate calculations.
' 29-JUN-2016   GRH 1.0.10  Added Refraction adjustments to coordinate calculations,
'                           and stored the values used:Site Ambient Temperature, and
'                           Apply Refraction Correction in the Profile object.
' 08-JUL-2016   GRH 1.0.11  Further testing SideOfPier and RA/Motor conversion.
' 25-JUL-2016   GRH 1.0.12  Simplified SideOfPier and MotorCount conversion commands
'                           and tested all the routines to make sure they pass all
'                           conformance testing including side of pier tests and 
'                           pier flip from pierWest to pierEast. Corrected problem
'                           with AbortSlew command. 
' 25-JUL-2016   GRH 1.2.0   RELEASE VERSION for BETA TESTING in FIELD
' 21-FEB-2017   GRH 1.2.1   UPDATED - Compiled under VS 2015 'RELEASE VERSION TO CUSTOMERS
' 28-MAY-2017   GRH 1.2.2   Tested with Cartes du Ciel ver. 4.0 corrected parked issue
' 03-AUG-2017   GRH 1.2.3   Added interface to calibrate ST4 port on PMC-Eight Controller.
'                           Added variable for status of RA slew (true, false) versus
'                           "Slewing" calculation.
' 11-NOV-2017   GRH 1.2.4   Added PulseGuide function - initial testing
' 15-FEB-2018   GRH 1.2.5   Updated PulseGuide functionality
' 16-FEB-2018   GRH 1.2.6   Fine Tuned the tracking rates and updated, verified,
'                           and tested the set park position. The park position is
'                           now stored in the ASCOM driver profile and is loaded on
'                           connection to the PMC-Eight if the current PMC-Eight 
'                           RA and DEC positions are zero.
' 18-FEB-2018   GRH 1.2.7   Updated the Connected() and the CommandString() functions
'                           to respond to the *HELLO* initial receive string on the Microchip
'                           RN-131 module. The ESP-WROOM-02 module does not send this 
'                           String so this code is not needed for that. Incorporated a
'                           WiFi module ID to bypass the code when using the ESP module.
' 19-FEB-2018   GRH 1.2.8   Added selection for tracking rate in setup box and a setting for
'                           rate offset value in arc-sec/sec
' 06-MAR-2018   GRH 1.2.9   Added selection for selecting the WiFi module - ESP-WROOM-02 or
'                           Microchip RN-131
' 17-MAR-2018   GRH 1.2.10  Added check for slewing status to SlewToTargetAsync subroutine
'                           to re-enable tracking at the end of the slew
' 28-MAR-2018   GRH 1.2.11  Made numerous changes to tracking and ratracking placement in
'                          Slewing(), SlewToTarget(), SlewToTargetAsync(), and Tracking()
'                          Tested with conformance version 6.3.60.5
' 27-JUN-2018   GRH 1.2.12 Corrected problem in Slewing() where not checking for IsPulseGuiding
'                          caused the pulseguide command to be changed to normal tracking when
'                          rate went less than minRARate
' 20-AUG-2018   GRH 1.2.13 Corrected problem with a format exception occuring when the profile is
'                          setup in countries other than US and UK. Used Convert.ToString function
' 20-DEC-2018   GRH 1.2.14 Added correction to motor conversion routines to correctly calculate based
'                          on northern or southern hemisphere and set the RA axis direction.
' 28-DEC-2018   GRH 1.2.15 Added final tweaks to southern hemisphere code and fully tested.
' 07-JAN-2019   GRH 1.2.16 RELEASE VERSION
' 28-SEP-2019   GRH 1.3.0  Added enhanced SRF settings - Changed from decimal fraction to 
'                          percent value (0-100) for SiderealRateFraction
' 08-JAN-2021   GRH 1.3.1  Updated driver to implement new SRF settings and write values to
'                          PMC-Eight memory using ESSf0! and ESSf1! commands. Needed for new
'                          PMC-Eight firmware version 20A01. Corrected issue with determining
'                          When at park and not tracking status.
' 19-JAN-2021   GRH 1.3.2  Corrected identified issue with change 1.3.1 and tested with firmware
'                          version 20A01
' 15-FEB-2021   GRH 1.3.3  BETA Release Version for Firmware 20A01.
' 07-MAR-2021   GRH 1.3.4  BETA Release Version for Public BETA Release.
' 12-APR-2021   GRH 2.0.0  RELEASE VERSION for Universal Firmware 20A01
' 23-MAY 2021   GRH 2.0.1  Corrected issue introduced in firmware update when slewing to a new
'                          target while in the middle of a slew to the original target. Added
'                          ramp down prior to slewing to a new target and also when aborting 
'                          a slew with new ESPt3XXXXXX! command.
' 30-MAY-2021   GRH 2.0.2  Added Parameters to apply backlash compensation and changed pulseguide to take
'                          out backlash using calculated rate from parameters.
' 03-JUN-2021   GRH 2.0.3  Hid backlash settings in miscellaneous tab of setup dialog form. Wes Feature
'                          not for general release
' 05-JUN-2021   GRH 2.0.4  Fixed issue If stopped slewing before getting to the PARK or HOME position then
'                          don't set PARK or HOME status to true, or stop tracking
' 06-JUN-2021   GRH 2.1    RELEASE VERSION for Universal Firmware 20A01.03
' 08-AUG-2021   GRH 2.1.1  Changed parked exception code in PulseGuide to workaround conformance test issue
'                          Fixed issue with detecting HOME position found in conformance test.
'                          Rem'd out backlash ASCOM Profile information, not currently used
'                          Tested for Universal Firmware version 1.1
' 27-MAR-2022   WEM 2.1.2  Corected move computations in SlewToTarget and SlewToTargetAsync
'                          Inhibited Slewing from a TRUE response if pulse guiding - corrects PHD guiding issue with
'                           with large guide rate fractions
'                          Addded a 3 second delay in DEC park motion to allow RA to move up away from tripod leg
'                          Altered Slewing to use mount specific RA rate +2 as minimum to indicate slewing
'                          Added code to Slewing to prevent aborted slews east when very small moves asked for
'  25-May-2022  WEM        Added inclusions for the Badlands Observatory, partial implemtation of Equitorial Fork mount
'                          Corrected several hard coded ESSd commands where MountPreferredDir was assumed to be 1, now works with 0
'                          Corrected POTH rate move button which woud not respond to POTH stop button
'                          Corrected Slewing indication when mount was guided faster than about .5 sidereal (rate was mount dependent)
'                          Corrected MoveAxis movmement direction when pier EAST, NS POTH rate move buttons used to go the wrong direction
'                          Added Mount Max Speeed to profile (40000 for ES mounts, 8000 for Scotty mount)
'                          Set Serial as default setting for connection in SETUP
'                          Updated ABOUT screen
'  20-JUNE-2022 WEM        Fixed DEC slew threshold in Slewing to 6 from 4 to get conformance test pass
'                          Corrected INT16 calcualtion in Primary axis RATE computation in MoveAxis to fix error - note conformance test will fail anyway because our max rate is less than their test case
'  07-SEPT-2022 WEM        Corrected Titan Direction and Motor Counts
'                          Added support for variale rates by rejiggering RATES.vb enumerated rates. Now NINA and SkyTrack have variable rates
'  14-SEPT-2023 WEM        Added MSRO support, copied from variale rate driver which I want to abandon
'                          Corrected iexos200 motor counts which had been defaulted to iexos100 but are 5760000.
'                          Re-added the southern hemisphere 'always slewing' bug.  change is in SLEWING
'  29-MAR-2024  WEM        Modification to correect isuues when running ConformU  These involved many changes otale to Rightascensionrate, declinationrate, Tracking, and various stuff all over the place to make
'                          that all go.  
'  
'  Herein begins the ConformU driver that will provide high resolution rate control for the declination axis.  It is compatible only with firmware
'  versions 2.0 and subsequent.
'
'  04-APR-2024  WEM        Changed DEC set rate calls to employ the ESTe command for high precision DEC axis rates
'  16-FEB-2026  WEM        Slewing property rewritten to use single ESV! state vector command,
'                          replacing 3-4 separate axis queries. DeviceHub now shows correct
'                          Slewing status where 2.1 reference failed after rate offset changes.
'                          RA_to_MotorCounts refactored to use Hour Angle sign instead of
'                          explicit PierSide parameter for determining motor count offset.
'  16-FEB-2026  WEM        Fixed Slewing returning True after RightAscensionRate offset applied.
'                          Fixed CorrectionSlew race condition that caused ConformU rate and
'                          PulseGuide test failures. Fixed PulseGuide confirmation and closed
'                          CorrectionSlew race window. Fixed correction slew being killed by
'                          tracking re-enable.
'  17-FEB-2026  WEM        Ported 2.1 features: AxisRates dynamic max rate with SkySafari support,
'                          DeclinationRate minimum filter with EQ mount direction logic,
'                          MSROEQ and ASKO SX260S mount support, mount dropdown additions,
'                          SkySafari RadioButton2 UI control with profile persistence.
'  17-FEB-2026  WEM        Fixed southern hemisphere support: DestinationSideOfPier, DEC direction
'                          for slew calculations, RA offset sign corrections.
'                          Updated LongMoveOffset defaults to 8/-4, removed redundant
'                          Scotty/ASKO overrides in SetupDialogForm.
'  19-FEB-2026  WEM        Corrected MountRADir for MSROEQ and ASKO mounts from 0 to 1.
'                          Removed incorrect MSROMount=True for ASKO (not a fork mount).
'                          Fixed DeclinationRate pierEast branch: removed incorrect southern
'                          hemisphere direction reversal that inverted DEC offset rates.
'                          Fixed DestinationSideOfPier to return same result both hemispheres
'                          per ASCOM convention. Added SlewDestinationSideOfPier helper to
'                          flip pier side for DEC motor calcs in southern hemisphere slews.
'                          ConformU result: 0 errors, 2 issues (AxisRate overlap only),
'                          verified both hemispheres.
' 
'  the above are released to BETA as 2.1, pushed to GIT repository
'
'  05-JUL-2026  WEM        WiFi (TCP) refraction fix -> version 6.0.0.3. Re-enabled the post-reply
'                          refraction delay (WIFI_REFRACTION, now 25ms) in the CommandString WiFi
'                          path. Measured straight to the mount (raw sockets, no driver): a command
'                          sent ~0ms after the previous reply is silently DROPPED ~50% of the time;
'                          any gap >= ~10ms => 100% success. Without the delay every other command
'                          dropped and cost ~1s (500ms read-timeout + 500ms retry1 + resend), so
'                          WiFi polling crawled (RA updated only ~every 8s). With the 25ms refraction
'                          commands succeed on the first try (~35ms) and WiFi is smooth. The drop is
'                          the mount/ESP needing a brief gap after it finishes a reply before the
'                          next command; envision (raw 54372) avoids it, this paces the AT path.
'--------------------------------------------------------------------------------------------------------
'
'
' Your driver's ID is ASCOM.ES_PMC8.Telescope
'
' The Guid attribute sets the CLSID for ASCOM.DeviceName.Telescope
' The ClassInterface/None addribute prevents an empty interface called
' _Telescope from being created and used as the [default] interface
'

' This definition is used to select code that's only applicable for one device type
#Const Device = "Telescope"
Imports ASCOM
Imports ASCOM.Astrometry
Imports ASCOM.Astrometry.AstroUtils
Imports ASCOM.DeviceInterface
Imports ASCOM.Utilities
Imports ASCOM.Astrometry.Transform
Imports MathNet.Numerics.LinearAlgebra

Imports System
Imports System.Collections
Imports System.Collections.Generic
Imports System.Globalization
Imports System.Runtime.InteropServices
Imports System.Text
Imports System.Net
Imports System.Net.Sockets

<Guid("5276d38b-2048-4b9c-8761-c9a94d6aa372")> _
<ClassInterface(ClassInterfaceType.None)>
<ComVisible(True)>
Public Class Telescope
    ' The Guid attribute sets the CLSID for ASCOM.ES_PMC8.Telescope
    ' The ClassInterface/None addribute prevents an empty interface called
    ' _ES_PMC8 from being created and used as the [default] interface

    ' TODO Replace the not implemented exceptions with code to implement the function or
    ' throw the appropriate ASCOM exception.
    '
    Implements ITelescopeV3
    '
    ' Driver ID and descriptive string that shows in the Chooser
    '
    Friend Shared driverID As String = "ASCOM.ES_PMC8.Telescope"
    Friend Shared driverDescription As String = "ES_PMC8 Telescope"
    Friend Shared comPortProfileName As String = "COM Port" 'Constants used for Profile persistence
    Friend Shared comSpeedProfileName As String = "COM Speed"
    Friend Shared traceStateProfileName As String = "Trace Level"
    Friend Shared IPAddressProfileName As String = "IP Address"
    Friend Shared IPPortProfileName As String = "IP Port"
    Friend Shared WirelessEnabledProfileName As String = "Wireless Enabled"
    Friend Shared WirelessProtocolProfileName As String = "Wireless Protocol"
    Friend Shared MountProfileName As String = "Mount Type"
    Friend Shared RateProfileName As String = "Mount Rate"
    Friend Shared MountRACountsProfileName As String = "Total RA Counts"
    Friend Shared MountDECCountsProfileName As String = "Total DEC Counts"
    Friend Shared MountMaxSpeedProfileName As String = "Mount Max Rate"
    Friend Shared ApertureDiameterProfileName As String = "Telescope Aperture Diameter"
    Friend Shared ApertureAreaProfileName As String = "Telescope Aperture Area"
    Friend Shared FocalLengthProfileName As String = "Telescope Focal Length"
    Friend Shared SiteLocationProfileName As String = "Site Location"
    Friend Shared SiteElevationProfileName As String = "Site Elevation meters"
    Friend Shared SiteLatitudeProfileName As String = "Site Latitude"
    Friend Shared SiteLongitudeProfileName As String = "Site Longitude"
    Friend Shared RARateOffsetProfileName As String = " RA Rate Offset arc-sec/sec"
    Friend Shared DECRateOffsetProfileName As String = "DEC Rate Offset arc-sec/sec"
    Friend Shared SiteAmbientTemperatureProfileName As String = "Site Ambient Temperature"
    Friend Shared ParkRAPositionProfileName As String = "RA Park Position"
    Friend Shared ParkDECPositionProfileName As String = "DEC Park Position"
    Friend Shared ApplyRefractionCorrectionProfileName As String = "RefractionApplied"
    Friend Shared RA_SiderealRateFractionProfileName As String = "RA Sidereal Rate Fraction"
    Friend Shared DEC_SiderealRateFractionProfileName As String = "DEC Sidereal Rate Fraction"
    Friend Shared MininumPulseTimeProfileName As String = "Minimum Pulse Time"
    Friend Shared WiFiModuleIDProfileName As String = "WIFI Module ID"
    Friend Shared SkySafari_rateProfileName As String = "SkySafari Rates Enable"
    Friend Shared LongMoveoffset1Profilename As String = "LongMoveOffset1"
    Friend Shared LongMoveoffset2Profilename As String = "LongMoveOffset2"
    Friend Shared RampOnlyoffset1Profilename As String = "RampOnlyOffset1"
    Friend Shared RampOnlyoffset2Profilename As String = "RampOnlyOffset2"
    'Friend Shared BacklashValueProfileName As String = "Backlash Value"
    'Friend Shared BacklashTimeProfileName As String = "Backlash Correction Time"
    'Friend Shared BacklashMinimumProfileName As String = "Minimum Backlash Value"
    'Friend Shared BacklashEnabledProfileName As String = "Backlash Enabled"

    Friend Shared comPortDefault As String = "COM3"
    Friend Shared comSpeedDefault As String = "115200"
    Friend Shared traceStateDefault As String = "False"
    Friend Shared IPAddressDefault As String = "192.168.47.1"
    Friend Shared IPPortDefault As String = "54372"
    Friend Shared WirelessEnabledDefault As String = "False"
    Friend Shared WirelessProtocolDefault As String = "TCP"
    Friend Shared MountDefault As String = "Losmandy G-11"
    Friend Shared RateDefault As String = "Sidereal"
    Friend Shared MountRACountsDefault As String = "4608000" 'G-11
    Friend Shared MountDECCountsDefault As String = "4608000" 'G-11
    Friend Shared ApertureDiameterDefault As String = Convert.ToString(0.102)
    Friend Shared ApertureAreaDefault As String = Convert.ToString(0.00817)
    Friend Shared FocalLengthDefault As String = Convert.ToString(0.714)
    Friend Shared SiteLocationDefault As String = "Explore Scientific HQ"
    Friend Shared SiteElevationDefault As String = Convert.ToString(403.0)
    Friend Shared SiteLatitudeDefault As String = Convert.ToString(36.18063)
    Friend Shared SiteLongitudeDefault As String = Convert.ToString(-94.18838)
    Friend Shared RARateOffsetDefalut As String = Convert.ToString(0.000)
    Friend Shared DECRateOffsetDefault As String = Convert.ToString(0.000)
    Friend Shared SiteAmbientTemperatureDefault As String = Convert.ToString(59.0)
    Friend Shared ParkRAPositionDefault As String = "0"
    Friend Shared ParkDECPositionDefault As String = "0"
    Friend Shared ApplyRefractionCorrectionDefault As String = "False"
    Friend Shared RA_SiderealRateFractionDefault As String = Convert.ToString(40)
    Friend Shared DEC_SiderealRateFractionDefault As String = Convert.ToString(40)
    Friend Shared MinimumPulseTimeDefault As String = "100"
    Friend Shared WiFiModuleIDDefault As String = "Microchip RN-131"
    Friend Shared MountMaxSpeedDefault As String = Convert.ToString(40000)
    Friend Shared SkySafari_rateDefault As String = Convert.ToString(0)
    ' Slew-time offset defaults (seconds). Used by ReadProfile when no profile entry exists.
    ' MUST stay in sync with the unconditional defaults set in SetupDialogForm.OK_Button_Click,
    ' otherwise opening Setup once silently changes the runtime values from the profile defaults
    ' to the dialog defaults (the dialog overwrites and WriteProfile then persists the new values).
    ' Mount-specific overrides (Scotty/ASKO -> 2.0/2.0 LongMove) are applied later in that cascade.
    ' Values below are empirical and may still need tuning per mount; consistency between the two
    ' locations is the only guarantee here.
    Friend Shared LongMoveoffset1default As String = Convert.ToString(8)
    Friend Shared LongMoveoffset2default As String = Convert.ToString(-4)
    Friend Shared Ramponlyoffset1default As String = Convert.ToString(4)
    Friend Shared Ramponlyoffset2default As String = Convert.ToString(4)
    'Friend Shared BacklashValueDefault As String = "0" 'arc-sec
    'Friend Shared BacklashTimeDefault As String = "100" 'milliseconds
    'Friend Shared BacklashMinimumDefault As String = "0" 'arc-sec
    'Friend Shared BacklashEnabledDefault As Boolean = False

    Friend Shared comPort As String ' Variables to hold the currrent device configuration
    Friend Shared comSpeed As String
    Friend Shared traceState As Boolean
    Friend Shared IPAddress As String
    Friend Shared IPPort As String
    Friend Shared WirelessEnabled As Boolean
    Friend Shared WirelessProtocol As String
    Friend Shared Mount As String
    Friend Shared Rate As String
    Friend Shared MountRACounts As Long
    Friend Shared MountDECCounts As Long
    Friend Shared MountMaxSpeed As Long
    Friend Shared SkySafari_rate As Single            'set up the rate array for sky safari or not 1 = skysafari, 0 not
    Friend Shared ApertureDiameterValue As Double
    Friend Shared ApertureAreaValue As Double
    Friend Shared FocalLengthValue As Double
    Friend Shared SiteLocation As String
    Friend Shared SiteElevationValue As Double
    Friend Shared SiteLongitudeValue As Double
    Friend Shared SiteLatitudeValue As Double
    ' Friend Shared RateOffsetValue As Double
    Friend Shared RARateOffsetValue As Double  'COnformU
    Friend Shared DECRateOffsetValue As Double   'ConformU
    Friend Shared SiteAmbientTemperatureValue As Double
    Friend Shared ParkRAPosition As Int32
    Friend Shared ParkDECPosition As Int32
    Friend Shared ApplyRefractionCorrection As Boolean
    Friend Shared RA_SiderealRateFraction As Single
    Friend Shared DEC_SiderealRateFraction As Single
    Friend Shared MinimumPulseTime As Int32
    Friend Shared WiFiModuleID As String
    Friend Shared BacklashValue As Int32
    Friend Shared BacklashTime As Int32
    Friend Shared BacklashMinimum As Int32
    Friend Shared BacklashEnabled As Boolean
    Friend Shared PreviousDirection As GuideDirections
    Friend Shared ScottyMount As Boolean
    Friend Shared MSROMount As Boolean
    Friend Shared LongMoveOffset1 As Single            'default for Slew to target long movement time offset, Mount dependent
    Friend Shared LongMoveOffset2 As Single
    Friend Shared RampOnlyOffset1 As Single            'default for Slew to target long ramp only time offset, Mount dependent
    Friend Shared RampOnlyOffset2 As Single
    ' conformU, move this variable from class private to shared
    Friend Shared MountRightAscensionRate As Double = 15.0     'default is sidereal per ASCOM ConformU
    Friend Shared Mounttrackingratevalue As Double = 15.0       'default is siderl per ascom


    Private connectedState As Boolean ' Private variable to hold the connected state
    Private pulseguidingState As Boolean 'Private variable to hold pulse-guiding active state
    Private utilities As Util ' Private variable to hold an ASCOM Utilities object
    Private astroUtilities As AstroUtils ' Private variable to hold an AstroUtils object to provide the Range method
    Private TL As TraceLogger ' Private variable to hold the trace logger object (creates a diagnostic log file with information that you specify)

    Private objSerial As ASCOM.Utilities.Serial 'Serial Port object
    'Private objUDPNetwork As System.Net.Sockets.UdpClient 'UDP network object
    'Private objUDPNetwork_S As System.Net.Sockets.UdpClient 'UDP Network Receiver object
    Private objTCPNetwork As System.Net.Sockets.TcpClient     'Persistent TCP connection
    Private objTCPStream As NetworkStream                       'Persistent network stream
    Private objTransform As New ASCOM.Astrometry.Transform.Transform 'Transform calculations
    Private SerMutex As New System.Threading.Mutex
    Private WiFiSemaphore As New System.Threading.SemaphoreSlim(1, 1)  'Non-reentrant gate - only one WiFi command at a time
    Private WiFiConnected As Boolean = False
    Private Const WIFI_READ_TIMEOUT As Integer = 500        '500ms read timeout - ESP needs headroom under load
    Private Const WIFI_CONNECT_TIMEOUT As Integer = 5000    '5 second connect timeout
    Private Const WIFI_MASTER_TIMEOUT As Integer = 10000   '10 second master timeout - if semaphore not acquired, ESP is dead
    Private Const WIFI_REFRACTION As Integer = 25           '25ms breathing room for ESP after each command (measured: >=~10ms => 100%, 0ms drops ~50%)
    Private Const WIFI_TERMINATOR As Char = "!"c
    Private CorrectionSlewActive As Boolean = False     'True while correction slew phase is active - keeps Slewing=True
    Private CorrectionSlewSent As Boolean = False       'True after correction ESPt sent - checked by Slewing with time guard
    Private CorrectionSlewSentTime As DateTime = DateTime.MinValue  'timestamp when correction ESPt was sent
    Private Const CORRECTION_THRESHOLD As Int32 = 10    '10 motor counts ≈ 3 arcseconds
    'Private PrevRA As Double
    'Private PrevDEC As Double
    Private PrevRAMotor As Int32
    Private PrevDECMotor As Int32

    Private j2000 As New DateTime
    Private deltaTime As TimeSpan
    Private LMSTtot As Double
    Private LMST As Double
    Private di As Double
    Private RATracking As Boolean = False
    'Private mountSlewing = False
    'Private RATarget As Double = 12.0
    'Private DECTarget As Double = 90.0
    Private RATarget As Double
    Private DECTarget As Double
    'Private RAParkStatus As Boolean = True
    'Private DECParkStatus As Boolean = True
    Private ParkStatus As Boolean = True
    Private HomeStatus As Boolean = True
    'Private RADirection As String = "CCW"
    'Private DECDirection As String = "CW"
    Private RATargetSet As Boolean = False
    Private DECTargetSet As Boolean = False
    Private minSlewRate As Int32 = 75     'Set to greater than 2x Sidereal Rate in motor counts/second
    'Private WpE_Normal As Boolean
    'Private EpW_Normal As Boolean
    'Private WpE_TtP As Boolean
    'Private EpW_TtP As Boolean
    Private SoP As Integer
    Private FlipMount As Boolean
    Private MountTrackingRate As DriveRates = DriveRates.driveSidereal

    Private MountPierSide As PierSide
    '    Private MountRightAscensionRate As Double
    Private MountDeclinationRate As Double
    Private AltAzSlew As Boolean = False
    Private MountRADir As Boolean
    '
    ' Constructor - Must be public for COM registration!
    '
    Public Sub New()

        ReadProfile() ' Read device configuration from the ASCOM Profile store
        TL = New TraceLogger("", "ES_PMC8")
        TL.Enabled = traceState
        TL.LogMessage("Telescope", "Starting initialization")
        connectedState = False ' Initialise connected to false
        utilities = New Util() ' Initialise util object
        astroUtilities = New AstroUtils 'Initialise new astro utiliites object
        objTransform.SiteLatitude = Me.SiteLatitude
        objTransform.SiteLongitude = Me.SiteLongitude
        objTransform.SiteElevation = Me.SiteElevation
        objTransform.SiteTemperature = SiteAmbientTemperatureValue
        ScottyMount = False
        MSROMount = False
        If Mount.ToString.Contains("Titan") Then
            MountRADir = 0
        ElseIf Mount.ToString.Contains("G-11") Then
            MountRADir = 1
        ElseIf Mount.ToString.Contains("EXOS II") Then
            MountRADir = 1
        ElseIf Mount.ToString.Contains("iEXOS") Then
            MountRADir = 1
        ElseIf Mount.ToString.Contains("Scotty") Then
            MountRADir = 0
            ScottyMount = True
        ElseIf Mount.ToString.Contains("MSROEQ") Then
            MountRADir = 1
            MSROMount = True
        ElseIf Mount.ToString.Contains("ASKO") Then
            MountRADir = 1
        End If
        TL.LogMessage("Telescope", "mount to string =")


        'TODO: Implement your additional construction here

        TL.LogMessage("Telescope", "Completed initialisation")
    End Sub

    '
    ' PUBLIC COM INTERFACE ITelescopeV3 IMPLEMENTATION
    '

#Region "Common properties And methods"
    ''' <summary>
    ''' Displays the Setup Dialog form.
    ''' If the user clicks the OK button to dismiss the form, then
    ''' the new settings are saved, otherwise the old values are reloaded.
    ''' THIS IS THE ONLY PLACE WHERE SHOWING USER INTERFACE IS ALLOWED!
    ''' </summary>


    Public Sub SetupDialog() Implements ITelescopeV3.SetupDialog
        ' consider only showing the setup dialog if not connected
        ' or call a different dialog if connected
        If IsConnected Then
            System.Windows.Forms.MessageBox.Show("Already connected, just press OK")
        End If

        Using F As SetupDialogForm = New SetupDialogForm()
            Dim result As System.Windows.Forms.DialogResult = F.ShowDialog()
            If result = DialogResult.OK Then
                WriteProfile() ' Persist device configuration values to the ASCOM Profile store
            End If
        End Using
    End Sub

    Public ReadOnly Property SupportedActions() As ArrayList Implements ITelescopeV3.SupportedActions
        Get
            Dim myActions As New ArrayList
            'List of actions,  may not be totally complete
            myActions.Add("CanPark")
            myActions.Add("CanSetTracking")
            myActions.Add("CanSlew")
            myActions.Add("CanSlewAltAz")
            myActions.Add("CanSlewAsync")
            myActions.Add("CanSlewAltAzAsync")
            myActions.Add("CanSync")
            myActions.Add("CanSyncAltAz")
            myActions.Add("CanUnpark")
            myActions.Add("CanMoveAxis")
            myActions.Add("DoesRefraction")
            myActions.Add("Can...")

            TL.LogMessage("SupportedActions Get", "Returning array list")
            Return myActions
        End Get
    End Property

    Public Function Action(ByVal ActionName As String, ByVal ActionParameters As String) As String Implements ITelescopeV3.Action
        Throw New ASCOM.ActionNotImplementedException("Action " & ActionName & " is not supported by this driver")
    End Function

    Public Sub CommandBlind(ByVal Command As String, Optional ByVal Raw As Boolean = False) Implements ITelescopeV3.CommandBlind
        CheckConnected("CommandBlind")
        ' Call CommandString and return as soon as it finishes
        Me.CommandString(Command, Raw)
        ' or
        Throw New ASCOM.MethodNotImplementedException("CommandBlind")
    End Sub

    Public Function CommandBool(ByVal Command As String, Optional ByVal Raw As Boolean = False) As Boolean _
        Implements ITelescopeV3.CommandBool
        CheckConnected("CommandBool")
        Dim ret As String = CommandString(Command, Raw)
        ' TODO decode the return string and return true or false
        ' or
        Throw New MethodNotImplementedException("CommandBool")
    End Function

    Public Function CommandString(ByVal Command As String, Optional ByVal Raw As Boolean = False) As String _
        Implements ITelescopeV3.CommandString
        'Dim objUDPNetwork_R As New System.Net.Sockets.UdpClient(CInt(UDPPort))
        'Dim RemoteIpEndPoint As New IPEndPoint(System.Net.IPAddress.Any, 0)
        Dim sendBytes As [Byte]()
        Dim receiveString As String
        Dim cmdString As String

        CheckConnected("CommandString")
        cmdString = Command
        ' it's a good idea to put all the low level communication with the device here,
        ' then all communication calls this function
        ' you need something to ensure that only one command is in progress at a time
        If IsConnected Then
            If Not WirelessEnabled Then
                objSerial.Transmit(Command)
                TL.LogMessage("Serial Command$ Transmitted", Command)
                'ES Command Language Terminator String ! (SHRIEK)
                receiveString = objSerial.ReceiveTerminated("!")
                cmdString = Trim(receiveString)
                TL.LogMessage("Serial Command$ Received", cmdString)
                objSerial.ClearBuffers() 'clear out waiting for next command

            ElseIf WirelessEnabled Then
                'WiFiSemaphore guarantees only one command is in flight at a time on the ESP8266.
                'Uses SemaphoreSlim(1,1) which is NOT reentrant - if the same thread somehow
                're-enters CommandString, it will block (caught by master timeout) rather than
                'silently corrupting the stream. 50ms refraction time after each command gives
                'the ESP breathing room before the next command.
                If Not WiFiSemaphore.Wait(WIFI_MASTER_TIMEOUT) Then
                    TL.LogMessage("CommandString", "WiFi master timeout on " & Command & " - ESP unresponsive, semaphore held > " & WIFI_MASTER_TIMEOUT & "ms")
                    cmdString = ""
                Else
                Try
                    Try
                        'Use persistent TCP connection
                        EnsureWiFiConnected()

                        'Send command
                        sendBytes = System.Text.Encoding.ASCII.GetBytes(Command)
                        TL.LogMessage("CommandString", "WiFi Sending: " & Command)
                        objTCPStream.Write(sendBytes, 0, sendBytes.Length)

                        'Read response until ! terminator
                        cmdString = ReadUntilTerminator()
                        TL.LogMessage("CommandString", "WiFi Received: " & cmdString)

                    Catch ex As Exception
                        TL.LogMessage("CommandString", "WiFi exception on cmd " & Command & ": " & ex.GetType().Name & " - " & ex.Message)

                        'Retry 1: just try reading again - the ESP8266 busy-send cycle
                        'likely delayed the response but it will arrive momentarily.
                        'Do NOT re-send the command (that causes cascading busy on the next cmd).
                        Try
                            TL.LogMessage("CommandString", "WiFi retry1 (read-only, no resend)")
                            cmdString = ReadUntilTerminator()
                            TL.LogMessage("CommandString", "WiFi retry1 succeeded: " & cmdString)
                        Catch ex2 As Exception
                            'Retry 2: resend on same connection
                            TL.LogMessage("CommandString", "WiFi retry1 failed: " & ex2.GetType().Name & " - " & ex2.Message)

                            Try
                                'Flush any stale data from the stream before retrying
                                If objTCPStream IsNot Nothing AndAlso objTCPStream.DataAvailable Then
                                    Dim flush(1023) As Byte
                                    objTCPStream.Read(flush, 0, flush.Length)
                                End If
                                System.Threading.Thread.Sleep(50)
                                sendBytes = System.Text.Encoding.ASCII.GetBytes(Command)
                                TL.LogMessage("CommandString", "WiFi retry2 (resend same conn): " & Command)
                                objTCPStream.Write(sendBytes, 0, sendBytes.Length)
                                cmdString = ReadUntilTerminator()
                                TL.LogMessage("CommandString", "WiFi retry2 succeeded: " & cmdString)
                            Catch ex3 As Exception
                                'Retry 3: close, reconnect, try once more
                                TL.LogMessage("CommandString", "WiFi retry2 failed: " & ex3.GetType().Name & " - " & ex3.Message)
                                CloseWiFiConnection()

                                Try
                                    EnsureWiFiConnected()
                                    sendBytes = System.Text.Encoding.ASCII.GetBytes(Command)
                                    TL.LogMessage("CommandString", "WiFi retry3 (reconnect): " & Command)
                                    objTCPStream.Write(sendBytes, 0, sendBytes.Length)
                                    cmdString = ReadUntilTerminator()
                                    TL.LogMessage("CommandString", "WiFi retry3 succeeded: " & cmdString)
                                Catch ex4 As Exception
                                    TL.LogMessage("CommandString", "WiFi retry3 failed: " & ex4.Message)
                                    CloseWiFiConnection()
                                    cmdString = ""  'All retries exhausted - return empty so callers don't parse the original command as a response
                                End Try
                            End Try
                        End Try
                    End Try

                    'Refraction time - give ESP breathing room before next command.
                    'MEASURED 2026-07-05 straight to the mount (raw sockets, no driver): a
                    'command sent ~0ms after the previous reply is silently dropped ~50% of
                    'the time; any gap >= ~10ms => 100% success. This is THE fix for the slow/
                    'dropped-command stalls (each drop otherwise cost ~1s of read-timeout+resend).
                    System.Threading.Thread.Sleep(WIFI_REFRACTION)

                Finally
                    WiFiSemaphore.Release()
                End Try
                End If

            End If
        ElseIf Not IsConnected Then
            Throw New ASCOM.MethodNotImplementedException("CommandString")
        End If
        Return cmdString

    End Function
    Private Function VerifyWiFiComm() As Boolean
        'Diagnostic method - verifies WiFi communication using persistent connection
        Try
            TL.LogMessage("VerifyWiFiComm", "Verifying WiFi communication to " & IPAddress)

            EnsureWiFiConnected()

            'Send ESGp0! to verify communication
            Dim sendBytes As [Byte]() = System.Text.Encoding.ASCII.GetBytes("ESGp0!")
            objTCPStream.Write(sendBytes, 0, sendBytes.Length)
            Dim responseData As String = ReadUntilTerminator()
            TL.LogMessage("VerifyWiFiComm", "Response: " & responseData)

            If Left(responseData, 5) = "ESGp0" Then
                TL.LogMessage("VerifyWiFiComm", "WiFi communication verified successfully")
                Return True
            Else
                TL.LogMessage("VerifyWiFiComm", "Invalid response from PMC-Eight")
                Return False
            End If

        Catch ex As Exception
            TL.LogMessage("VerifyWiFiComm", "WiFi verification failed: " & ex.Message)
            CloseWiFiConnection()
            Return False
        End Try

    End Function

    Private Function VerifySerialComml() As Boolean
        'Declare local parameters
        Dim RAPos1 As Int32
        Dim RAPos2 As Int32
        Dim DECPos1 As Int32
        Dim RACommand As String
        Dim DECCommand As String
        Dim RAReceived As String
        Dim DECReceived As String

        Try
            objSerial = New ASCOM.Utilities.Serial
            TL.LogMessage("Connected Set", "Connecting to port " + comPort)
            If Len(comPort) = 4 Then
                objSerial.Port = Right(comPort,
                                       1)
            ElseIf Len(comPort) = 5 Then
                objSerial.Port = Right(comPort,
                                       2)
            End If
            objSerial.Speed = comSpeed
            objSerial.ReceiveTimeout = 1
            objSerial.DataBits = 8
            objSerial.StopBits = SerialStopBits.One
            objSerial.Parity = SerialParity.None
            objSerial.Handshake = SerialHandshake.None
            objSerial.RTSEnable = False
            objSerial.DTREnable = False
            objSerial.Connected = True
            objSerial.ClearBuffers()
            connectedState = True

            'Determine Tracking Status and set RATracking value
            RAPos1 = GetRAMotorPosition()
            'utilities.WaitForMilliseconds(500)
            RAPos2 = GetRAMotorPosition()
            DECPos1 = GetDECMotorPosition()

            If Math.Abs(RAPos1 - RAPos2) > 0 Then
                RATracking = True
            ElseIf Math.Abs(RAPos1 - RAPos2) = 0 Then
                RATracking = False
            End If

            'Determine Park Status and Home Status and set accordingly
            If (RAPos1 = ParkRAPosition) And (DECPos1 = ParkDECPosition) Then
                ParkStatus = True
            Else
                'set motor position to saved parked position
                RACommand = "ESSp0" & Mid(ParkRAPosition.ToString("X8"), 3, 6) & "!"
                DECCommand = "ESSp1" & Mid(ParkDECPosition.ToString("X8"), 3, 6) & "!"
                'send command to set RA and DEC motors to PARK position
                SerMutex.WaitOne()
                RAReceived = CommandString(RACommand)
                SerMutex.ReleaseMutex()
                SerMutex.WaitOne()
                DECReceived = CommandString(DECCommand)
                SerMutex.ReleaseMutex()
                ParkStatus = True
            End If
            TL.LogMessage("Connected", "Determined and set PARK Position. PARKStatus=" & AtPark.ToString & " POSITION= " & RAPos1.ToString & ", " & DECPos1)
            'set Home Status to true only if at NCP motor position 0,0
            If (RAPos1 = 0) And (DECPos1 = 0) Then
                HomeStatus = True
            Else
                HomeStatus = False
            End If
            TL.LogMessage("Connected", "Determined HOME Status. HOMEStatus=" & AtPark.ToString)
        Catch e As Sockets.SocketException
            Console.WriteLine(e.ToString())
        End Try
        connectedState = True
    End Function
    Public Property Connected() As Boolean Implements ITelescopeV3.Connected

        Get
            TL.LogMessage("Connected Get", IsConnected.ToString())
            Return IsConnected
        End Get
        Set(value As Boolean)
            'Declare local parameters
            Dim RAPos1 As Int32
            Dim RAPos2 As Int32
            Dim DECPos1 As Int32
            Dim RACommand As String
            Dim DECCommand As String
            Dim RA_SRFCommand As String
            Dim DEC_SRFCommand As String
            Dim RAReceived As String
            Dim DECReceived As String

            TL.LogMessage("Connected Set", value.ToString())
            If value = IsConnected Then
                Return
            End If

            If value Then
                'connectedState = True
                ' TODO connect to the device
                'Set serial parameters and Connect to comport ---------------------
                If Not WirelessEnabled Then
                    Try
                        objSerial = New ASCOM.Utilities.Serial
                        TL.LogMessage("Connected Set", "Connecting to port " + comPort)
                        If Len(comPort) = 4 Then
                            objSerial.Port = Right(comPort,
                                                   1)
                        ElseIf Len(comPort) = 5 Then
                            objSerial.Port = Right(comPort,
                                                   2)
                        End If

                        objSerial.Speed = comSpeed
                        objSerial.ReceiveTimeout = 1
                        objSerial.DataBits = 8
                        objSerial.StopBits = SerialStopBits.One
                        objSerial.Parity = SerialParity.None
                        objSerial.Handshake = SerialHandshake.None
                        objSerial.RTSEnable = False
                        objSerial.DTREnable = False
                        objSerial.Connected = True
                        objSerial.ClearBuffers()
                        connectedState = True
                        TL.LogMessage("Connected Set", "Connection Successful to port " + comPort)
                        '                        MessageBox.Show("Go when connected")   'BT connect delay

                        'set Sidereal Rate Fraction values to saved SRF values
                        RA_SRFCommand = "ESSf000" & Hex(RA_SiderealRateFraction).ToString & "!"
                        DEC_SRFCommand = "ESSf100" & Hex(DEC_SiderealRateFraction).ToString & "!"

                        'send command to set RA and DEC SRF values into PMC-Eight eeprom
                        SerMutex.WaitOne()
                        RAReceived = CommandString(RA_SRFCommand)
                        SerMutex.ReleaseMutex()
                        SerMutex.WaitOne()
                        DECReceived = CommandString(DEC_SRFCommand)
                        Debug.Print(DECReceived)
                        SerMutex.ReleaseMutex()
                        TL.LogMessage("Connected Set", "Completed Setting SRF Value in EEPROM")

                        DeclinationRate = DECRateOffsetValue * 0.9972695677     'conformu: sort of.  add this to pick up new dec rate offset parameter fromsetup page, prescale because units are converted in declinationrate


                        'Determine Tracking Status and set RATracking value
                        RAPos1 = GetRAMotorPosition()
                        utilities.WaitForMilliseconds(1000)
                        RAPos2 = GetRAMotorPosition()
                        DECPos1 = GetDECMotorPosition()

                        TL.LogMessage("Connected", "RA Position:" & RAPos1.ToString)
                        TL.LogMessage("Connected", "DEC Position:" & DECPos1.ToString)

                        'If (RAPos1 <> 0) And (RAPos1 <> ParkRAPosition) Then
                        'Tracking = True
                        ' If there is no motion in RA then Mount is not tracking
                        If RAPos1 <> RAPos2 Then
                            Tracking = True    'if the two ra positions are the same then it tracking
                        Else
                            Tracking = False   'if the two positions are not the same then tracking (or slewing)
                        End If
                        '  Determine if slewing
                        If Slewing Then
                            Tracking = False
                        End If

                        'set Home Status to true only if at NCP/SCP motor position 0,0
                        '
                        'If (RAPos1 = 0) And (DECPos1 = 0) Then
                        If (Not Tracking) And (Math.Abs(RAPos1) < 10) And (DECPos1 = 0) Then   'include some deadband
                            HomeStatus = True
                            TL.LogMessage("Connected", "Determined HOME Status. HOMEStatus=" & AtHome.ToString)
                            Tracking = False
                            TL.LogMessage("Connected", "Startup Tracking FALSE")
                        Else
                            HomeStatus = False
                            TL.LogMessage("Connected", "Determined HOME Status. HOMEStatus=" & AtHome.ToString)
                            'Tracking = True
                            'TL.LogMessage("Connected", "Startup Tracking TRUE")
                        End If

                        'Determine Park Status and Home Status and set accordingly if not tracking on startup
                        If Tracking = False And AtPark Then
                            'set motor position to saved parked position
                            RACommand = "ESSp0" & Mid(ParkRAPosition.ToString("X8"), 3, 6) & "!"
                            DECCommand = "ESSp1" & Mid(ParkDECPosition.ToString("X8"), 3, 6) & "!"
                            'send command to set RA and DEC motors to PARK position
                            SerMutex.WaitOne()
                            RAReceived = CommandString(RACommand)
                            SerMutex.ReleaseMutex()
                            SerMutex.WaitOne()
                            DECReceived = CommandString(DECCommand)
                            SerMutex.ReleaseMutex()
                            ParkStatus = True
                        End If
                        TL.LogMessage("Connected", "Determined and set PARK Position. PARKStatus=" & AtPark.ToString & " POSITION= " & RAPos1.ToString & ", " & DECPos1)
                    Catch e As Sockets.SocketException
                        Console.WriteLine(e.ToString())
                    End Try
                    '                    connectedState = True
                ElseIf WirelessEnabled Then
                    'Open persistent TCP connection to PMC-Eight WiFi module
                    Try
                        EnsureWiFiConnected()

                        'Verify communication by sending ESGp0!
                        Dim responseData As String = ""
                        If WiFiSemaphore.Wait(WIFI_MASTER_TIMEOUT) Then
                            Try
                                Dim verifyBytes As [Byte]() = System.Text.Encoding.ASCII.GetBytes("ESGp0!")
                                objTCPStream.Write(verifyBytes, 0, verifyBytes.Length)
                                responseData = ReadUntilTerminator()
                                'System.Threading.Thread.Sleep(WIFI_REFRACTION)
                            Finally
                                WiFiSemaphore.Release()
                            End Try
                        End If
                        TL.LogMessage("Connected WiFi", "Verify response: " & responseData)

                        'Check if response is valid and set connection state to True if valid
                        If Left(responseData, 5) = "ESGp0" Then
                            connectedState = True
                            TL.LogMessage("Connected Set", "Connected to IP Address Successful " & IPAddress)
                        End If

                        'Determine Tracking Status and set RATracking value
                        RAPos1 = GetRAMotorPosition()
                        RAPos2 = GetRAMotorPosition()
                        DECPos1 = GetDECMotorPosition()

                        If Math.Abs(RAPos1 - RAPos2) > 0 Then
                            RATracking = True
                        ElseIf Math.Abs(RAPos1 - RAPos2) = 0 Then
                            RATracking = False
                        End If

                        'Determine Park Status and Home Status and set accordingly
                        If (RAPos1 = ParkRAPosition) And (DECPos1 = ParkDECPosition) Then
                            ParkStatus = True
                        Else
                            'set motor position to saved parked position
                            RACommand = "ESSp0" & Mid(ParkRAPosition.ToString("X8"), 3, 6) & "!"
                            DECCommand = "ESSp1" & Mid(ParkDECPosition.ToString("X8"), 3, 6) & "!"
                            'send command to set RA and DEC motors to PARK position
                            SerMutex.WaitOne()
                            RAReceived = CommandString(RACommand)
                            SerMutex.ReleaseMutex()
                            SerMutex.WaitOne()
                            DECReceived = CommandString(DECCommand)
                            SerMutex.ReleaseMutex()
                            ParkStatus = True
                        End If
                        TL.LogMessage("Connected", "Determined and set PARK Position. PARKStatus=" & AtPark.ToString & " POSITION= " & RAPos1.ToString & ", " & DECPos1)
                        'set Home Status to true only if at NCP motor position 0,0
                        If (RAPos1 = 0) And (DECPos1 = 0) Then
                            HomeStatus = True
                        Else
                            HomeStatus = False
                        End If
                        TL.LogMessage("Connected", "Determined HOME Status. HOMEStatus=" & AtHome.ToString)
                    Catch ex As Exception
                        TL.LogMessage("Connected Set", "WiFi connection failed: " & ex.Message)
                        CloseWiFiConnection()
                        connectedState = False
                    End Try
                End If
                '------------------------------------------------
            Else
                '                connectedState = False
                ' TODO disconnect from the device
                '------------------------------------------------
                If Not WirelessEnabled Then
                    TL.LogMessage("Connected Set", "Disconnecting from port " + comPort)
                    objSerial.ClearBuffers()
                    objSerial.Connected = False
                    objSerial.Dispose()
                    objSerial = Nothing
                    connectedState = False
                Else
                    TL.LogMessage("Connected Set", "Disconnecting from Network")
                    CloseWiFiConnection()
                    connectedState = False
                End If
                '------------------------------------------------
            End If
        End Set
    End Property

    Public ReadOnly Property Description As String Implements ITelescopeV3.Description
        Get
            ' this pattern seems to be needed to allow a public property to return a private field
            Dim d As String = driverDescription
            TL.LogMessage("Description Get", d)
            Return d
        End Get
    End Property

    Public ReadOnly Property DriverInfo As String Implements ITelescopeV3.DriverInfo
        Get
            Dim m_version As Version = System.Reflection.Assembly.GetExecutingAssembly().GetName().Version
            ' TODO customise this driver description
            Dim s_driverInfo As String = "Explore Scientific PMC-Eight Mount Controller ASCOM Driver. Developed by GRHubbell. Contact Explore Scientific at www.explorescientificusa.com . Version: " + m_version.Major.ToString() + "." + m_version.Minor.ToString()
            TL.LogMessage("DriverInfo Get", s_driverInfo)
            Return s_driverInfo
        End Get
    End Property

    Public ReadOnly Property DriverVersion() As String Implements ITelescopeV3.DriverVersion
        Get
            ' Get our own assembly and report its version number
            TL.LogMessage("DriverVersion Get", Reflection.Assembly.GetExecutingAssembly.GetName.Version.ToString(2))
            Return Reflection.Assembly.GetExecutingAssembly.GetName.Version.ToString(2)
        End Get
    End Property

    Public ReadOnly Property InterfaceVersion() As Short Implements ITelescopeV3.InterfaceVersion
        Get
            TL.LogMessage("InterfaceVersion Get", "3")
            Return 3
        End Get
    End Property

    Public ReadOnly Property Name As String Implements ITelescopeV3.Name
        Get
            Dim s_name As String = "Explore Scientific PMC-Eight ASCOM Driver"
            TL.LogMessage("Name Get", s_name)
            Return s_name
        End Get
    End Property

    Public Sub Dispose() Implements ITelescopeV3.Dispose
        ' Clean up WiFi connection if active
        CloseWiFiConnection()
        ' Clean up the tracelogger and util objects
        TL.Enabled = False
        TL.Dispose()
        TL = Nothing
        utilities.Dispose()
        utilities = Nothing
        astroUtilities.Dispose()
        astroUtilities = Nothing
    End Sub

#End Region

#Region "ITelescope Implementation"
    Public Sub AbortSlew() Implements ITelescopeV3.AbortSlew
        Dim abortCommand As String
        Try
            If Not AtPark Then
                '*********************************************************************************************************************************************
                'TODO Put code here to check if slewing and if so use the ESPt3 command to ramp down prior to
                'aborting the slew. GRH 2021-05-23

                If Slewing Then
                    'send command to ramp down from slewing on both axes
                    abortCommand = "ESPt3000000!"
                    SerMutex.WaitOne()
                    CommandString(abortCommand)
                    SerMutex.ReleaseMutex()
                End If

                'wait until mount ramps down before starting slew to new target
                While Slewing
                    utilities.WaitForMilliseconds(50)
                    Application.DoEvents()
                End While

                '*********************************************************************************************************************************************
                'Set right ascension tracking Rate to 0 (zero)
                abortCommand = "ESSr00000!"
                SerMutex.WaitOne()
                CommandString(abortCommand)
                SerMutex.ReleaseMutex()
                'Set declination Rate to 0 (zero)
                abortCommand = "ESSr10000!"
                SerMutex.WaitOne()
                CommandString(abortCommand)
                SerMutex.ReleaseMutex()
                If RATracking = True Then
                    Tracking = True
                Else
                    Tracking = False
                End If
            Else
                TL.LogMessage("AbortSlew", "Parked!")
                Throw New ASCOM.ParkedException("AbortSlew")
            End If
        Catch ex As Exception
            TL.LogMessage("AbortSlew", "Invalid Operation")
            Throw New ASCOM.InvalidOperationException("AbortSlew")
        End Try

    End Sub

    Public ReadOnly Property AlignmentMode() As AlignmentModes Implements ITelescopeV3.AlignmentMode
        Get
            TL.LogMessage("AlignmentMode Get", "Implemented")
            AlignmentMode = AlignmentModes.algGermanPolar
            Return AlignmentMode

            'Throw New ASCOM.PropertyNotImplementedException("AlignmentMode", False)
        End Get
    End Property

    Public ReadOnly Property Altitude() As Double Implements ITelescopeV3.Altitude
        Get
            Dim Altitude__1 As Double
            'Dim AltReceived As String = "0"

            If IsConnected Then
                'objTransform.SetApparent(Me.RightAscension, Me.Declination)
                'objTransform = New ASCOM.Astrometry.Transform.Transform
                Me.objTransform.SetTopocentric(Me.RightAscension, Me.Declination)

                Altitude__1 = objTransform.ElevationTopocentric


                'Altitude__1 = 90.0 - Me.SiteLatitude + Me.Declination
            ElseIf Not IsConnected Then
                Throw New ASCOM.MethodNotImplementedException("Altitude")
            End If
            TL.LogMessage("Altitude", "Get - " & Altitude__1)
            Return Altitude__1
        End Get
    End Property

    Public ReadOnly Property ApertureArea() As Double Implements ITelescopeV3.ApertureArea
        Get
            TL.LogMessage("ApertureDiameter", "Get - " & ApertureAreaValue.ToString)
            Return ApertureAreaValue
        End Get
    End Property

    Public ReadOnly Property ApertureDiameter() As Double Implements ITelescopeV3.ApertureDiameter
        Get
            TL.LogMessage("ApertureDiameter", "Get - " & ApertureDiameterValue.ToString)
            Return ApertureDiameterValue
        End Get
    End Property

    Public ReadOnly Property AtHome() As Boolean Implements ITelescopeV3.AtHome
        Get
            TL.LogMessage("AtHome", "Get - " & HomeStatus.ToString())
            Return HomeStatus
        End Get
    End Property

    Public ReadOnly Property AtPark() As Boolean Implements ITelescopeV3.AtPark
        Get
            TL.LogMessage("AtPark", "Get - " & ParkStatus.ToString())
            Return ParkStatus
        End Get
    End Property

    Public Function AxisRates(Axis As TelescopeAxes) As IAxisRates Implements ITelescopeV3.AxisRates
        Dim max_Scope_Rate As Double
        If Axis = TelescopeAxes.axisPrimary Then
            max_Scope_Rate = (MountMaxSpeed * 360.0) / MountRACounts - 0.1
        ElseIf Axis = TelescopeAxes.axisSecondary Then
            max_Scope_Rate = (MountMaxSpeed * 360.0) / MountDECCounts - 0.1
        End If

        TL.LogMessage("AxisRates", "Get - " & Axis.ToString())
        TL.LogMessage("AxisRates", "Max rate is " & max_Scope_Rate.ToString())
        Return New AxisRates(Axis, max_Scope_Rate, SkySafari_rate)
    End Function

    Public ReadOnly Property Azimuth() As Double Implements ITelescopeV3.Azimuth
        Get
            Dim Azimuth__1 As Double
            Dim AzReceived As String = "0"

            If IsConnected Then
                'Azimuth__1 = Me.RightAscension
                'objTransform.SetApparent(Me.RightAscension, Me.Declination)
                'objTransform = New ASCOM.Astrometry.Transform.Transform
                Me.objTransform.SetTopocentric(Me.RightAscension, Me.Declination)

                Azimuth__1 = objTransform.AzimuthTopocentric

            ElseIf Not IsConnected Then
                Throw New ASCOM.MethodNotImplementedException("Azimuth")
            End If
            TL.LogMessage("Azimuth", "Get - " & Azimuth__1)
            Return Azimuth__1
        End Get
    End Property

    Public ReadOnly Property CanFindHome() As Boolean Implements ITelescopeV3.CanFindHome
        Get
            TL.LogMessage("CanFindHome", "Get - " & True.ToString())
            Return True
        End Get
    End Property

    Public Function CanMoveAxis(Axis As TelescopeAxes) As Boolean Implements ITelescopeV3.CanMoveAxis
        TL.LogMessage("CanMoveAxis", "Get - " & Axis.ToString())
        Select Case Axis
            Case TelescopeAxes.axisPrimary
                Return True
            Case TelescopeAxes.axisSecondary
                Return True
            Case TelescopeAxes.axisTertiary
                Return False
                'Case Else
                'Throw New ASCOM.InvalidValueException("CanMoveAxis", Axis.ToString(), "2")
                'Return False
        End Select
    End Function

    Public ReadOnly Property CanPark() As Boolean Implements ITelescopeV3.CanPark
        Get
            TL.LogMessage("CanPark", "Get - " & True.ToString())
            Return True
        End Get
    End Property

    Public ReadOnly Property CanPulseGuide() As Boolean Implements ITelescopeV3.CanPulseGuide
        Get
            TL.LogMessage("CanPulseGuide", "Get - " & True.ToString())
            Return True
        End Get
    End Property

    Public ReadOnly Property CanSetDeclinationRate() As Boolean Implements ITelescopeV3.CanSetDeclinationRate
        Get
            TL.LogMessage("CanSetDeclinationRate", "Get - " & True.ToString())
            Return True
        End Get
    End Property

    Public ReadOnly Property CanSetGuideRates() As Boolean Implements ITelescopeV3.CanSetGuideRates
        Get
            TL.LogMessage("CanSetGuideRates", "Get - " & True.ToString())
            Return True
        End Get
    End Property

    Public ReadOnly Property CanSetPark() As Boolean Implements ITelescopeV3.CanSetPark
        Get
            TL.LogMessage("CanSetPark", "Get - " & True.ToString())
            Return True
        End Get
    End Property

    Public ReadOnly Property CanSetPierSide() As Boolean Implements ITelescopeV3.CanSetPierSide
        Get
            If ScottyMount Or MSROMount Then
                TL.LogMessage("CanSetPierSide", "Get for Fork/EQ Mount - " & False.ToString())
                Return False
            Else
                TL.LogMessage("CanSetPierSide", "Get - " & True.ToString())
                Return True
            End If
        End Get
    End Property

    Public ReadOnly Property CanSetRightAscensionRate() As Boolean Implements ITelescopeV3.CanSetRightAscensionRate
        Get
            TL.LogMessage("CanSetRightAscensionRate", "Get - " & True.ToString())
            Return True
        End Get
    End Property

    Public ReadOnly Property CanSetTracking() As Boolean Implements ITelescopeV3.CanSetTracking
        Get
            TL.LogMessage("CanSetTracking", "Get - " & True.ToString())
            Return True
        End Get
    End Property

    Public ReadOnly Property CanSlew() As Boolean Implements ITelescopeV3.CanSlew
        Get
            TL.LogMessage("CanSlew", "Get - " & True.ToString())
            Return True
        End Get
    End Property

    Public ReadOnly Property CanSlewAltAz() As Boolean Implements ITelescopeV3.CanSlewAltAz
        Get
            TL.LogMessage("CanSlewAltAz", "Get - " & True.ToString())
            Return True
        End Get
    End Property

    Public ReadOnly Property CanSlewAltAzAsync() As Boolean Implements ITelescopeV3.CanSlewAltAzAsync
        Get
            TL.LogMessage("CanSlewAltAzAsync", "Get - " & True.ToString())
            Return True
        End Get
    End Property

    Public ReadOnly Property CanSlewAsync() As Boolean Implements ITelescopeV3.CanSlewAsync
        Get
            TL.LogMessage("CanSlewAsync", "Get - " & True.ToString())
            Return True
        End Get
    End Property

    Public ReadOnly Property CanSync() As Boolean Implements ITelescopeV3.CanSync
        Get
            TL.LogMessage("CanSync", "Get - " & True.ToString())
            'Throw New ASCOM.MethodNotImplementedException
            Return True
        End Get
    End Property

    Public ReadOnly Property CanSyncAltAz() As Boolean Implements ITelescopeV3.CanSyncAltAz
        Get
            TL.LogMessage("CanSyncAltAz", "Get - " & True.ToString())
            'Throw New ASCOM.MethodNotImplementedException
            Return True
        End Get
    End Property

    Public ReadOnly Property CanUnpark() As Boolean Implements ITelescopeV3.CanUnpark
        Get
            TL.LogMessage("CanUnpark", "Get - " & True.ToString())
            Return True
        End Get
    End Property

    Public ReadOnly Property Declination() As Double Implements ITelescopeV3.Declination
        Get
            Dim declination_1 As Double
            If IsConnected Then
                declination_1 = MotorCounts_to_DEC(GetDECMotorPosition())
            ElseIf Not IsConnected Then
                Throw New ASCOM.NotConnectedException("Declination")
            End If
            If declination_1 >= 0 Then
                TL.LogMessage("Declination", "Get - +" & utilities.DegreesToDMS(declination_1, "d", Chr(39), Chr(34)))
            Else
                TL.LogMessage("Declination", "Get - " & utilities.DegreesToDMS(declination_1, "d", Chr(39), Chr(34)))
            End If
            Return declination_1
        End Get
    End Property

    Public Property DeclinationRate() As Double Implements ITelescopeV3.DeclinationRate
        Get



            Dim tempDECRate As Double


            '  below removed for Conformu, Dim statements also removed for varables no longer used
            ''{

            'Try
            '    If IsConnected Then
            '        SOP = SideOfPier
            '        SerMutex.WaitOne()
            '        tempDecdir = CommandString("ESGd1!")
            '        SerMutex.ReleaseMutex()
            '        If Mid(tempDecdir, 6, 1) = "1" Then
            '            ratesign = 1
            '            pier = "East dir =1,  rate positive"
            '            If SOP = PierSide.pierWest Then
            '                ratesign = -1
            '                pier = "West dir = 1, rate negative "
            '            End If
            '        End If

            '        If Mid(tempDecdir, 6, 1) = "0" Then
            '            ratesign = -1
            '            pier = "EAST dir = 0, rate negative"
            '            If SOP = PierSide.pierWest Then
            '                ratesign = 1
            '                pier = "West dir = 1 rate positive"
            '            End If
            '        End If
            '        SerMutex.WaitOne()
            '        tempDECRate = CommandString("ESGr1!")
            '        SerMutex.ReleaseMutex()
            '        DecRate = Convert.ToInt32("0000" + Mid(tempDECRate, 6, 4), 16)
            '        DECratearcsec = (DecRate * (1296000 / Telescope.MountDECCounts)) * ratesign   'arc secs

            '    ElseIf Not IsConnected Then
            '        Throw New ASCOM.NotConnectedException("DeclinationRate")
            '    End If


            'Catch ex As Exception
            '    Throw New ASCOM.NotConnectedException("DeclinationRate")
            '
            'End Try}
            '
            '  added for Conformu
            '
            '  DEclination rate is sent in in arc sec per SI second.  And SI second is a tad longer than a sidereal second,
            '  conversion factor is 0.9972695677 per ascom documentation
            '  the rate offset is stored as arc sec per second, thus we divide stored rate by scle factor and return same
            tempDECRate = DECRateOffsetValue / 0.9972695677

            TL.LogMessage("DeclinationRate", "Get: " & tempDECRate.ToString)
            Return tempDECRate

        End Get
        Set(value As Double)
            Try

                Dim cmdString As String
                'Dim rcvString As String
                Dim arcSecPerCount As Double
                Dim ratevalue As Double
                Dim intratevalue As Int32
                '               Dim PreferredDir As String
                '               Dim PreferredDirBar As String
                Dim rate As Double
                Dim command As String
                Dim recvstring As String

                If ABS_Value(value) < 0.1 Then    ' limit declination rate offset to +-.1"/solar second or larger
                    TL.LogMessage("DeclinationRate", "Rate setting too low (less than 0.1), corrected to 0, value was " & value.ToString)
                    value = 0.00
                End If
                MountDeclinationRate = value * 0.9972695677 'conformU scale factor applied      'set the property so conform does not run over our set rate timeing which is a bit long due to all the pmc8 commands
                DECRateOffsetValue = MountDeclinationRate  'new conformU, we save dec rate in arc seconds per sidereal sec which is pmc8 currency

                '
                '  this is the new stuff that uses the prefered directions and the pierside for the dec rate command
                '
                rate = DECRateOffsetValue    'conformU change  rate here is arc sec/sidereal sec
                TL.LogMessage("DeclinationRate", "SET rate: " & rate.ToString & " arc secs per sec")
                '
                '  for an EQ mount like MSRO, DEC must move in direction 0 to move to north, pierside makes no sense
                '  direction 1 to move south.
                '  In southern Hemisphere this is reversed...
                '

                If (SideOfPier = PierSide.pierWest) Or (ScottyMount Or MSROMount) Then  'looking east, so set dec direction accoding to sign of rate
                    If rate > 0 Then    'assume positive rate is toward north pole (assumed direction)
                        If ((ScottyMount Or MSROMount) And SiteLatitude >= 0.000) Then
                            command = "ESSd10" '      move CcW toward north pole
                            TL.LogMessage("DeclinationRate", " positive rate, EQ and North Lat, move north")
                        ElseIf ((ScottyMount Or MSROMount) And SiteLatitude < 0.000) Then
                            command = "ESSd11"  'southern hemisphere if EQ mount
                            TL.LogMessage("DeclinationRate", " positive rate, EQ and South Lat, move north")
                        Else
                            command = "ESSd10" '      move CcW toward north pole
                            TL.LogMessage("DeclinationRate", " Pier west, pos rate")
                        End If

                        SerMutex.WaitOne()
                        recvstring = CommandString(command)
                        SerMutex.ReleaseMutex()

                    Else                'rate negative to move to south pole (assumption)
                        If ((ScottyMount Or MSROMount) And SiteLatitude >= 0.00) Then
                            command = "ESSd11" '      move CW toward south pole
                            TL.LogMessage("DeclinationRate", " negative rate, EQ and North Lat, move south")
                        ElseIf ((ScottyMount Or MSROMount) And SiteLatitude < 0.00) Then
                            command = "ESSd10"      'EQ in south reverse direction
                            TL.LogMessage("DeclinationRate", " negative rate, EQ and South Lat, move south")
                        Else
                            command = "ESSd11" '      move CW toward south pole
                            TL.LogMessage("DeclinationRate", " Pier west, neg rate")
                        End If

                        SerMutex.WaitOne()
                        recvstring = CommandString(command)
                        SerMutex.ReleaseMutex()

                    End If
                Else      'side of pier is pierside.east
                    If rate > 0 Then    'assume positive rate is toward north pole (assumed direction)
                        command = "ESSd11" '      move CW toward north pole
                        TL.LogMessage("DeclinationRate", " Pier East pos rate")
                    Else                'rate negative to move to south pole (assumption)
                        command = "ESSd10" '      move CCW toward south pole
                        TL.LogMessage("DeclinationRate", " Pier East neg rate")
                    End If
                    SerMutex.WaitOne()
                    recvstring = CommandString(command)
                    SerMutex.ReleaseMutex()

                End If



                If rate < 0 Then     '  direction of dec is set so we use the positvie value for rate
                    value = -rate
                Else
                    value = rate
                End If

                '
                '  this is the way it all was
                '''''''''''''''''''''''''''''''''''''''''''''''''''''''''
                arcSecPerCount = 1296000.0 / Telescope.MountDECCounts
                ' Set Tracking Rate for desired Rate (uses ESTr0000! command)

                ratevalue = (value / arcSecPerCount) * 25.0      'conformu full high precision value for rate means scale up by 25
                intratevalue = CInt(ratevalue)                   'this gets us the 4 hex nibbles
                cmdString = "ESTe1" & intratevalue.ToString("X4") & "!"       'conformu high precision DEC call

                SerMutex.WaitOne()
                CommandString(cmdString)
                SerMutex.ReleaseMutex()
                '        MessageBox.Show("returned from cmdstring")
                '                MountDeclinationRate = value
                '        MessageBox.Show("wrote value into mountdeclinationrate variable")
                TL.LogMessage("DeclinationRate", "SET - " & MountDeclinationRate.ToString)
            Catch ex As Exception
                TL.LogMessage("DeclinationRate Set", "Invalid Operation")
                Throw New ASCOM.InvalidOperationException("DeclinationRate")
            End Try
        End Set
    End Property

    Public Function DestinationSideOfPier(RightAscension As Double, Declination As Double) As PierSide Implements ITelescopeV3.DestinationSideOfPier

        Try
            Dim HA As Double
            HA = SiderealTime - RightAscension
            'If HA < 0 Then    'negative hourangle so test magnitude larger than 12
            '    If -1.0 * HA > 12.0# Then
            '        HA += 24.0#
            '    End If
            'Else
            '    If HA >= 12.0# Then
            '        HA -= 12.0
            '    End If
            'End If




            If HA < -12.0# Then
                HA = HA + 24.0#
            ElseIf HA >= 12.0# Then
                HA = HA - 24.0#
            End If

            'Pierside in north and south is the same given an hour angle
            If HA < 0.0# Then
                Return PierSide.pierWest
            Else
                Return PierSide.pierEast
            End If



            'If HA < 0.0# Then
            'Return PierSide.pierWest
            'Else
            'Return PierSide.pierEast
            'End If

            'Return PierSide.pierUnknown
        Catch ex As Exception
            TL.LogMessage("DestinationSideOfPier", "Invalid Operation")
            Throw New ASCOM.InvalidOperationException("DestinationSideOfPier")
        End Try

    End Function

    ''' <summary>
    ''' Returns the pier side for slew calculations, flipped for southern hemisphere.
    ''' DestinationSideOfPier follows ASCOM convention (same both hemispheres),
    ''' but DEC_to_MotorCounts needs the flipped value in the south.
    ''' </summary>
    Private Function SlewDestinationSideOfPier(RA As Double, DEC As Double) As PierSide
        Dim SOP As PierSide = DestinationSideOfPier(RA, DEC)
        If Telescope.SiteLatitudeValue < 0 Then
            If SOP = PierSide.pierWest Then
                SOP = PierSide.pierEast
            Else
                SOP = PierSide.pierWest
            End If
        End If
        Return SOP
    End Function

    Public Property DoesRefraction() As Boolean Implements ITelescopeV3.DoesRefraction
        Get
            Try
                DoesRefraction = ApplyRefractionCorrection
            Catch ex As Exception
                TL.LogMessage("DoesRefraction Get", "Error")
                Throw New ASCOM.InvalidOperationException("DoesRefraction")
            End Try
            'TL.LogMessage("DoesRefraction Get", "Not implemented")
            'Throw New ASCOM.PropertyNotImplementedException("DoesRefraction", False)
        End Get
        Set(value As Boolean)
            Try
                ApplyRefractionCorrection = value
                objTransform.Refraction = value
            Catch ex As Exception
                TL.LogMessage("DoesRefraction Set", "Error")
                Throw New ASCOM.InvalidOperationException("DoesRefraction")
            End Try
            'TL.LogMessage("DoesRefraction Set", "Not implemented")
            'Throw New ASCOM.PropertyNotImplementedException("DoesRefraction", True)
        End Set
    End Property

    Public ReadOnly Property EquatorialSystem() As EquatorialCoordinateType Implements ITelescopeV3.EquatorialSystem
        Get
            Dim equatorialSystem__1 As EquatorialCoordinateType = EquatorialCoordinateType.equTopocentric
            TL.LogMessage("DeclinationRate", "Get - " & equatorialSystem__1.ToString())
            Return equatorialSystem__1
        End Get
    End Property

    Public Sub FindHome() Implements ITelescopeV3.FindHome
        Dim RACommand As String
        Dim DECCommand As String
        Dim RAReceived As String
        Dim DECReceived As String

        Try
            If AtPark Then
                Throw New ASCOM.ParkedException("Parked!")
                Return
            End If
            If (GetRAMotorPosition() = 0 And GetDECMotorPosition() = 0) Then
                HomeStatus = True
                TL.LogMessage("FindHome", "HomeStatus = TRUE 0808")
                Return
            End If
            If (Not AtPark) And (Not Slewing) Then
                TL.LogMessage("Find Home", "Going Home!")
                Tracking = False
                'HomeStatus = True
                'RACommand = "ESPt0000000!"
                RACommand = "ESPt2000000!"      'gohome and stop
                DECCommand = "ESPt1000000!"
                'send command to slew to HOME position
                SerMutex.WaitOne()
                RAReceived = CommandString(RACommand)
                SerMutex.ReleaseMutex()
                SerMutex.WaitOne()
                DECReceived = CommandString(DECCommand)
                SerMutex.ReleaseMutex()

                'wait until reaching destination or starting a new slew in the middle of a slew.
                While Slewing
                    utilities.WaitForMilliseconds(200)
                    Application.DoEvents()
                End While
                '-------------------------------------------------------------------------------------------
                ' If stopped slewing before getting to the HOME position then don't set HOME status
                ' or stop tracking - GRH 2021-06-05
                '-------------------------------------------------------------------------------------------
                'Check current position and set status' accordingly
                If (Math.Abs(GetRAMotorPosition()) > 100) Or (Math.Abs(GetDECMotorPosition()) > 100) Then
                    HomeStatus = False
                    RATracking = True
                    Tracking = True
                    TL.LogMessage("FindHome", "HomeStatus = FALSE")
                ElseIf (Math.Abs(GetRAMotorPosition()) <= 100) And (Math.Abs(GetDECMotorPosition()) <= 100) Then
                    HomeStatus = True
                    RATracking = False
                    Tracking = False
                    TL.LogMessage("FindHome", "HomeStatus = TRUE")
                End If
            Else
                TL.LogMessage("Find Home", "HOMED!")
                Throw New ASCOM.ParkedException("HOMED!")
            End If
        Catch ex As Exception
            TL.LogMessage("FindHome", "Invalid Operation")
            Throw New ASCOM.InvalidOperationException("Find Home")
        End Try
    End Sub

    Public ReadOnly Property FocalLength() As Double Implements ITelescopeV3.FocalLength
        Get
            TL.LogMessage("ApertureDiameter", "Get - " & FocalLengthValue.ToString)
            Return FocalLengthValue
        End Get
    End Property

    Public Property GuideRateDeclination() As Double Implements ITelescopeV3.GuideRateDeclination
        ' Value is in degrees/second
        Get
            Try
                Dim GuideRateDeclination_1 As Double
                GuideRateDeclination_1 = (15.0 * (DEC_SiderealRateFraction / 100)) / 3600
                Return GuideRateDeclination_1
            Catch ex As Exception
                TL.LogMessage("GuideRateDeclination Get", "Error")
                Throw New ASCOM.InvalidOperationException("GuideRateDeclination Get")
            End Try
            'TL.LogMessage("GuideRateDeclination Get", "Not implemented")
            'Throw New ASCOM.PropertyNotImplementedException("GuideRateDeclination", False)
        End Get
        Set(value As Double)
            Try
                DEC_SiderealRateFraction = 100 * (3600 * value) / 15.0
            Catch ex As Exception
                TL.LogMessage("GuideRateDeclination Set", "Error")
                Throw New ASCOM.InvalidOperationException("GuideRateDeclination Set")
            End Try
            'TL.LogMessage("GuideRateDeclination Set", "Not implemented")
            'Throw New ASCOM.PropertyNotImplementedException("GuideRateDeclination", True)
        End Set
    End Property

    Public Property GuideRateRightAscension() As Double Implements ITelescopeV3.GuideRateRightAscension
        ' Value is in degrees/second
        Get
            Try
                Dim GuideRateRightAscension_1 As Double
                GuideRateRightAscension_1 = (15.0 * (RA_SiderealRateFraction / 100)) / 3600
                Return GuideRateRightAscension_1
            Catch ex As Exception
                TL.LogMessage("GuideRateRightAscension Get", "Error")
                Throw New ASCOM.InvalidOperationException("GuideRateRightAscension Get")
            End Try
            'TL.LogMessage("GuideRateRightAscension Get", "Not implemented")
            'Throw New ASCOM.PropertyNotImplementedException("GuideRateRightAscension", False)
        End Get
        Set(value As Double)
            Try
                RA_SiderealRateFraction = 100 * (3600 * value) / 15.0
            Catch ex As Exception
                TL.LogMessage("GuideRateRightAscension Set", "Error")
                Throw New ASCOM.InvalidOperationException("GuideRateRightAscension Set")
            End Try
            'TL.LogMessage("GuideRateRightAscension Set", "Not implemented")
            'Throw New ASCOM.PropertyNotImplementedException("GuideRateRightAscension", True)
        End Set
    End Property

    Public ReadOnly Property IsPulseGuiding() As Boolean Implements ITelescopeV3.IsPulseGuiding
        ' Query firmware for pulse guide state via ESGq! command
        ' Returns ESGqab where ab is 00 (not guiding) or 01/10/11 (guiding)
        Get
            Dim Command As String
            Dim Received As String
            Dim mutexgot As Boolean

            Try
                mutexgot = False
                If IsConnected Then
                    TL.LogMessage("IsPulseGuide", "Start of Call, RATracking = " & RATracking.ToString)

                    SerMutex.WaitOne()
                    mutexgot = True

                    Command = "ESGq!" ' Fetch guide state
                    Received = CommandString(Command)
                    If Mid(Received, 5, 2) <> "00" Then
                        IsPulseGuiding = True
                    Else
                        IsPulseGuiding = False
                    End If
                    pulseguidingState = IsPulseGuiding
                    TL.LogMessage("IsPulseGuide", "Pulse guide state is " & IsPulseGuiding.ToString)

                    SerMutex.ReleaseMutex()
                    mutexgot = False
                Else
                    TL.LogMessage("IsPulseGuide", "Not Connected!")
                    Throw New ASCOM.NotConnectedException("IsPulseGuide...")
                End If
            Catch ex As Exception
                TL.LogMessage("IsPulseGuide", "Exception: " & ex.Message)
                If mutexgot Then
                    SerMutex.ReleaseMutex()
                End If
            End Try
        End Get
    End Property

    Public Sub MoveAxis(Axis As TelescopeAxes, Rate As Double) Implements ITelescopeV3.MoveAxis

        If IsConnected Then
            Try 'to move the axis specified
                Dim Command As String
                Dim myRate As Integer
                Dim recvString As String
                Dim PreferredDir As String
                Dim PreferredDirBar As String
                'check to see if parked first and throw error if so...
                If AtPark Then
                    TL.LogMessage("MoveAxis", "At Park!")
                    Throw New ASCOM.ParkedException("PARKED!")
                    Return
                End If
                If MountRADir Then         'meaning preferred is 1
                    PreferredDir = "1"
                    PreferredDirBar = "0"
                    If Axis = TelescopeAxes.axisSecondary Then
                        Rate = -Rate     'reverese poth move direction if motor is oposite normal  only for the poth move buttons
                    End If

                Else                       'meaning preferred is 0
                    PreferredDir = "0"
                    PreferredDirBar = "1"
                    If Axis = TelescopeAxes.axisPrimary Then
                        Rate = -Rate     'reverese poth move direction if motor is oposite normal  only for the poth move buttons
                    End If
                End If

                If Axis = TelescopeAxes.axisPrimary Then

                    TL.LogMessage("MoveAxis", "Primary")
                    'Get and set direction as needed
                    If (Rate < -0.00125#) And ((Rate + (MountMaxSpeed * 360 / MountRACounts)) > 0.00) Then  'constrain rate to mount max move rate to pass conform this is for negative rates
                        'Get current direction - only swap if going opposite preferred way
                        Command = "ESGd0!"
                        SerMutex.WaitOne()
                        recvString = CommandString(Command)
                        SerMutex.ReleaseMutex()
                        If Mid(recvString, 6, 1) <> PreferredDir Then 'CW'   used to be = "0"
                            Command = "ESSd0" + PreferredDir + "!"  'used to be ESSr01
                            SerMutex.WaitOne()
                            recvString = CommandString(Command)
                            SerMutex.ReleaseMutex()
                            TL.LogMessage("MoveAxis", "<> Preferred Dir neg rate")
                        End If
                    ElseIf (Rate > 0.00125#) And Rate < (MountMaxSpeed * 360 / MountRACounts) Then   'constrain the max rate to mount max deg per second to pass conform this is for positive rates
                        'Get current direction - only swap if going opposite preferred way
                        Command = "ESGd0!"
                        SerMutex.WaitOne()
                        recvString = CommandString(Command)
                        SerMutex.ReleaseMutex()
                        If Mid(recvString, 6, 1) = PreferredDir Then 'CCW'   was = "1"
                            Command = "ESSd0" + PreferredDirBar + "!"    'Was ESSd00!
                            SerMutex.WaitOne()
                            recvString = CommandString(Command)
                            SerMutex.ReleaseMutex()
                            TL.LogMessage("MoveAxis", "= Preferred Dir positive  rate")
                        End If
                    ElseIf Rate = 0.0000# Then
                        'MoveAxis Rate=0 means restore tracking per ASCOM spec
                        If Tracking Then
                            Tracking = True     'set RA to tracking rate
                        Else
                            Tracking = False    'force the RA to stop
                        End If
                        TL.LogMessage("MoveAxis", "RA rate was 0 so set back to tracking rate")
                        Return
                    Else
                        Throw New ASCOM.InvalidValueException("MoveAxis")
                        TL.LogMessage("MoveAxis", "Invalid Rate Value")
                    End If
                    'Set rate
                    myRate = Convert.ToInt32((Math.Abs(Rate) * MountRACounts) / 360.0#)
                    Command = "ESSr0" + Format(myRate, "X4") + "!"
                    SerMutex.WaitOne()
                    recvString = CommandString(Command)
                    SerMutex.ReleaseMutex()
                    TL.LogMessage("MoveAxis", "RA Axis Move Rate Command: " & Command.ToString)
                ElseIf Axis = TelescopeAxes.axisSecondary Then
                    If (Not ScottyMount) And (Not MSROMount) And (SideOfPier = PierSide.pierEast) Then
                        Rate = -Rate                        'revers rate for everyone on pier east
                    End If

                    TL.LogMessage("MoveAxis", "axisSecondary Move: " & Rate.ToString)
                    '                   If ScottyMount Then
                    '                   Rate = -Rate
                    '                End If
                    'Get and set direction as needed
                    If (Rate < -0.00125#) And ((Rate + (MountMaxSpeed * 360 / MountDECCounts)) > 0.00) Then  'constrain rate to mount max move rate to pass conformThen
                        TL.LogMessage("MoveAxis", "Rate: " & Rate.ToString)
                        'Get current direction - only swap if going opposite preferred way
                        Command = "ESGd1!"
                        SerMutex.WaitOne()
                        recvString = CommandString(Command)
                        SerMutex.ReleaseMutex()
                        If Mid(recvString, 6, 1) = PreferredDir Then 'CW'  was ="1"
                            Command = "ESSd1" + PreferredDirBar + "!"    'used to be ESSd10!
                            SerMutex.WaitOne()
                            recvString = CommandString(Command)
                            SerMutex.ReleaseMutex()
                            TL.LogMessage("MoveAxis", " Secondary Preferred Dir neg rate")
                        End If
                    ElseIf Rate > 0.00125# And Rate < (MountMaxSpeed * 360 / MountDECCounts) Then   'constrain the max rate to mount max deg per second to pass conformThen
                        'Get current direction - only swap if going opposite preferred way
                        TL.LogMessage("MoveAxis", "Rate: " & Rate.ToString)
                        Command = "ESGd1!"
                        SerMutex.WaitOne()
                        recvString = CommandString(Command)
                        SerMutex.ReleaseMutex()
                        If Mid(recvString, 6, 1) <> PreferredDir Then 'CCW' was ="0"
                            Command = "ESSd1" + PreferredDir + "!"  ' used to be ESSd11!
                            SerMutex.WaitOne()
                            recvString = CommandString(Command)
                            SerMutex.ReleaseMutex()
                            TL.LogMessage("MoveAxis", " Secondary <>  Preferred Dir pos rate")
                        End If
                        TL.LogMessage("MoveAxis", "Direction gotten and set if needed: ")
                    ElseIf Rate = 0.0000# Then
                        'do nothing
                    Else
                        Throw New ASCOM.InvalidValueException("MoveAxis")
                        TL.LogMessage("MoveAxis", "Invalid Rate Value:" & Rate.ToString)
                    End If
                    'Set rate
                    TL.LogMessage("MoveAxis", "ABS RATE: " & Rate.ToString)
                    TL.LogMessage("MoveAxis", "MountDecCounts" & MountDECCounts.ToString)
                    TL.LogMessage("MoveAxis", "about to convert:")

                    myRate = Convert.ToInt32((Math.Abs(Rate) * MountDECCounts) / 360.0#)
                    TL.LogMessage("MoveAxis", "myRate:" & myRate.ToString)

                    Command = "ESSr1" + Format(myRate, "X4") + "!"
                    SerMutex.WaitOne()
                    recvString = CommandString(Command)
                    SerMutex.ReleaseMutex()
                    TL.LogMessage("MoveAxis", "DEC Axis Move Rate Command: " & Command)
                ElseIf Axis = TelescopeAxes.axisTertiary Then
                    TL.LogMessage("MoveAxis", "Method Not Implemented Tertiary")
                    Throw New ASCOM.MethodNotImplementedException("MoveAxis Tertiary")
                End If
            Catch ex As Exception
                TL.LogMessage("MoveAxis", "Invalid Value where I am looking")
                TL.LogMessage("MoveAxis", "ABS RATE: " & Rate.ToString)
                TL.LogMessage("MoveAxis", "MountDecCounts" & MountDECCounts.ToString)
                TL.LogMessage("MoveAxis", "about to convert:")
                Throw New ASCOM.InvalidValueException("MoveAxis")
            End Try
        Else
            TL.LogMessage("MoveAxis", "Not Connected")
            Throw New ASCOM.NotConnectedException("Not Connected")
        End If
    End Sub

    Public Sub Park() Implements ITelescopeV3.Park
        Dim RACommand As String
        Dim DECCommand As String
        Dim RAReceived As String
        Dim DECReceived As String

        Try
            If Not AtPark Then 'And (Not Slewing) Then
                TL.LogMessage("Park", "Parking Mount")
                'slewing during parking is not tracking CHANGED 2021-08-07 GRH
                'RATracking = False
                'Tracking = False
                'ParkStatus = True
                'Set Park to stored values
                'RACommand = "ESPt0" & Format(ParkRAPosition, "X6") & "!"
                'RACommand = "ESPt0" & Mid(ParkRAPosition.ToString("X8"), 3, 6) & "!"
                RACommand = "ESPt2" & Mid(ParkRAPosition.ToString("X8"), 3, 6) & "!"   'parking, stop mount
                '
                SerMutex.WaitOne()
                RAReceived = CommandString(RACommand)
                SerMutex.ReleaseMutex()
                utilities.WaitForMilliseconds(3000)     'wait for the RA to get ging to avoid DEC leg collision
                'DECCommand = "ESPt1" & Format(ParkDECPosition, "X6") & "!"
                DECCommand = "ESPt1" & Mid(ParkDECPosition.ToString("X8"), 3, 6) & "!"
                'RACommand = "ESPt0000000!"
                'DECCommand = "ESPt1000000!"
                'send command to slew to PARK position
                'SerMutex.WaitOne()
                'RAReceived = CommandString(RACommand)
                'SerMutex.ReleaseMutex()
                SerMutex.WaitOne()
                DECReceived = CommandString(DECCommand)
                SerMutex.ReleaseMutex()

                'RATracking = False
                'wait until reaching destination or starting a new slew in the middle of a slew.
                While Slewing
                    'Change time to 100 mS xyzzy
                    utilities.WaitForMilliseconds(100)
                    Application.DoEvents()
                End While
                'While (GetRAMotorPosition() <> ParkRAPosition) And (GetDECMotorPosition() <> ParkDECPosition)
                'utilities.WaitForMilliseconds(200)
                'ication.DoEvents()
                'End While
                'settle for 1 second after slewing at park position
                'utilities.WaitForMilliseconds(2000)
                '-------------------------------------------------------------------------------------------
                ' If stopped slewing before getting to the PARK position then don't set park status
                ' or stop tracking - GRH 2021-06-05
                '-------------------------------------------------------------------------------------------
                'Check current position and set status' accordingly
                If (Math.Abs(GetRAMotorPosition() - ParkRAPosition) < 250) And (Math.Abs(GetDECMotorPosition() - ParkDECPosition) < 250) Then
                    'If (GetRAMotorPosition() <> ParkRAPosition) Or (GetDECMotorPosition() <> ParkDECPosition) Then
                    'If (GetRAMotorPosition() <> 0) Or (GetDECMotorPosition() <> 0) Then
                    'utilities.WaitForMilliseconds(200)
                    ParkStatus = True
                    RATracking = False
                    Tracking = False
                    TL.LogMessage("Park", "I think its Parked")
                    TL.LogMessage("Park", "ParkStatus = TRUE")
                Else
                    ParkStatus = False
                    RATracking = True
                    Tracking = True
                    TL.LogMessage("Park", "I think its NOT Parked")
                    TL.LogMessage("Park", "ParkStatus = FALSE")
                End If
            Else
                TL.LogMessage("Park", "Already PARKED!")
                Throw New ASCOM.ParkedException("PARKED!")
            End If
        Catch ex As Exception
            TL.LogMessage("Park", "Invalid Operation")
            'Throw New ASCOM.InvalidOperationException("Park")
        End Try
    End Sub

    Public Sub PulseGuide(Direction As GuideDirections, Duration As Integer) Implements ITelescopeV3.PulseGuide

        'Direction  0-North, 1-South, 2-East, 3-West
        'Uses firmware-timed ESSq command: ESSqAdDDDD! where A=axis, d=dir, DDDD=duration in ms (hex)
        'Firmware handles timing internally - returns immediately

        Dim RACommand As String
        Dim DECCommand As String
        Dim RAReceived As String
        Dim DECReceived As String
        Dim move_dir As String
        Dim mutexgot As Boolean

        'test to force the parked exception GRH 20210808
        If AtPark Then
            TL.LogMessage("PulseGuide", "PARKED! 0808")
            Throw New ASCOM.ParkedException("PARKED!")
            Return
        End If
        Try
            mutexgot = False
            TL.LogMessage("PulseGuide", "Start of Call, NOT PARKED, RATracking =" & RATracking.ToString)

            If IsConnected Then
                TL.LogMessage("PulseGuide", "Start of Call, NOT PARKED, RATracking =" & RATracking.ToString)
                'assert tracking -
                Tracking = True
                SerMutex.WaitOne()
                mutexgot = True
                If RATracking Then
                    TL.LogMessage("PulseGuide", "Start, Duration = " & Duration.ToString & " mS")
                    TL.LogMessage("PulseGuide", "Direction = " & Direction.ToString)
                    TL.LogMessage("PulseGuide", "Side of pier = " & SideOfPier.ToString)

                    '  ESSqAdDDDD! where A is axis (0=RA, 1=DEC), d is direction, DDDD is duration in ms (hex)
                    '  For DEC: d=1 for CW (same as old code when pierside East and move north)
                    '  For RA: d=1 means increase rate, d=0 means decrease rate

                    Select Case Direction
                        Case 0 'North (DEC axis)
                            move_dir = "1"   'if pierside East, North means send direction 1
                            If SideOfPier = PierSide.pierWest Then 'North means send direction 0
                                move_dir = "0"
                            End If
                            DECCommand = "ESSq1" & move_dir & Duration.ToString("X4") & "!"
                            DECReceived = CommandString(DECCommand)
                        Case 1 'South (DEC axis)
                            move_dir = "0"      'if pierside is East, south means send direction 0
                            If SideOfPier = PierSide.pierWest Then 'South means send direction 1
                                move_dir = "1"
                            End If
                            DECCommand = "ESSq1" & move_dir & Duration.ToString("X4") & "!"
                            DECReceived = CommandString(DECCommand)
                        Case 2 'East (RA axis - increase rate)
                            RACommand = "ESSq01" & Duration.ToString("X4") & "!"
                            RAReceived = CommandString(RACommand)
                        Case 3 'West (RA axis - decrease rate)
                            RACommand = "ESSq00" & Duration.ToString("X4") & "!"
                            RAReceived = CommandString(RACommand)
                    End Select

                    'Wait for firmware to confirm guiding is active before returning
                    Dim guideConfirmed As Boolean = False
                    For i As Integer = 1 To 10
                        System.Threading.Thread.Sleep(50)
                        Dim guideState As String = CommandString("ESGq!")
                        If Mid(guideState, 5, 2) <> "00" Then
                            guideConfirmed = True
                            Exit For
                        End If
                    Next
                    pulseguidingState = guideConfirmed
                    TL.LogMessage("PulseGuide", "Guide confirmed=" & guideConfirmed.ToString & " pulseguidingState=" & pulseguidingState.ToString)

                    SerMutex.ReleaseMutex()
                    mutexgot = False
                End If
                If mutexgot Then
                    SerMutex.ReleaseMutex()
                    mutexgot = False
                End If
            End If
        Catch ex As Exception
            TL.LogMessage("PulseGuide", "Exception: " & ex.Message)
            If mutexgot Then
                SerMutex.ReleaseMutex()
            End If
        End Try
        'Make sure we didn't leave the mutex held from a try bomb
        If SerMutex.WaitOne(0) Then
            SerMutex.ReleaseMutex()
        End If
    End Sub

    Public ReadOnly Property RightAscension() As Double Implements ITelescopeV3.RightAscension
        Get
            Dim rightAscension_1 As Double

            If IsConnected Then
                rightAscension_1 = MotorCounts_to_RA(GetRAMotorPosition())
            ElseIf Not IsConnected Then
                Throw New ASCOM.NotConnectedException("RightAscension")
            End If
            TL.LogMessage("RightAscension", "Get - " & utilities.HoursToHMS(rightAscension_1, "h", "m", "s"))
            Return rightAscension_1
        End Get
    End Property

    Public Property RightAscensionRate() As Double Implements ITelescopeV3.RightAscensionRate
        ' for conformU this is completey chnaged.  thsi routine is supposed to just set the offset RA rate so it can be used by tracking
        ' rate offset is saved away in variable RateOffsetValue
        ' value is passed in in units of sec of RA per siderial second.  So our arc sec/sidereal sec conversion is to 
        ' multiply by 15 arc sec/ ra second.  Retrun the rate in sec of RA /sidereal sec so divide save value by 15 for GET


        Get
            ''{removed all of this for ConformU
            ''Dim RAratearcsec As Double = 0.0
            'Dim tempRAdir As String
            'Dim tempRArate As String
            'Dim RArate As Int32
            'Dim ratesign As Int16
            'Try
            '    If IsConnected Then
            '        SerMutex.WaitOne()
            '        tempRAdir = CommandString("ESGd0!")
            '        SerMutex.ReleaseMutex()
            '        If Mid(tempRAdir, 6, 1) = "1" Then
            '            ratesign = 1
            '        Else
            '            ratesign = 0
            '        End If
            '        SerMutex.WaitOne()
            '        tempRArate = CommandString("ESGr0!")
            '        SerMutex.ReleaseMutex()
            '        RArate = Convert.ToInt32("0000" + Mid(tempRArate, 6, 4), 16)
            '        RAratearcsec = (RArate * (1296000 / Telescope.MountRACounts)) * ratesign   'arc secs

            '    ElseIf Not IsConnected Then
            '        Throw New ASCOM.NotConnectedException("RightAscensionRate")
            '    End If

            'Catch ex As Exception

            'End Try}

            Dim RAratearcsec As Double
            RAratearcsec = -1.0 * RARateOffsetValue / 15.0   'change the arc/sec to sec of Ra/sec, ex 1.5"/sec returns .1 because RateOffsetValue is saved as "/sec, also change sign back for ascom consistency as we stored neg value

            TL.LogMessage("RightAscensionRate", "Get: " & RAratearcsec.ToString() & " arc sec/sec per sec of RA")
            Return RAratearcsec
        End Get
        Set(value As Double)

            ' this is removed for ConformU and replaced with simple set of the
            ' rateoffsetvalue, after conversion to "/sec
            '    Try
            '        Dim cmdString As String
            '        'Dim rcvString As String
            '        Dim arcSecPerCount As Double
            '        Dim ratevalue As Double
            '        Dim intratevalue As Int32
            '        MountRightAscensionRate = value      'set the property so confomrance test doesnt run over our ESTr delay
            '        arcSecPerCount = 1296000.0 / Telescope.MountRACounts
            '        ' Set Tracking Rate for desired Rate (uses ESTr0000! command)
            '        ratevalue = (value / arcSecPerCount) * 25.0
            '        intratevalue = Convert.ToInt32(Math.Round(ratevalue))
            '        cmdString = "ESTr" & intratevalue.ToString("X4") & "!"
            '        SerMutex.WaitOne()
            '        CommandString(cmdString)
            '        SerMutex.ReleaseMutex()
            '        '                MountRightAscensionRate = value
            '        TL.LogMessage("RightAscensionRate", "SET - " & MountRightAscensionRate.ToString)
            '    Catch ex As Exception
            '        TL.LogMessage("RightAscensionRate Set", "Invalid Operation")
            '        Throw New ASCOM.InvalidOperationException("RightAscensionRate")
            '    End Try
            ' new for conformU
            RARateOffsetValue = value * -1.0 * 15.0     'the ASCOM RA are seconds of RA per sidereal second eg .1 fromascom = 1.5"/sidereal sec. Positive rate imples slow down mount per ascom
            TL.LogMessage("RightAscensionRate", "Set:  " & RARateOffsetValue.ToString() & "arc sec/sec")
            If RATracking Then   ' if we set the tracking rate, it seems ConformU wants it to begn gonig at that rate.
                '               MountRightAscensionRate = Mounttrackingratevalue + RARateOffsetValue  'apply offset if tracking this is actually done in tracking property
                Tracking = True   'set property to generate ESTr
            End If

        End Set



    End Property

    Public Sub SetPark() Implements ITelescopeV3.SetPark
        Try
            ParkRAPosition = GetRAMotorPosition()
            ParkDECPosition = GetDECMotorPosition()
            WriteProfile()
            ParkStatus = True

        Catch ex As Exception
            TL.LogMessage("SetPark", "Invalid Operation")
            Throw New ASCOM.InvalidOperationException("SetPark")

        End Try

    End Sub

    Public Property SideOfPier() As PierSide Implements ITelescopeV3.SideOfPier

        Get
            Dim DECMP As Int32
            Dim SOP As PierSide

            If Telescope.SiteLatitudeValue >= 0 Then
                DECMP = GetDECMotorPosition()
                If DECMP > 0 Then       'used to be >= changed to try to pass conformu.
                    SOP = PierSide.pierWest
                    MountPierSide = PierSide.pierWest
                ElseIf DECMP <= 0 Then     'used to be < ony changed to pass conform U
                    SOP = PierSide.pierEast
                    MountPierSide = PierSide.pierEast
                End If
            ElseIf Telescope.SiteLatitudeValue < 0 Then
                DECMP = GetDECMotorPosition()
                If DECMP >= 0 Then
                    SOP = PierSide.pierEast
                    MountPierSide = PierSide.pierEast
                ElseIf DECMP < 0 Then
                    SOP = PierSide.pierWest
                    MountPierSide = PierSide.pierWest
                End If
            End If
            'DECMP = GetDECMotorPosition()

            Return SOP
        End Get
        Set(value As PierSide)
            If ScottyMount Or MSROMount Then
                TL.LogMessage("SideOfPier", "EQ Mount, Cannot Set SOP")
                Throw New ASCOM.InvalidOperationException("Set Side of Pier called, CanSet is False")
                Return
            End If
            Try
                If SideOfPier <> value Then
                    FlipMount = True
                    SlewToCoordinates(RightAscension, Declination)
                End If
                MountPierSide = value

            Catch ex As Exception
                TL.LogMessage("SideOfPier", "Set SideOfPier Fail")
                Throw New ASCOM.InvalidValueException("Set SideOfPier Fail")
            End Try
        End Set
    End Property

    Public ReadOnly Property SiderealTime() As Double Implements ITelescopeV3.SiderealTime
        Get
            Dim my_LMST As Double
            my_LMST = Sidereal_Time()
            TL.LogMessage("SiderealTime", "Get - " & utilities.HoursToHMS(my_LMST, "h", "m", "s"))
            Return my_LMST
        End Get
    End Property

    Public Property SiteElevation() As Double Implements ITelescopeV3.SiteElevation
        Get
            'TL.LogMessage("SiteElevation Get", "Not implemented")
            'Throw New ASCOM.PropertyNotImplementedException("SiteElevation", False)
            TL.LogMessage("SiteElevation", "Get - " & SiteElevationValue.ToString)
            Return Convert.ToDouble(SiteElevationValue)
        End Get
        Set(value As Double)
            'TL.LogMessage("SiteElevation Set", "Not implemented")
            'Throw New ASCOM.PropertyNotImplementedException("SiteElevation", True)
            If (value > 10000.0 Or value < -300) Then
                Throw New ASCOM.InvalidValueException("Invalid Site Elevation Value, -300 to 10000")
                Exit Property
            End If
            SiteElevationValue = Convert.ToString(value)
            WriteProfile()
        End Set
    End Property

    Public Property SiteLatitude() As Double Implements ITelescopeV3.SiteLatitude
        Get
            'TL.LogMessage("SiteLatitude Get", "Not implemented")
            'Throw New ASCOM.PropertyNotImplementedException("SiteLatitude", False)
            TL.LogMessage("SiteLatitude", "Get - " & SiteLatitudeValue.ToString)
            Return Convert.ToDouble(SiteLatitudeValue)
        End Get
        Set(value As Double)
            'TL.LogMessage("SiteLatitude Set", "Not implemented")
            'Throw New ASCOM.PropertyNotImplementedException("SiteLatitude", True)
            If (value > 90.0 Or value < -90.0) Then
                Throw New ASCOM.InvalidValueException("Invalid Site Latitude Value, -90 to +90")
                Exit Property
            End If
            SiteLatitudeValue = value  'ConformU
            WriteProfile()
        End Set
    End Property

    Public Property SiteLongitude() As Double Implements ITelescopeV3.SiteLongitude
        Get
            'TL.LogMessage("SiteLongitude Get", "Not implemented")
            'Throw New ASCOM.PropertyNotImplementedException("SiteLongitude", False)
            TL.LogMessage("SiteLongitude", "Get - " & SiteLongitudeValue.ToString)
            Return Convert.ToDouble(SiteLongitudeValue)
        End Get
        Set(value As Double)
            'TL.LogMessage("SiteLongitude Set", "Not implemented")
            'Throw New ASCOM.PropertyNotImplementedException("SiteLongitude", True)
            If (value > 180.0 Or value < -180.0) Then
                Throw New ASCOM.InvalidValueException("Invalid Site Longitude Value, -180.0 to +180.0")
                Exit Property
            End If
            SiteLongitudeValue = value    'ConformU
            WriteProfile()
        End Set
    End Property

    Public Property SlewSettleTime() As Short Implements ITelescopeV3.SlewSettleTime
        Get
            TL.LogMessage("SlewSettleTime Get", "Not implemented")
            Throw New ASCOM.PropertyNotImplementedException("SlewSettleTime", False)
        End Get
        Set(value As Short)
            TL.LogMessage("SlewSettleTime Set", "Not implemented")
            Throw New ASCOM.PropertyNotImplementedException("SlewSettleTime", True)
        End Set
    End Property

    Public Sub SlewToAltAz(Azimuth As Double, Altitude As Double) Implements ITelescopeV3.SlewToAltAz
        Try
            If Azimuth < 0.0# Or Azimuth >= 360.0 Or Altitude < 0.0# Or Altitude > 90.0# Then
                Throw New ASCOM.InvalidValueException("SlewToAltAz")
            End If
            objTransform.SetAzimuthElevation(Azimuth, Altitude)
            TargetRightAscension = objTransform.RATopocentric
            TargetDeclination = objTransform.DECTopocentric
            AltAzSlew = True
            SlewToTarget()
            'Tracking = False

        Catch ex As Exception
            TL.LogMessage("SlewToAltAz", "Invalid Operation")
            Throw New ASCOM.InvalidValueException("SlewToAltAz")
        End Try

    End Sub

    Public Sub SlewToAltAzAsync(Azimuth As Double, Altitude As Double) Implements ITelescopeV3.SlewToAltAzAsync
        Try
            If Azimuth < 0.0# Or Azimuth >= 360.0 Or Altitude < 0.0# Or Altitude > 90.0# Then
                Throw New ASCOM.InvalidValueException("SlewToAltAz")
            End If
            objTransform.SetAzimuthElevation(Azimuth, Altitude)
            TargetRightAscension = objTransform.RATopocentric
            TargetDeclination = objTransform.DECTopocentric
            AltAzSlew = True
            SlewToTargetAsync()
            'Tracking = False

        Catch ex As Exception
            TL.LogMessage("SlewToAltAzAsync", "Invalid Operation")
            Throw New ASCOM.InvalidValueException("SlewToAltAzSync")
        End Try
    End Sub

    Public Sub SlewToCoordinates(RightAscension As Double, Declination As Double) Implements ITelescopeV3.SlewToCoordinates

        Try
            'check to see if parked first and throw error if so...
            If (Not AtPark) Then 'And Tracking Then
                TargetRightAscension = RightAscension
                TargetDeclination = Declination
                SlewToTarget()
            Else
                TL.LogMessage("SlewToCoordinates", "@Park OR NOT Tracking!")
                Throw New ASCOM.ParkedException("SlewToCoordinate")
            End If
        Catch ex As Exception
            TL.LogMessage("SlewToCoordinates", "Invalid Operation")
            Throw New ASCOM.InvalidValueException("SlewToCoordinates")
        End Try

    End Sub

    Public Sub SlewToCoordinatesAsync(RightAscension As Double, Declination As Double) Implements ITelescopeV3.SlewToCoordinatesAsync
        Try
            'check to see if parked first and throw error if so...
            If (Not AtPark) Then 'And Tracking Then
                TargetRightAscension = RightAscension
                TargetDeclination = Declination
                SlewToTargetAsync()
            Else
                TL.LogMessage("SlewToCoordinatesAsync", "@Park OR NOT Tracking!")
                Throw New ASCOM.ParkedException("SlewToCoordinates")
            End If
        Catch ex As Exception
            TL.LogMessage("SlewToCoordinatesAsync", "Invalid Operation")
            Throw New ASCOM.InvalidValueException("SlewToCoordinateAsync")
        End Try

    End Sub

    Private Function AdjustRAOffset(MoveDistance As Integer) As Integer
        ' This function is used to correct the slight error in calculating the RA offset
        ' during slewing between 1 and 6 degree slews in RA. GRH 2021-02-15

        Dim Slope As Single
        Dim Intercept As Int16
        Dim MotorOffset As Int16

        'Defined the linear correction fit  in motor counts
        Slope = -0.0025 'offset/movedistance
        Intercept = 192 'bias term
        MotorOffset = (Slope * MoveDistance) + Intercept

        'limit input range from 1 to 6 degrees
        If (MoveDistance < 16000) Or (MoveDistance > 60000) Then
            MotorOffset = 0
        End If


        'AdjustRAOffset = MotorOffset
        'rem this out to return actual value otherwise this has no impact on value
        AdjustRAOffset = 0

        TL.LogMessage("Adjust RA Offset", "RA Distance Value: " & MoveDistance.ToString)
        TL.LogMessage("Adjust RA OFfset", "RA_OffsetAdjustment Value: " & MotorOffset.ToString)

    End Function
    Public Sub SlewToTarget() Implements ITelescopeV3.SlewToTarget
        'Synchronous wrapper: use the async two-pass slew (with correction) and block until done
        Try
            If IsConnected Then
                If Not AtPark Then
                    TL.LogMessage("SlewToTarget", "Calling SlewToTargetAsync and waiting for completion")
                    SlewToTargetAsync()
                    While Slewing
                        utilities.WaitForMilliseconds(200)
                        Application.DoEvents()
                    End While
                    TL.LogMessage("SlewToTarget", "Slew complete")
                Else
                    TL.LogMessage("SlewTo...", "At Park!")
                    Throw New ASCOM.ParkedException("SlewTo...")
                End If
            End If
        Catch ex As ASCOM.ParkedException
            Throw
        Catch ex As Exception
            TL.LogMessage("SlewToTarget", "Invalid Operation" & ex.Message)
            Throw New ASCOM.InvalidOperationException("SlewTo...")
        End Try
    End Sub
    Public Sub SlewToTargetAsync() Implements ITelescopeV3.SlewToTargetAsync
        'Two-pass goto: first slew gets close, correction slew nails it
        'Must return immediately (ASCOM async contract) - correction runs on background thread
        TL.LogMessage("SlewToTargetAsync", "Starting first slew pass")
        ExecuteSlewToTargetOnce()

        'Lock Slewing=True immediately - prevents race where ConformU sees Slewing=False
        'between first slew completion and correction thread setting the flag
        CorrectionSlewActive = True

        'Fire background thread to monitor first slew and send correction when it finishes
        Dim correctionThread As New System.Threading.Thread(AddressOf CorrectionSlewMonitor)
        correctionThread.IsBackground = True
        correctionThread.Name = "CorrectionSlew"
        correctionThread.Start()
        TL.LogMessage("SlewToTargetAsync", "Correction monitor thread started - returning to caller")
    End Sub

    Private Function IsMotorSlewing() As Boolean
        'Check if mount motors are running at slew rates (not just tracking or pulse guiding)
        'Used by CorrectionSlewMonitor - bypasses CorrectionSlewActive flag in Slewing property
        Dim StateVector As String
        Dim mutexgot As Boolean = False
        Try
            SerMutex.WaitOne()
            mutexgot = True
            StateVector = CommandString("ESV!")
            SerMutex.ReleaseMutex()
            mutexgot = False

            If StateVector.Length < 30 Then Return False

            'If pulse guiding is active, that's not a goto slew
            If Mid(StateVector, 16, 1) <> "0" OrElse Mid(StateVector, 29, 1) <> "0" Then
                Return False
            End If

            Dim RARate As Double = Convert.ToInt32("000" + Mid(StateVector, 11, 5), 16) / 25.0
            Dim DECRate As Double = Convert.ToInt32("000" + Mid(StateVector, 24, 5), 16) / 25.0
            Dim expectedRA As Double = ABS_Value(MountRightAscensionRate) * MountRACounts / 1296000.0
            Dim expectedDEC As Double = ABS_Value(DECRateOffsetValue) * MountDECCounts / 1296000.0

            'RA slewing if rate differs from expected by more than 1 motor count/sec and rate is non-zero
            If (ABS_Value(RARate - expectedRA) > 1) And (RARate <> 0) Then Return True
            'DEC slewing if rate differs from expected by more than 1 motor count/sec and rate is non-zero
            If (ABS_Value(DECRate - expectedDEC) > 1) And (DECRate <> 0) Then Return True

            Return False
        Catch ex As Exception
            If mutexgot Then SerMutex.ReleaseMutex()
            Return False
        End Try
    End Function

    Private Sub CorrectionSlewMonitor()
        'CorrectionSlewActive is already True (set by SlewToTargetAsync before thread start)
        'This keeps Slewing=True for external callers throughout the entire correction phase
        Try
            'Wait for mount to start moving before polling - avoid false "not slewing" on startup
            System.Threading.Thread.Sleep(2000)

            'Wait for first slew to finish - poll motor rates directly via IsMotorSlewing()
            'Cannot use Slewing property here because CorrectionSlewActive makes it always return True
            TL.LogMessage("CorrectionSlew", "Waiting for first slew to complete...")
            While IsMotorSlewing()
                System.Threading.Thread.Sleep(1000)
            End While
            TL.LogMessage("CorrectionSlew", "First slew complete")

            'Read actual motor positions after slew settled
            Dim RA_Actual As Int32 = GetRAMotorPosition()
            Dim DEC_Actual As Int32 = GetDECMotorPosition()

            'Recalculate target motor counts using fresh SiderealTime
            Dim RA_Target As Int32 = RA_to_MotorCounts(TargetRightAscension)
            Dim DEC_Target As Int32 = DEC_to_MotorCounts(TargetDeclination, SlewDestinationSideOfPier(TargetRightAscension, TargetDeclination))

            Dim RA_Error As Int32 = Math.Abs(RA_Target - RA_Actual)
            Dim DEC_Error As Int32 = Math.Abs(DEC_Target - DEC_Actual)

            TL.LogMessage("CorrectionSlew", "RA_Actual=" & RA_Actual.ToString & " RA_Target=" & RA_Target.ToString & " RA_Error=" & RA_Error.ToString)
            TL.LogMessage("CorrectionSlew", "DEC_Actual=" & DEC_Actual.ToString & " DEC_Target=" & DEC_Target.ToString & " DEC_Error=" & DEC_Error.ToString)

            If RA_Error > CORRECTION_THRESHOLD Or DEC_Error > CORRECTION_THRESHOLD Then
                TL.LogMessage("CorrectionSlew", "Correction slew needed - calling second pass")
                'Keep CorrectionSlewActive=True so external callers see Slewing=True
                'Pass isCorrection=True to skip ramp-down/wait (mount is already stopped)
                ExecuteSlewToTargetOnce(isCorrection:=True)
                CorrectionSlewSentTime = DateTime.Now
                CorrectionSlewSent = True
                TL.LogMessage("CorrectionSlew", "Correction slew sent - time guard active")
            Else
                TL.LogMessage("CorrectionSlew", "No correction needed - within threshold")
            End If
        Catch ex As Exception
            TL.LogMessage("CorrectionSlew", "Error: " & ex.Message)
        Finally
            CorrectionSlewActive = False
        End Try
    End Sub

    Private Sub ExecuteSlewToTargetOnce(Optional isCorrection As Boolean = False)
        Dim RAReceived As String
        Dim DECReceived As String
        Dim RACounts As Int32
        Dim DECCounts As Int32
        Dim RACounts_Current As Int32
        'Dim DECCounts_Current As Int32
        Dim RA_offset As Int32
        'Dim DEC_offset As Int32
        Dim RACommand As String
        Dim DECCommand As String
        Dim RA_timevalue As Double
        Dim RA_OffsetAdjustment As Int32
        Dim RA_Move As Int32
        Dim RA_Move_West As Boolean

        RA_timevalue = (125 / ((15 * MountRACounts) / 1296000))
        Dim error_code As Integer = 0
        Try
            If IsConnected Then
                'check to see if parked first and throw error if so...
                TL.LogMessage("SlewToTargetAsync", "Connected")
                If Not AtPark Then
                    '*********************************************************************************************************************************************
                    'TODO Put code here to check if slewing and if so use the ESPt3 command to ramp down prior to going
                    'to the new target. GRH 2021-05-22

                    If Not isCorrection Then
                        'Skip ramp-down check on correction pass - mount is already stopped
                        If Slewing Then
                            'send command to ramp down from slewing on both axes
                            RACommand = "ESPt3000000!"
                            SerMutex.WaitOne()
                            error_code = 1          'try to send command
                            RAReceived = CommandString(RACommand)
                            SerMutex.ReleaseMutex()
                        End If

                        'wait until mount ramps down before starting slew to new target
                        While Slewing
                            utilities.WaitForMilliseconds(50)
                            Application.DoEvents()
                        End While
                    End If
                    '*********************************************************************************************************************************************
                    TL.LogMessage("SlewToTargetAsync", "Not Parked")
                    'Calculate counts from coordinates
                    RACounts = RA_to_MotorCounts(TargetRightAscension)
                    DECCounts = DEC_to_MotorCounts(TargetDeclination, SlewDestinationSideOfPier(TargetRightAscension, TargetDeclination))

                    'Get current counts to calculate slew time for offset adjustment for tracking
                    RACounts_Current = GetRAMotorPosition()   'read actual motor position directly
                    TL.LogMessage("SlewToTargetAsync", "Current position is " & RACounts_Current.ToString)
                    'DECCounts_Current = DEC_to_MotorCounts(Declination, SideOfPier)

                    RA_Move = Math.Abs(RACounts - RACounts_Current)
                    If RACounts_Current < RACounts Then
                        RA_Move_West = True
                    End If

                    'Calculate RA Offset Adjustment to slew
                    If RA_Move_West Then
                        RA_OffsetAdjustment = AdjustRAOffset(RA_Move)
                    Else
                        RA_OffsetAdjustment = 0
                    End If

                    Dim moveoffset As Double
                    moveoffset = 1.0
                    If SiteLatitude < 0.00 Then
                        Moveoffset = -1.0
                    End If

                    If Not AltAzSlew And RATracking Then
                        'Calculate new target value including RA tracking offset
                        'Offset always applied - even tiny moves take 2s on PMC8 firmware
                        If RACounts < RACounts_Current Then 'Slewing to the East (Future) Offset subtracted
                            TL.LogMessage("SlewToTargetAsync", "RACounts < RACurrent ")

                            'Get current counts to figure out delta counts
                            RACounts_Current = GetRAMotorPosition()
                            If Math.Abs(RACounts - RACounts_Current) < Math.Round((5000.0 - 2.0 * MountRACounts / 86400.0#)) Then    'total move (move +offset) less than 5000
                                RA_offset = Math.Round(moveoffset * 2.0 * (MountRACounts / 86400.0#))      'short move with cruise only
                                TL.LogMessage("SlewToTargetAsync", " < 5000 used")

                            ElseIf Math.Abs(RACounts - RACounts_Current) > (2.0 * MountMaxSpeed) Then     'total move (move plus offset less than 80000

                                RA_offset = Math.Round(((CDbl((Math.Abs(RACounts - RACounts_Current))) / CDbl(MountMaxSpeed)) + moveoffset * LongMoveOffset1) * (1.0 * MountRACounts / 86400.0#))  'long slew with cruise
                                TL.LogMessage("SlewToTargetAsync", "long move used ")

                            Else 'set to a value that brings the short slew to perfect stop, ramp up, ramp down, no cruise
                                RA_offset = Math.Round(moveoffset * RampOnlyOffset1 * MountRACounts / 86400.0#)
                                TL.LogMessage("SlewToTargetAsync", "ramp only ")

                            End If             'if target > 0 change sign of offset

                            '                     If (RACounts > 0) And (RACounts_Current > 0) Then
                            '                    RA_offset = -RA_offset    'in thsi case substract offset
                            '               End If



                        ElseIf RACounts_Current < RACounts Then 'Slewing to the West (Past) Offset added
                            TL.LogMessage("SlewToTargetAsync", "RACurrent < RACounts ")

                            'Get current counts to figure out delta counts
                            RACounts_Current = GetRAMotorPosition()
                            If Math.Abs(RACounts - RACounts_Current) < (5000 - 2.0 * MountRACounts / 86400.0#) Then    'total move (move +offset) less than 5000
                                RA_offset = Math.Round(moveoffset * 2.0 * (MountRACounts / 86400.0#))      'short move with cruise only
                                TL.LogMessage("SlewToTargetAsync", "<5000 used ")

                            ElseIf Math.Abs(RACounts - RACounts_Current) > (2.0 * MountMaxSpeed) Then     'total move (move plus offset less than 80000

                                RA_offset = Math.Round(((CDbl((Math.Abs(RACounts - RACounts_Current))) / CDbl(MountMaxSpeed)) + moveoffset * LongMoveOffset2) * (1.0 * MountRACounts / 86400.0#))  'long slew with cruise
                                TL.LogMessage("SlewToTargetAsync", "Long slew ")

                            Else 'set to a value that brings the short slew to perfect stop, ramp up, ramp down, no cruise
                                RA_offset = Math.Round(moveoffset * RampOnlyOffset2 * MountRACounts / 86400.0#)
                                TL.LogMessage("SlewToTargetAsync", "Ramp only ")

                            End If

                        End If
                        TL.LogMessage("SlewToTargetAsync", "NEW POTH RA Motor Count target " & RACounts.ToString)

                        If Math.Abs(RACounts_Current - RACounts) > 1 Then
                            RACounts = RACounts + RA_offset
                        Else
                            RACounts = 0    'handle case where only the DEC was asked to move
                        End If

                        TL.LogMessage("SlewToTargetAsync", "OFFSet RA Motor Count target " & RACounts.ToString)
                        TL.LogMessage("SlewToTargetAsync", "RA OFFset " & RA_offset.ToString)

                        'TL.LogMessage("SlewToTargetAsync", "RA_KOVE counts " & RA_Move.ToString)
                        'TL.LogMessage("SlewToTargetAsync", "RA_OFfset Value: " & RA_offset.ToString)
                        'TL.LogMessage("SlewToTargetAsync", "RACounts_Current: " & RACounts_Current.ToString)

                        'DECCounts_Current = DEC_to_MotorCounts(Declination, SideOfPier)
                    End If

                    'If DECCounts > DECCounts_Current Then
                    'TL.LogMessage("SlewToTarget", "DEC INCREASING " & DECCounts - DECCounts_Current)
                    'ElseIf DECCounts_Current > DECCounts Then
                    'TL.LogMessage("SlewToTarget", "DEC DECREASING " & DECCounts - DECCounts_Current)
                    'End If
                    '*************************************************************************************************************************
                    TL.LogMessage("SlewToTargetAsync", "Set Point Command String...")
                    'Set Point Command Strings
                    If AltAzSlew Then
                        'RACounts = RACounts
                        RACommand = "ESPt2" & Mid(RACounts.ToString("X8"), 3, 6) & "!"
                        DECCommand = "ESPt1" & Mid(DECCounts.ToString("X8"), 3, 6) & "!"
                    Else
                        'RACounts = RACounts + RA_offset
                        RACommand = "ESPt0" & Mid(RACounts.ToString("X8"), 3, 6) & "!"
                        DECCommand = "ESPt1" & Mid(DECCounts.ToString("X8"), 3, 6) & "!"
                    End If
                    TL.LogMessage("SlewToTargetAsync", "Slewing East..." & RACommand)

                    If AltAzSlew Then
                        'Turn Tracking off prior to slewing
                        Tracking = False
                    End If

                    'send commands to slew to position - skip RA if DEC-only move
                    If RACounts <> 0 Then
                        SerMutex.WaitOne()
                        RAReceived = CommandString(RACommand)
                        SerMutex.ReleaseMutex()
                        TL.LogMessage("SlewToTargetAsync", "SlewTo RA Target:" & RAReceived.ToString)
                    End If
                    SerMutex.WaitOne()
                    DECReceived = CommandString(DECCommand)
                    SerMutex.ReleaseMutex()
                    TL.LogMessage("SlewToTargetAsync", "SlewTo DEC Target" & DECReceived.ToString)

                    If AltAzSlew = True Then
                        AltAzSlew = False
                        'Tracking = False
                    End If
                Else
                    TL.LogMessage("SlewTo...", "At Park!")
                    Throw New ASCOM.ParkedException("SlewTo...")
                End If
            End If

        Catch ex As Exception
            TL.LogMessage("SlewToTargetAsync", "Invalid Operation error_code is " & error_code.ToString)
            Throw New ASCOM.InvalidOperationException("SlewToAsync...")
            'Throw New ASCOM.InvalidValueException("SlewTo...")
        End Try
    End Sub
    ''' <summary>
    ''' 
    ''' </summary>

    Public ReadOnly Property Slewing() As Boolean Implements ITelescopeV3.Slewing
        Get
            Dim StateVector As String
            Dim RARate As Double
            Dim DECRate As Double
            Dim SlewingState As Boolean
            Dim temp_pulse_guiding As Boolean
            Dim Slewing_Temp As Boolean
            Dim mutexgot As Boolean
            mutexgot = False
            Dim Command As String

            Try
                'If correction slew is in progress, report True so external callers don't interfere
                If CorrectionSlewActive Then
                    TL.LogMessage("Slewing Get", "Correction slew active - returning True")
                    Return True
                End If

                'Time guard: after correction ESPt sent, keep Slewing=True for 2 seconds
                'Prevents tracking re-enable from killing correction goto during ramp
                If CorrectionSlewSent Then
                    If DateTime.Now.Subtract(CorrectionSlewSentTime).TotalSeconds < 2 Then
                        TL.LogMessage("Slewing Get", "Correction time guard active - returning True")
                        Return True
                    Else
                        CorrectionSlewSent = False
                        TL.LogMessage("Slewing Get", "Correction time guard expired - clearing flag")
                    End If
                End If
                TL.LogMessage("Slewing Get", "Start of try")
                SerMutex.WaitOne()
                mutexgot = True
                Command = "ESV!"  ' Fetch state vector - rates and pulse guide status in one atomic read
                temp_pulse_guiding = False   'initial state of pulse guide assumed false
                StateVector = CommandString(Command)

                'Validate response before parsing
                If StateVector.Length < 30 Then
                    TL.LogMessage("Slewing Get", "Invalid ESV! response, length=" & StateVector.Length & " raw=" & StateVector)
                    SerMutex.ReleaseMutex()
                    mutexgot = False
                    Return False
                End If

                If Mid(StateVector, 16, 1) <> "0" Then ' pulse guiding is true if either pulse guide flag is <>0
                    temp_pulse_guiding = True
                ElseIf Mid(StateVector, 29, 1) <> "0" Then
                    temp_pulse_guiding = True
                End If

                TL.LogMessage("Slewing Get", "Fetched StateVector, pulse guiding is " & temp_pulse_guiding.ToString)
                TL.LogMessage("Slewing Get", "hex of rates are RA: " & Mid(StateVector, 11, 5) & " and DEC " & Mid(StateVector, 24, 5))
                RARate = Convert.ToInt32("000" + Mid(StateVector, 11, 5), 16) / 25.0     'high res 5 nibbles, scale rates down but hold precision
                DECRate = Convert.ToInt32("000" + Mid(StateVector, 24, 5), 16) / 25.0
                TL.LogMessage("Slewing Get", "RARate=" & RARate.ToString)
                TL.LogMessage("Slewing Get", "DECRate=" & DECRate.ToString)

                ' Compare actual rates against expected rates (tracking + offsets) with +/-2 tolerance
                Dim RA_rate_check As Double
                RA_rate_check = ABS_Value(MountRightAscensionRate)
                Dim RASlewing As Boolean
                Dim DECSlewing As Boolean
                Dim DEC_rate_check As Double
                DEC_rate_check = ABS_Value(DECRateOffsetValue)
                TL.LogMessage("Slewing Get", "RA_rate_check (arcsec/sec) " & RA_rate_check.ToString)
                TL.LogMessage("Slewing Get", "DEC_rate_check (dec rate offset) " & DEC_rate_check.ToString)
                Slewing_Temp = False

                ' Test if RA rate differs from expected by more than 1 motor count/sec
                If (ABS_Value(RARate - (RA_rate_check * (MountRACounts / 1296000.0))) > 1) And (RARate <> 0) Then
                    RASlewing = True
                Else
                    RASlewing = False
                End If
                ' Test if DEC rate differs from expected by more than 1 motor count/sec
                If (ABS_Value(DECRate - (DEC_rate_check * MountDECCounts / 1296000.0)) > 1) And (DECRate <> 0) Then
                    DECSlewing = True
                Else
                    DECSlewing = False
                End If

                If (RASlewing Or DECSlewing) And (temp_pulse_guiding = False) And (RARate <> 0) Then
                    Slewing_Temp = True
                    TL.LogMessage("Slewing Get", "Slewing is True from DEC or RA, RA <> 0")
                End If

                If DECSlewing And (temp_pulse_guiding = False) And (RARate = 0) Then
                    Slewing_Temp = True
                    TL.LogMessage("Slewing Get", "Slewing is True from DEC, RA rate is 0")
                End If

                If (DECRate = 0) And (RASlewing = False) Then  'DEC stopped and RA at expected rate
                    Slewing_Temp = False
                    TL.LogMessage("Slewing Get", "Slewing is False from DEC = 0, RA is not slewing")
                End If

                If DECRate = 0 And RARate = 0 Then   'parked or at home
                    Slewing_Temp = False
                End If

                TL.LogMessage("Slewing Get", "Current RA Position - $$$ " + GetRAMotorPosition().ToString)
                TL.LogMessage("Slewing Get", "Slewing is " & Slewing_Temp.ToString)

                If Not AltAzSlew Then
                    TL.LogMessage("Slewing Get", "Called AltAzSlew")
                    If (ParkStatus = True) Or (HomeStatus = True) Then
                        If ParkStatus Then
                            TL.LogMessage("Slewing Get", "ParkStatus True")
                            ParkStatus = True
                        ElseIf HomeStatus Then
                            TL.LogMessage("Slewing Get", "HomeStatus True")
                            HomeStatus = True
                        End If
                    End If
                End If

                TL.LogMessage("Slewing Get", "RARate= " + RARate.ToString)
                TL.LogMessage("Slewing Get", "MountRADir= " + MountRADir.ToString)
                TL.LogMessage("Slewing Get", "DECRate= " + DECRate.ToString)

                ' Re-enable tracking after slew completes
                If (RATracking = True) And (temp_pulse_guiding = False) And (Not Slewing_Temp) Then
                    TL.LogMessage("Slewing Get", "Checked pulse guiding state")
                    Tracking = True
                    TL.LogMessage("Slewing Get", "Enabled Tracking after Slew")
                End If

            Catch ex As Exception
                TL.LogMessage("Slewing Get", "Invalid Operation")
                TL.LogMessage("Slewing Get", ex.ToString())
                If mutexgot Then
                    SerMutex.ReleaseMutex()
                    mutexgot = False
                End If
            End Try
            Slewing = Slewing_Temp
            SlewingState = Slewing_Temp
            If mutexgot Then
                SerMutex.ReleaseMutex()
            End If
            Return Slewing
        End Get
    End Property

    Public Sub SyncToAltAz(Azimuth As Double, Altitude As Double) Implements ITelescopeV3.SyncToAltAz
        Dim tempRA As Double
        Dim tempDEC As Double
        Try
            If Azimuth < 0.0# Or Azimuth >= 360.0 Or Altitude < -90.0# Or Altitude > 90.0# Then
                Throw New ASCOM.InvalidValueException("SyncToAltAz")
            End If
            objTransform.SetAzimuthElevation(Azimuth, Altitude)
            tempRA = objTransform.RATopocentric
            tempDEC = objTransform.DECTopocentric
            TargetRightAscension = tempRA
            TargetDeclination = tempDEC
            SyncToTarget()
        Catch ex As Exception
            TL.LogMessage("SyncToAltAz", "Invalid Value")
            Throw New ASCOM.InvalidValueException("SyncToAltAz")
        End Try
    End Sub

    Public Sub SyncToCoordinates(RightAscension As Double, Declination As Double) Implements ITelescopeV3.SyncToCoordinates
        Try
            'check to see if parked first and throw error if so...
            If Not AtPark Then
                TargetRightAscension = RightAscension
                TargetDeclination = Declination
                SyncToTarget()
            Else
                TL.LogMessage("SyncToCoordinates", "At Park!")
                Throw New ASCOM.ParkedException("SyncToCoordinates")
            End If
        Catch ex As Exception
            TL.LogMessage("SyncToCoordinatesAsync", "Invalid Value")
            Throw New ASCOM.InvalidValueException("SyncToCoordinates")
        End Try
    End Sub

    Public Sub SyncToTarget() Implements ITelescopeV3.SyncToTarget
        Dim RACounts As Int32
        Dim DECCounts As Int32
        Dim RACommand As String
        Dim DECCommand As String
        Dim RAReceived As String
        Dim DECReceived As String

        Try
            If IsConnected Then
                'check to see if parked first and throw error if so...
                If Not AtPark Then
                    RACounts = RA_to_MotorCounts(TargetRightAscension)
                    DECCounts = DEC_to_MotorCounts(TargetDeclination, SlewDestinationSideOfPier(TargetRightAscension, TargetDeclination))
                    RACommand = "ESSp0" & Mid(RACounts.ToString("X8"), 3, 6) & "!"
                    DECCommand = "ESSp1" & Mid(DECCounts.ToString("X8"), 3, 6) & "!"
                    'send command to SYNC to target position
                    SerMutex.WaitOne()
                    RAReceived = CommandString(RACommand)
                    SerMutex.ReleaseMutex()
                    SerMutex.WaitOne()
                    DECReceived = CommandString(DECCommand)
                    SerMutex.ReleaseMutex()
                    If Tracking Then
                        'reassert tracking
                        Tracking() = True
                    End If
                Else
                    TL.LogMessage("SyncToTarget", "At Park!")
                    Throw New ASCOM.ParkedException("SyncToTarget")
                End If
            End If
        Catch ex As Exception
            TL.LogMessage("SyncToTarget", "Invalid Operation")
            Throw New ASCOM.InvalidOperationException("SyncToTarget")
        End Try
    End Sub

    Public Property TargetDeclination() As Double Implements ITelescopeV3.TargetDeclination
        Get
            Try
                If Not DECTargetSet Then
                    Throw New ASCOM.ValueNotSetException
                End If
                If Not (DECTarget < -90.0 Or DECTarget > 90.0) Then
                    Return DECTarget
                Else
                    Throw New ASCOM.InvalidValueException
                End If
            Catch ex As Exception
                Throw New ASCOM.ValueNotSetException
            End Try
        End Get
        Set(value As Double)
            Try
                If Not (value > 90.0 Or value < -90.0) Then
                    DECTarget = value
                    DECTargetSet = True
                Else
                    Throw New ASCOM.InvalidValueException
                    'Exit Property
                End If
            Catch ex As Exception
                Throw New ASCOM.InvalidValueException
            End Try
        End Set
    End Property

    Public Property TargetRightAscension() As Double Implements ITelescopeV3.TargetRightAscension

        Get
            Try
                If Not RATargetSet Then
                    Throw New ASCOM.ValueNotSetException
                End If
                If Not (RATarget < 0.0 Or RATarget >= 24.0) Then
                    Return RATarget
                Else
                    Throw New ASCOM.InvalidValueException
                End If
            Catch ex As Exception
                Throw New ASCOM.ValueNotSetException
            End Try
        End Get
        Set(value As Double)
            Try
                If Not (value >= 24.0 Or value < 0.0) Then
                    RATarget = value
                    RATargetSet = True
                Else
                    Throw New ASCOM.InvalidValueException
                    'Exit Property
                End If
            Catch ex As Exception
                Throw New ASCOM.InvalidValueException
            End Try
        End Set
    End Property

    Public Property Tracking() As Boolean Implements ITelescopeV3.Tracking

        Get
            'Dim tracking__1 As Boolean = True
            TL.LogMessage("Tracking", "Get - " & RATracking.ToString())
            Tracking = RATracking
        End Get
        Set(value As Boolean)
            Dim TrackCommand As String
            Dim dirCommand As String = "ESSd01!"
            'Dim TrackRate As Int32
            Dim arcsecpercount As Double
            Dim ratevalue As Double
            Dim Intratevalue As Int32
            Dim cmdstring As String

            Try
                If value = True Then
                    'Unpark mount if parked prior to tracking
                    If AtPark Then
                        Unpark()
                    End If
                    'Not AtHome if tracking is enabled
                    If AtHome Then
                        HomeStatus = False
                    End If
                    'check to see if already tracking
                    'Set correct direction for hemisphere, 1 for Northern, 0 for Southern based on latitude value
                    If Telescope.SiteLatitudeValue >= 0 Then
                        dirCommand = "ESSd01!"
                    ElseIf Telescope.SiteLatitudeValue < 0 Then
                        dirCommand = "ESSd00!"
                    End If
                    '
                    '  remove this for COnformU
                    '{'Set tracking rate according to value selected on setup box
                    'Select Case Telescope.Rate
                    '    Case "Sidereal"
                    '        TrackingRate = DriveRates.driveSidereal
                    '        MountTrackingRate = DriveRates.driveSidereal
                    '    Case "Lunar"
                    '        TrackingRate = DriveRates.driveLunar
                    '        MountTrackingRate = DriveRates.driveLunar
                    '    Case "Solar"
                    '        TrackingRate = DriveRates.driveSolar
                    '        MountTrackingRate = DriveRates.driveSolar
                    '    Case "King"
                    '        TrackingRate = DriveRates.driveKing
                    '        MountTrackingRate = DriveRates.driveKing
                    'End Select}

                    '
                    '}  This is new for ConformU
                    'New tracing is to actually turn on the mount at the correct offset RA tracking rate stored in Mountrightascensionrate variable, set by tracking rate property


                    '
                    arcsecpercount = 1296000.0 / Telescope.MountRACounts
                    ratevalue = ((Mounttrackingratevalue + RARateOffsetValue) / arcsecpercount) * 25.0  'arcsec/sec/arc sec/mtr cnt
                    Intratevalue = Convert.ToInt32(Math.Round(ratevalue))
                    MountRightAscensionRate = Mounttrackingratevalue + RARateOffsetValue  'update so Slewing check knows the expected rate
                    'If RARateOffset is negative and larger than tracking rate, rate goes negative
                    'Take abs value and reverse direction
                    If Intratevalue < 0 Then
                        Intratevalue = -1 * Intratevalue
                        If dirCommand = "ESSd01!" Then
                            dirCommand = "ESSd00!"
                        Else
                            dirCommand = "ESSd01!"
                        End If
                    End If
                    SerMutex.WaitOne()
                    CommandString(dirCommand)
                    SerMutex.ReleaseMutex()
                    cmdString = "ESTr" & Intratevalue.ToString("X4") & "!"
                    SerMutex.WaitOne()
                    CommandString(cmdString)
                    SerMutex.ReleaseMutex()
                    RATracking = True
                    TL.LogMessage("Tracking Set", "RATracking = True")
                    '}
                    'below is same as before ConformU
                ElseIf value = False Then
                    'Set tracking Rate to 0 (zero) - use ESSr to preserve firmware's ESTr sidereal memory
                    TrackCommand = "ESSr00000!"
                    SerMutex.WaitOne()
                    CommandString(TrackCommand)
                    SerMutex.ReleaseMutex()
                    RATracking = False
                    TL.LogMessage("Tracking Set", "RATracking = False")
                End If
            Catch ex As Exception
                TL.LogMessage("Tracking Set", "Invalid Operation")
                Throw New ASCOM.InvalidOperationException("Tracking")
            End Try
        End Set
    End Property

    Public Property TrackingRate() As DriveRates Implements ITelescopeV3.TrackingRate
        Get
            TL.LogMessage("TrackingRate", "GET" & MountTrackingRate.ToString & " arcsec/sec")
            'Throw New ASCOM.PropertyNotImplementedException("TrackingRate", False)
            'For Each myTrackingRate In TrackingRate
            ' ConformU change, return Rightascesionrate in "/sec
            '            Return MountTrackingRate
            Return MountTrackingRate                        'return the tracking rate from tracking rates collection COnformU
            'Next
        End Get
        Set(value As DriveRates)    'set the tracking rate to the correct rate from the tracking rate collection and also apply the raghtascension offset conformu
            '            Dim cmdString As String
            'Dim rcvString As String
            '            Dim arcSecPerCount As Double
            '           Dim ratevalue As Double
            '            Dim intratevalue As Int32
            Dim rate_offset As Double

            rate_offset = RARateOffsetValue     'get the rate offset

            Try
                ' removed ConformU                arcSecPerCount = 1296000.0 / Telescope.MountRACounts
                Select Case value
                    Case DriveRates.driveSidereal 'Sidereal Rate 
                        ' Set Tracking Rate for Sidereal Rate (uses ESTr0000! command)
                        'this section removed for ConformU rework.  TrackingRate Set just sets the RightascensionRate
                        '{ratevalue = ((15.0 + rate_offset) / arcSecPerCount) * 25.0
                        'intratevalue = Convert.ToInt32(Math.Round(ratevalue))
                        'cmdString = "ESTr" & intratevalue.ToString("X4") & "!"
                        'SerMutex.WaitOne()
                        'CommandString(cmdString)
                        'SerMutex.ReleaseMutex()
                        'MountTrackingRate = DriveRates.driveSidereal
                        'MountRightAscensionRate = 15.0}
                        MountRightAscensionRate = 15.0 + rate_offset   'offset rate new ConformU 
                        MountTrackingRate = DriveRates.driveSidereal    'ConformU
                        MountTrackingRateValue = 15.0   'conformu
                    Case DriveRates.driveLunar
                        ' Set Tracking Rate for Sidereal Rate (uses ESTr0000! command)
                        'this section removed for ConformU rework.  TrackingRate Set just sets the RightascensionRate
                        '{ratevalue = ((14.685 + rate_offset) / arcSecPerCount) * 25.0
                        'intratevalue = Convert.ToInt32(Math.Round(ratevalue))
                        'cmdString = "ESTr" & intratevalue.ToString("X4") & "!"
                        'SerMutex.WaitOne()
                        'CommandString(cmdString)
                        'SerMutex.ReleaseMutex()
                        'MountTrackingRate = DriveRates.driveLunar
                        'MountRightAscensionRate = 14.685}
                        MountRightAscensionRate = 14.685 + rate_offset   'offset rate new ConformU 
                        MountTrackingRate = DriveRates.driveLunar ' ConformU
                        MountTrackingRateValue = 14.685   'conformu
                    Case DriveRates.driveSolar
                        ' Set Tracking Rate for Sidereal Rate (uses ESTr0000! command)
                        'this section removed for ConformU rework.  TrackingRate Set just sets the RightascensionRate
                        '{ratevalue = ((15.041 + rate_offset) / arcSecPerCount) * 25.0
                        'intratevalue = Convert.ToInt32(Math.Round(ratevalue))
                        'cmdString = "ESTr" & intratevalue.ToString("X4") & "!"
                        'SerMutex.WaitOne()
                        'CommandString(cmdString)
                        'SerMutex.ReleaseMutex()
                        'MountTrackingRate = DriveRates.driveSolar
                        'MountRightAscensionRate = 15.041}
                        MountRightAscensionRate = 15.041 + rate_offset   'offset rate new ConformU 
                        MountTrackingRate = DriveRates.driveSolar   'conformU
                        MountTrackingRateValue = 15.041   'conformu
                    Case DriveRates.driveKing
                        ' Set Tracking Rate for Sidereal Rate (uses ESTr0000! command)
                        'this section removed for ConformU rework.  TrackingRate Set just sets the RightascensionRate
                        '{ratevalue = ((15.0369 + rate_offset) / arcSecPerCount) * 25.0
                        'intratevalue = Convert.ToInt32(Math.Round(ratevalue))
                        'cmdString = "ESTr" & intratevalue.ToString("X4") & "!"
                        'SerMutex.WaitOne()
                        'CommandString(cmdString)
                        'SerMutex.ReleaseMutex()
                        'MountTrackingRate = DriveRates.driveKing
                        'MountRightAscensionRate = 15.0369}
                        MountRightAscensionRate = 15.0369 + rate_offset   'offset rate new ConformU 
                        MountTrackingRate = DriveRates.driveKing  'ConformU
                        MountTrackingRateValue = 15.0369            'Conformu

                End Select
                If RATracking Then   ' if we set the tracking rate, it seems ConformU wants it to begn gonig at that rate.
                    Tracking = True
                End If
            Catch ex As Exception
                Throw New ASCOM.InvalidOperationException("TrackingRate")
                TL.LogMessage("TrackingRate Set", "Invalid Operation")
                'Throw New ASCOM.InvalidValueException("TrackingRate")
                'TL.LogMessage("TrackingRate Set", "Invalid Value")
            End Try

            'if value out of range throw an Invalid Value Exception
            If (value < 0) Or (value > 4) Then
                Throw New ASCOM.InvalidValueException("TrackingRate")
                TL.LogMessage("TrackingRate Set", "Invalid Value")
            End If

        End Set
    End Property

    Public ReadOnly Property TrackingRates() As ITrackingRates Implements ITelescopeV3.TrackingRates
        Get
            Dim trackingRates__1 As ITrackingRates = New TrackingRates()
            'Dim trackingRates__1 As New TrackingRates
            TL.LogMessage("TrackingRates", "Get - ")
            For Each driveRate As DriveRates In trackingRates__1
                TL.LogMessage("TrackingRates", "Get - " & driveRate.ToString())
            Next
            Return trackingRates__1
            'Return New TrackingRates()
        End Get
    End Property

    Public Property UTCDate() As DateTime Implements ITelescopeV3.UTCDate
        Get
            Dim utcDate__1 As DateTime = DateTime.UtcNow
            TL.LogMessage("UTC DateTime", "Get - " & Format(utcDate__1, "MM/dd/yy HH:mm:ss"))
            Return utcDate__1
        End Get
        Set(value As DateTime)
            Throw New ASCOM.PropertyNotImplementedException("UTCDate", True)
        End Set
    End Property

    Public Sub Unpark() Implements ITelescopeV3.Unpark
        Try
            If ParkStatus = True Then
                ParkStatus = False
                TL.LogMessage("Unpark", "Found Parkstatus=TRUE, UnParked!")
            ElseIf ParkStatus = False Then
                'Do nothing but report the call
                TL.LogMessage("Unpark", "Found Parkedstatus=FALSE, Already UnParked!")
                'Throw New ASCOM.ParkedException("Mount Parked, UNPARK")
            End If
            'TestMotorCalcs()
        Catch ex As Exception
            TL.LogMessage("Unpark", "Unparked")
            'Throw New ASCOM.MethodNotImplementedException("Unpark")

        End Try
    End Sub

#End Region

#Region "Private properties and methods"
    ' here are some useful properties and methods that can be used as required
    ' to help with

#Region "ASCOM Registration"

    Private Shared Sub RegUnregASCOM(ByVal bRegister As Boolean)

        Using P As New Profile() With {.DeviceType = "Telescope"}
            If bRegister Then
                P.Register(driverID, driverDescription)
            Else
                P.Unregister(driverID)
            End If
        End Using

    End Sub

    <ComRegisterFunction()>
    Public Shared Sub RegisterASCOM(ByVal T As Type)

        RegUnregASCOM(True)

    End Sub

    <ComUnregisterFunction()>
    Public Shared Sub UnregisterASCOM(ByVal T As Type)

        RegUnregASCOM(False)

    End Sub

#End Region

    ''' <summary>
    ''' Returns true if there is a valid connection to the driver hardware
    ''' </summary>
    Private ReadOnly Property IsConnected As Boolean
        Get
            ' TODO check that the driver hardware connection exists and is connected to the hardware
            Return connectedState
        End Get
    End Property

    ''' <summary>
    ''' Use this function to throw an exception if we aren't connected to the hardware
    ''' </summary>
    ''' <param name="message"></param>
    Private Sub CheckConnected(ByVal message As String)
        If Not IsConnected Then
            Throw New NotConnectedException(message)
        End If
    End Sub

    ''' <summary>
    ''' Read the device configuration from the ASCOM Profile store
    ''' </summary>
    Friend Sub ReadProfile()
        Using driverProfile As New Profile()
            driverProfile.DeviceType = "Telescope"
            traceState = Convert.ToBoolean(driverProfile.GetValue(driverID, traceStateProfileName, String.Empty, traceStateDefault))
            comPort = driverProfile.GetValue(driverID, comPortProfileName, String.Empty, comPortDefault)
            comSpeed = driverProfile.GetValue(driverID, comSpeedProfileName, String.Empty, comSpeedDefault)
            IPAddress = driverProfile.GetValue(driverID, IPAddressProfileName, String.Empty, IPAddressDefault)
            IPPort = driverProfile.GetValue(driverID, IPPortProfileName, String.Empty, IPPortDefault)
            WirelessEnabled = Convert.ToBoolean(driverProfile.GetValue(driverID, WirelessEnabledProfileName, String.Empty, WirelessEnabledDefault))
            WirelessProtocol = driverProfile.GetValue(driverID, WirelessProtocolProfileName, String.Empty, WirelessProtocolDefault)
            Mount = driverProfile.GetValue(driverID, MountProfileName, String.Empty, MountDefault)
            Rate = driverProfile.GetValue(driverID, RateProfileName, String.Empty, RateDefault)
            MountRACounts = Convert.ToInt32(driverProfile.GetValue(driverID, MountRACountsProfileName, String.Empty, MountRACountsDefault))
            MountDECCounts = Convert.ToInt32(driverProfile.GetValue(driverID, MountDECCountsProfileName, String.Empty, MountDECCountsDefault))
            ApertureDiameterValue = Convert.ToDouble(driverProfile.GetValue(driverID, ApertureDiameterProfileName, String.Empty, ApertureDiameterDefault))
            ApertureAreaValue = Convert.ToDouble(driverProfile.GetValue(driverID, ApertureAreaProfileName, String.Empty, ApertureAreaDefault))
            FocalLengthValue = Convert.ToDouble(driverProfile.GetValue(driverID, FocalLengthProfileName, String.Empty, FocalLengthDefault))
            SiteLocation = driverProfile.GetValue(driverID, SiteLocationProfileName, String.Empty, SiteLocationDefault)
            SiteElevationValue = Convert.ToDouble(driverProfile.GetValue(driverID, SiteElevationProfileName, String.Empty, SiteElevationDefault))
            SiteLatitudeValue = Convert.ToDouble(driverProfile.GetValue(driverID, SiteLatitudeProfileName, String.Empty, SiteLatitudeDefault))
            SiteLongitudeValue = Convert.ToDouble(driverProfile.GetValue(driverID, SiteLongitudeProfileName, String.Empty, SiteLongitudeDefault))
            DECRateOffsetValue = Convert.ToDouble(driverProfile.GetValue(driverID, DECRateOffsetProfileName, String.Empty, DECRateOffsetDefault))
            RARateOffsetValue = Convert.ToDouble(driverProfile.GetValue(driverID, RARateOffsetProfileName, String.Empty, RARateOffsetDefalut))
            SiteAmbientTemperatureValue = Convert.ToDouble(driverProfile.GetValue(driverID, SiteAmbientTemperatureProfileName, String.Empty, SiteAmbientTemperatureDefault))
            ApplyRefractionCorrection = Convert.ToBoolean(driverProfile.GetValue(driverID, ApplyRefractionCorrectionProfileName, String.Empty, ApplyRefractionCorrectionDefault))
            RA_SiderealRateFraction = Convert.ToInt16(driverProfile.GetValue(driverID, RA_SiderealRateFractionProfileName, String.Empty, RA_SiderealRateFractionDefault))
            DEC_SiderealRateFraction = Convert.ToInt16(driverProfile.GetValue(driverID, DEC_SiderealRateFractionProfileName, String.Empty, DEC_SiderealRateFractionDefault))
            MinimumPulseTime = Convert.ToInt16(driverProfile.GetValue(driverID, MininumPulseTimeProfileName, String.Empty, MinimumPulseTimeDefault))
            ParkRAPosition = Convert.ToInt32(driverProfile.GetValue(driverID, ParkRAPositionProfileName, String.Empty, ParkRAPositionDefault))
            ParkDECPosition = Convert.ToInt32(driverProfile.GetValue(driverID, ParkDECPositionProfileName, String.Empty, ParkDECPositionDefault))
            WiFiModuleID = driverProfile.GetValue(driverID, WiFiModuleIDProfileName, String.Empty, WiFiModuleIDDefault)
            MountMaxSpeed = Convert.ToInt64(driverProfile.GetValue(driverID, MountMaxSpeedProfileName, String.Empty, MountMaxSpeedDefault))
            SkySafari_rate = Convert.ToSingle(driverProfile.GetValue(driverID, SkySafari_rateProfileName, String.Empty, SkySafari_rateDefault))
            LongMoveOffset1 = Convert.ToSingle(driverProfile.GetValue(driverID, LongMoveoffset1Profilename, String.Empty, LongMoveoffset1default))
            LongMoveOffset2 = Convert.ToSingle(driverProfile.GetValue(driverID, LongMoveoffset2Profilename, String.Empty, LongMoveoffset2default))
            RampOnlyOffset1 = Convert.ToSingle(driverProfile.GetValue(driverID, RampOnlyoffset1Profilename, String.Empty, Ramponlyoffset1default))
            RampOnlyOffset2 = Convert.ToSingle(driverProfile.GetValue(driverID, RampOnlyoffset2Profilename, String.Empty, Ramponlyoffset2default))
            'BacklashValue = Convert.ToInt32(driverProfile.GetValue(driverID, BacklashValueProfileName, String.Empty, BacklashValueDefault))
            'BacklashTime = Convert.ToInt32(driverProfile.GetValue(driverID, BacklashTimeProfileName, String.Empty, BacklashTimeDefault))
            'BacklashMinimum = Convert.ToInt32(driverProfile.GetValue(driverID, BacklashMinimumProfileName, String.Empty, BacklashMinimumDefault))
            'BacklashEnabled = Convert.ToBoolean(driverProfile.GetValue(driverID, BacklashEnabledProfileName, String.Empty, BacklashEnabledDefault))
        End Using
    End Sub

    ''' <summary>
    ''' Write the device configuration to the  ASCOM  Profile store
    ''' </summary>
    Friend Sub WriteProfile()
        Using driverProfile As New Profile()
            driverProfile.DeviceType = "Telescope"
            driverProfile.WriteValue(driverID, traceStateProfileName, traceState.ToString())
            driverProfile.WriteValue(driverID, comPortProfileName, comPort.ToString())
            driverProfile.WriteValue(driverID, comSpeedProfileName, comSpeed.ToString())
            driverProfile.WriteValue(driverID, IPAddressProfileName, IPAddress.ToString())
            driverProfile.WriteValue(driverID, IPPortProfileName, IPPort.ToString())
            driverProfile.WriteValue(driverID, WirelessEnabledProfileName, WirelessEnabled.ToString())
            driverProfile.WriteValue(driverID, WirelessProtocolProfileName, WirelessProtocol.ToString())
            driverProfile.WriteValue(driverID, MountProfileName, Mount.ToString())
            driverProfile.WriteValue(driverID, RateProfileName, Rate.ToString())
            driverProfile.WriteValue(driverID, MountRACountsProfileName, MountRACounts.ToString())
            driverProfile.WriteValue(driverID, MountDECCountsProfileName, MountDECCounts.ToString())
            driverProfile.WriteValue(driverID, ApertureDiameterProfileName, ApertureDiameterValue.ToString())
            driverProfile.WriteValue(driverID, ApertureAreaProfileName, ApertureAreaValue.ToString())
            driverProfile.WriteValue(driverID, FocalLengthProfileName, FocalLengthValue.ToString())
            driverProfile.WriteValue(driverID, SiteLocationProfileName, SiteLocation.ToString())
            driverProfile.WriteValue(driverID, SiteElevationProfileName, SiteElevationValue.ToString())
            driverProfile.WriteValue(driverID, SiteLatitudeProfileName, SiteLatitudeValue.ToString())
            driverProfile.WriteValue(driverID, SiteLongitudeProfileName, SiteLongitudeValue.ToString())
            driverProfile.WriteValue(driverID, RARateOffsetProfileName, RARateOffsetValue.ToString())
            driverProfile.WriteValue(driverID, DECRateOffsetProfileName, DECRateOffsetValue.ToString())
            driverProfile.WriteValue(driverID, SiteAmbientTemperatureProfileName, SiteAmbientTemperatureValue.ToString())
            driverProfile.WriteValue(driverID, ApplyRefractionCorrectionProfileName, ApplyRefractionCorrection.ToString())
            driverProfile.WriteValue(driverID, RA_SiderealRateFractionProfileName, RA_SiderealRateFraction.ToString())
            driverProfile.WriteValue(driverID, DEC_SiderealRateFractionProfileName, DEC_SiderealRateFraction.ToString())
            driverProfile.WriteValue(driverID, MininumPulseTimeProfileName, MinimumPulseTime.ToString())
            driverProfile.WriteValue(driverID, ParkRAPositionProfileName, ParkRAPosition.ToString())
            driverProfile.WriteValue(driverID, ParkDECPositionProfileName, ParkDECPosition.ToString())
            driverProfile.WriteValue(driverID, WiFiModuleIDProfileName, WiFiModuleID.ToString())
            driverProfile.WriteValue(driverID, MountMaxSpeedProfileName, MountMaxSpeed.ToString())
            driverProfile.WriteValue(driverID, SkySafari_rateProfileName, SkySafari_rate.ToString())
            driverProfile.WriteValue(driverID, LongMoveoffset1Profilename, LongMoveOffset1.ToString())
            driverProfile.WriteValue(driverID, LongMoveoffset2Profilename, LongMoveOffset2.ToString())
            driverProfile.WriteValue(driverID, RampOnlyoffset1Profilename, RampOnlyOffset1.ToString())
            driverProfile.WriteValue(driverID, RampOnlyoffset2Profilename, RampOnlyOffset2.ToString())
            'driverProfile.WriteValue(driverID, BacklashTimeProfileName, BacklashTime.ToString())
            'driverProfile.WriteValue(driverID, BacklashMinimumProfileName, BacklashMinimum.ToString())
            'driverProfile.WriteValue(driverID, BacklashEnabledProfileName, BacklashEnabled.ToString())
        End Using
    End Sub

    Private Function EvalCommand(CmdString As String, ReturnString As String) As Boolean
        Dim sent As String
        Dim received As String
        'only check that the sub command type and axis is the same.
        sent = Mid(CmdString, 1, 5)
        received = Mid(ReturnString, 1, 5)
        If sent <> received Then
            EvalCommand = False
            'objSerial.ClearBuffers()
        Else
            EvalCommand = True
        End If
    End Function

    ''' <summary>
    ''' Reads from the persistent WiFi stream byte-by-byte until the ! terminator is received.
    ''' Returns the complete response string including the terminator.
    ''' </summary>
    Private Function ReadUntilTerminator() As String
        Dim sb As New System.Text.StringBuilder()
        Dim b As Integer
        Dim gotContent As Boolean = False

        Do
            b = objTCPStream.ReadByte()
            If b = -1 Then
                Throw New System.IO.IOException("WiFi stream closed unexpectedly")
            End If
            'Skip leading control characters (LF, CR, etc.) before response
            If Not gotContent Then
                If b < &H20 Then
                    Continue Do
                End If
                gotContent = True
            End If
            Dim c As Char = Convert.ToChar(b)
            sb.Append(c)
            If c = WIFI_TERMINATOR Then
                Exit Do
            End If
        Loop

        Return sb.ToString()
    End Function

    ''' <summary>
    ''' Opens or verifies the persistent WiFi TCP connection.
    ''' Handles the Microchip RN-131 *HELLO* greeting on fresh connect.
    ''' </summary>
    Private Sub EnsureWiFiConnected()
        'Check if already connected and stream is usable
        If WiFiConnected AndAlso objTCPNetwork IsNot Nothing AndAlso objTCPNetwork.Connected AndAlso objTCPStream IsNot Nothing Then
            Return
        End If

        'Close any stale connection first
        CloseWiFiConnection()

        TL.LogMessage("EnsureWiFiConnected", "Opening TCP connection to " & IPAddress & ":" & IPPort)

        'Open new connection
        objTCPNetwork = New TcpClient()
        Dim result = objTCPNetwork.BeginConnect(IPAddress, CInt(IPPort), Nothing, Nothing)
        Dim success = result.AsyncWaitHandle.WaitOne(WIFI_CONNECT_TIMEOUT)
        If Not success Then
            objTCPNetwork.Close()
            objTCPNetwork = Nothing
            Throw New Sockets.SocketException(10060) 'Connection timed out
        End If
        objTCPNetwork.EndConnect(result)

        objTCPNetwork.NoDelay = True               'Disable Nagle - send commands immediately
        objTCPStream = objTCPNetwork.GetStream()
        objTCPStream.ReadTimeout = WIFI_READ_TIMEOUT

        'Handle Microchip RN-131 *HELLO* greeting on fresh connect
        'The greeting does not end with ! so we use a raw buffer read
        If WiFiModuleID = "Microchip RN-131" Then
            Dim greetBuf(255) As Byte
            Dim greetBytes As Integer = objTCPStream.Read(greetBuf, 0, greetBuf.Length)
            Dim greeting As String = System.Text.Encoding.ASCII.GetString(greetBuf, 0, greetBytes)
            TL.LogMessage("EnsureWiFiConnected", "RN-131 greeting: " & greeting)
        End If

        WiFiConnected = True
        TL.LogMessage("EnsureWiFiConnected", "TCP connection established to " & IPAddress & ":" & IPPort)
    End Sub

    ''' <summary>
    ''' Cleanly closes the persistent WiFi TCP connection.
    ''' Safe to call even if not connected - never throws.
    ''' </summary>
    Private Sub CloseWiFiConnection()
        Try
            If objTCPStream IsNot Nothing Then
                objTCPStream.Close()
                objTCPStream = Nothing
            End If
            If objTCPNetwork IsNot Nothing Then
                objTCPNetwork.Close()
                objTCPNetwork = Nothing
            End If
        Catch ex As Exception
            'Never throw from cleanup
        End Try
        WiFiConnected = False
        TL.LogMessage("CloseWiFiConnection", "WiFi disconnected")
    End Sub

    Private Function Sidereal_Time() As Double
        j2000 = "1/1/2000 12:00:00"
        deltaTime = DateTime.UtcNow() - j2000
        LMSTtot = 0.77905727325# + (1.00273790935079# * deltaTime.TotalDays)  '1.00273781191135448   0.7790572732640
        di = Math.Floor(LMSTtot)
        LMST = ((LMSTtot - di) * 360.0#) + Telescope.SiteLongitudeValue
        If (LMST < 0) Then
            LMST = LMST + 360.0#
        ElseIf (LMST > 360.0#) Then
            LMST = LMST - 360.0#
        End If
        LMST = 24.0# * (LMST / 360.0#)
        Return LMST
    End Function

    Private Function GetRAMotorPosition() As Int32
        Dim RAReceived As String
        Dim HourAnglePlusSix As Int32
        Dim cmdString As String
        Dim recString As String
        'TL.LogMessage("GetRAMotorPosition", "ConnectedState value " + Convert.ToString(connectedState))
        'TL.LogMessage("GetRAMotorPosition", "IsConnected State " + Convert.ToString(IsConnected))
        If IsConnected Then
            cmdString = "ESGp0!"
            SerMutex.WaitOne()
            'TL.LogMessage("GetRAMotoPosition", "Sending ESGp0!")
            recString = CommandString(cmdString)
            SerMutex.ReleaseMutex()
            'Validate response length before parsing - ESGp0 response should be 12 chars: "ESGp0XXXXXX!"
            'When all WiFi retries fail, CommandString returns "" which would cause Mid/Convert errors
            If recString.Length >= 12 AndAlso EvalCommand(cmdString, recString) Then
                RAReceived = "00" + Mid(recString, 6, 6)
                HourAnglePlusSix = Convert.ToInt32(RAReceived, 16)
                If HourAnglePlusSix >= 8388608 Then ' calculate negative value
                    HourAnglePlusSix = 0 - (16777216 - HourAnglePlusSix)
                End If
                PrevRAMotor = HourAnglePlusSix
            Else
                TL.LogMessage("GetRAMotorPosition", "Invalid response, using previous: " & recString)
                HourAnglePlusSix = PrevRAMotor
            End If
        ElseIf Not IsConnected Then
            Throw New ASCOM.NotConnectedException("GetRAMotorPosition")
        End If
        Return HourAnglePlusSix

    End Function

    Private Function GetDECMotorPosition() As Int32
        Dim DECReceived As String
        Dim Degrees As Int32
        Dim cmdString As String
        Dim RecString As String

        If IsConnected Then
            cmdString = "ESGp1!"
            SerMutex.WaitOne()
            RecString = CommandString(cmdString)
            SerMutex.ReleaseMutex()
            'Validate response length before parsing - ESGp1 response should be 12 chars: "ESGp1XXXXXX!"
            'When all WiFi retries fail, CommandString returns "" which would cause Mid/Convert errors
            If RecString.Length >= 12 AndAlso EvalCommand(cmdString, RecString) Then
                DECReceived = "00" + Mid(RecString, 6, 6)
                Degrees = Convert.ToInt32(DECReceived, 16)
                If Degrees >= 8388608 Then ' calculate negative value
                    Degrees = 0 - (16777216 - Degrees)
                End If
                PrevDECMotor = Degrees
            Else
                TL.LogMessage("GetDECMotorPosition", "Invalid response, using previous: " & RecString)
                Degrees = PrevDECMotor
            End If
        ElseIf Not IsConnected Then
            Throw New ASCOM.NotConnectedException("GetDECMotorPosition")
        End If
        Return Degrees

    End Function

    Public Function RA_to_MotorCounts(RA_value As Double) As Int32
        Dim MotorCounts As Int32
        Dim HourAngle As Double

        HourAngle = SiderealTime - RA_value
        TL.LogMessage("RA_to_MotorCounts", "Hour angle " & HourAngle.ToString)

        'limit values to +/- 12 hours
        If HourAngle > 12 Then
            HourAngle = HourAngle - 24
        ElseIf HourAngle <= -12 Then
            HourAngle = HourAngle + 24
        End If
        TL.LogMessage("RA_to_MotorCounts", "Hour angle Adjusted " & HourAngle.ToString)

        ' Use HA sign to determine pier side instead of explicit PierSide parameter
        ' HA <= 0: object east of meridian (pierWest), HA > 0: object west of meridian (pierEast)
        ' For south hemisphere, negate the result

        If HourAngle <= 0 Then
            If ScottyMount Or MSROMount Then
                MotorCounts = HourAngle * MountRACounts / 24
            Else
                MotorCounts = (HourAngle + 6) * MountRACounts / 24
            End If
        Else
            If ScottyMount Or MSROMount Then
                MotorCounts = HourAngle * MountRACounts / 24
            Else
                MotorCounts = (HourAngle - 6) * MountRACounts / 24
            End If
        End If
        If Telescope.SiteLatitudeValue < 0.00 Then
            MotorCounts = -1.0 * MotorCounts
        End If

        TL.LogMessage("RA_to_MotorCounts", "Motor Counts " & MotorCounts.ToString)
        Return MotorCounts
    End Function

    Public Function MotorCounts_to_RA(MC_value As Int32) As Double
        Dim MotorAngle As Double
        Dim RA_value As Double
        Dim HourAngle As Double
        Dim DECCounts As Int32

        DECCounts = GetDECMotorPosition()

        MotorAngle = (24.0# * MC_value) / Telescope.MountRACounts

        If Telescope.SiteLatitudeValue >= 0 Then
            If DECCounts < 0 Then
                If ScottyMount Or MSROMount Then
                    HourAngle = MotorAngle   'SCotty starts at 0 HA
                Else
                    HourAngle = MotorAngle + 6     'Normal GEM
                End If
            ElseIf DECCounts >= 0 Then
                If ScottyMount Or MSROMount Then
                    HourAngle = MotorAngle   'SCotty starts at 0 HA
                Else
                    HourAngle = MotorAngle - 6     'Normal GEM
                End If
            End If
        ElseIf Telescope.SiteLatitudeValue < 0 Then
            If DECCounts < 0 Then
                If ScottyMount Or MSROMount Then
                    HourAngle = -MotorAngle   'SCotty starts at 0 HA
                Else
                    HourAngle = -(MotorAngle + 6)     'Normal GEM
                End If
            ElseIf DECCounts >= 0 Then
                If ScottyMount Or MSROMount Then
                    HourAngle = -MotorAngle   'SCotty starts at 0 HA
                Else
                    HourAngle = -(MotorAngle - 6)     'Normal GEM
                End If
            End If
        End If

        RA_value = SiderealTime - HourAngle

        If RA_value >= 24.0# Then
            RA_value = RA_value - 24.0#
        ElseIf RA_value < 0.0# Then
            RA_value = RA_value + 24.0#
        End If

        Return RA_value
    End Function

    Public Function DEC_to_MotorCounts(DEC_value As Double, SOP As PierSide) As Int32
        Dim MotorAngle As Double
        Dim MotorCounts As Int32


        'Adjust DEC value for northern or southern hemisiphere (GRH 2018-12-20)
        If Telescope.SiteLatitudeValue >= 0 Then
            If SOP = PierSide.pierEast Then
                If ScottyMount Or MSROMount Then
                    MotorAngle = -(DEC_value)       ' SCotty starts at Dec = 0
                Else
                    MotorAngle = (DEC_value - 90.0#)       ' Normal GEM
                End If
            ElseIf SOP = PierSide.pierWest Then
                If ScottyMount Or MSROMount Then
                    MotorAngle = -(DEC_value)       ' SCotty starts at Dec = 0
                Else
                    MotorAngle = -(DEC_value - 90.0#)       ' Normal GEM
                End If
            End If
        ElseIf Telescope.SiteLatitudeValue < 0 Then
            If SOP = PierSide.pierEast Then
                If ScottyMount Or MSROMount Then
                    MotorAngle = (DEC_value)       ' SCotty starts at Dec = 0
                Else
                    MotorAngle = -(DEC_value + 90.0#)       ' Normal GEM
                End If
            ElseIf SOP = PierSide.pierWest Then
                If ScottyMount Or MSROMount Then
                    MotorAngle = (DEC_value)       ' SCotty starts at Dec = 0
                Else
                    MotorAngle = (DEC_value + 90.0#)       ' Normal GEM
                End If
            End If

        End If

        MotorCounts = (MotorAngle / 360.0) * Telescope.MountDECCounts

        Return MotorCounts
    End Function

    Public Function MotorCounts_to_DEC(MC_value As Int32) As Double
        Dim MotorAngle As Double
        Dim DEC_value As Double

        MotorAngle = (360.0# * MC_value) / Telescope.MountDECCounts


        'Adjust DEC value for southern hemisiphere (GRH 2018-12-20)
        If Telescope.SiteLatitudeValue >= 0 Then
            If MotorAngle >= 0 Then
                If ScottyMount Or MSROMount Then
                    DEC_value = -MotorAngle     'SCotty home is dec 0
                Else
                    DEC_value = 90.0# - MotorAngle   'normal GEM
                End If
            ElseIf MotorAngle < 0 Then
                If ScottyMount Or MSROMount Then
                    DEC_value = -MotorAngle     'SCotty home is dec 0
                Else
                    DEC_value = 90.0# + MotorAngle   'normal GEM
                End If
            End If
        ElseIf Telescope.SiteLatitudeValue < 0 Then
            If MotorAngle >= 0 Then
                If ScottyMount Or MSROMount Then
                    DEC_value = -MotorAngle     'SCotty home is dec 0
                Else
                    DEC_value = -90.0# + MotorAngle   'normal GEM
                End If
            ElseIf MotorAngle < 0 Then
                If ScottyMount Or MSROMount Then
                    DEC_value = MotorAngle     'SCotty home is dec 0
                Else
                    DEC_value = -90.0# - MotorAngle   'normal GEM
                End If
            End If

        End If

        Return DEC_value

    End Function

    Private Sub TestMotorCalcs()
        'put test code here to test private routines
        Dim RA_test As Double
        Dim DEC_test As Double
        Dim RAMC_test As Int32
        Dim DECMC_test As Int32
        Dim RA_test_res As Double
        Dim DEC_test_res As Double
        'Dim RAMC_test_res As Int32
        Dim DECMC_test_res As Int32
        Dim ST As Double
        Dim ALT_test As Double
        Dim AZ_test As Double

        'normal PierEast
        ALT_test = 45.0
        AZ_test = 225.0

        objTransform.SetAzimuthElevation(AZ_test, ALT_test)
        RA_test = objTransform.RATopocentric
        DEC_test = objTransform.DECTopocentric

        DECMC_test = DEC_to_MotorCounts(DEC_test, PierSide.pierEast)
        'RAMC_test = RA_to_MotorCounts(RA_test)
        'RAMC_test = RA_to_MotorCounts(RA_test)
        DECMC_test = DEC_to_MotorCounts(DEC_test, PierSide.pierWest)
        'RAMC_test = RA_to_MotorCounts(RA_test)
        'RAMC_test = RA_to_MotorCounts(RA_test)

        'normal PierWest
        ALT_test = 45.0
        AZ_test = 135.0

        objTransform.SetAzimuthElevation(AZ_test, ALT_test)
        RA_test = objTransform.RATopocentric
        DEC_test = objTransform.DECTopocentric

        DECMC_test = DEC_to_MotorCounts(DEC_test, PierSide.pierWest)
        'RAMC_test = RA_to_MotorCounts(RA_test)
        DECMC_test = DEC_to_MotorCounts(DEC_test, PierSide.pierEast)
        'RAMC_test = RA_to_MotorCounts(RA_test)

        ST = SiderealTime
        ' -------------------------------------------------------------------------------------
        'RA_test = ST + 3
        If RA_test >= 24.0 Then
            RA_test = RA_test - 24.0
        ElseIf RA_test < 0.0 Then
            RA_test = RA_test + 24.0
        End If

        'DEC_test = 45.0
        'RAMC_test = RA_to_MotorCounts(RA_test)
        DECMC_test = DEC_to_MotorCounts(DEC_test, PierSide.pierEast)
        RA_test_res = MotorCounts_to_RA(RAMC_test)
        DEC_test_res = MotorCounts_to_DEC(DECMC_test)
        'RAMC_test_res = RA_to_MotorCounts(RA_test_res)
        DECMC_test_res = DEC_to_MotorCounts(DEC_test_res, PierSide.pierEast)

        RA_test = ST + 3
        If RA_test >= 24.0 Then
            RA_test = RA_test - 24.0
        ElseIf RA_test < 0.0 Then
            RA_test = RA_test + 24.0
        End If
        DEC_test = 45.0
        'RAMC_test = RA_to_MotorCounts(RA_test)
        DECMC_test = DEC_to_MotorCounts(DEC_test, PierSide.pierWest)
        RA_test_res = MotorCounts_to_RA(RAMC_test)
        DEC_test_res = MotorCounts_to_DEC(DECMC_test)
        'RAMC_test_res = RA_to_MotorCounts(RA_test_res)
        DECMC_test_res = DEC_to_MotorCounts(DEC_test_res, PierSide.pierWest)

        ' -------------------------------------------------------------------------------------
        RA_test = ST + 9
        If RA_test >= 24.0 Then
            RA_test = RA_test - 24.0
        ElseIf RA_test < 0.0 Then
            RA_test = RA_test + 24.0
        End If
        DEC_test = 45.0
        'RAMC_test = RA_to_MotorCounts(RA_test)
        DECMC_test = DEC_to_MotorCounts(DEC_test, PierSide.pierEast)
        RA_test_res = MotorCounts_to_RA(RAMC_test)
        DEC_test_res = MotorCounts_to_DEC(DECMC_test)
        'RAMC_test_res = RA_to_MotorCounts(RA_test_res)
        DECMC_test_res = DEC_to_MotorCounts(DEC_test_res, PierSide.pierEast)

        RA_test = ST + 9
        If RA_test >= 24.0 Then
            RA_test = RA_test - 24.0
        ElseIf RA_test < 0.0 Then
            RA_test = RA_test + 24.0
        End If
        DEC_test = 45.0
        'RAMC_test = RA_to_MotorCounts(RA_test)
        DECMC_test = DEC_to_MotorCounts(DEC_test, PierSide.pierWest)
        RA_test_res = MotorCounts_to_RA(RAMC_test)
        DEC_test_res = MotorCounts_to_DEC(DECMC_test)
        'RAMC_test_res = RA_to_MotorCounts(RA_test_res)
        DECMC_test_res = DEC_to_MotorCounts(DEC_test_res, PierSide.pierWest)

        ' -------------------------------------------------------------------------------------
        RA_test = ST - 3
        If RA_test >= 24.0 Then
            RA_test = RA_test - 24.0
        ElseIf RA_test < 0.0 Then
            RA_test = RA_test + 24.0
        End If
        DEC_test = 45.0
        'RAMC_test = RA_to_MotorCounts(RA_test)
        DECMC_test = DEC_to_MotorCounts(DEC_test, PierSide.pierEast)
        RA_test_res = MotorCounts_to_RA(RAMC_test)
        DEC_test_res = MotorCounts_to_DEC(DECMC_test)
        'RAMC_test_res = RA_to_MotorCounts(RA_test_res)
        DECMC_test_res = DEC_to_MotorCounts(DEC_test_res, PierSide.pierEast)

        RA_test = ST - 3
        If RA_test >= 24.0 Then
            RA_test = RA_test - 24.0
        ElseIf RA_test < 0.0 Then
            RA_test = RA_test + 24.0
        End If
        DEC_test = 45.0
        'RAMC_test = RA_to_MotorCounts(RA_test)
        DECMC_test = DEC_to_MotorCounts(DEC_test, PierSide.pierWest)
        RA_test_res = MotorCounts_to_RA(RAMC_test)
        DEC_test_res = MotorCounts_to_DEC(DECMC_test)
        'RAMC_test_res = RA_to_MotorCounts(RA_test_res)
        DECMC_test_res = DEC_to_MotorCounts(DEC_test_res, PierSide.pierWest)

        ' -------------------------------------------------------------------------------------
        RA_test = ST - 9
        If RA_test >= 24.0 Then
            RA_test = RA_test - 24.0
        ElseIf RA_test < 0.0 Then
            RA_test = RA_test + 24.0
        End If
        DEC_test = 45.0
        'RAMC_test = RA_to_MotorCounts(RA_test)
        DECMC_test = DEC_to_MotorCounts(DEC_test, PierSide.pierEast)
        RA_test_res = MotorCounts_to_RA(RAMC_test)
        DEC_test_res = MotorCounts_to_DEC(DECMC_test)
        'RAMC_test_res = RA_to_MotorCounts(RA_test_res)
        DECMC_test_res = DEC_to_MotorCounts(DEC_test_res, PierSide.pierEast)

        RA_test = ST - 9
        If RA_test >= 24.0 Then
            RA_test = RA_test - 24.0
        ElseIf RA_test < 0.0 Then
            RA_test = RA_test + 24.0
        End If
        DEC_test = 45.0
        'RAMC_test = RA_to_MotorCounts(RA_test)
        DECMC_test = DEC_to_MotorCounts(DEC_test, PierSide.pierWest)
        RA_test_res = MotorCounts_to_RA(RAMC_test)
        DEC_test_res = MotorCounts_to_DEC(DECMC_test)
        'RAMC_test_res = RA_to_MotorCounts(RA_test_res)
        DECMC_test_res = DEC_to_MotorCounts(DEC_test_res, PierSide.pierWest)

        ST = SiderealTime


    End Sub


    Private Function ALT_to_MotorCounts() As Long

    End Function

    Private Function AZ_to_MotorCounts() As Long

    End Function

    Public Function ABS_Value(value As Double) As Double
        If value > 0 Then
            Return value
        Else
            Return value * -1.0
        End If
    End Function

    Private Function CalcPointingState(RACounts As Int32, DECCounts As Int32) As PierSide
        Dim HACounts As Double
        Dim STCounts As Int32
        Dim DECRange As Int32

        STCounts = MountRACounts / 4
        DECRange = MountDECCounts / 2
        HACounts = STCounts - RACounts
        'WpE Normal - POINT A
        If ((HACounts >= 0) And (HACounts <= STCounts)) And ((DECCounts >= 0) And (DECCounts <= DECRange)) Then
            'WpE_Normal = True
            'EpW_Normal = False
            'WpE_TtP = False
            'EpW_TtP = False
            Return PierSide.pierWest

            'WpE TtP - POINT B
        ElseIf ((HACounts >= 0) And (HACounts <= STCounts)) And ((DECCounts < 0) And (DECCounts >= -DECRange)) Then
            'WpE_Normal = False
            'EpW_Normal = False
            'WpE_TtP = True
            'EpW_TtP = False
            Return PierSide.pierEast

            'EpW Normal - POINT D
        ElseIf ((HACounts < 0) And (HACounts >= -STCounts)) And ((DECCounts < 0) And (DECCounts >= -DECRange)) Then
            'WpE_Normal = False
            'EpW_Normal = True
            'WpE_TtP = False
            'EpW_TtP = False
            Return PierSide.pierEast

            'EpW TtP - POINT C
        ElseIf ((HACounts < 0) And (HACounts >= -STCounts)) And ((DECCounts >= 0) And (DECCounts <= DECRange)) Then
            'WpE_Normal = False
            'EpW_Normal = False
            'WpE_TtP = False
            'EpW_TtP = True
            Return PierSide.pierWest

        End If


    End Function

#End Region

End Class
