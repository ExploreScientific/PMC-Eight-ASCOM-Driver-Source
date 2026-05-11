CUMULATIVE RELEASE NOTES

For future reference this Read Me file is located in C:\ES_PMC8_Utilities.txt

Release Date:  3/15/2024

This new install includes several files and utilities to make the PMC-Eight system better.  The installer will:

1.  Install the newest ASCOM driver for the PMC-Eight.
2.  Create a directory c:\ES_PMC8_Utilities.  The directory will contain the Universal Software Configuration Tool (UFCT) and a program to configure the PMC8 Wi-Fi for automatic connection to the home Wi-Fi network, making the PMC8 accessible through your home router.

A.  UFCT 1.3 Changes

In addition to some nice to haves in a program like this, added the ability to program the PMC8. This will make using the old CM something of a past item. For this capability to work, you must also have the propellent.exe and propellent.dll files in the same directory that the UFCT is in. The installer for the new driver does this for you.

Added is a capability to run the mount using LRDU buttons, Programming the PMC8 firmware, a means for reading documentation files, tool tips, and a refreshed look to the interface.

B.  PMC8 Firmware 20A01.4.5.binary

Update to PMC8 firmware. Corrects a reported issue for indi/ekos combination brought about by erroneous transmission over the TCP or UDP link of modem configuration commands. This was reported to cause command parsing errors.

The release also corrects issues related to the Titan mount.

The firmware works with the current (i.e., before this release) ASCOM driver.

This version 20A01.4.5 provides Bluetooth (BT) connectivity between the iexos100 with Bluetooth capability and a Windows PC. By using the BT, you may bypass both the Wi-Fi and serial and go directly to the PMC8 via BT. This is made possible because the PC can alias its BT connection to a COM port, which is used by the ASCOM driver. This capability works with ASCOM only, as Explorestars requires UDP Wi-Fi connectivity (ie not the COM port).

To use this BT capability, your iexos100 must have an esp32 Wi-Fi module that has been programmed with the BT stack. You may find out if your unit has this by running the UFCT and observing the splash screen after pressing the UFCT "Reboot and show splash" button. The module and its BT capability will be reported as NO Bluetooth or Bluetooth ON. If you have an ESP32 module but BT is not enabled, it is possible to retro fit the BT capability, although it involves opening your iexos to expose the board, installing some jumper wires, downloading and running some software to program the module. There is a procedure written for this and if you want to try it you can contact ES about obtaining it. I have to clear it with ES that they will allow you to do it if your mount is in warranty. Those who have purchased their mounts recently probably have the correct ESP32 programming already.

Older iexos100 units have a different Wi-Fi module (8266) which is not BT capable.

To use the BT capability on the PC, follow these steps:

1. After loading it into your iexos, assuming you have an iexos with BT enabled (see if the splash screen declares BT) go ahead and boot it.
2. Open device manager on the PC and find the comm port for the Standard serial over Bluetooth.
3. Open the BT settings on the PC and make sure the PMC8 is paired with the PC
4. Now open the BT setup on the PC. Find More BT options. Click on the Com tab. You will see two com ports, one with the SPP_SERVER and one for input. You will use the SPP_Server comm port to connect to.  Select OUTGOING as the method.  

I suggest you use the device hub. Things are a bit more responsive, but Poth also works.

5. Open the ASCOM hub settings. In the Device Hub this is done via the tools menu, and you keep selecting the ES PMC8 items until you eventually get to set the properties.  It is the properties setting that finally opens the driver setup window where the COM port and other driver settings are located.  Select serial but put in the comm port you found for the BT connection with the SPP_SERVER.  You will have to OK your way back to the main hub screen.
6. At that point you click connect. Now the first time you do this you will get a pop-up dialog box from windows asking you to connect or not. Do it.

That's it. Hub will connect and you are good to go. Once you have made these BT settings in your PC, I believe they are remembered, and in the future all you will need to do is Connect in the hub, and the PC will, after checking in with you, hook up.

C.  Configure PMC8 for Home Network:

This program allows you to configure your PMC-Eight mount to connect to your home network Wi-Fi, which lets you control it from computers attached to the home Wi-Fi, rather than directly to the PMC8. This utility can handle either the exos2, G-11, or iexos100 (that is the rn131 or the ESP32 module).

Please use the UFCT to ensure the Wi-Fi mode is set the way you want it (UDP/TCP) prior to running this program. Also, if you are going to use this with Explorestars, be sure to set the Explorestars app to use the new network address shown in the new app, rather than the 192.168.47.1 which is the normal IP. The port number remains the same.

Explorestars use still requires the PMC8 modem be in UDP mode.

This app remembers the network credentials you enter, so you won’t have to enter them each time. This includes the SSID, password, and the Com port number. IMPORTANT: If you change your com port arrangement after setting the credentials (meaning have less than you used to) you may experience an error upon opening the app. The remedy is to delete the iexos_IP.txt file that is found in the same directory the application is in.

The iexos PMC8 does not remember the new network settings yet, this will require a firmware change. For now you must run this app each time you reboot the PMC8, for example between sessions.  Not sue if remembering the network will be included in the firmware in the future.

The program releases the serial port after each button press, so you may leave it open on your desktop and still use the serial port with other programs, such as the UFCT or ASCOM. Note ASCOM does not release the com port, and the hub must be shut down before you use this program again.

It is best if you set your exos2 or G-11 to Wi-Fi channel 0 (any PMC8 that has the RN131 modem should use channel 0, but any PMC8 that has the ESP module should not be set to channel 0) by using the UFCT before switching it to your home network. Remember, if you want to go back to the PMC8 network, change that Wi-Fi channel to something else.  Having the Wi-Fi channel set to 0 allows the rn131 to select the SSID on whatever channel the network is present on.  This is helpful if you move your mount around and have multiple access points on different channels.

If you use this program to put your G-11 or Exos2 onto your home network (that is units with the RN131 modem) it will stay there until you restore it to factory configuration. It will stay there for every subsequent boot until you restore it. This is done by running the RESTORE RN131 function in the UFCT. This app cannot reset the RN131 modem back to the normal mode of being an ad-hoc network server, but the UFCT will.

Instructions:

1.  Open the application.  Select the correct COM port for the PMC8 serial.  Set the radio button for the modem type you have in your PMC8
2.  First time you run it you will have to enter your network SSID and password in the appropriate entry windows
3.  You can use the GET PMC8 IP ADDRESS anytime to see what the PMC8 IP address is
4.  After setting the home SSID and password, you can press the WRITE to PMC8 button to set the PMC8 up for your home network.  
5.  After writing the values the windows will display the PMC8 IP address assigned by your home router to the PMC8.  This is the IP address you will use in your application.  It will need to be put in the ASCOM driver setup for the IP address.  

In Summary, remember: The iexos will not remember the home Wi-Fi setup between boots of the PMC-Eight.  The RN-131 based units such as the G-11 and EXOS2 remember the home network setting and thus to restore the unit to its native mode you must execute the RESET RN131 function in the UFCT.

D.  ASCOM Driver  2024 01 16 date code on setup page

The new driver incorporates several changes.

1.  It now supports variable rates.  This allows you to change slew rates to arbitrary levels in programs like NINA, SkyTrack. 
2.  It has various bug fixes and internal improvements.  Chiefly, it includes the fix for the southern hemisphere "always slewing" bug reported some time ago and corrected in the October 2022 Beta found in the files section of the subgroup.
3.  It is correct for the Titan mount.
