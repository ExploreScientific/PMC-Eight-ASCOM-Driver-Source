Imports System.Windows.Forms
Imports System.Runtime.InteropServices
Imports ASCOM.Utilities
Imports ASCOM.ES_PMC8

<ComVisible(False)>
Public Class SetupDialogForm
    '    Friend objTCPNetwork As New System.Net.Sockets.TcpClient 'TCP network object
    '    Friend stream As Net.Sockets.NetworkStream

    Private Sub OK_Button_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles OK_Button.Click ' OK button event handler
        ' Persist new values of user settings to the ASCOM profile
        Telescope.comPort = ComboBox1.Text ' Update the state variables with results from the dialogue
        Telescope.comSpeed = ComboBox2.Text
        Telescope.traceState = chkTrace.Checked
        Telescope.IPAddress = TextBox1.Text
        Telescope.IPPort = TextBox2.Text
        Telescope.WirelessEnabled = RadioButton3.Checked
        Telescope.WiFiModuleID = cboWiFiModule.Text
        If RadioButton3.Checked Then
            Telescope.WirelessProtocol = "TCP"
        End If
        Telescope.MSROMount = False
        Telescope.ScottyMount = False
        Telescope.MountMaxSpeed = 40000         'default max speed is 40000
        Telescope.LongMoveOffset1 = 8           'default for Slew to target long movement time offset
        Telescope.LongMoveOffset2 = -4          'default for slew.  Build in the negative signs as needed
        Telescope.RampOnlyOffset1 = 4
        Telescope.RampOnlyOffset2 = 4
        Telescope.Mount = ComboBox3.Text
        If Telescope.Mount = "Losmandy G-11" Then
            Telescope.MountRACounts = 4608000
            Telescope.MountDECCounts = 4608000
        ElseIf Telescope.Mount = "Losmandy Titan" Then
            Telescope.MountRACounts = 6048000        '270 teetth, 400per rev 32 micro per step 1.75 ratio on pulleys
            Telescope.MountDECCounts = 6048000
        ElseIf Telescope.Mount = "Explore Scientific EXOS II" Then
            Telescope.MountRACounts = 4147200
            Telescope.MountDECCounts = 4147200
        ElseIf Telescope.Mount = "Explore Scientific iEXOS-100" Then
            Telescope.MountRACounts = 4147200
            Telescope.MountDECCounts = 4147200
        ElseIf Telescope.Mount = "Explore Scientific iEXOS-200" Then
            Telescope.MountRACounts = 5760000
            Telescope.MountDECCounts = 5760000
        ElseIf Telescope.Mount = "Explore Scientific iEXOS-300" Then
            Telescope.MountRACounts = 4147200       'future TBD
            Telescope.MountDECCounts = 4147200      'future Tbd
        ElseIf Telescope.Mount = "MSROEQ" Then
            Telescope.MountRACounts = 5760000       '180 teeth, 400 per rev, 32 micro steps, 2.5 pulley ratio
            Telescope.MountDECCounts = 5760000
            '            MountSpecificMax = 43200                '3 degree max for this mount wem note - no variable rates in this version
            Telescope.MSROMount = True
        ElseIf Telescope.Mount = "ASKO SX260S" Then         'new MSRO monster mount
            Telescope.MountRACounts = 9163636      '350 teeth, 200 per rev, 32 micro steps, 45/11 gear ratio
            Telescope.MountDECCounts = 9600000     '300 teeth, 200 per rev, 32 micros, 5.0 reduction gear box
            Telescope.MountMaxSpeed = 16000
            Telescope.LongMoveOffset1 = 2.0          'ASKO long move offset
            Telescope.LongMoveOffset2 = 2.0
        ElseIf Telescope.Mount = "Scotty" Then
            Telescope.MountRACounts = 5760000
            Telescope.MountDECCounts = 5760000
            Telescope.MountMaxSpeed = 8000               'Scotty max rate is this, same in firmware specific to this mount type
            Telescope.ScottyMount = True
            Telescope.LongMoveOffset1 = 2.0          'Scotty long move offset
            Telescope.LongMoveOffset2 = 2.0
            Telescope.RampOnlyOffset1 = 4.0          'Scotty ramp only offset
            Telescope.RampOnlyOffset2 = 4.0

        End If

        If RadioButton2.Checked Then
            Telescope.SkySafari_rate = 1.0         'set the rate array for no variable rates
        Else
            Telescope.SkySafari_rate = 0.00        'set the rate array with variable rates
        End If
        Telescope.Rate = ComboBox4.Text
        Telescope.RARateOffsetValue = CDbl(Val(tbRateOffset.Text))   'ConformU
        Telescope.DECRateOffsetValue = CDbl(Val(TextBox3.Text))      'ConformU



        If Telescope.Rate = "Sidereal" Then
            'conform U changes here.  When OK is clicked, set the mountrihtascensionrate.

            Telescope.MountRightAscensionRate = 15.0 + Telescope.RARateOffsetValue    '

        ElseIf Telescope.Rate = "Lunar" Then

            Telescope.MountRightAscensionRate = 14.685 + Telescope.RARateOffsetValue    '


        ElseIf Telescope.Rate = "Solar" Then
            Telescope.MountRightAscensionRate = 15.041 + Telescope.RARateOffsetValue    '


        ElseIf Telescope.Rate = "King" Then
            Telescope.MountRightAscensionRate = 15.0369 + Telescope.RARateOffsetValue    '


        End If

        Telescope.SiteLocation = tbSiteLocation.Text
        Telescope.SiteLatitudeValue = tbSiteLatitude.Text
        Telescope.SiteLongitudeValue = tbSiteLongitude.Text
        Telescope.SiteElevationValue = tbSiteElevation.Text
        Telescope.ApertureDiameterValue = tbApertureDiameter.Text
        Telescope.ApertureAreaValue = tbApertureArea.Text
        Telescope.FocalLengthValue = tbFocalLength.Text
        '        Telescope.RateOffsetValue = tbRateOffset.Text
        Telescope.SiteAmbientTemperatureValue = Convert.ToDouble(tbAmbientTemp.Text)
        Telescope.ApplyRefractionCorrection = cbRefraction.CheckState
        Telescope.RA_SiderealRateFraction = nud_RA.Value
        Telescope.DEC_SiderealRateFraction = nud_DEC.Value
        Telescope.MinimumPulseTime = nud_PulseTime.Value
        'Telescope.BacklashValue = tbBacklash.Text
        'Telescope.BacklashTime = tbBacklashTime.Text
        'Telescope.BacklashMinimum = tbMinBacklash.Text
        '        Telescope.RARateOffsetValue = Telescope.RARateOffsetValue   'COnformU:  there is some question if user input shoud be made negative, as RA rate increase for Conformu is a negative value
        '       Telescope.DECRateOffsetValue = Telescope.DECRateOffsetValue  'ConformU


        Me.DialogResult = System.Windows.Forms.DialogResult.OK
        Me.Close()
    End Sub

    Private Sub Cancel_Button_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Cancel_Button.Click 'Cancel button event handler
        Me.DialogResult = System.Windows.Forms.DialogResult.Cancel
        Me.Close()
    End Sub

    Private Sub ShowAscomWebPage(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox1.DoubleClick, PictureBox1.Click
        ' Click on ASCOM logo event handler
        Try
            System.Diagnostics.Process.Start("http://ascom-standards.org/")
        Catch noBrowser As System.ComponentModel.Win32Exception
            If noBrowser.ErrorCode = -2147467259 Then
                MessageBox.Show(noBrowser.Message)
            End If
        Catch other As System.Exception
            MessageBox.Show(other.Message)
        End Try
    End Sub

    Private Sub ShowAscomWebPage2(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox2.DoubleClick, PictureBox2.Click
        ' Click on Explore Scientific logo event handler
        Try
            System.Diagnostics.Process.Start("http://explorescientificusa.com/")
        Catch noBrowser As System.ComponentModel.Win32Exception
            If noBrowser.ErrorCode = -2147467259 Then
                MessageBox.Show(noBrowser.Message)
            End If
        Catch other As System.Exception
            MessageBox.Show(other.Message)
        End Try
    End Sub

    Private Sub SetupDialogForm_Load(sender As System.Object, e As System.EventArgs) Handles MyBase.Load ' Form load event handler
        ' Retrieve current values of user settings from the ASCOM Profile 
        Using objSerial As New ASCOM.Utilities.Serial
            For Each item In objSerial.AvailableCOMPorts
                ComboBox1.Items.Add(item)
            Next
        End Using
        chkTrace.Checked = Telescope.traceState
        ComboBox1.Text = Telescope.comPort
        ComboBox2.Text = Telescope.comSpeed
        TextBox1.Text = Telescope.IPAddress
        TextBox2.Text = Telescope.IPPort
        RadioButton1.Checked = Not Telescope.WirelessEnabled
        If Telescope.WirelessProtocol = "TCP" Then
            RadioButton3.Checked = Telescope.WirelessEnabled
        End If
        RadioButton1.Checked = Not Telescope.WirelessEnabled
        ComboBox3.Text = Telescope.Mount
        ComboBox4.Text = Telescope.Rate
        tbSiteLocation.Text = Telescope.SiteLocation
        tbSiteElevation.Text = Telescope.SiteElevationValue.ToString
        tbSiteLatitude.Text = Telescope.SiteLatitudeValue.ToString
        tbSiteLongitude.Text = Telescope.SiteLongitudeValue.ToString
        tbApertureDiameter.Text = Telescope.ApertureDiameterValue.ToString
        tbApertureArea.Text = Telescope.ApertureAreaValue.ToString
        tbFocalLength.Text = Telescope.FocalLengthValue.ToString
        tbRateOffset.Text = Telescope.RARateOffsetValue.ToString
        TextBox3.Text = Telescope.DECRateOffsetValue.ToString
        tbAmbientTemp.Text = Telescope.SiteAmbientTemperatureValue.ToString
        cbRefraction.Checked = Telescope.ApplyRefractionCorrection
        nud_RA.Value = Telescope.RA_SiderealRateFraction
        nud_DEC.Value = Telescope.DEC_SiderealRateFraction
        nud_PulseTime.Value = Telescope.MinimumPulseTime
        cboWiFiModule.Text = Telescope.WiFiModuleID
        RadioButton2.Checked = (Telescope.SkySafari_rate = 1)
        tbBacklash.Text = Telescope.BacklashValue
        tbBacklashTime.Text = Telescope.BacklashTime
        tbMinBacklash.Text = Telescope.BacklashMinimum
        If Telescope.BacklashEnabled = False Then
            cbBacklash.Checked = False
        ElseIf Telescope.BacklashEnabled = True Then
            cbBacklash.Checked = True
        End If

        ' Create the ToolTip and associate with the Form container.
        Dim toolTip1 As New ToolTip()

        ' Set up the delays for the ToolTip.
        toolTip1.AutoPopDelay = 5000
        toolTip1.InitialDelay = 1000
        toolTip1.ReshowDelay = 500
        ' Force the ToolTip text to be displayed whether or not the form is active.
        toolTip1.ShowAlways = True

        ' Set up the ToolTip text for the Button and Checkbox.
        toolTip1.SetToolTip(Me.OK_Button, "Save the changes")
        toolTip1.SetToolTip(Me.Cancel_Button, "Cancel the changes")
        toolTip1.SetToolTip(Me.PictureBox2, "Explore Scientific Website")
        toolTip1.SetToolTip(Me.PictureBox1, "ASCOM Standards Website")
        toolTip1.SetToolTip(Me.ComboBox1, "Select Serial Port Number")
        toolTip1.SetToolTip(Me.ComboBox2, "Select Serial Port Speed")
        toolTip1.SetToolTip(Me.ComboBox3, "Select Mount Type")
        toolTip1.SetToolTip(Me.ComboBox4, "Select Mount Rate")
        toolTip1.SetToolTip(Me.tbSiteLocation, "Set City Name")
        toolTip1.SetToolTip(Me.tbSiteLatitude, "Latitude + for North, - for South")
        toolTip1.SetToolTip(Me.tbSiteLongitude, "Longitude - for West, + for EAST")
        toolTip1.SetToolTip(Me.tbApertureDiameter, "Telescope Aperture Diameter (meters)")
        toolTip1.SetToolTip(Me.tbApertureArea, "Telescope Aperture Area (meter^2)")
        toolTip1.SetToolTip(Me.tbFocalLength, "Telescope Focal Length (meters)")
        toolTip1.SetToolTip(Me.nud_RA, "Pulse Guiding Sidereal Rate Fraction for RA Axis")
        toolTip1.SetToolTip(Me.nud_DEC, "Pulse Guiding Sidereal Rate Fraction for DEC Axis")
        toolTip1.SetToolTip(Me.nud_PulseTime, "Minimum Pulse Guiding Pulse Time Interval milli-seconds")
        toolTip1.SetToolTip(Me.cboWiFiModule, "WiFi Module installed on PMC-Eight")
        toolTip1.SetToolTip(Me.tbBacklash, "Amount of Backlash to Correct arc-seconds")
        toolTip1.SetToolTip(Me.tbBacklashTime, "Amount of Time To Correct Backlash Milliseconds")
        toolTip1.SetToolTip(Me.tbMinBacklash, "Minimum Amount of Backlash to Correct")
        toolTip1.SetToolTip(Me.cbBacklash, "Enable DEC Backlash Correction")
    End Sub

    Private Sub ToolStripMenuItem1_Click(sender As Object, e As EventArgs) Handles ToolStripMenuItem1.Click
        Dim frmAbout As New AboutBox1
        frmAbout.Show()
    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        Dim frmAbout As New AboutBox1
        frmAbout.Show()
    End Sub

    Private Sub btn_ST4Calibration_Click(sender As Object, e As EventArgs) Handles btn_ST4Calibration.Click
        Dim frmST4Calibration As New AboutBox1
    End Sub

    Private Sub cbBacklash_CheckedChanged(sender As Object, e As EventArgs) Handles cbBacklash.CheckedChanged
        'toggle backlash controls visible
        If Not tbBacklash.Visible Then
            Telescope.BacklashEnabled = True
            tbBacklash.Visible = True
            tbBacklashTime.Visible = True
            tbMinBacklash.Visible = True
            Label42.Visible = True
            Label43.Visible = True
            Label44.Visible = True
            Label45.Visible = True
            Label46.Visible = True
            Label47.Visible = True
        ElseIf tbBacklash.Visible Then
            Telescope.BacklashEnabled = False
            tbBacklash.Visible = False
            tbBacklashTime.Visible = False
            tbMinBacklash.Visible = False
            Label42.Visible = False
            Label43.Visible = False
            Label44.Visible = False
            Label45.Visible = False
            Label46.Visible = False
            Label47.Visible = False
        End If
    End Sub

    Private Sub TabPage1_Click(sender As Object, e As EventArgs) Handles TabPage1.Click

    End Sub

    Private Sub Label19_Click(sender As Object, e As EventArgs) Handles Label19.Click

    End Sub

    Private Sub ComboBox3_SelectedIndexChanged(sender As Object, e As EventArgs) Handles ComboBox3.SelectedIndexChanged

    End Sub

    Private Sub RadioButton1_CheckedChanged(sender As Object, e As EventArgs) Handles RadioButton1.CheckedChanged

    End Sub

    Private Sub RadioButton3_CheckedChanged(sender As Object, e As EventArgs) Handles RadioButton3.CheckedChanged

    End Sub

    Private Sub ComboBox4_SelectedIndexChanged(sender As Object, e As EventArgs) Handles ComboBox4.SelectedIndexChanged

    End Sub

    Private Sub tbRateOffset_TextChanged(sender As Object, e As EventArgs) Handles tbRateOffset.TextChanged

    End Sub

    Private Sub tbMinBacklash_TextChanged(sender As Object, e As EventArgs) Handles tbMinBacklash.TextChanged

    End Sub

    Private Sub tbBacklash_TextChanged(sender As Object, e As EventArgs) Handles tbBacklash.TextChanged

    End Sub
End Class
