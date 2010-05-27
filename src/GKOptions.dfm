object fmOptions: TfmOptions
  Left = 325
  Top = 118
  BorderStyle = bsDialog
  Caption = #1053#1072#1089#1090#1088#1086#1081#1082#1080
  ClientHeight = 409
  ClientWidth = 513
  Color = clBtnFace
  DefaultMonitor = dmMainForm
  Font.Charset = RUSSIAN_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 513
    Height = 361
    ActivePage = SheetCommon
    Align = alTop
    TabOrder = 0
    object SheetCommon: TTabSheet
      Caption = #1054#1073#1097#1080#1077
      object rgCode: TRadioGroup
        Left = 8
        Top = 8
        Width = 217
        Height = 57
        Caption = #1050#1086#1076#1080#1088#1086#1074#1082#1072' '#1089#1086#1093#1088#1072#1085#1077#1085#1080#1103' '#1092#1072#1081#1083#1086#1074
        Items.Strings = (
          'ASCII'
          'UTF-8')
        TabOrder = 0
      end
      object GroupBox3: TGroupBox
        Left = 8
        Top = 72
        Width = 217
        Height = 81
        Caption = #1057#1080#1089#1090#1077#1084#1072
        TabOrder = 1
        object CheckAppRegister: TCheckBox
          Left = 8
          Top = 24
          Width = 201
          Height = 17
          Caption = #1056#1077#1075#1080#1089#1090#1088#1072#1094#1080#1103' '#1087#1088#1086#1075#1088#1072#1084#1084#1099' '#1074' '#1089#1080#1089#1090#1077#1084#1077
          TabOrder = 0
        end
        object CheckExtRegister: TCheckBox
          Left = 8
          Top = 48
          Width = 201
          Height = 17
          Caption = #1056#1077#1075#1080#1089#1090#1088#1072#1094#1080#1103' '#1092#1072#1081#1083#1086#1074' '#1074' '#1089#1080#1089#1090#1077#1084#1077
          TabOrder = 1
        end
      end
      object GroupBox4: TGroupBox
        Left = 8
        Top = 160
        Width = 217
        Height = 161
        Caption = #1047#1072#1075#1088#1091#1079#1082#1072' '#1080#1079' '#1048#1085#1090#1077#1088#1085#1077#1090#1072
        TabOrder = 2
        object Label1: TLabel
          Left = 16
          Top = 56
          Width = 37
          Height = 13
          Caption = #1057#1077#1088#1074#1077#1088
        end
        object Label2: TLabel
          Left = 16
          Top = 80
          Width = 25
          Height = 13
          Caption = #1055#1086#1088#1090
        end
        object Label3: TLabel
          Left = 16
          Top = 104
          Width = 30
          Height = 13
          Caption = #1051#1086#1075#1080#1085
        end
        object Label4: TLabel
          Left = 16
          Top = 128
          Width = 37
          Height = 13
          Caption = #1055#1072#1088#1086#1083#1100
        end
        object chkProxy: TCheckBox
          Left = 16
          Top = 24
          Width = 185
          Height = 17
          Caption = #1048#1089#1087#1086#1083#1100#1079#1086#1074#1072#1090#1100' '#1087#1088#1086#1082#1089#1080'-'#1089#1077#1088#1074#1077#1088
          TabOrder = 0
        end
        object edProxyServer: TEdit
          Left = 80
          Top = 48
          Width = 121
          Height = 21
          TabOrder = 1
        end
        object edProxyPort: TEdit
          Left = 80
          Top = 72
          Width = 121
          Height = 21
          TabOrder = 2
        end
        object edProxyLogin: TEdit
          Left = 80
          Top = 96
          Width = 121
          Height = 21
          TabOrder = 3
        end
        object edProxyPass: TEdit
          Left = 80
          Top = 120
          Width = 121
          Height = 21
          PasswordChar = '*'
          TabOrder = 4
          Text = 'edProxyPass'
        end
      end
      object GroupBox6: TGroupBox
        Left = 232
        Top = 8
        Width = 265
        Height = 41
        Caption = #1048#1085#1089#1090#1088#1091#1084#1077#1085#1090#1099
        TabOrder = 3
        object CheckGEDCOMOptimize: TCheckBox
          Left = 8
          Top = 16
          Width = 249
          Height = 17
          Caption = #1054#1087#1090#1080#1084#1080#1079#1072#1094#1080#1103' '#1089#1086#1093#1088#1072#1085#1103#1077#1084#1099#1093' '#1092#1072#1081#1083#1086#1074
          TabOrder = 0
        end
      end
      object GroupBox7: TGroupBox
        Left = 232
        Top = 56
        Width = 265
        Height = 41
        Caption = #1055#1086#1076#1089#1082#1072#1079#1082#1080
        TabOrder = 4
        object CheckShowOnStart: TCheckBox
          Left = 8
          Top = 16
          Width = 225
          Height = 17
          Caption = #1055#1086#1082#1072#1079#1099#1074#1072#1090#1100' '#1087#1088#1080' '#1089#1090#1072#1088#1090#1077
          TabOrder = 0
        end
      end
    end
    object SheetView: TTabSheet
      Caption = #1048#1085#1090#1077#1088#1092#1077#1081#1089
      ImageIndex = 3
      object PageControl2: TPageControl
        Left = 0
        Top = 0
        Width = 505
        Height = 333
        ActivePage = SheetViewCommon
        Align = alClient
        TabOrder = 0
        object SheetViewCommon: TTabSheet
          Caption = #1042#1089#1077' '#1089#1087#1080#1089#1082#1080
          object rgFNPFormat: TRadioGroup
            Left = 8
            Top = 8
            Width = 185
            Height = 97
            Caption = #1060#1086#1088#1084#1072#1090' '#1080#1084#1077#1085' '#1074' '#1089#1087#1080#1089#1082#1072#1093
            Items.Strings = (
              #1060#1072#1084#1080#1083#1080#1103'_'#1048#1084#1103'_'#1054#1090#1095#1077#1089#1090#1074#1086
              #1060#1072#1084#1080#1083#1080#1103'; '#1048#1084#1103'_'#1054#1090#1095#1077#1089#1090#1074#1086
              #1060#1072#1084#1080#1083#1080#1103'; '#1048#1084#1103'; '#1054#1090#1095#1077#1089#1090#1074#1086)
            TabOrder = 0
          end
          object rgDateFormat: TRadioGroup
            Left = 8
            Top = 120
            Width = 185
            Height = 65
            Caption = #1060#1086#1088#1084#1072#1090' '#1076#1072#1090#1099' '#1074' '#1089#1087#1080#1089#1082#1072#1093
            Items.Strings = (
              'DD.MM.YYYY'
              'YYYY.MM.DD')
            TabOrder = 1
          end
          object CheckPlacesWithAddress: TCheckBox
            Left = 8
            Top = 200
            Width = 185
            Height = 17
            Caption = #1042#1082#1083#1102#1095#1072#1090#1100' '#1072#1076#1088#1077#1089' '#1074' '#1089#1090#1088#1086#1082#1080' '#1084#1077#1089#1090
            TabOrder = 2
          end
        end
        object SheetViewPersons: TTabSheet
          Caption = #1057#1087#1080#1089#1086#1082' '#1087#1077#1088#1089#1086#1085
          ImageIndex = 1
          object SpeedButton1: TSpeedButton
            Left = 352
            Top = 8
            Width = 24
            Height = 24
            Glyph.Data = {
              C6050000424DC605000000000000360400002800000014000000140000000100
              0800000000009001000000000000000000000001000000000000000000000000
              80000080000000808000800000008000800080800000C0C0C000C0DCC000F0CA
              A6000020400000206000002080000020A0000020C0000020E000004000000040
              20000040400000406000004080000040A0000040C0000040E000006000000060
              20000060400000606000006080000060A0000060C0000060E000008000000080
              20000080400000806000008080000080A0000080C0000080E00000A0000000A0
              200000A0400000A0600000A0800000A0A00000A0C00000A0E00000C0000000C0
              200000C0400000C0600000C0800000C0A00000C0C00000C0E00000E0000000E0
              200000E0400000E0600000E0800000E0A00000E0C00000E0E000400000004000
              20004000400040006000400080004000A0004000C0004000E000402000004020
              20004020400040206000402080004020A0004020C0004020E000404000004040
              20004040400040406000404080004040A0004040C0004040E000406000004060
              20004060400040606000406080004060A0004060C0004060E000408000004080
              20004080400040806000408080004080A0004080C0004080E00040A0000040A0
              200040A0400040A0600040A0800040A0A00040A0C00040A0E00040C0000040C0
              200040C0400040C0600040C0800040C0A00040C0C00040C0E00040E0000040E0
              200040E0400040E0600040E0800040E0A00040E0C00040E0E000800000008000
              20008000400080006000800080008000A0008000C0008000E000802000008020
              20008020400080206000802080008020A0008020C0008020E000804000008040
              20008040400080406000804080008040A0008040C0008040E000806000008060
              20008060400080606000806080008060A0008060C0008060E000808000008080
              20008080400080806000808080008080A0008080C0008080E00080A0000080A0
              200080A0400080A0600080A0800080A0A00080A0C00080A0E00080C0000080C0
              200080C0400080C0600080C0800080C0A00080C0C00080C0E00080E0000080E0
              200080E0400080E0600080E0800080E0A00080E0C00080E0E000C0000000C000
              2000C0004000C0006000C0008000C000A000C000C000C000E000C0200000C020
              2000C0204000C0206000C0208000C020A000C020C000C020E000C0400000C040
              2000C0404000C0406000C0408000C040A000C040C000C040E000C0600000C060
              2000C0604000C0606000C0608000C060A000C060C000C060E000C0800000C080
              2000C0804000C0806000C0808000C080A000C080C000C080E000C0A00000C0A0
              2000C0A04000C0A06000C0A08000C0A0A000C0A0C000C0A0E000C0C00000C0C0
              2000C0C04000C0C06000C0C08000C0C0A000F0FBFF00A4A0A000808080000000
              FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FDFDFDFDFDFD
              FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD
              FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD
              FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD00000000000000FDFDFDFDFDFDFDFDFD
              FDFDFDFD00DADADADADA00FDFDFDFDFDFDFDFDFDFDFDFDFD00E8E8E8E8DA00FD
              FDFDFDFDFDFDFDDA0000000000FEE8E8E8DA000000000000FDFDFDFDDAFF09F2
              FEFEFEE8E8DADADADADA00FDFDFDFDFDFDDAFF09FEFEFEFEE8E8E8E8DA00FDFD
              FDFDFDFDFDFDDAFF09FEFEFEFEE8E8DA00FDFDFDFDFDFDFDFDFDFDDAFF09F2FE
              FEFEDA00FDFDFDFDFDFDFDFDFDFDFDFDDAFF09FEFEDA00FDFDFDFDFDFDFDFDFD
              FDFDFDFDFDDAFF09FE00FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDDAFF00FDFDFD
              FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDDAFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD
              FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD
              FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD
              FDFDFDFDFDFDFDFDFDFD}
            OnClick = SpeedButton1Click
          end
          object SpeedButton2: TSpeedButton
            Left = 352
            Top = 40
            Width = 24
            Height = 24
            Glyph.Data = {
              C6050000424DC605000000000000360400002800000014000000140000000100
              0800000000009001000000000000000000000001000000000000000000000000
              80000080000000808000800000008000800080800000C0C0C000C0DCC000F0CA
              A6000020400000206000002080000020A0000020C0000020E000004000000040
              20000040400000406000004080000040A0000040C0000040E000006000000060
              20000060400000606000006080000060A0000060C0000060E000008000000080
              20000080400000806000008080000080A0000080C0000080E00000A0000000A0
              200000A0400000A0600000A0800000A0A00000A0C00000A0E00000C0000000C0
              200000C0400000C0600000C0800000C0A00000C0C00000C0E00000E0000000E0
              200000E0400000E0600000E0800000E0A00000E0C00000E0E000400000004000
              20004000400040006000400080004000A0004000C0004000E000402000004020
              20004020400040206000402080004020A0004020C0004020E000404000004040
              20004040400040406000404080004040A0004040C0004040E000406000004060
              20004060400040606000406080004060A0004060C0004060E000408000004080
              20004080400040806000408080004080A0004080C0004080E00040A0000040A0
              200040A0400040A0600040A0800040A0A00040A0C00040A0E00040C0000040C0
              200040C0400040C0600040C0800040C0A00040C0C00040C0E00040E0000040E0
              200040E0400040E0600040E0800040E0A00040E0C00040E0E000800000008000
              20008000400080006000800080008000A0008000C0008000E000802000008020
              20008020400080206000802080008020A0008020C0008020E000804000008040
              20008040400080406000804080008040A0008040C0008040E000806000008060
              20008060400080606000806080008060A0008060C0008060E000808000008080
              20008080400080806000808080008080A0008080C0008080E00080A0000080A0
              200080A0400080A0600080A0800080A0A00080A0C00080A0E00080C0000080C0
              200080C0400080C0600080C0800080C0A00080C0C00080C0E00080E0000080E0
              200080E0400080E0600080E0800080E0A00080E0C00080E0E000C0000000C000
              2000C0004000C0006000C0008000C000A000C000C000C000E000C0200000C020
              2000C0204000C0206000C0208000C020A000C020C000C020E000C0400000C040
              2000C0404000C0406000C0408000C040A000C040C000C040E000C0600000C060
              2000C0604000C0606000C0608000C060A000C060C000C060E000C0800000C080
              2000C0804000C0806000C0808000C080A000C080C000C080E000C0A00000C0A0
              2000C0A04000C0A06000C0A08000C0A0A000C0A0C000C0A0E000C0C00000C0C0
              2000C0C04000C0C06000C0C08000C0C0A000F0FBFF00A4A0A000808080000000
              FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FDFDFDFDFDFD
              FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD
              FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD00
              FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD00DA00FDFDFDFDFDFDFDFDFDFDFD
              FDFDFDFDFD00E8E8DA00FDFDFDFDFDFDFDFDFDFDFDFDFDFD00E8FEE8E8DA00FD
              FDFDFDFDFDFDFDFDFDFDFDDAE8FEFEFEE8E8DA00FDFDFDFDFDFDFDFDFDFDDAE8
              FEFEFEFEFEE8E8DA00FDFDFDFDFDFDFDFDDA09090909FEFEFEE8E8E8DA00FDFD
              FDFDFDFDDAFFFFFFFF0909FEFEE8E8E8E8DA00FDFDFDFDDADADADADADAFF09F2
              FEE800DADADADA00FDFDFDFDFDFDFDFDDAFF09F2FEE800FDFDFDFDFDFDFDFDFD
              FDFDFDFDDAFF09F2FEE800FDFDFDFDFDFDFDFDFDFDFDFDFDDADADADADADADAFD
              FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD
              FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD
              FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD
              FDFDFDFDFDFDFDFDFDFD}
            OnClick = SpeedButton2Click
          end
          object ListPersonColumns: TListView
            Left = 8
            Top = 8
            Width = 337
            Height = 289
            Checkboxes = True
            Columns = <>
            ReadOnly = True
            RowSelect = True
            TabOrder = 0
            ViewStyle = vsList
            OnChange = ListPersonColumnsChange
          end
          object btnDefList: TBitBtn
            Left = 352
            Top = 272
            Width = 137
            Height = 25
            Caption = #1047#1085#1072#1095#1077#1085#1080#1103' '#1087#1086' '#1091#1084#1086#1083#1095#1072#1085#1080#1102
            TabOrder = 1
            OnClick = btnDefListClick
          end
        end
      end
    end
    object SheetPedigree: TTabSheet
      Caption = #1056#1086#1076#1086#1089#1083#1086#1074#1085#1099#1077' '#1076#1088#1077#1074#1072' '#1080' '#1088#1086#1089#1087#1080#1089#1080
      ImageIndex = 1
      object GroupBox1: TGroupBox
        Left = 8
        Top = 8
        Width = 241
        Height = 185
        Caption = #1054#1090#1086#1073#1088#1072#1078#1077#1085#1080#1077' '#1087#1077#1088#1089#1086#1085' '#1074' '#1076#1088#1077#1074#1077
        TabOrder = 0
        object CheckFamily: TCheckBox
          Left = 16
          Top = 16
          Width = 209
          Height = 17
          Alignment = taLeftJustify
          Caption = #1060#1072#1084#1080#1083#1080#1103
          TabOrder = 0
        end
        object CheckName: TCheckBox
          Left = 16
          Top = 40
          Width = 209
          Height = 17
          Alignment = taLeftJustify
          Caption = #1048#1084#1103
          TabOrder = 1
        end
        object CheckPatronymic: TCheckBox
          Left = 16
          Top = 64
          Width = 209
          Height = 17
          Alignment = taLeftJustify
          Caption = #1054#1090#1095#1077#1089#1090#1074#1086
          TabOrder = 2
        end
        object CheckDiffLines: TCheckBox
          Left = 16
          Top = 88
          Width = 209
          Height = 17
          Alignment = taLeftJustify
          Caption = #1056#1072#1079#1085#1099#1077' '#1089#1090#1088#1086#1082#1080' ('#1080#1084#1103' '#1080' '#1086#1090#1095#1077#1089#1090#1074#1086')'
          TabOrder = 3
        end
        object CheckBirthDate: TCheckBox
          Left = 16
          Top = 112
          Width = 209
          Height = 17
          Alignment = taLeftJustify
          Caption = #1044#1072#1090#1072' '#1088#1086#1078#1076#1077#1085#1080#1103
          TabOrder = 4
        end
        object CheckDeathDate: TCheckBox
          Left = 16
          Top = 136
          Width = 209
          Height = 17
          Alignment = taLeftJustify
          Caption = #1044#1072#1090#1072' '#1089#1084#1077#1088#1090#1080
          TabOrder = 5
        end
        object CheckKinship: TCheckBox
          Left = 16
          Top = 160
          Width = 209
          Height = 17
          Alignment = taLeftJustify
          Caption = #1057#1090#1077#1087#1077#1085#1100' '#1088#1086#1076#1089#1090#1074#1072
          Enabled = False
          TabOrder = 6
        end
      end
      object GroupBox2: TGroupBox
        Left = 256
        Top = 8
        Width = 241
        Height = 185
        Caption = #1062#1074#1077#1090#1072' '#1074' '#1076#1088#1077#1074#1077
        TabOrder = 1
        object PanMaleColor: TPanel
          Left = 16
          Top = 16
          Width = 97
          Height = 25
          Cursor = crHandPoint
          Caption = #1052#1091#1078#1095#1080#1085#1072
          Color = 16762566
          TabOrder = 0
          OnClick = PanMaleColorClick
        end
        object PanFemaleColor: TPanel
          Left = 128
          Top = 16
          Width = 97
          Height = 25
          Cursor = crHandPoint
          Caption = #1046#1077#1085#1097#1080#1085#1072
          Color = 13027071
          TabOrder = 1
          OnClick = PanMaleColorClick
        end
        object PanUnkSexColor: TPanel
          Left = 16
          Top = 56
          Width = 209
          Height = 25
          Cursor = crHandPoint
          Caption = #1053#1077#1080#1079#1074#1077#1089#1090#1085#1099#1081' '#1087#1086#1083
          Color = 16762623
          TabOrder = 2
          OnClick = PanMaleColorClick
        end
        object PanUnHusbandColor: TPanel
          Left = 16
          Top = 96
          Width = 209
          Height = 25
          Cursor = crHandPoint
          Caption = #1056#1072#1079#1074#1077#1076#1077#1085#1085#1099#1081' '#1089#1091#1087#1088#1091#1075
          Color = 16766935
          TabOrder = 3
          OnClick = PanMaleColorClick
        end
        object PanUnWifeColor: TPanel
          Left = 16
          Top = 136
          Width = 209
          Height = 25
          Cursor = crHandPoint
          Caption = #1056#1072#1079#1074#1077#1076#1077#1085#1085#1072#1103' '#1089#1091#1087#1088#1091#1075#1072
          Color = 14145535
          TabOrder = 4
          OnClick = PanMaleColorClick
        end
      end
      object GroupBox5: TGroupBox
        Left = 8
        Top = 216
        Width = 241
        Height = 105
        Caption = #1043#1077#1085#1077#1088#1072#1094#1080#1103' '#1088#1086#1089#1087#1080#1089#1077#1081
        TabOrder = 2
        object CheckAttributes: TCheckBox
          Left = 16
          Top = 24
          Width = 209
          Height = 17
          Alignment = taLeftJustify
          Caption = #1042#1082#1083#1102#1095#1072#1103' '#1072#1090#1088#1080#1073#1091#1090#1099' '#1087#1077#1088#1089#1086#1085
          TabOrder = 0
        end
        object CheckNotes: TCheckBox
          Left = 16
          Top = 48
          Width = 209
          Height = 17
          Alignment = taLeftJustify
          Caption = #1042#1082#1083#1102#1095#1072#1103' '#1079#1072#1084#1077#1090#1082#1080
          TabOrder = 1
        end
        object CheckSources: TCheckBox
          Left = 16
          Top = 72
          Width = 209
          Height = 17
          Alignment = taLeftJustify
          Caption = #1042#1082#1083#1102#1095#1072#1103' '#1080#1089#1090#1086#1095#1085#1080#1082#1080
          TabOrder = 2
        end
      end
      object EditPedigreeFormat: TRadioGroup
        Left = 256
        Top = 216
        Width = 241
        Height = 57
        Caption = #1060#1086#1088#1084#1072#1090
        Items.Strings = (
          #1048#1079#1073#1099#1090#1086#1095#1085#1099#1081
          #1050#1086#1084#1087#1072#1082#1090#1085#1099#1081' ('#1090#1088#1072#1076#1080#1094#1080#1086#1085#1085#1099#1081' '#1059#1048#1056#1054')')
        TabOrder = 3
      end
    end
  end
  object btnAccept: TBitBtn
    Left = 336
    Top = 376
    Width = 81
    Height = 25
    Caption = #1055#1088#1080#1085#1103#1090#1100
    TabOrder = 1
    OnClick = btnAcceptClick
    Kind = bkOK
  end
  object btnCancel: TBitBtn
    Left = 424
    Top = 376
    Width = 81
    Height = 25
    Caption = #1054#1090#1084#1077#1085#1080#1090#1100
    TabOrder = 2
    Kind = bkCancel
  end
  object ColorDialog1: TColorDialog
    Left = 352
    Top = 248
  end
end
