object fmOptions: TfmOptions
  Left = 325
  Top = 118
  BorderStyle = bsDialog
  Caption = #1053#1072#1089#1090#1088#1086#1081#1082#1080
  ClientHeight = 409
  ClientWidth = 513
  Color = clBtnFace
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
        Width = 185
        Height = 65
        Caption = #1050#1086#1076#1080#1088#1086#1074#1082#1072' '#1089#1086#1093#1088#1072#1085#1077#1085#1080#1103' '#1092#1072#1081#1083#1086#1074
        Items.Strings = (
          'ASCII'
          'UTF-8')
        TabOrder = 0
      end
      object rgFNPFormat: TRadioGroup
        Left = 8
        Top = 88
        Width = 185
        Height = 97
        Caption = #1060#1086#1088#1084#1072#1090' '#1080#1084#1077#1085' '#1074' '#1089#1087#1080#1089#1082#1072#1093
        Items.Strings = (
          #1060#1072#1084#1080#1083#1080#1103'_'#1048#1084#1103'_'#1054#1090#1095#1077#1089#1090#1074#1086
          #1060#1072#1084#1080#1083#1080#1103'; '#1048#1084#1103'_'#1054#1090#1095#1077#1089#1090#1074#1086
          #1060#1072#1084#1080#1083#1080#1103'; '#1048#1084#1103'; '#1054#1090#1095#1077#1089#1090#1074#1086)
        TabOrder = 1
      end
      object rgDateFormat: TRadioGroup
        Left = 8
        Top = 200
        Width = 185
        Height = 65
        Caption = #1060#1086#1088#1084#1072#1090' '#1076#1072#1090#1099' '#1074' '#1089#1087#1080#1089#1082#1072#1093
        Items.Strings = (
          'DD.MM.YYYY'
          'YYYY.MM.DD')
        TabOrder = 2
      end
      object GroupBox3: TGroupBox
        Left = 208
        Top = 8
        Width = 217
        Height = 81
        Caption = #1057#1080#1089#1090#1077#1084#1072
        TabOrder = 3
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
        Left = 208
        Top = 104
        Width = 217
        Height = 161
        Caption = #1047#1072#1075#1088#1091#1079#1082#1072' '#1080#1079' '#1048#1085#1090#1077#1088#1085#1077#1090#1072
        TabOrder = 4
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
      object CheckPlacesWithAddress: TCheckBox
        Left = 8
        Top = 280
        Width = 185
        Height = 17
        Caption = #1042#1082#1083#1102#1095#1072#1090#1100' '#1072#1076#1088#1077#1089' '#1074' '#1089#1090#1088#1086#1082#1080' '#1084#1077#1089#1090
        TabOrder = 5
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
    end
    object SheetTools: TTabSheet
      Caption = #1048#1085#1089#1090#1088#1091#1084#1077#1085#1090#1099
      ImageIndex = 2
      object CheckCleanEmptyFamilies: TCheckBox
        Left = 8
        Top = 8
        Width = 361
        Height = 17
        Caption = #1044#1080#1072#1075#1085#1086#1089#1090#1080#1082#1072': '#1091#1076#1072#1083#1077#1085#1080#1077' '#1079#1072#1087#1080#1089#1077#1081' '#1089#1077#1084#1077#1081' '#1073#1077#1079' '#1089#1091#1087#1088#1091#1075#1086#1074
        Enabled = False
        TabOrder = 0
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
