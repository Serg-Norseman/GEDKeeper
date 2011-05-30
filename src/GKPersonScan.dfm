object fmPersonScan: TfmPersonScan
  Left = 418
  Top = 155
  BorderStyle = bsDialog
  Caption = #1044#1086#1073#1072#1074#1083#1077#1085#1080#1077' '#1087#1077#1088#1089#1086#1085' '#1080#1079' '#1080#1089#1090#1086#1095#1085#1080#1082#1072
  ClientHeight = 457
  ClientWidth = 649
  Color = clBtnFace
  DefaultMonitor = dmMainForm
  Font.Charset = RUSSIAN_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object btnParse: TBitBtn
    Left = 464
    Top = 424
    Width = 81
    Height = 25
    Caption = #1044#1086#1073#1072#1074#1080#1090#1100
    TabOrder = 0
    OnClick = btnParseClick
  end
  object btnClose: TBitBtn
    Left = 560
    Top = 424
    Width = 81
    Height = 25
    Caption = #1047#1072#1082#1088#1099#1090#1100
    TabOrder = 1
    Kind = bkCancel
  end
  object PageControl1: TPageControl
    Left = 8
    Top = 8
    Width = 633
    Height = 401
    ActivePage = tsSimpleInput
    TabOrder = 2
    object tsSimpleInput: TTabSheet
      Caption = #1055#1088#1086#1089#1090#1086#1081' '#1074#1074#1086#1076
      object Label1: TLabel
        Left = 8
        Top = 8
        Width = 133
        Height = 13
        Caption = #1055#1086#1083#1085#1086#1077' '#1080#1084#1103' ('#1092#1086#1088#1084#1072#1090' '#1060#1048#1054')'
      end
      object Label2: TLabel
        Left = 8
        Top = 232
        Width = 42
        Height = 13
        Caption = #1047#1072#1084#1077#1090#1082#1072
      end
      object btnMale: TSpeedButton
        Left = 424
        Top = 24
        Width = 23
        Height = 21
        GroupIndex = 1
        Down = True
        Caption = #1052
      end
      object btnFemale: TSpeedButton
        Left = 448
        Top = 24
        Width = 23
        Height = 21
        GroupIndex = 1
        Caption = #1046
      end
      object EditName: TEdit
        Left = 8
        Top = 24
        Width = 409
        Height = 21
        TabOrder = 0
        OnKeyPress = EditNameKeyPress
      end
      object MemoNote: TMemo
        Left = 8
        Top = 245
        Width = 465
        Height = 121
        TabOrder = 1
      end
      object Panel1: TPanel
        Left = 8
        Top = 56
        Width = 465
        Height = 81
        BorderStyle = bsSingle
        TabOrder = 2
        object Label3: TLabel
          Left = 8
          Top = 32
          Width = 80
          Height = 13
          Caption = #1044#1072#1090#1072' '#1088#1086#1078#1076#1077#1085#1080#1103
        end
        object Label5: TLabel
          Left = 112
          Top = 32
          Width = 85
          Height = 13
          Caption = #1052#1077#1089#1090#1086' '#1088#1086#1078#1076#1077#1085#1080#1103
        end
        object EditBirthDate: TMaskEdit
          Left = 8
          Top = 48
          Width = 97
          Height = 21
          EditMask = '!99/99/9999;1;_'
          MaxLength = 10
          TabOrder = 0
          Text = '  .  .    '
          OnChange = EditBirthDateChange
        end
        object EditBirthPlace: TEdit
          Left = 112
          Top = 48
          Width = 337
          Height = 21
          TabOrder = 1
          OnChange = EditBirthDateChange
        end
        object CheckBirth: TCheckBox
          Left = 8
          Top = 8
          Width = 96
          Height = 17
          Caption = #1056#1086#1076#1080#1083#1089#1103
          TabOrder = 2
        end
      end
      object Panel2: TPanel
        Left = 8
        Top = 144
        Width = 465
        Height = 81
        BorderStyle = bsSingle
        TabOrder = 3
        object Label6: TLabel
          Left = 8
          Top = 32
          Width = 64
          Height = 13
          Caption = #1044#1072#1090#1072' '#1089#1084#1077#1088#1090#1080
        end
        object Label7: TLabel
          Left = 112
          Top = 32
          Width = 69
          Height = 13
          Caption = #1052#1077#1089#1090#1086' '#1089#1084#1077#1088#1090#1080
        end
        object CheckDeath: TCheckBox
          Left = 8
          Top = 8
          Width = 95
          Height = 17
          Caption = #1059#1084#1077#1088
          TabOrder = 0
        end
        object EditDeathDate: TMaskEdit
          Left = 8
          Top = 48
          Width = 97
          Height = 21
          EditMask = '!99/99/9999;1;_'
          MaxLength = 10
          TabOrder = 1
          Text = '  .  .    '
          OnChange = EditDeathDateChange
        end
        object EditDeathPlace: TEdit
          Left = 112
          Top = 48
          Width = 337
          Height = 21
          TabOrder = 2
          OnChange = EditDeathDateChange
        end
      end
    end
    object tsSourceInput: TTabSheet
      Caption = #1048#1089#1090#1086#1095#1085#1080#1082' ('#1084#1077#1090#1088#1080#1082#1080'/'#1088#1077#1074#1080#1079#1080#1080')'
      ImageIndex = 1
      object Label4: TLabel
        Left = 8
        Top = 56
        Width = 48
        Height = 13
        Caption = #1048#1089#1090#1086#1095#1085#1080#1082
      end
      object Label8: TLabel
        Left = 304
        Top = 56
        Width = 75
        Height = 13
        Caption = #1051#1080#1089#1090'/'#1089#1090#1088#1072#1085#1080#1094#1072
      end
      object Label9: TLabel
        Left = 520
        Top = 56
        Width = 19
        Height = 13
        Caption = #1043#1086#1076
      end
      object Label10: TLabel
        Left = 8
        Top = 88
        Width = 95
        Height = 13
        Caption = #1053#1072#1089#1077#1083#1077#1085#1085#1099#1081' '#1087#1091#1085#1082#1090
      end
      object cbSource: TComboBox
        Left = 64
        Top = 48
        Width = 225
        Height = 21
        ItemHeight = 13
        TabOrder = 0
        Text = 'cbSource'
      end
      object edPage: TEdit
        Left = 384
        Top = 48
        Width = 121
        Height = 21
        TabOrder = 1
        Text = 'edPage'
      end
      object edSourceYear: TMaskEdit
        Left = 560
        Top = 48
        Width = 57
        Height = 21
        EditMask = '!0000;1;_'
        MaxLength = 4
        TabOrder = 2
        Text = '    '
      end
      object edPlace: TEdit
        Left = 120
        Top = 80
        Width = 497
        Height = 21
        TabOrder = 3
        Text = 'edPlace'
      end
      object sgData: TStringGrid
        Left = 8
        Top = 184
        Width = 609
        Height = 177
        ColCount = 6
        DefaultColWidth = 80
        DefaultRowHeight = 20
        FixedCols = 0
        RowCount = 100
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goEditing]
        TabOrder = 4
        OnKeyDown = sgDataKeyDown
        OnSelectCell = sgDataSelectCell
      end
      object cbPersonLink: TComboBox
        Left = 336
        Top = 296
        Width = 145
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 5
        Visible = False
        OnChange = cbPersonLinkChange
        OnKeyDown = cbPersonLinkKeyDown
      end
      object rgSourceKind: TRadioGroup
        Left = 8
        Top = 0
        Width = 609
        Height = 38
        Caption = #1058#1080#1087' '#1080#1089#1090#1086#1095#1085#1080#1082#1072
        Columns = 2
        ItemIndex = 0
        Items.Strings = (
          #1056#1077#1074#1080#1079#1089#1082#1072#1103' '#1089#1082#1072#1079#1082#1072
          #1052#1077#1090#1088#1080#1095#1077#1089#1082#1072#1103' '#1082#1085#1080#1075#1072)
        TabOrder = 6
        OnClick = rgSourceKindClick
      end
      object gbMetrics: TGroupBox
        Left = 8
        Top = 120
        Width = 609
        Height = 50
        Caption = #1052#1077#1090#1088#1080#1095#1077#1089#1082#1072#1103' '#1082#1085#1080#1075#1072
        Enabled = False
        TabOrder = 7
        object Label11: TLabel
          Left = 8
          Top = 24
          Width = 72
          Height = 13
          Caption = #1044#1072#1090#1072' '#1089#1086#1073#1099#1090#1080#1103
        end
        object Label12: TLabel
          Left = 248
          Top = 24
          Width = 64
          Height = 13
          Caption = #1058#1080#1087' '#1089#1086#1073#1099#1090#1080#1103
        end
        object edEventDate: TMaskEdit
          Left = 96
          Top = 16
          Width = 129
          Height = 21
          EditMask = '!99/99/9999;1;_'
          MaxLength = 10
          TabOrder = 0
          Text = '  .  .    '
        end
        object cbEventType: TComboBox
          Left = 328
          Top = 16
          Width = 145
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 1
          Items.Strings = (
            #1056#1086#1078#1076#1077#1085#1080#1077
            #1057#1084#1077#1088#1090#1100
            #1041#1088#1072#1082)
        end
      end
    end
  end
end
