object fmResearchEdit: TfmResearchEdit
  Left = 632
  Top = 151
  BorderStyle = bsDialog
  Caption = #1056#1077#1076#1072#1082#1090#1080#1088#1086#1074#1072#1085#1080#1077' '#1080#1089#1089#1083#1077#1076#1086#1074#1072#1085#1080#1103
  ClientHeight = 457
  ClientWidth = 609
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
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 0
    Top = 0
    Width = 609
    Height = 97
    Align = alTop
    TabOrder = 1
    object Label1: TLabel
      Left = 8
      Top = 24
      Width = 48
      Height = 13
      Caption = #1053#1072#1079#1074#1072#1085#1080#1077
    end
    object Label2: TLabel
      Left = 8
      Top = 48
      Width = 55
      Height = 13
      Caption = #1055#1088#1080#1086#1088#1080#1090#1077#1090
    end
    object Label3: TLabel
      Left = 248
      Top = 48
      Width = 54
      Height = 13
      Caption = #1057#1086#1089#1090#1086#1103#1085#1080#1077
    end
    object Label4: TLabel
      Left = 8
      Top = 72
      Width = 51
      Height = 13
      Caption = #1047#1072#1087#1091#1097#1077#1085#1086
    end
    object Label5: TLabel
      Left = 248
      Top = 72
      Width = 56
      Height = 13
      Caption = #1047#1072#1074#1077#1088#1096#1077#1085#1086
    end
    object Label6: TLabel
      Left = 488
      Top = 48
      Width = 43
      Height = 13
      Caption = #1055#1088#1086#1094#1077#1085#1090
    end
    object EditName: TEdit
      Left = 72
      Top = 16
      Width = 529
      Height = 21
      TabOrder = 0
    end
    object EditPriority: TComboBox
      Left = 72
      Top = 40
      Width = 161
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 1
    end
    object EditStatus: TComboBox
      Left = 312
      Top = 40
      Width = 161
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 2
    end
    object EditStartDate: TMaskEdit
      Left = 72
      Top = 64
      Width = 161
      Height = 21
      EditMask = '!99/99/9999;1;_'
      MaxLength = 10
      TabOrder = 5
      Text = '  .  .    '
    end
    object EditStopDate: TMaskEdit
      Left = 312
      Top = 64
      Width = 161
      Height = 21
      EditMask = '!99/99/9999;1;_'
      MaxLength = 10
      TabOrder = 6
      Text = '  .  .    '
    end
    object EditPercent: TEdit
      Left = 544
      Top = 40
      Width = 41
      Height = 21
      TabOrder = 3
      Text = '0'
    end
    object UpDown1: TUpDown
      Left = 585
      Top = 40
      Width = 15
      Height = 21
      Associate = EditPercent
      Increment = 5
      TabOrder = 4
    end
  end
  object PagesGroupData: TPageControl
    Left = 0
    Top = 97
    Width = 609
    Height = 312
    ActivePage = SheetTasks
    Align = alTop
    TabOrder = 0
    object SheetTasks: TTabSheet
      Caption = #1047#1072#1076#1072#1095#1080
      ImageIndex = 5
    end
    object SheetCommunications: TTabSheet
      Caption = #1050#1086#1084#1084#1091#1085#1080#1082#1072#1094#1080#1080
      ImageIndex = 2
    end
    object SheetGroups: TTabSheet
      Caption = #1043#1088#1091#1087#1087#1099
      ImageIndex = 3
    end
    object SheetNotes: TTabSheet
      Caption = #1047#1072#1084#1077#1090#1082#1080
      ImageIndex = 1
    end
  end
  object btnAccept: TBitBtn
    Left = 432
    Top = 424
    Width = 81
    Height = 25
    Caption = #1055#1088#1080#1085#1103#1090#1100
    TabOrder = 2
    OnClick = btnAcceptClick
    Kind = bkOK
  end
  object btnCancel: TBitBtn
    Left = 520
    Top = 424
    Width = 81
    Height = 25
    Caption = #1054#1090#1084#1077#1085#1080#1090#1100
    TabOrder = 3
    Kind = bkCancel
  end
end
