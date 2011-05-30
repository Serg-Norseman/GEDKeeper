object fmGroupEdit: TfmGroupEdit
  Left = 366
  Top = 153
  BorderStyle = bsDialog
  Caption = #1056#1077#1076#1072#1082#1090#1080#1088#1086#1074#1072#1085#1080#1077' '#1075#1088#1091#1087#1087#1099
  ClientHeight = 457
  ClientWidth = 482
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
    Width = 482
    Height = 49
    Align = alTop
    TabOrder = 1
    object Label1: TLabel
      Left = 8
      Top = 24
      Width = 48
      Height = 13
      Caption = #1053#1072#1079#1074#1072#1085#1080#1077
    end
    object edName: TEdit
      Left = 72
      Top = 16
      Width = 401
      Height = 21
      TabOrder = 0
    end
  end
  object PagesGroupData: TPageControl
    Left = 0
    Top = 49
    Width = 482
    Height = 360
    ActivePage = SheetMembers
    Align = alTop
    TabOrder = 0
    object SheetMembers: TTabSheet
      Caption = #1059#1095#1072#1089#1090#1085#1080#1082#1080
      ImageIndex = 5
    end
    object SheetNotes: TTabSheet
      Caption = #1047#1072#1084#1077#1090#1082#1080
      ImageIndex = 1
    end
    object SheetMultimedia: TTabSheet
      Caption = #1052#1091#1083#1100#1090#1080#1084#1077#1076#1080#1072
      ImageIndex = 2
    end
  end
  object btnAccept: TBitBtn
    Left = 304
    Top = 424
    Width = 81
    Height = 25
    Caption = #1055#1088#1080#1085#1103#1090#1100
    TabOrder = 2
    OnClick = btnAcceptClick
    Kind = bkOK
  end
  object btnCancel: TBitBtn
    Left = 392
    Top = 424
    Width = 81
    Height = 25
    Caption = #1054#1090#1084#1077#1085#1080#1090#1100
    TabOrder = 3
    Kind = bkCancel
  end
end
