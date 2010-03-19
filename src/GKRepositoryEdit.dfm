object fmRepositoryEdit: TfmRepositoryEdit
  Left = 395
  Top = 208
  BorderStyle = bsDialog
  Caption = #1040#1088#1093#1080#1074
  ClientHeight = 369
  ClientWidth = 417
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object btnAccept: TBitBtn
    Left = 240
    Top = 336
    Width = 81
    Height = 25
    Caption = #1055#1088#1080#1085#1103#1090#1100
    TabOrder = 2
    OnClick = btnAcceptClick
    Kind = bkOK
  end
  object btnCancel: TBitBtn
    Left = 328
    Top = 336
    Width = 81
    Height = 25
    Caption = #1054#1090#1084#1077#1085#1080#1090#1100
    TabOrder = 4
    Kind = bkCancel
  end
  object GroupBox1: TGroupBox
    Left = 0
    Top = 0
    Width = 417
    Height = 41
    Align = alTop
    TabOrder = 3
    object Label1: TLabel
      Left = 8
      Top = 20
      Width = 48
      Height = 13
      Caption = #1053#1072#1079#1074#1072#1085#1080#1077
    end
    object edName: TEdit
      Left = 72
      Top = 12
      Width = 337
      Height = 21
      TabOrder = 0
    end
  end
  object PagesData: TPageControl
    Left = 0
    Top = 41
    Width = 417
    Height = 280
    ActivePage = SheetNotes
    Align = alTop
    TabOrder = 0
    object SheetNotes: TTabSheet
      Caption = #1047#1072#1084#1077#1090#1082#1080
      ImageIndex = 1
    end
  end
  object btnAddress: TBitBtn
    Left = 8
    Top = 336
    Width = 81
    Height = 25
    Caption = #1040#1076#1088#1077#1089'...'
    TabOrder = 1
    OnClick = btnAddressClick
    NumGlyphs = 2
  end
end
