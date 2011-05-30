object fmAddressEdit: TfmAddressEdit
  Left = 426
  Top = 198
  BorderStyle = bsDialog
  Caption = #1040#1076#1088#1077#1089
  ClientHeight = 330
  ClientWidth = 409
  Color = clBtnFace
  DefaultMonitor = dmMainForm
  Font.Charset = RUSSIAN_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object btnAccept: TBitBtn
    Left = 232
    Top = 296
    Width = 81
    Height = 25
    Caption = #1055#1088#1080#1085#1103#1090#1100
    TabOrder = 1
    OnClick = btnAcceptClick
    Kind = bkOK
  end
  object btnCancel: TBitBtn
    Left = 320
    Top = 296
    Width = 81
    Height = 25
    Caption = #1054#1090#1084#1077#1085#1080#1090#1100
    TabOrder = 2
    Kind = bkCancel
  end
  object PageAddrData: TPageControl
    Left = 0
    Top = 0
    Width = 409
    Height = 281
    ActivePage = SheetCommon
    Align = alTop
    TabOrder = 0
    object SheetCommon: TTabSheet
      Caption = #1040#1076#1088#1077#1089
      ImageIndex = 3
      object Label1: TLabel
        Left = 8
        Top = 8
        Width = 37
        Height = 13
        Caption = #1057#1090#1088#1072#1085#1072
      end
      object Label2: TLabel
        Left = 216
        Top = 8
        Width = 75
        Height = 13
        Caption = #1064#1090#1072#1090'/'#1054#1073#1083#1072#1089#1090#1100
      end
      object Label3: TLabel
        Left = 8
        Top = 56
        Width = 31
        Height = 13
        Caption = #1043#1086#1088#1086#1076
      end
      object Label4: TLabel
        Left = 216
        Top = 56
        Width = 73
        Height = 13
        Caption = #1055#1086#1095#1090#1086#1074#1099#1081' '#1082#1086#1076
      end
      object Label5: TLabel
        Left = 8
        Top = 104
        Width = 31
        Height = 13
        Caption = #1040#1076#1088#1077#1089
      end
      object edCountry: TEdit
        Left = 8
        Top = 24
        Width = 201
        Height = 21
        TabOrder = 0
        Text = 'edCountry'
      end
      object edState: TEdit
        Left = 216
        Top = 24
        Width = 177
        Height = 21
        TabOrder = 1
        Text = 'edState'
      end
      object edCity: TEdit
        Left = 8
        Top = 72
        Width = 201
        Height = 21
        TabOrder = 2
        Text = 'edCity'
      end
      object edPostalCode: TEdit
        Left = 216
        Top = 72
        Width = 177
        Height = 21
        TabOrder = 3
        Text = 'edPostalCode'
      end
      object edAddress: TEdit
        Left = 8
        Top = 120
        Width = 385
        Height = 21
        TabOrder = 4
        Text = 'edAddress'
      end
    end
    object SheetPhones: TTabSheet
      Caption = #1058#1077#1083#1077#1092#1086#1085#1099
      ImageIndex = 1
    end
    object SheetEmails: TTabSheet
      Caption = #1069#1083'. '#1087#1086#1095#1090#1072
      ImageIndex = 2
    end
    object SheetWebPages: TTabSheet
      Caption = #1042#1077#1073'-'#1089#1090#1088#1072#1085#1080#1094#1099
      ImageIndex = 3
    end
  end
end
