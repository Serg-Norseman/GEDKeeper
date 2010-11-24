object fmOrganizer: TfmOrganizer
  Left = 408
  Top = 256
  BorderStyle = bsDialog
  Caption = #1054#1088#1075#1072#1085#1072#1081#1079#1077#1088
  ClientHeight = 522
  ClientWidth = 736
  Color = clBtnFace
  Font.Charset = RUSSIAN_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 736
    Height = 522
    ActivePage = SheetAddresses
    Align = alClient
    TabOrder = 0
    object SheetAddresses: TTabSheet
      Caption = #1040#1076#1088#1077#1089#1072
    end
    object SheetTelephones: TTabSheet
      Caption = #1058#1077#1083#1077#1092#1086#1085#1099
      ImageIndex = 1
    end
    object SheetEMails: TTabSheet
      Caption = #1055#1086#1095#1090#1072
      ImageIndex = 2
    end
    object SheetWebs: TTabSheet
      Caption = #1057#1072#1081#1090#1099
      ImageIndex = 3
    end
  end
end
