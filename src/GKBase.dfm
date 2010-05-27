object fmBase: TfmBase
  Left = 294
  Top = 114
  Width = 970
  Height = 576
  Caption = 'GEDKeeper'
  Color = clBtnFace
  DefaultMonitor = dmMainForm
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsMDIChild
  OldCreateOrder = True
  Position = poScreenCenter
  Visible = True
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnDeactivate = FormDeactivate
  PixelsPerInch = 96
  TextHeight = 13
  object PageRecords: TPageControl
    Left = 0
    Top = 0
    Width = 962
    Height = 549
    ActivePage = SheetLocations
    Align = alClient
    TabOrder = 0
    OnChange = PageRecordsChange
    object SheetPersons: TTabSheet
      Caption = #1055#1077#1088#1089#1086#1085#1099
    end
    object SheetFamilies: TTabSheet
      Caption = #1057#1077#1084#1100#1080
      ImageIndex = 4
    end
    object SheetNotes: TTabSheet
      Caption = #1047#1072#1084#1077#1090#1082#1080
      ImageIndex = 1
    end
    object SheetMultimedia: TTabSheet
      Caption = #1052#1091#1083#1100#1090#1080#1084#1077#1076#1080#1072
      ImageIndex = 2
    end
    object SheetSources: TTabSheet
      Caption = #1048#1089#1090#1086#1095#1085#1080#1082#1080
      ImageIndex = 3
    end
    object SheetRepositories: TTabSheet
      Caption = #1040#1088#1093#1080#1074#1099
      ImageIndex = 6
    end
    object SheetGroups: TTabSheet
      Caption = #1043#1088#1091#1087#1087#1099
      ImageIndex = 5
    end
    object SheetResearches: TTabSheet
      Caption = #1048#1089#1089#1083#1077#1076#1086#1074#1072#1085#1080#1103
      ImageIndex = 7
    end
    object SheetTasks: TTabSheet
      Caption = #1047#1072#1076#1072#1095#1080
      ImageIndex = 8
    end
    object SheetCommunications: TTabSheet
      Caption = #1050#1086#1084#1084#1091#1085#1080#1082#1072#1094#1080#1080
      ImageIndex = 9
    end
    object SheetLocations: TTabSheet
      Caption = #1052#1077#1089#1090#1072
      ImageIndex = 10
    end
  end
  object ActionList1: TActionList
    Left = 160
    Top = 216
    object actTest: TAction
      Category = 'Misc'
      Caption = 'Test'
      ShortCut = 122
      OnExecute = actTestExecute
    end
    object actConvertResidence: TAction
      Category = 'Misc'
      Caption = 'actConvertResidence'
      ShortCut = 16506
      OnExecute = actConvertResidenceExecute
    end
  end
end
