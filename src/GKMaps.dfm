object fmMaps: TfmMaps
  Left = 322
  Top = 105
  Width = 989
  Height = 639
  Caption = #1050#1072#1088#1090#1099
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 289
    Top = 0
    Width = 4
    Height = 593
    Beveled = True
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 593
    Width = 981
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 289
    Height = 593
    ActivePage = tsPlaces
    Align = alLeft
    TabOrder = 1
    object tsPlaces: TTabSheet
      Caption = #1052#1077#1089#1090#1072
      object TreeView1: TTreeView
        Left = 0
        Top = 173
        Width = 281
        Height = 392
        Align = alClient
        Indent = 19
        SortType = stText
        TabOrder = 0
      end
      object GroupBox2: TGroupBox
        Left = 0
        Top = 0
        Width = 281
        Height = 173
        Align = alTop
        Caption = #1042#1099#1073#1086#1088#1082#1072
        TabOrder = 1
        object Label2: TLabel
          Left = 8
          Top = 88
          Width = 140
          Height = 13
          Caption = #1042#1089#1105' '#1087#1086' '#1079#1072#1076#1072#1085#1085#1086#1084#1091' '#1095#1077#1083#1086#1074#1077#1082#1091
        end
        object ComboPersons: TComboBox
          Left = 8
          Top = 104
          Width = 265
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 0
        end
        object CheckResidence: TCheckBox
          Left = 8
          Top = 64
          Width = 129
          Height = 17
          Caption = #1052#1077#1089#1090#1086' '#1078#1080#1090#1077#1083#1100#1089#1090#1074#1072
          TabOrder = 1
        end
        object CheckDeath: TCheckBox
          Left = 8
          Top = 40
          Width = 129
          Height = 17
          Caption = #1052#1077#1089#1090#1086' '#1089#1084#1077#1088#1090#1080
          TabOrder = 2
        end
        object CheckBirth: TCheckBox
          Left = 8
          Top = 16
          Width = 129
          Height = 17
          Caption = #1052#1077#1089#1090#1086' '#1088#1086#1078#1076#1077#1085#1080#1103
          TabOrder = 3
        end
        object btnSelectPlaces: TButton
          Left = 198
          Top = 136
          Width = 75
          Height = 25
          Caption = #1055#1086#1082#1072#1079#1072#1090#1100
          Enabled = False
          TabOrder = 4
          OnClick = btnSelectPlacesClick
        end
      end
    end
    object tsSearch: TTabSheet
      Caption = #1055#1086#1080#1089#1082
      ImageIndex = 1
      object GroupBox1: TGroupBox
        Left = 0
        Top = 0
        Width = 281
        Height = 137
        Align = alTop
        Caption = #1055#1086' '#1082#1086#1086#1088#1076#1080#1085#1072#1090#1072#1084
        TabOrder = 0
        object lbLatitude: TLabel
          Left = 8
          Top = 16
          Width = 44
          Height = 13
          Caption = #1064#1080#1088#1086#1090#1072':'
        end
        object lbLongitude: TLabel
          Left = 88
          Top = 16
          Width = 47
          Height = 13
          Caption = #1044#1086#1083#1075#1086#1090#1072':'
        end
        object Label1: TLabel
          Left = 8
          Top = 64
          Width = 48
          Height = 13
          Caption = #1053#1072#1079#1074#1072#1085#1080#1077
        end
        object edLatitude: TEdit
          Left = 8
          Top = 32
          Width = 73
          Height = 21
          TabOrder = 0
          Text = '57.916667'
        end
        object edLongitude: TEdit
          Left = 88
          Top = 32
          Width = 73
          Height = 21
          TabOrder = 1
          Text = '59.966667'
        end
        object btnSearch: TButton
          Left = 168
          Top = 104
          Width = 105
          Height = 25
          Caption = #1055#1086#1080#1089#1082
          TabOrder = 2
          OnClick = btnSearchClick
        end
        object edSearch: TEdit
          Left = 8
          Top = 80
          Width = 153
          Height = 21
          TabOrder = 3
          Text = #1052#1086#1089#1082#1074#1072
        end
        object RadioGroup1: TRadioGroup
          Left = 168
          Top = 16
          Width = 105
          Height = 81
          Caption = #1058#1080#1087
          ItemIndex = 0
          Items.Strings = (
            #1082#1086#1086#1088#1076#1080#1085#1072#1090#1099
            #1085#1072#1079#1074#1072#1085#1080#1077)
          TabOrder = 4
        end
      end
      object ListView: TListView
        Left = 0
        Top = 137
        Width = 281
        Height = 428
        Align = alClient
        Columns = <
          item
            Caption = #1040#1076#1088#1077#1089
            Width = 100
          end
          item
            Caption = #1064#1080#1088#1086#1090#1072
            Width = 75
          end
          item
            Caption = #1044#1086#1083#1075#1086#1090#1072
            Width = 75
          end>
        HideSelection = False
        ReadOnly = True
        TabOrder = 1
        ViewStyle = vsReport
        OnSelectItem = ListViewSelectItem
      end
    end
    object TabSheet1: TTabSheet
      Caption = #1059#1087#1088#1072#1074#1083#1077#1085#1080#1077
      ImageIndex = 2
      object Label3: TLabel
        Left = 8
        Top = 8
        Width = 53
        Height = 13
        Caption = #1058#1080#1087' '#1082#1072#1088#1090#1099
        Transparent = True
      end
      object btnSaveImage: TButton
        Left = 8
        Top = 72
        Width = 75
        Height = 25
        Caption = 'Save to...'
        TabOrder = 0
        OnClick = btnSaveImageClick
      end
      object cbMapType: TComboBox
        Left = 8
        Top = 24
        Width = 97
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        ItemIndex = 3
        TabOrder = 1
        Text = #1060#1080#1079#1080#1095#1077#1089#1082#1072#1103
        OnChange = cbMapTypeChange
        Items.Strings = (
          #1054#1073#1099#1095#1085#1072#1103
          #1057#1087#1091#1090#1085#1080#1082
          #1043#1080#1073#1088#1080#1076
          #1060#1080#1079#1080#1095#1077#1089#1082#1072#1103)
      end
    end
  end
  object Panel1: TPanel
    Left = 293
    Top = 0
    Width = 688
    Height = 593
    Align = alClient
    BorderStyle = bsSingle
    TabOrder = 2
  end
  object SaveDialog1: TSaveDialog
    Filter = 'Image files|*.bmp'
    Left = 409
    Top = 223
  end
end
