object fmMaps: TfmMaps
  Left = 0
  Top = 97
  Width = 989
  Height = 639
  Caption = #1050#1072#1088#1090#1099
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 12
  object Splitter1: TSplitter
    Left = 289
    Top = 0
    Width = 4
    Height = 593
    Beveled = True
  end
  object PageControl: TPageControl
    Left = 293
    Top = 0
    Width = 688
    Height = 593
    ActivePage = tsMap
    Align = alClient
    TabOrder = 0
    object tsMap: TTabSheet
      Caption = #1050#1072#1088#1090#1072
      ImageIndex = 1
      object ScrollBoxGeocoding: TScrollBox
        Left = 0
        Top = 0
        Width = 680
        Height = 566
        Align = alClient
        BevelInner = bvNone
        BevelKind = bkSoft
        BorderStyle = bsNone
        TabOrder = 0
        object ImageGeocoding: TImage
          Left = 0
          Top = 0
          Width = 163
          Height = 193
          AutoSize = True
          Center = True
        end
      end
    end
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
    ActivePage = tsSearch
    Align = alLeft
    TabOrder = 2
    object tsPlaces: TTabSheet
      Caption = #1052#1077#1089#1090#1072
      object TreeView1: TTreeView
        Left = 8
        Top = 8
        Width = 249
        Height = 337
        Indent = 19
        SortType = stText
        TabOrder = 0
      end
      object Button1: TButton
        Left = 8
        Top = 352
        Width = 75
        Height = 25
        Caption = 'Button1'
        TabOrder = 1
        OnClick = Button1Click
      end
      object ProgressBar1: TProgressBar
        Left = 8
        Top = 384
        Width = 249
        Height = 16
        TabOrder = 2
      end
      object CheckBirth: TCheckBox
        Left = 24
        Top = 416
        Width = 97
        Height = 17
        Caption = 'Birth'
        TabOrder = 3
      end
      object CheckDeath: TCheckBox
        Left = 24
        Top = 440
        Width = 97
        Height = 17
        Caption = 'Death'
        TabOrder = 4
      end
      object CheckResidence: TCheckBox
        Left = 24
        Top = 464
        Width = 97
        Height = 17
        Caption = 'Residence'
        TabOrder = 5
      end
      object Button2: TButton
        Left = 24
        Top = 488
        Width = 75
        Height = 25
        Caption = #1055#1086#1082#1072#1079#1072#1090#1100
        TabOrder = 6
        OnClick = Button2Click
      end
      object ComboBox1: TComboBox
        Left = 120
        Top = 416
        Width = 153
        Height = 20
        Enabled = False
        ItemHeight = 12
        TabOrder = 7
        Text = 'ComboBox1'
      end
    end
    object tsSearch: TTabSheet
      Caption = #1055#1086#1080#1089#1082
      ImageIndex = 1
      object GroupBox1: TGroupBox
        Left = 0
        Top = 0
        Width = 281
        Height = 81
        Align = alTop
        Caption = #1055#1086' '#1082#1086#1086#1088#1076#1080#1085#1072#1090#1072#1084
        TabOrder = 0
        object lbLatitude: TLabel
          Left = 14
          Top = 24
          Width = 43
          Height = 12
          Caption = #1064#1080#1088#1086#1090#1072':'
        end
        object lbLongitude: TLabel
          Left = 14
          Top = 48
          Width = 44
          Height = 12
          Caption = #1044#1086#1083#1075#1086#1090#1072':'
        end
        object edLatitude: TEdit
          Left = 70
          Top = 22
          Width = 73
          Height = 20
          TabOrder = 0
          Text = '57.916667'
        end
        object edLongitude: TEdit
          Left = 70
          Top = 46
          Width = 73
          Height = 20
          TabOrder = 1
          Text = '59.966667'
        end
        object btRefersh: TButton
          Left = 188
          Top = 48
          Width = 85
          Height = 25
          Caption = #1054#1073#1085#1086#1074#1080#1090#1100
          TabOrder = 2
          OnClick = btRefershClick
        end
      end
      object GroupBox2: TGroupBox
        Left = 0
        Top = 81
        Width = 281
        Height = 48
        Align = alTop
        Caption = #1055#1086' '#1085#1072#1079#1074#1072#1085#1080#1102
        TabOrder = 1
        object edSearch: TEdit
          Left = 8
          Top = 14
          Width = 169
          Height = 20
          TabOrder = 0
          Text = #1091#1083#1080#1094#1072' '#1051#1077#1085#1080#1085#1072
        end
        object btSearch: TButton
          Left = 182
          Top = 10
          Width = 91
          Height = 25
          Caption = #1055#1086#1080#1089#1082
          TabOrder = 1
          OnClick = btSearchClick
        end
      end
      object ListView: TListView
        Left = 0
        Top = 129
        Width = 281
        Height = 437
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
        TabOrder = 2
        ViewStyle = vsReport
      end
    end
    object TabSheet1: TTabSheet
      Caption = #1059#1087#1088#1072#1074#1083#1077#1085#1080#1077
      ImageIndex = 2
    end
  end
end
