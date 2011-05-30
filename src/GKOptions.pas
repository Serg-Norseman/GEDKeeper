unit GKOptions; {prepare:fin; trans:fin}

{$I GEDKeeper.inc}

interface

uses
  SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, Buttons, GKCommon, GKLists, GKLangs;

type
  TfmOptions = class(TForm, ILocalization)
    PageControl1: TPageControl;
    SheetCommon: TTabSheet;
    rgCode: TRadioGroup;
    btnAccept: TBitBtn;
    btnCancel: TBitBtn;
    SheetTree: TTabSheet;
    GroupBox1: TGroupBox;
    chkFamily: TCheckBox;
    chkName: TCheckBox;
    chkPatronymic: TCheckBox;
    chkDiffLines: TCheckBox;
    chkBirthDate: TCheckBox;
    chkDeathDate: TCheckBox;
    chkKinship: TCheckBox;
    GroupBox2: TGroupBox;
    PanMaleColor: TPanel;
    PanFemaleColor: TPanel;
    PanUnkSexColor: TPanel;
    PanUnHusbandColor: TPanel;
    PanUnWifeColor: TPanel;
    ColorDialog1: TColorDialog;
    GroupBox4: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    chkProxy: TCheckBox;
    edProxyServer: TEdit;
    edProxyPort: TEdit;
    edProxyLogin: TEdit;
    edProxyPass: TEdit;
    SheetView: TTabSheet;
    PageControl2: TPageControl;
    SheetViewCommon: TTabSheet;
    SheetViewPersons: TTabSheet;
    ListPersonColumns: TListView;
    btnColumnUp: TSpeedButton;
    btnColumnDown: TSpeedButton;
    btnDefList: TBitBtn;
    rgFNPFormat: TRadioGroup;
    rgDateFormat: TRadioGroup;
    chkPlacesWithAddress: TCheckBox;
    GroupBox7: TGroupBox;
    chkShowOnStart: TCheckBox;
    rgEditMode: TRadioGroup;
    chkHighlightUnparented: TCheckBox;
    chkHighlightUnmarried: TCheckBox;
    chkOnlyYears: TCheckBox;
    chkSignsVisible: TCheckBox;
    chkChildlessExclude: TCheckBox;
    Label5: TLabel;
    PanDefFont: TPanel;
    FontDialog1: TFontDialog;
    SheetPedigree: TTabSheet;
    GroupBox5: TGroupBox;
    chkAttributes: TCheckBox;
    chkNotes: TCheckBox;
    chkSources: TCheckBox;
    EditPedigreeFormat: TRadioGroup;
    Label6: TLabel;
    cbLanguages: TComboBox;
    chkTreeDecorative: TCheckBox;
    chkPortraitsVisible: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure btnAcceptClick(Sender: TObject);
    procedure PanMaleColorClick(Sender: TObject);
    procedure btnColumnUpClick(Sender: TObject);
    procedure btnColumnDownClick(Sender: TObject);
    procedure ListPersonColumnsChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure btnDefListClick(Sender: TObject);
    procedure PanDefFontClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FOptions: TGlobalOptions;
    FPersonColumns: TPersonColumnsList;

    procedure UpdateColumnsList();
    procedure UpdateControls();
  public
    property Options: TGlobalOptions read FOptions write FOptions;

    procedure SetLang();
  end;

implementation

uses GedCom551, GKEngine, GKMain, GKUtils;

{$R *.dfm}

procedure TfmOptions.FormCreate(Sender: TObject);
begin
  SetLang();
end;

procedure TfmOptions.SetLang();
begin
  btnAccept.Caption := LSList[LSID_DlgAccept];
  btnCancel.Caption := LSList[LSID_DlgCancel];

  Caption := LSList[LSID_MIOptions];

  SheetCommon.Caption := LSList[LSID_Common];
  SheetView.Caption := LSList[LSID_Interface];
  SheetTree.Caption := LSList[LSID_Trees];
  SheetPedigree.Caption := LSList[LSID_Pedigrees];

  ///

  rgCode.Caption := LSList[LSID_SaveCoding];
  rgEditMode.Caption := LSList[LSID_WorkMode];
    rgEditMode.Items[0] := LSList[LSID_Simple];
    rgEditMode.Items[1] := LSList[LSID_Expert];

  GroupBox4.Caption := LSList[LSID_Internet];
  chkProxy.Caption := LSList[LSID_ProxyUse];
  Label1.Caption := LSList[LSID_ProxyServer];
  Label2.Caption := LSList[LSID_ProxyPort];
  Label3.Caption := LSList[LSID_ProxyLogin];
  Label4.Caption := LSList[LSID_ProxyPassword];

  GroupBox7.Caption := LSList[LSID_Tips];
  chkShowOnStart.Caption := LSList[LSID_StartupTips];

  Label6.Caption := LSList[LSID_Language];

  SheetViewCommon.Caption := LSList[LSID_ListsAll];
  SheetViewPersons.Caption := LSList[LSID_ListPersons];
    rgFNPFormat.Caption := LSList[LSID_NamesFormat];
    rgFNPFormat.Items[0] := LSList[LSID_NF1];
    rgFNPFormat.Items[1] := LSList[LSID_NF2];
    rgFNPFormat.Items[2] := LSList[LSID_NF3];
    rgDateFormat.Caption := LSList[LSID_DateFormat];
    chkPlacesWithAddress.Caption := LSList[LSID_PlacesWithAddress];
    chkHighlightUnparented.Caption := LSList[LSID_HighlightUnparented];
    chkHighlightUnmarried.Caption := LSList[LSID_HighlightUnmarried];
    btnDefList.Caption := LSList[LSID_DefList];

  GroupBox1.Caption := LSList[LSID_ViewTree];
    chkFamily.Caption := LSList[LSID_Surname];
    chkName.Caption := LSList[LSID_Name];
    chkPatronymic.Caption := LSList[LSID_Patronymic];
    chkDiffLines.Caption := LSList[LSID_DiffLines];          
    chkBirthDate.Caption := LSList[LSID_BirthDate];
    chkDeathDate.Caption := LSList[LSID_DeathDate];
    chkOnlyYears.Caption := LSList[LSID_OnlyYears];
    chkKinship.Caption := LSList[LSID_Kinship];
    chkSignsVisible.Caption := LSList[LSID_SignsVisible];
    chkTreeDecorative.Caption := LSList[LSID_TreeDecorative];
    chkPortraitsVisible.Caption := LSList[LSID_PortraitsVisible];
    chkChildlessExclude.Caption := LSList[LSID_ChildlessExclude];

  GroupBox2.Caption := LSList[LSID_Decor];
    PanMaleColor.Caption := LSList[LSID_Man];
    PanFemaleColor.Caption := LSList[LSID_Woman];
    PanUnkSexColor.Caption := LSList[LSID_UnkSex];      
    PanUnHusbandColor.Caption := LSList[LSID_UnHusband];
    PanUnWifeColor.Caption := LSList[LSID_UnWife];
    Label5.Caption := LSList[LSID_Font];

  GroupBox5.Caption := LSList[LSID_PedigreeGen];
    chkAttributes.Caption := LSList[LSID_IncludeAttributes];
    chkNotes.Caption := LSList[LSID_IncludeNotes];
    chkSources.Caption := LSList[LSID_IncludeSources];
    EditPedigreeFormat.Caption := LSList[LSID_PedigreeFormat];
    EditPedigreeFormat.Items[0] := LSList[LSID_PF1];
    EditPedigreeFormat.Items[1] := LSList[LSID_PF2];
end;

procedure TfmOptions.UpdateControls();
begin
  PanDefFont.Font.Assign(FOptions.ChartOptions.DefFont);
  PanDefFont.Caption :=
    FOptions.ChartOptions.DefFont.Name + ', ' + IntToStr(FOptions.ChartOptions.DefFont.Size);
end;

procedure TfmOptions.FormShow(Sender: TObject);
var
  i, idx: Integer;
  lng_rec: TLangRecord;
begin
  case FOptions.DefCharacterSet of
    csASCII: rgCode.ItemIndex := 0;
    csUTF8: rgCode.ItemIndex := 1;
  end;

  rgFNPFormat.ItemIndex := Ord(FOptions.DefNameFormat);
  rgDateFormat.ItemIndex := Ord(FOptions.DefDateFormat);
  chkPlacesWithAddress.Checked := FOptions.PlacesWithAddress;
  chkHighlightUnparented.Checked := FOptions.ListPersons_HighlightUnparented;
  chkHighlightUnmarried.Checked := FOptions.ListPersons_HighlightUnmarried;

  chkFamily.Checked := FOptions.ChartOptions.FamilyVisible;
  chkName.Checked := FOptions.ChartOptions.NameVisible;
  chkPatronymic.Checked := FOptions.ChartOptions.PatronymicVisible;
  chkDiffLines.Checked := FOptions.ChartOptions.DiffLines;
  chkBirthDate.Checked := FOptions.ChartOptions.BirthDateVisible;
  chkDeathDate.Checked := FOptions.ChartOptions.DeathDateVisible;
  chkOnlyYears.Checked := FOptions.ChartOptions.OnlyYears;
  chkKinship.Checked := FOptions.ChartOptions.Kinship;
  chkSignsVisible.Checked := FOptions.ChartOptions.SignsVisible;

  chkChildlessExclude.Checked := FOptions.ChartOptions.ChildlessExclude;
  chkTreeDecorative.Checked := FOptions.ChartOptions.Decorative;
  chkPortraitsVisible.Checked := FOptions.ChartOptions.PortraitsVisible;

  PanMaleColor.Color := FOptions.ChartOptions.MaleColor;
  PanFemaleColor.Color := FOptions.ChartOptions.FemaleColor;
  PanUnkSexColor.Color := FOptions.ChartOptions.UnkSexColor;
  PanUnHusbandColor.Color := FOptions.ChartOptions.UnHusbandColor;
  PanUnWifeColor.Color := FOptions.ChartOptions.UnWifeColor;

  chkProxy.Checked := FOptions.Proxy.UseProxy;
  edProxyServer.Text := FOptions.Proxy.Server;
  edProxyPort.Text := FOptions.Proxy.Port;
  edProxyLogin.Text := FOptions.Proxy.Login;
  edProxyPass.Text := FOptions.Proxy.Password;

  chkAttributes.Checked := FOptions.PedigreeOptions.IncludeAttributes;
  chkNotes.Checked := FOptions.PedigreeOptions.IncludeNotes;
  chkSources.Checked := FOptions.PedigreeOptions.IncludeSources;

  EditPedigreeFormat.ItemIndex := Ord(FOptions.PedigreeOptions.Format);

  chkShowOnStart.Checked := FOptions.ShowTips;
  rgEditMode.ItemIndex := Ord(FOptions.WorkMode);

  FPersonColumns := FOptions.ListPersonsColumns;
  UpdateColumnsList();

  UpdateControls();

  cbLanguages.Clear;
  cbLanguages.Items.AddObject(LSDefName, TObject(LSDefCode));
  idx := 0;
  for i := 0 to FOptions.LangsCount - 1 do begin
    lng_rec := FOptions.Langs[i];
    if (FOptions.InterfaceLang = lng_rec.Code) then idx := i + 1;
    cbLanguages.Items.AddObject(lng_rec.Name, TObject(lng_rec.Code));
  end;
  cbLanguages.ItemIndex := idx;
end;

procedure TfmOptions.btnAcceptClick(Sender: TObject);
var
  code: Integer;
begin
  FOptions.ListPersonsColumns := FPersonColumns;

  case rgCode.ItemIndex of
    0: FOptions.DefCharacterSet := csASCII;
    1: FOptions.DefCharacterSet := csUTF8;
  end;

  FOptions.DefNameFormat := TNameFormat(rgFNPFormat.ItemIndex);
  FOptions.DefDateFormat := TDateFormat(rgDateFormat.ItemIndex);
  FOptions.PlacesWithAddress := chkPlacesWithAddress.Checked;
  FOptions.ListPersons_HighlightUnparented := chkHighlightUnparented.Checked;
  FOptions.ListPersons_HighlightUnmarried := chkHighlightUnmarried.Checked;

  FOptions.ChartOptions.FamilyVisible := chkFamily.Checked;
  FOptions.ChartOptions.NameVisible := chkName.Checked;
  FOptions.ChartOptions.PatronymicVisible := chkPatronymic.Checked;
  FOptions.ChartOptions.DiffLines := chkDiffLines.Checked;
  FOptions.ChartOptions.BirthDateVisible := chkBirthDate.Checked;
  FOptions.ChartOptions.DeathDateVisible := chkDeathDate.Checked;
  FOptions.ChartOptions.OnlyYears := chkOnlyYears.Checked;
  FOptions.ChartOptions.Kinship := chkKinship.Checked;
  FOptions.ChartOptions.SignsVisible := chkSignsVisible.Checked;

  FOptions.ChartOptions.ChildlessExclude := chkChildlessExclude.Checked;
  FOptions.ChartOptions.Decorative := chkTreeDecorative.Checked;
  FOptions.ChartOptions.PortraitsVisible := chkPortraitsVisible.Checked;

  FOptions.ChartOptions.MaleColor := PanMaleColor.Color;
  FOptions.ChartOptions.FemaleColor := PanFemaleColor.Color;
  FOptions.ChartOptions.UnkSexColor := PanUnkSexColor.Color;
  FOptions.ChartOptions.UnHusbandColor := PanUnHusbandColor.Color;
  FOptions.ChartOptions.UnWifeColor := PanUnWifeColor.Color;

  FOptions.Proxy.UseProxy := chkProxy.Checked;
  FOptions.Proxy.Server := edProxyServer.Text;
  FOptions.Proxy.Port := edProxyPort.Text;
  FOptions.Proxy.Login := edProxyLogin.Text;
  FOptions.Proxy.Password := edProxyPass.Text;

  FOptions.PedigreeOptions.IncludeAttributes := chkAttributes.Checked;
  FOptions.PedigreeOptions.IncludeNotes := chkNotes.Checked;
  FOptions.PedigreeOptions.IncludeSources := chkSources.Checked;

  FOptions.PedigreeOptions.Format := TPedigreeFormat(EditPedigreeFormat.ItemIndex);

  FOptions.ShowTips := chkShowOnStart.Checked;
  FOptions.WorkMode := TWorkMode(rgEditMode.ItemIndex);

  code := Integer(cbLanguages.Items.Objects[cbLanguages.ItemIndex]);
  fmGEDKeeper.LoadLanguage(code);
end;

procedure TfmOptions.PanMaleColorClick(Sender: TObject);
begin
  ColorDialog1.Color := TPanel(Sender).Color;

  if ColorDialog1.Execute
  then TPanel(Sender).Color := ColorDialog1.Color;
end;

procedure TfmOptions.PanDefFontClick(Sender: TObject);
begin
  FontDialog1.Font.Assign(FOptions.ChartOptions.DefFont);
  if FontDialog1.Execute
  then FOptions.ChartOptions.DefFont.Assign(FontDialog1.Font);

  UpdateControls();
end;

procedure TfmOptions.UpdateColumnsList();
var
  i: Integer;
  pct: TPersonColumnType;
  item: TListItem;
begin
  ListPersonColumns.OnChange := nil;
  ListPersonColumns.Items.BeginUpdate;
  try
    ListPersonColumns.Items.Clear;

    for i := 0 to High(FPersonColumns) do begin
      pct := FPersonColumns[i].colType;

      item := ListPersonColumns.Items.Add();
      item.Caption := LSList[PersonColumnsName[pct].Name];
      item.Checked := FPersonColumns[i].colActive;
    end;
  finally
    ListPersonColumns.Items.EndUpdate;
  end;
  ListPersonColumns.OnChange := ListPersonColumnsChange;
end;

procedure TfmOptions.btnColumnUpClick(Sender: TObject);
var
  idx: Integer;
  props: TPersonColumnProps;
begin
  idx := ListPersonColumns.ItemIndex;
  if (idx <= 0) then Exit;

  props := FPersonColumns[idx - 1];
  FPersonColumns[idx - 1] := FPersonColumns[idx];
  FPersonColumns[idx] := props;

  UpdateColumnsList();

  ListPersonColumns.ItemIndex := idx - 1;
end;

procedure TfmOptions.btnColumnDownClick(Sender: TObject);
var
  idx: Integer;
  props: TPersonColumnProps;
begin
  idx := ListPersonColumns.ItemIndex;
  if (idx < 0) or (idx >= High(FPersonColumns)) then Exit;

  props := FPersonColumns[idx + 1];
  FPersonColumns[idx + 1] := FPersonColumns[idx];
  FPersonColumns[idx] := props;

  UpdateColumnsList();

  ListPersonColumns.ItemIndex := idx + 1;
end;

procedure TfmOptions.ListPersonColumnsChange(Sender: TObject;
  Item: TListItem; Change: TItemChange);
begin
  if (Item <> nil)
  then FPersonColumns[Item.Index].colActive := Item.Checked;
end;

procedure TfmOptions.btnDefListClick(Sender: TObject);
begin
  FPersonColumns := DefPersonColumns;
  UpdateColumnsList();
end;

end.
