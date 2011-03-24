unit GKLocationEdit; {prepare:fin}

{$I GEDKeeper.inc}

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, Buttons,
  ComCtrls, Contnrs, ExtCtrls, GedCom551, GKBase, GKEngine, GKCtrls, GKLists,
  GKMapBrowser;

type
  TfmLocationEdit = class(TForm)
    btnAccept: TBitBtn;
    btnCancel: TBitBtn;
    PagesData: TPageControl;
    SheetNotes: TTabSheet;
    SheetMultimedia: TTabSheet;
    SheetCommon: TTabSheet;
    Label1: TLabel;
    EditName: TEdit;
    Label2: TLabel;
    EditLatitude: TEdit;
    Label3: TLabel;
    EditLongitude: TEdit;
    GroupBox1: TGroupBox;
    ListGeoCoords: TListView;
    btnSearch: TBitBtn;
    btnSelect: TBitBtn;
    btnSelectName: TBitBtn;
    btnShowOnMap: TButton;
    panMap: TPanel;
    procedure btnAcceptClick(Sender: TObject);
    procedure EditNameChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnSearchClick(Sender: TObject);
    procedure btnSelectClick(Sender: TObject);
    procedure btnSelectNameClick(Sender: TObject);
    procedure EditNameKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ListGeoCoordsClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnShowOnMapClick(Sender: TObject);
  private
    FLocationRecord: TGEDCOMLocationRecord;
    FMapBrowser: TMapBrowser;
    FMediaList: TSheetList;
    FNotesList: TSheetList;
    FSearchPoints: TObjectList;

    procedure ControlsRefresh();
    function GetBase: TfmBase;
    procedure ListModify(Sender: TObject; ItemData: TObject; Action: TRecAction);
    procedure SetLocationRecord(const Value: TGEDCOMLocationRecord);
  public
    property Base: TfmBase read GetBase;
    property LocationRecord: TGEDCOMLocationRecord read FLocationRecord write SetLocationRecord;
  end;

implementation

{$R *.dfm}

procedure TfmLocationEdit.FormCreate(Sender: TObject);
begin
  FMapBrowser := TMapBrowser.Create(Self);
  TWinControl(FMapBrowser).Parent := panMap;
  FMapBrowser.Align := alClient;
  FMapBrowser.InitMap();

  FNotesList := TSheetList.Create(SheetNotes, lmBox);
  FNotesList.OnModify := ListModify;
  Base.SetupRecNotesList(FNotesList);

  FMediaList := TSheetList.Create(SheetMultimedia);
  FMediaList.OnModify := ListModify;
  Base.SetupRecMediaList(FMediaList);

  FSearchPoints := TObjectList.Create(True);
end;

procedure TfmLocationEdit.FormDestroy(Sender: TObject);
begin
  FSearchPoints.Free;
end;

procedure TfmLocationEdit.ControlsRefresh();
begin
  Base.RecListNotesRefresh(FLocationRecord, FNotesList.List, nil);
  Base.RecListMediaRefresh(FLocationRecord, TGKListView(FMediaList.List), nil);
end;

procedure TfmLocationEdit.SetLocationRecord(const Value: TGEDCOMLocationRecord);
begin
  FLocationRecord := Value;

  EditName.Text := FLocationRecord.Name;
  EditLatitude.Text := FLocationRecord.Map.Lati;
  EditLongitude.Text := FLocationRecord.Map.Long;

  ControlsRefresh();
end;

procedure TfmLocationEdit.btnAcceptClick(Sender: TObject);
begin
  FLocationRecord.Name := EditName.Text;
  FLocationRecord.Map.Lati := EditLatitude.Text;
  FLocationRecord.Map.Long := EditLongitude.Text;

  Base.ChangeRecord(FLocationRecord);
end;

procedure TfmLocationEdit.ListModify(Sender: TObject; ItemData: TObject; Action: TRecAction);
begin
  if (Sender = FNotesList) then begin
    if Base.ModifyRecNote(Self, FLocationRecord, TGEDCOMNotes(ItemData), Action)
    then ControlsRefresh();
  end
  else
  if (Sender = FMediaList) then begin
    if Base.ModifyRecMultimedia(Self, FLocationRecord, TGEDCOMMultimediaLink(ItemData), Action)
    then ControlsRefresh();
  end;
end;

function TfmLocationEdit.GetBase: TfmBase;
begin
  Result := TfmBase(Owner);
end;

procedure TfmLocationEdit.EditNameChange(Sender: TObject);
begin
  Caption := 'Местоположение "'+EditName.Text+'"';
end;

procedure TfmLocationEdit.EditNameKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_DOWN) and (ssCtrl in Shift)
  then EditName.Text := AnsiLowerCase(EditName.Text);
end;

procedure TfmLocationEdit.btnSearchClick(Sender: TObject);
var
  k: Integer;
  pt: TGMapPoint;
  item: TListItem;
begin
  ListGeoCoords.Items.BeginUpdate;
  FMapBrowser.BeginUpdate();
  try
    FSearchPoints.Clear;
    RequestGeoCoords(EditName.Text, FSearchPoints);

    ListGeoCoords.Clear;
    FMapBrowser.ClearPoints();

    for k := 0 to FSearchPoints.Count - 1 do
      if (FSearchPoints[k] is TGMapPoint) then begin
        pt := TGMapPoint(FSearchPoints[k]);

        item := ListGeoCoords.Items.Add();
        item.Caption := pt.Hint;
        item.Data := pt;
        item.SubItems.Add(Format('%.6f', [pt.Latitude]));
        item.SubItems.Add(Format('%.6f', [pt.Longitude]));

        FMapBrowser.AddPoint(pt.Latitude, pt.Longitude, pt.Hint);
      end;

    FMapBrowser.ZoomToBounds();
  finally
    FMapBrowser.EndUpdate();
    ListGeoCoords.Items.EndUpdate();
  end;
end;

procedure TfmLocationEdit.ListGeoCoordsClick(Sender: TObject);
var
  item: TListItem;
  pt: TGMapPoint;
begin
  item := ListGeoCoords.Selected;
  if (item = nil) then Exit;

  pt := TGMapPoint(item.Data);
  if (pt <> nil)
  then FMapBrowser.SetCenter(pt.Latitude, pt.Longitude);
end;

procedure TfmLocationEdit.btnShowOnMapClick(Sender: TObject);
begin
  if (EditLatitude.Text <> '') and (EditLongitude.Text <> '')
  then FMapBrowser.SetCenter(StrToFloat(EditLatitude.Text), StrToFloat(EditLongitude.Text));
end;

procedure TfmLocationEdit.btnSelectClick(Sender: TObject);
var
  item: TListItem;
begin
  item := ListGeoCoords.Selected;
  if (item <> nil) then begin
    EditLatitude.Text := item.SubItems[0];
    EditLongitude.Text := item.SubItems[1];
  end;
end;

procedure TfmLocationEdit.btnSelectNameClick(Sender: TObject);
var
  item: TListItem;
begin
  item := ListGeoCoords.Selected;
  if (item <> nil) then begin
    EditName.Text := item.Caption;
  end;
end;

end.
