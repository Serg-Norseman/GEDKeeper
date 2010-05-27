unit GKLocationEdit;

{$I GEDKeeper.inc}

interface

uses
  SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, Buttons, ComCtrls,
  GedCom551, GKBase, GKCommon, GKSheetList, bsCtrls, Contnrs;

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
    procedure btnAcceptClick(Sender: TObject);
    procedure EditNameChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnSearchClick(Sender: TObject);
    procedure btnSelectClick(Sender: TObject);
    procedure btnSelectNameClick(Sender: TObject);
  private
    FLocationRecord: TGEDCOMLocationRecord;

    FNotesList: TSheetList;
    FMediaList: TSheetList;

    procedure ControlsRefresh();
    function GetBase: TfmBase;
    procedure ListModify(Sender: TObject; ItemData: TObject; Action: TRecAction);
    procedure SetLocationRecord(const Value: TGEDCOMLocationRecord);
  public
    property Base: TfmBase read GetBase;
    property LocationRecord: TGEDCOMLocationRecord read FLocationRecord write SetLocationRecord;
  end;

implementation

uses
  {$IFNDEF DELPHI_NET}GKMaps, {$ENDIF}
  GKMain, GKRecordSelect;

{$R *.dfm}

procedure TfmLocationEdit.FormCreate(Sender: TObject);
begin
  FNotesList := TSheetList.Create(SheetNotes, lmBox);
  FNotesList.OnModify := ListModify;
  Base.SetupRecNotesList(FNotesList);

  FMediaList := TSheetList.Create(SheetMultimedia);
  FMediaList.OnModify := ListModify;
  Base.SetupRecMediaList(FMediaList);
end;

procedure TfmLocationEdit.ControlsRefresh();
begin
  Base.RecListNotesRefresh(FLocationRecord, FNotesList.List, nil);
  Base.RecListMediaRefresh(FLocationRecord, TBSListView(FMediaList.List), nil);
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
    if Base.ModifyRecNote(FLocationRecord, TGEDCOMNotes(ItemData), Action)
    then ControlsRefresh();
  end
  else
  if (Sender = FMediaList) then begin
    if Base.ModifyRecMultimedia(FLocationRecord, TGEDCOMMultimediaLink(ItemData), Action)
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

procedure TfmLocationEdit.btnSearchClick(Sender: TObject);
var
  points: TObjectList;
  k: Integer;
  pt: TMapPoint;
  item: TListItem;
begin
  ListGeoCoords.Clear;

  points := TObjectList.Create(True);
  try
    RequestGeoCoords(EditName.Text, points);

    for k := 0 to points.Count - 1 do
      if (points[k] is TMapPoint) then begin
        pt := TMapPoint(points[k]);

        item := ListGeoCoords.Items.Add();
        item.Caption := pt.Address;
        item.SubItems.Add(Format('%.6f', [pt.Lat]));
        item.SubItems.Add(Format('%.6f', [pt.Lon]));
      end;
  finally
    points.Destroy;
  end;
end;

procedure TfmLocationEdit.btnSelectClick(Sender: TObject);
var
  item: TListItem;
begin
  item := ListGeoCoords.Selected;
  if (item = nil) then Exit;

  EditLatitude.Text := item.SubItems[0];
  EditLongitude.Text := item.SubItems[1];
end;

procedure TfmLocationEdit.btnSelectNameClick(Sender: TObject);
var
  item: TListItem;
begin
  item := ListGeoCoords.Selected;
  if (item = nil) then Exit;

  EditName.Text := item.Caption;
end;

end.
