unit GKMaps;

{$I GEDKeeper.inc}

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls, Buttons,
  ExtCtrls, ComCtrls, Dialogs, Contnrs, GedCom551, GKEngine, GKMapBrowser;

type
  TPlaceRef = class
  public
    DateTime: TDateTime;
    Ref: TGEDCOMCustomEvent;
  end;

  TPlace = class
  public
    Name: string;
    Points: TObjectList;
    PlaceRefs: TObjectList;

    constructor Create;
    destructor Destroy; override;
  end;

  TfmMaps = class(TForm)
    StatusBar1: TStatusBar;
    Splitter1: TSplitter;
    PageControl1: TPageControl;
    tsPlaces: TTabSheet;
    TreePlaces: TTreeView;
    SaveDialog1: TSaveDialog;
    Panel1: TPanel;
    GroupBox2: TGroupBox;
    ComboPersons: TComboBox;
    chkResidence: TCheckBox;
    chkDeath: TCheckBox;
    chkBirth: TCheckBox;
    btnSelectPlaces: TButton;
    Label2: TLabel;
    btnSaveImage: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnSelectPlacesClick(Sender: TObject);
    procedure btnSaveImageClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure TreePlacesDblClick(Sender: TObject);
  private
    FBaseRoot: TTreeNode;
    FMapBrowser: TMapBrowser;
    FMapPoints: TObjectList;
    FPlaces: TObjectList;
    FSelectedPersons: TList;
    FTree: TGEDCOMTree;

    procedure PlacesLoad();
    procedure PreparePointsList(aPoints: TObjectList);
  public
    property SelectedPersons: TList read FSelectedPersons write FSelectedPersons;
    property Tree: TGEDCOMTree read FTree write FTree;
  end;

var
  fmMaps: TfmMaps;

implementation

{$R *.dfm}

uses
  GKProgress;

{ TPlace }

constructor TPlace.Create;
begin
  Points := TObjectList.Create(True);
  PlaceRefs := TObjectList.Create(False);
end;

destructor TPlace.Destroy;
begin
  PlaceRefs.Destroy;
  Points.Destroy;

  inherited Destroy;
end;

{ TfmMaps }

procedure TfmMaps.FormCreate(Sender: TObject);
begin
  FMapBrowser := TMapBrowser.Create(Self);
  TWinControl(FMapBrowser).Parent := Panel1;
  FMapBrowser.Align := alClient;
  FMapBrowser.InitMap();

  FMapPoints := TObjectList.Create(True);
  FPlaces := TObjectList.Create(True);

  FBaseRoot := TreePlaces.Items.AddChild(nil, 'Места');
end;

procedure TfmMaps.FormDestroy(Sender: TObject);
begin
  FPlaces.Destroy;
  FMapPoints.Destroy;
end;

procedure TfmMaps.PreparePointsList(aPoints: TObjectList);
var
  i: Integer;
  pt: TGMapPoint;
begin
  FMapBrowser.BeginUpdate();
  try
    FMapBrowser.ClearPoints();
    for i := 0 to aPoints.Count - 1 do begin
      pt := TGMapPoint(aPoints[i]);
      FMapBrowser.AddPoint(pt.Latitude, pt.Longitude, pt.Hint);
    end;

    FMapBrowser.ZoomToBounds();
  finally
    FMapBrowser.EndUpdate();
  end;
end;

procedure TfmMaps.PlacesLoad();

  function FindTreeNode(aPlace: string): TTreeNode;
  var
    idx: Integer;
  begin
    Result := nil;

    for idx := 0 to TreePlaces.Items.Count - 1 do
      if (TreePlaces.Items[idx].Text = aPlace) then begin
        Result := TreePlaces.Items[idx];
        Exit;
      end;
  end;

  procedure AddPlace(aPlace: TGEDCOMPlace; aRef: TGEDCOMCustomEvent);
  var
    locRec: TGEDCOMLocationRecord;
    place_name, pt_title: string;
    node: TTreeNode;
    place: TPlace;
    pt: TGMapPoint;
    k: Integer;
    pRef: TPlaceRef;
  begin
    locRec := TGEDCOMLocationRecord(aPlace.Location.Value);
    if (locRec <> nil)
    then place_name := locRec.Name
    else place_name := aPlace.StringValue;

    node := FindTreeNode(place_name);
    if (node = nil) then begin
      node := TreePlaces.Items.AddChild(FBaseRoot, place_name);

      place := TPlace.Create;
      place.Name := place_name;
      FPlaces.Add(place);

      node.Data := place;

      // prepare place
      if (locRec = nil) then begin
        RequestGeoCoords(place_name, place.Points);

        for k := 0 to place.Points.Count - 1 do
          if (place.Points[k] is TGMapPoint) then begin
            pt := TGMapPoint(place.Points[k]);

            pt_title := pt.Hint + Format(' [%.6f, %.6f]', [pt.Latitude, pt.Longitude]);
            TreePlaces.Items.AddChildObjectFirst(node, pt_title, pt);
          end;
      end else begin
        pt := TGMapPoint.Create;
        pt.Hint := place_name;
        pt.Longitude := StrToFloatDef(locRec.Map.Long, -1);
        pt.Latitude := StrToFloatDef(locRec.Map.Lati, -1);
        place.Points.Add(pt);

        pt_title := pt.Hint + Format(' [%.6f, %.6f]', [pt.Latitude, pt.Longitude]);
        TreePlaces.Items.AddChildObjectFirst(node, pt_title, pt);
      end;
    end else begin
      place := TPlace(node.Data);
    end;

    pRef := TPlaceRef.Create;
    pRef.DateTime := GEDCOMDateToDate(aRef.Detail.Date.Value);
    pRef.Ref := aRef;
    place.PlaceRefs.Add(pRef);
  end;

var
  i, k, p_cnt: Integer;
  rec: TGEDCOMRecord;
  ind: TGEDCOMIndividualRecord;
  ev: TGEDCOMCustomEvent;
  res: Boolean;
begin
  ComboPersons.Items.BeginUpdate;
  TreePlaces.Items.BeginUpdate;
  ProgressInit(FTree.RecordsCount, 'Загрузка и поиск мест');
  try
    FPlaces.Clear;

    ComboPersons.Clear;
    ComboPersons.Sorted := False;
    ComboPersons.Items.AddObject('( не выбран )', nil);

    for i := 0 to FTree.RecordsCount - 1 do begin
      rec := FTree.Records[i];

      res := (rec is TGEDCOMIndividualRecord)
         and ((FSelectedPersons = nil)
           or ((FSelectedPersons <> nil) and (FSelectedPersons.IndexOf(rec) >= 0)));

      if (res) then begin
        ind := rec as TGEDCOMIndividualRecord;
        p_cnt := 0;

        for k := 0 to ind.IndividualEventsCount - 1 do begin
          ev := ind.IndividualEvents[k];
          if (ev.Detail.Place.StringValue <> '') then begin
            AddPlace(ev.Detail.Place, ev);
            Inc(p_cnt);
          end;
        end;

        if (p_cnt > 0)
        then ComboPersons.Items.AddObject(GetNameStr(ind) + ' [' + IntToStr(p_cnt) + ']', ind);
      end;

      ProgressStep();
    end;

    TreePlaces.AlphaSort(True);
    TreePlaces.Items[0].Expand(True);

    ComboPersons.Sorted := True;
  finally
    ProgressDone();
    TreePlaces.Items.EndUpdate;
    ComboPersons.Items.EndUpdate;
  end;
end;

procedure TfmMaps.btnSelectPlacesClick(Sender: TObject);

  procedure CopyPoint(aPt: TGMapPoint);
  var
    pt: TGMapPoint;
    i: Integer;
  begin
    for i := 0 to FMapPoints.Count - 1 do begin
      pt := TGMapPoint(FMapPoints[i]);
      if (pt.Hint = aPt.Hint)
      then Exit;
    end;

    pt := TGMapPoint.Create;
    pt.Latitude := aPt.Latitude;
    pt.Longitude := aPt.Longitude;
    pt.Hint := aPt.Hint;
    FMapPoints.Add(pt);
  end;

var
  i, k: Integer;
  place: TPlace;
  ref: TGEDCOMCustomEvent;
  cond: set of (pcBirth, pcDeath, pcResidence);
  ind: TGEDCOMIndividualRecord;
begin
  cond := [];
  if chkBirth.Checked then Include(cond, pcBirth);
  if chkDeath.Checked then Include(cond, pcDeath);
  if chkResidence.Checked then Include(cond, pcResidence);

  ind := nil;
  if (ComboPersons.ItemIndex >= 0)
  then ind := TGEDCOMIndividualRecord(ComboPersons.Items.Objects[ComboPersons.ItemIndex]);

  FMapBrowser.ShowLines := (ind <> nil);

  FMapPoints.Clear;
  for i := 0 to FPlaces.Count - 1 do begin
    place := TPlace(FPlaces[i]);
    if (place.Points.Count < 1) then Continue;

    for k := 0 to place.PlaceRefs.Count - 1 do begin
      ref := TPlaceRef(place.PlaceRefs[k]).Ref;

      //if (ref is TGEDCOMCustomEvent) then begin
        if ((ind <> nil) and (ref.Parent = ind))
        or ((pcBirth in cond) and (ref.Name = 'BIRT'))
        or ((pcDeath in cond) and (ref.Name = 'DEAT'))
        or ((pcResidence in cond) and (ref.Name = 'RESI'))
        then CopyPoint(TGMapPoint(place.Points[0]));
      //end;
    end;
  end;

  PreparePointsList(FMapPoints);
end;

// обработка нажатия кнопки "Обновить"
procedure TfmMaps.btnSaveImageClick(Sender: TObject);
begin
  if (SaveDialog1.Execute)
  then FMapBrowser.SaveSnapshot(SaveDialog1.FileName);
end;

procedure TfmMaps.FormShow(Sender: TObject);
begin
  PlacesLoad();
  btnSelectPlaces.Enabled := True;
end;

procedure TfmMaps.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) then Close;
end;

procedure TfmMaps.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TfmMaps.TreePlacesDblClick(Sender: TObject);
var
  node: TTreeNode;
  pt: TGMapPoint;
begin
  node := TreePlaces.Selected;
  if (node = nil) then Exit;

  pt := TGMapPoint(node.Data);
  if (pt <> nil)
  then FMapBrowser.SetCenter(pt.Latitude, pt.Longitude);
end;

end.
