unit GKMaps; {trans:fin}

{$I GEDKeeper2.inc}

interface

uses
  System.Drawing, System.ComponentModel, System.Windows.Forms,
  VCLStub, GedCom551, GKEngine, GKCtrls, GKMapBrowser, GKProgress, GKLangs;

type
  TfmMaps = class(System.Windows.Forms.Form)
  private
    type
      TPlaceRef = class
      public
        DateTime: DateTime;
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

  strict private
    StatusBar1: System.Windows.Forms.StatusBar;
    PageControl1: System.Windows.Forms.TabControl;
    tsPlaces: System.Windows.Forms.TabPage;
    TreePlaces: System.Windows.Forms.TreeView;
    SaveDialog1: System.Windows.Forms.SaveFileDialog;
    Panel1: System.Windows.Forms.Panel;
    GroupBox2: System.Windows.Forms.GroupBox;
    ComboPersons: System.Windows.Forms.ComboBox;
    chkResidence: System.Windows.Forms.CheckBox;
    chkDeath: System.Windows.Forms.CheckBox;
    chkBirth: System.Windows.Forms.CheckBox;
    btnSelectPlaces: System.Windows.Forms.Button;
    btnSaveImage: System.Windows.Forms.Button;
    radTotal: System.Windows.Forms.RadioButton;
    radSelected: System.Windows.Forms.RadioButton;
    chkLinesVisible: System.Windows.Forms.CheckBox;

    FBaseRoot: TreeNode;
    FMapBrowser: TMapBrowser;
    FMapPoints: TObjectList;
    FPlaces: TObjectList;
    FSelectedPersons: TList;
    FTree: TGEDCOMTree;

    procedure PlacesLoad();
    procedure PreparePointsList(aPoints: TObjectList; ByPerson: Boolean = False);
    procedure InitializeComponent;
    procedure radTotal_Click(sender: System.Object; e: System.EventArgs);
    procedure TfmMaps_KeyDown(sender: System.Object; e: System.Windows.Forms.KeyEventArgs);
    procedure btnSaveImage_Click(sender: System.Object; e: System.EventArgs);
    procedure btnSelectPlaces_Click(sender: System.Object; e: System.EventArgs);
    procedure TreePlaces_DoubleClick(sender: System.Object; e: System.EventArgs);
    procedure TfmMaps_Load(sender: System.Object; e: System.EventArgs);
  strict protected
    procedure Dispose(Disposing: Boolean); override;
  public
    constructor Create(aTree: TGEDCOMTree; aSelectedPersons: TList);

    procedure SetLang();
  end;

implementation

{ TPlace }

constructor TfmMaps.TPlace.Create;
begin
  inherited Create;

  Points := TObjectList.Create(True);
  PlaceRefs := TObjectList.Create(False);
end;

destructor TfmMaps.TPlace.Destroy;
begin
  PlaceRefs.Free;
  Points.Free;

  inherited Destroy;
end;

{ TfmMaps }

procedure TfmMaps.InitializeComponent;
begin
  Self.StatusBar1 := System.Windows.Forms.StatusBar.Create;
  Self.PageControl1 := System.Windows.Forms.TabControl.Create;
  Self.tsPlaces := System.Windows.Forms.TabPage.Create;
  Self.TreePlaces := System.Windows.Forms.TreeView.Create;
  Self.GroupBox2 := System.Windows.Forms.GroupBox.Create;
  Self.ComboPersons := System.Windows.Forms.ComboBox.Create;
  Self.chkResidence := System.Windows.Forms.CheckBox.Create;
  Self.chkDeath := System.Windows.Forms.CheckBox.Create;
  Self.chkBirth := System.Windows.Forms.CheckBox.Create;
  Self.btnSelectPlaces := System.Windows.Forms.Button.Create;
  Self.btnSaveImage := System.Windows.Forms.Button.Create;
  Self.radTotal := System.Windows.Forms.RadioButton.Create;
  Self.radSelected := System.Windows.Forms.RadioButton.Create;
  Self.chkLinesVisible := System.Windows.Forms.CheckBox.Create;
  Self.SaveDialog1 := System.Windows.Forms.SaveFileDialog.Create;
  Self.Panel1 := System.Windows.Forms.Panel.Create;
  Self.PageControl1.SuspendLayout;
  Self.tsPlaces.SuspendLayout;
  Self.GroupBox2.SuspendLayout;
  Self.SuspendLayout;
  // 
  // StatusBar1
  // 
  Self.StatusBar1.Location := System.Drawing.Point.Create(0, 485);
  Self.StatusBar1.Name := 'StatusBar1';
  Self.StatusBar1.Size := System.Drawing.Size.Create(829, 19);
  Self.StatusBar1.TabIndex := 3;
  // 
  // PageControl1
  // 
  Self.PageControl1.Controls.Add(Self.tsPlaces);
  Self.PageControl1.Dock := System.Windows.Forms.DockStyle.Left;
  Self.PageControl1.Location := System.Drawing.Point.Create(0, 0);
  Self.PageControl1.Name := 'PageControl1';
  Self.PageControl1.SelectedIndex := 0;
  Self.PageControl1.Size := System.Drawing.Size.Create(289, 485);
  Self.PageControl1.TabIndex := 1;
  // 
  // tsPlaces
  // 
  Self.tsPlaces.Controls.Add(Self.TreePlaces);
  Self.tsPlaces.Controls.Add(Self.GroupBox2);
  Self.tsPlaces.Location := System.Drawing.Point.Create(4, 22);
  Self.tsPlaces.Name := 'tsPlaces';
  Self.tsPlaces.Size := System.Drawing.Size.Create(281, 459);
  Self.tsPlaces.TabIndex := 0;
  Self.tsPlaces.Text := 'Места';
  // 
  // TreePlaces
  // 
  Self.TreePlaces.Dock := System.Windows.Forms.DockStyle.Fill;
  Self.TreePlaces.ImageIndex := -1;
  Self.TreePlaces.Location := System.Drawing.Point.Create(0, 185);
  Self.TreePlaces.Name := 'TreePlaces';
  Self.TreePlaces.SelectedImageIndex := -1;
  Self.TreePlaces.Size := System.Drawing.Size.Create(281, 274);
  Self.TreePlaces.TabIndex := 0;
  Include(Self.TreePlaces.DoubleClick, Self.TreePlaces_DoubleClick);
  // 
  // GroupBox2
  // 
  Self.GroupBox2.Controls.Add(Self.ComboPersons);
  Self.GroupBox2.Controls.Add(Self.chkResidence);
  Self.GroupBox2.Controls.Add(Self.chkDeath);
  Self.GroupBox2.Controls.Add(Self.chkBirth);
  Self.GroupBox2.Controls.Add(Self.btnSelectPlaces);
  Self.GroupBox2.Controls.Add(Self.btnSaveImage);
  Self.GroupBox2.Controls.Add(Self.radTotal);
  Self.GroupBox2.Controls.Add(Self.radSelected);
  Self.GroupBox2.Controls.Add(Self.chkLinesVisible);
  Self.GroupBox2.Dock := System.Windows.Forms.DockStyle.Top;
  Self.GroupBox2.Location := System.Drawing.Point.Create(0, 0);
  Self.GroupBox2.Name := 'GroupBox2';
  Self.GroupBox2.Size := System.Drawing.Size.Create(281, 185);
  Self.GroupBox2.TabIndex := 1;
  Self.GroupBox2.TabStop := False;
  Self.GroupBox2.Text := 'Выборка';
  // 
  // ComboPersons
  // 
  Self.ComboPersons.DropDownStyle := System.Windows.Forms.ComboBoxStyle.DropDownList;
  Self.ComboPersons.Location := System.Drawing.Point.Create(8, 104);
  Self.ComboPersons.Name := 'ComboPersons';
  Self.ComboPersons.Size := System.Drawing.Size.Create(265, 21);
  Self.ComboPersons.TabIndex := 5;
  // 
  // chkResidence
  // 
  Self.chkResidence.Location := System.Drawing.Point.Create(19, 64);
  Self.chkResidence.Name := 'chkResidence';
  Self.chkResidence.Size := System.Drawing.Size.Create(129, 17);
  Self.chkResidence.TabIndex := 3;
  Self.chkResidence.Text := 'Места проживания';
  // 
  // chkDeath
  // 
  Self.chkDeath.Location := System.Drawing.Point.Create(19, 48);
  Self.chkDeath.Name := 'chkDeath';
  Self.chkDeath.Size := System.Drawing.Size.Create(129, 17);
  Self.chkDeath.TabIndex := 2;
  Self.chkDeath.Text := 'Места смерти';
  // 
  // chkBirth
  // 
  Self.chkBirth.Location := System.Drawing.Point.Create(19, 32);
  Self.chkBirth.Name := 'chkBirth';
  Self.chkBirth.Size := System.Drawing.Size.Create(129, 17);
  Self.chkBirth.TabIndex := 1;
  Self.chkBirth.Text := 'Места рождения';
  // 
  // btnSelectPlaces
  // 
  Self.btnSelectPlaces.Enabled := False;
  Self.btnSelectPlaces.Location := System.Drawing.Point.Create(198, 152);
  Self.btnSelectPlaces.Name := 'btnSelectPlaces';
  Self.btnSelectPlaces.Size := System.Drawing.Size.Create(75, 25);
  Self.btnSelectPlaces.TabIndex := 6;
  Self.btnSelectPlaces.Text := 'Показать';
  Include(Self.btnSelectPlaces.Click, Self.btnSelectPlaces_Click);
  // 
  // btnSaveImage
  // 
  Self.btnSaveImage.Location := System.Drawing.Point.Create(8, 152);
  Self.btnSaveImage.Name := 'btnSaveImage';
  Self.btnSaveImage.Size := System.Drawing.Size.Create(121, 25);
  Self.btnSaveImage.TabIndex := 7;
  Self.btnSaveImage.Text := 'Сохранить снимок...';
  Include(Self.btnSaveImage.Click, Self.btnSaveImage_Click);
  // 
  // radTotal
  // 
  Self.radTotal.Location := System.Drawing.Point.Create(8, 16);
  Self.radTotal.Name := 'radTotal';
  Self.radTotal.Size := System.Drawing.Size.Create(198, 17);
  Self.radTotal.TabIndex := 0;
  Self.radTotal.Text := 'По всем людям';
  Include(Self.radTotal.Click, Self.radTotal_Click);
  // 
  // radSelected
  // 
  Self.radSelected.Location := System.Drawing.Point.Create(8, 87);
  Self.radSelected.Name := 'radSelected';
  Self.radSelected.Size := System.Drawing.Size.Create(198, 17);
  Self.radSelected.TabIndex := 4;
  Self.radSelected.Text := 'Только по выбранному';
  Include(Self.radSelected.Click, Self.radTotal_Click);
  // 
  // chkLinesVisible
  // 
  Self.chkLinesVisible.Checked := True;
  Self.chkLinesVisible.CheckState := System.Windows.Forms.CheckState.Checked;
  Self.chkLinesVisible.Location := System.Drawing.Point.Create(8, 128);
  Self.chkLinesVisible.Name := 'chkLinesVisible';
  Self.chkLinesVisible.Size := System.Drawing.Size.Create(265, 17);
  Self.chkLinesVisible.TabIndex := 8;
  Self.chkLinesVisible.Text := 'Отображать линии';
  // 
  // SaveDialog1
  // 
  Self.SaveDialog1.DefaultExt := 'jpg';
  Self.SaveDialog1.Filter := 'Image files|*.jpg';
  // 
  // Panel1
  // 
  Self.Panel1.Dock := System.Windows.Forms.DockStyle.Fill;
  Self.Panel1.Location := System.Drawing.Point.Create(289, 0);
  Self.Panel1.Name := 'Panel1';
  Self.Panel1.Size := System.Drawing.Size.Create(540, 485);
  Self.Panel1.TabIndex := 4;
  // 
  // TfmMaps
  // 
  Self.AutoScaleBaseSize := System.Drawing.Size.Create(5, 14);
  Self.ClientSize := System.Drawing.Size.Create(829, 504);
  Self.Controls.Add(Self.Panel1);
  Self.Controls.Add(Self.PageControl1);
  Self.Controls.Add(Self.StatusBar1);
  Self.Font := System.Drawing.Font.Create('Tahoma', 8.25, System.Drawing.FontStyle.Regular, 
      System.Drawing.GraphicsUnit.Point, (Byte(204)));
  Self.FormBorderStyle := System.Windows.Forms.FormBorderStyle.FixedDialog;
  Self.KeyPreview := True;
  Self.Name := 'TfmMaps';
  Self.StartPosition := System.Windows.Forms.FormStartPosition.CenterScreen;
  Self.Text := 'Карты';
  Include(Self.KeyDown, Self.TfmMaps_KeyDown);
  Include(Self.Load, Self.TfmMaps_Load);
  Self.PageControl1.ResumeLayout(False);
  Self.tsPlaces.ResumeLayout(False);
  Self.GroupBox2.ResumeLayout(False);
  Self.ResumeLayout(False);
end;

constructor TfmMaps.Create(aTree: TGEDCOMTree; aSelectedPersons: TList);
begin
  inherited Create;
  InitializeComponent;

  FTree := aTree;
  FSelectedPersons := aSelectedPersons;

  FMapBrowser := TMapBrowser.Create();
  FMapBrowser.Dock := DockStyle.Fill;
  FMapBrowser.InitMap();
  Panel1.Controls.Add(FMapBrowser);

  FMapPoints := TObjectList.Create(True);
  FPlaces := TObjectList.Create(True);

  FBaseRoot := TreePlaces.Nodes.Add(LSList[LSID_RPLocations]);

  radTotal.Checked := True;

  SetLang();
end;

procedure TfmMaps.Dispose(Disposing: Boolean);
begin
  if Disposing then
  begin
    FPlaces.Free;
    FMapPoints.Free;
  end;
  inherited Dispose(Disposing);
end;

procedure TfmMaps.SetLang();
begin
  Text := LSList[LSID_MIMap];
  tsPlaces.Text := LSList[LSID_RPLocations];
  GroupBox2.Text := LSList[LSID_MapSelection];
  radTotal.Text := LSList[LSID_MapSelOnAll];
  chkBirth.Text := LSList[LSID_MSBirthPlaces];
  chkDeath.Text := LSList[LSID_MSDeathPlaces];
  chkResidence.Text := LSList[LSID_MSResiPlace];
  radSelected.Text := LSList[LSID_MapSelOnSelected];
  btnSaveImage.Text := LSList[LSID_SaveImage];
  btnSelectPlaces.Text := LSList[LSID_Show];
end;

procedure TfmMaps.PreparePointsList(aPoints: TObjectList; ByPerson: Boolean = False);
var
  i: Integer;
  pt: TMapBrowser.TGMapPoint;
  stHint: string;
begin
  FMapBrowser.BeginUpdate();
  try
    FMapBrowser.ClearPoints();
    for i := 0 to aPoints.Count - 1 do begin
      pt := TMapBrowser.TGMapPoint(aPoints[i]);

      stHint := pt.Hint;
      if ByPerson then stHint := stHint + ' [' + pt.Date.ToString() + ']';

      FMapBrowser.AddPoint(pt.Latitude, pt.Longitude, stHint);
    end;

    FMapBrowser.ZoomToBounds();
  finally
    FMapBrowser.EndUpdate();
  end;
end;

procedure TfmMaps.radTotal_Click(sender: System.Object; e: System.EventArgs);
begin
  chkBirth.Enabled := radTotal.Checked;
  chkDeath.Enabled := radTotal.Checked;
  chkResidence.Enabled := radTotal.Checked;
  ComboPersons.Enabled := radSelected.Checked;
  chkLinesVisible.Enabled := radSelected.Checked;
end;

procedure TfmMaps.PlacesLoad();

  function FindTreeNode(aPlace: string): TreeNode;
  var
    idx: Integer;
  begin
    Result := nil;

    for idx := 0 to TreePlaces.Nodes.Count - 1 do
      if (TreePlaces.Nodes[idx].Text = aPlace) then begin
        Result := TreePlaces.Nodes[idx];
        Exit;
      end;
  end;

  procedure AddPlace(aPlace: TGEDCOMPlace; aRef: TGEDCOMCustomEvent);
  var
    locRec: TGEDCOMLocationRecord;
    place_name, pt_title: string;
    node: TreeNode;
    place: TPlace;
    pt: TMapBrowser.TGMapPoint;
    k: Integer;
    pRef: TPlaceRef;
  begin
    locRec := TGEDCOMLocationRecord(aPlace.Location.Value);
    if (locRec <> nil)
    then place_name := locRec.Name
    else place_name := aPlace.StringValue;

    node := FindTreeNode(place_name);
    if (node = nil) then begin
      place := TPlace.Create;
      place.Name := place_name;
      FPlaces.Add(place);

      node := TGKTreeNode.Create(place_name, place);
      FBaseRoot.Nodes.Add(node);

      // prepare place
      if (locRec = nil) then begin
        TMapBrowser.RequestGeoCoords(place_name, place.Points);

        for k := 0 to place.Points.Count - 1 do
          if (place.Points[k] is TMapBrowser.TGMapPoint) then begin
            pt := TMapBrowser.TGMapPoint(place.Points[k]);

            pt_title := pt.Hint + System.String.Format(' [{0:0.000000}, {1:0.000000}]', [pt.Latitude, pt.Longitude]);
            node.Nodes.Add(TGKTreeNode.Create(pt_title, pt));
          end;
      end else begin
        pt := TMapBrowser.TGMapPoint.Create;
        pt.Hint := place_name;
        pt.Longitude := System.Double.Parse(locRec.Map.Long{, -1});
        pt.Latitude := System.Double.Parse(locRec.Map.Lati{, -1});
        place.Points.Add(pt);

        pt_title := pt.Hint + System.String.Format(' [{0:0.000000}, {1:0.000000}]', [pt.Latitude, pt.Longitude]);
        node.Nodes.Add(TGKTreeNode.Create(pt_title, pt));
      end;
    end else begin
      place := TPlace(TGKTreeNode(node).Data);
    end;

    pRef := TPlaceRef.Create;
    pRef.DateTime := TGenEngine.GEDCOMDateToDate(aRef.Detail.Date.Value);
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
  ComboPersons.BeginUpdate;
  TreePlaces.BeginUpdate;
  TfmProgress.ProgressInit(FTree.RecordsCount, LSList[LSID_LoadingLocations]);
  try
    FPlaces.Clear;

    ComboPersons.Items.Clear;
    ComboPersons.Sorted := False;
    ComboPersons.Items.Add(TComboItem.Create(LSList[LSID_NotSelected], nil));

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
        then ComboPersons.Items.Add(TComboItem.Create(TGenEngine.GetNameStr(ind) + ' [' + p_cnt.ToString() + ']', ind));
      end;

      TfmProgress.ProgressStep();
    end;

    FBaseRoot.ExpandAll;

    ComboPersons.Sorted := True;
  finally
    TfmProgress.ProgressDone();
    TreePlaces.EndUpdate;
    ComboPersons.EndUpdate;
  end;
end;

procedure TfmMaps.btnSelectPlaces_Click(sender: System.Object; e: System.EventArgs);

  procedure CopyPoint(aPt: TMapBrowser.TGMapPoint; aRef: TPlaceRef);
  var
    pt: TMapBrowser.TGMapPoint;
    i: Integer;
  begin
    for i := 0 to FMapPoints.Count - 1 do begin
      pt := TMapBrowser.TGMapPoint(FMapPoints[i]);
      if (pt.Hint = aPt.Hint)
      then Exit;
    end;

    pt := TMapBrowser.TGMapPoint.Create;
    pt.Latitude := aPt.Latitude;
    pt.Longitude := aPt.Longitude;
    pt.Hint := aPt.Hint;
    pt.Date := aRef.DateTime;
    FMapPoints.Add(pt);
  end;

  procedure SortPointsByDate();
  var
    i, k: Integer;
    pt1, pt2: TMapBrowser.TGMapPoint;
  begin
    for i := 0 to FMapPoints.Count - 1 do begin
      for k := i + 1 to FMapPoints.Count - 1 do begin
        pt1 := TMapBrowser.TGMapPoint(FMapPoints[i]);
        pt2 := TMapBrowser.TGMapPoint(FMapPoints[k]);

        if (pt1.Date > pt2.Date)
        then FMapPoints.Exchange(i, k);
      end;
    end;
  end;

var
  i, k: Integer;
  place: TPlace;
  ref: TGEDCOMCustomEvent;
  condBirth, condDeath, condResidence: Boolean;
  ind: TGEDCOMIndividualRecord;
begin
  ind := nil;

  if (radTotal.Checked) then begin
    condBirth := chkBirth.Checked;
    condDeath := chkDeath.Checked;
    condResidence := chkResidence.Checked;
  end else if (radSelected.Checked) then begin
    condBirth := False;
    condDeath := False;
    condResidence := False;

    if (ComboPersons.SelectedIndex >= 0)
    then ind := TGEDCOMIndividualRecord(TComboItem(ComboPersons.Items[ComboPersons.SelectedIndex]).Data);
  end;

  FMapBrowser.ShowLines := (ind <> nil) and (chkLinesVisible.Checked);

  FMapPoints.Clear;
  for i := 0 to FPlaces.Count - 1 do begin
    place := TPlace(FPlaces[i]);
    if (place.Points.Count < 1) then Continue;

    for k := 0 to place.PlaceRefs.Count - 1 do begin
      ref := TPlaceRef(place.PlaceRefs[k]).Ref;

      //if (ref is TGEDCOMCustomEvent) then begin
        if ((ind <> nil) and (ref.Parent = ind))
        or ((condBirth) and (ref.Name = 'BIRT'))
        or ((condDeath) and (ref.Name = 'DEAT'))
        or ((condResidence) and (ref.Name = 'RESI'))
        then CopyPoint(TMapBrowser.TGMapPoint(place.Points[0]), TPlaceRef(place.PlaceRefs[k]));
      //end;
    end;
  end;

  if (ind <> nil) then SortPointsByDate();

  PreparePointsList(FMapPoints, (ind <> nil));
end;

// обработка нажатия кнопки "Обновить"
procedure TfmMaps.btnSaveImage_Click(sender: System.Object; e: System.EventArgs);
begin
  if (SaveDialog1.ShowDialog = System.Windows.Forms.DialogResult.OK)
  then FMapBrowser.SaveSnapshot(SaveDialog1.FileName);
end;

procedure TfmMaps.TfmMaps_Load(sender: System.Object; e: System.EventArgs);
begin
  PlacesLoad();
  btnSelectPlaces.Enabled := True;
end;

procedure TfmMaps.TfmMaps_KeyDown(sender: System.Object; e: System.Windows.Forms.KeyEventArgs);
begin
  if (e.KeyCode = Keys.Escape) then Close;
end;

procedure TfmMaps.TreePlaces_DoubleClick(sender: System.Object; e: System.EventArgs);
var
  node: TGKTreeNode;
  pt: TMapBrowser.TGMapPoint;
begin
  node := TGKTreeNode(TreePlaces.SelectedNode);
  if (node = nil) then Exit;

  pt := TMapBrowser.TGMapPoint(node.Data);
  if (pt <> nil)
  then FMapBrowser.SetCenter(pt.Latitude, pt.Longitude);
end;

end.
