unit GKMaps;

// Partial Copyright by "Static Google Maps API & Geocoding Demo © Maxim Mazitov"

{$DEFINE SYNAPSE}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, WinInet, AxCtrls, 
  ComCtrls, XMLDoc, XMLIntf, xmldom, msxmldom, Contnrs, GedCom551,
  BaseMapFrames, GoogleMapFrames;

type
  TPlace = class
  public
    Name: String;
    Points: TObjectList;
    Refs: TObjectList;

    constructor Create;
    destructor Destroy; override;
  end;

  TMapPoint = class
  public
    Lat: Double;
    Lon: Double;
    Address: String;
  end;

  TfmMaps = class(TForm)
    StatusBar1: TStatusBar;
    Splitter1: TSplitter;
    PageControl1: TPageControl;
    tsPlaces: TTabSheet;
    tsSearch: TTabSheet;
    GroupBox1: TGroupBox;
    lbLatitude: TLabel;
    edLatitude: TEdit;
    lbLongitude: TLabel;
    edLongitude: TEdit;
    TabSheet1: TTabSheet;
    btnSearch: TButton;
    TreeView1: TTreeView;
    ListView: TListView;
    Label1: TLabel;
    edSearch: TEdit;
    RadioGroup1: TRadioGroup;
    SaveDialog1: TSaveDialog;
    btnSaveImage: TButton;
    Panel1: TPanel;
    GroupBox2: TGroupBox;
    ComboPersons: TComboBox;
    CheckResidence: TCheckBox;
    CheckDeath: TCheckBox;
    CheckBirth: TCheckBox;
    btnSelectPlaces: TButton;
    Label2: TLabel;
    Label3: TLabel;
    cbMapType: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnSearchClick(Sender: TObject);
    procedure btnSelectPlacesClick(Sender: TObject);
    procedure ListViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure btnSaveImageClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure cbMapTypeChange(Sender: TObject);
  private
    FXMLDocument: TXMLDocument;
    FTree: TGEDCOMTree;
    FMapPoints: TObjectList;
    FPlaces: TObjectList;
    FMapFrame: TBaseMapFrame;
    FMapFile: string;

    procedure PlacesLoad();

    procedure SetMapFile();

    procedure DoGeocodingRequest(SearchString: String; aPoints: TObjectList);
    function DoReverseGeocodingRequest(Latitude: Double; Longitude: Double): string;
    procedure FillGeocodingListView(Points: TObjectList);
    procedure PreparePointsList(aPoints: TObjectList);

    procedure PointsChangingEvent(Sender: TBaseMapFrame;
      PointsChangingType: TPointsChangingType; Index: Integer);
    function GetPointsFrame(): TCoordsRect;
  public
    property Tree: TGEDCOMTree read FTree write FTree;
  end;

var
  fmMaps: TfmMaps;

implementation

uses
  GKCommon {$IFDEF SYNAPSE}, HTTPSend{$ENDIF}, GKUtils, GKProgress, GKMain;

const
  GoogleKey = 'ABQIAAAAIcIQgkzLQ27NamNDh2wULxTh9o9-e_HqfKVqUrQPniGEP9J6uhSJmXGEipvip6lxpu_ZXrXaeHwWgQ';

{$R *.dfm}

// отправка запроса и получение ответа
function GetInetFile(const FileURL: String; Stream: TMemoryStream): Boolean;
const
  BufferSize = 1024;
var
  Utf8FileUrl: UTF8String; // Гугл принимает и отдает UTF8 кодировку!!!
{$IFNDEF SYNAPSE}
  hSession, hURL: HInternet;
  Buffer: array [1..BufferSize] of Byte;
  BufferLen: DWORD;
  sAppName: string;
  Headers: String;
{$ELSE}
  HTTP: THTTPSend;
  proxy: TProxy;
{$ENDIF}
begin
  // переводим в принимаемую Гуглем кодировку
  Utf8FileUrl := AnsiToUtf8(FileURL);

  {$IFNDEF SYNAPSE}
  sAppName := ExtractFileName(Application.ExeName);
  hSession := InternetOpen(PChar(sAppName), INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
  try
    // рассказывем Гуглю, что русские мы
    Headers := 'Accept-Language: ru';
    hURL := InternetOpenURL(hSession, PChar(Utf8FileUrl), PChar(Headers), Length(Headers), 0, 0);
    try
      Stream.Clear;
      repeat
        InternetReadFile(hURL, @Buffer, SizeOf(Buffer), BufferLen);
        Stream.Write(Buffer, BufferLen);
      until BufferLen = 0;
      Result := True;
    finally
      InternetCloseHandle(hURL);
    end;
  finally
    InternetCloseHandle(hSession);
  end;
  {$ELSE}
  HTTP := THTTPSend.Create;
  try
    proxy := fmGEDKeeper.Options.Proxy;
    if (proxy.UseProxy) then begin
      HTTP.ProxyUser := proxy.Login;
      HTTP.ProxyPass := proxy.Password;
      HTTP.ProxyHost := proxy.Server;
      HTTP.ProxyPort := proxy.Port;
    end;

    HTTP.MimeType := 'application/x-www-form-urlencoded';
    HTTP.HTTPMethod('GET', Utf8FileUrl);

    Stream.LoadFromStream(HTTP.Document);

    Result := True;
  finally
    HTTP.Free;
  end;
  {$ENDIF}
end;

// получение карты с заданным центром, масштабом и с маркером в центре
function GetMap(Latitude: Double; Longitude: Double; Scale: Integer): TOleGraphic; overload;
var
  FileOnNet: String;
  Stream: TMemoryStream;
begin
  // создаем поток
  Stream := TMemoryStream.Create;
  try
    // формируем url для запроса
    FileOnNet := 'http://maps.google.ru/staticmap?center=%.6f,%.6f&zoom=%d&size=640x640'
      +'&markers=%.6f,%.6f,blues'
      +'&maptype=mobile&key=%s';
    FileOnNet := Format(FileOnNet, [Latitude, Longitude, Scale, Latitude, Longitude, GoogleKey]);
    // получение потока с данными ответа
    if GetInetFile(FileOnNet, Stream) then begin
      // создаем графический объект
      Stream.Position := 0;
      Result := TOleGraphic.Create;
      Result.LoadFromStream(Stream);
    end else Result := nil;
  finally
    Stream.Free;
  end;
end;

// получение карты со списком точек
function GetMap(Points: TObjectList): TOleGraphic; overload;
var
  FileOnNet: String;
  Stream: TMemoryStream;
  Markers: String;
  i: Integer;
  Point: TMapPoint;
begin
  // проверяем наличие точек
  if (Points.Count < 1) then begin
    Result := nil;
    Exit;
  end;

  // формируем список маркеров
  Markers := '';
  for i := 0 to Points.Count - 1 do
    if (Points[i] is TMapPoint) then begin
      Point := TMapPoint(Points[i]);
      Markers := Markers + Format('%.6f,%.6f|', [Point.Lat, Point.Lon]);
    end;

  // создаем поток
  Stream := TMemoryStream.Create;
  try
    // формируем url для запроса
    FileOnNet := 'http://maps.google.ru/staticmap?size=640x640'
      +'&markers=%s'
      +'&maptype=mobile&key=%s';
    FileOnNet := Format(FileOnNet, [Markers, GoogleKey]);
    // получение потока с данными ответа
    if GetInetFile(FileOnNet, Stream) then begin
      // создаем графический объект
      Stream.Position := 0;
      Result := TOleGraphic.Create;
      Result.LoadFromStream(Stream);
    end else Result := nil;
  finally
    Stream.Free;
  end;
end;

{ TPlace }

constructor TPlace.Create;
begin
  Points := TObjectList.Create(True);
  Refs := TObjectList.Create(False);
end;

destructor TPlace.Destroy;
begin
  Refs.Destroy;
  Points.Destroy;

  inherited Destroy;
end;

const
  MapContent: string =
    '<html>'#13#10+
    '  <head>'#13#10+
    '    <meta http-equiv="content-type" content="text/html; charset=utf-8"/>'#13#10+
    '    <title>Map</title>'#13#10+
    '    <script src="http://maps.google.ru/maps?file=api&amp;v=2&amp;key='+GoogleKey+'" type="text/javascript"></script>'#13#10+
    '    <script type="text/javascript">'#13#10+
    '    var map;'#13#10+
    '    function addMarker(latitude, longitude, hint)'#13#10+
    '    {'#13#10+
    '        var blueIcon = new GIcon(G_DEFAULT_ICON);'#13#10+
    '        markerOptions = { icon : blueIcon, title : hint };'#13#10+
    '        var latlng = new GLatLng(latitude,longitude);'#13#10+
    '        map.addOverlay(new GMarker(latlng, markerOptions));'#13#10+
    '    }'#13#10+
    '    function initialize() {'#13#10+
    '      if (GBrowserIsCompatible()) {'#13#10+
    '        map = new GMap2(document.getElementById("map"));'#13#10+
    '        map.setCenter(new GLatLng(55.755786, 37.617633), 11, %s);'#13#10+
    '        map.addControl(new GLargeMapControl());'#13#10+
    '        map.addControl(new GMapTypeControl());'#13#10+
    '        map.addControl(new GOverviewMapControl());'#13#10+
    '        //addMarker(59.944265, 30.319948, "This be standard marker without letter");'#13#10+
    '      }'#13#10+
    '    }'#13#10+
    '    </script>'#13#10+
    '  </head>'#13#10+
    '  <body onload="initialize()" onunload="GUnload()">'#13#10+
    '    <div id="map" style="position:absolute; width: 100%; height: 100%; left: 0px; top: 0px;"></div>'#13#10+
    '    <noscript><b style="font-family:Tahoma;">JavaScript must be switched on for use Google Maps.</b></noscript>'#13#10+
    '  </body>'#13#10+
    '</html>';

{ TfmMaps }

procedure TfmMaps.FormCreate(Sender: TObject);
begin
  FMapFrame := TGoogleMapFrame.Create(Self);
  FMapFrame.PointsChangingEvent := PointsChangingEvent;
  FMapFrame.Parent := Panel1;
  FMapFrame.Align := alClient;

  SetMapFile();

  FMapPoints := TObjectList.Create(True);
  FPlaces := TObjectList.Create(True);

  DecimalSeparator := '.';
  FXMLDocument := TXMLDocument.Create(Self);
end;

procedure TfmMaps.FormDestroy(Sender: TObject);
begin
  FPlaces.Destroy;
  FMapPoints.Destroy;

  if (FMapFrame <> nil) then begin
    FMapFrame.Parent := nil;
    FMapFrame.Free;
  end;

  DeleteFile(FMapFile);
end;

procedure TfmMaps.SetMapFile();
const
  MapTypes: array [0..3] of string = (
    'G_NORMAL_MAP', 'G_SATELLITE_MAP', 'G_HYBRID_MAP', 'G_PHYSICAL_MAP'
  );
var
  tf: TextFile;
  idx: Integer;
  ts: string;
begin
  idx := cbMapType.ItemIndex;
  if (idx < 0) then idx := 3;
  ts := StringReplace(MapContent, '%s', MapTypes[idx], []);

  FMapFile := GetTempDir() + 'GEDKeeperMap.html';
  AssignFile(tf, FMapFile); Rewrite(tf);
  Writeln(tf, ts);
  CloseFile(tf);

  FMapFrame.SetLocation(FMapFile);
end;

procedure TfmMaps.PointsChangingEvent(Sender: TBaseMapFrame;
  PointsChangingType: TPointsChangingType; Index: Integer);
{var
  Item: TListItem;
  Point: TMapPoint;}
begin
  {case PointsChangingType of
    // после вызова EndUpdate
    pctContiniousUpdate:begin
      FillPointsList(Sender);
    end;
    // после вызова сброса маршрута
    pctCleared:begin
      FillPointsList(Sender);
    end;
    // после добавления точки
    pctAdded:begin
      Point:=FMapFrame.Points[Index];
      Item:=lvPoints.Items.Insert(Index);
      Item.Caption:=IntToStr(Index+1);
      Item.SubItems.Text:=Format('%.7f x %.7f',[Point.Latitude,Point.Longitude]);
    end;
    // после изменения точки
    pctModified:begin
      Point:=FMapFrame.Points[Index];
      Item:=lvPoints.Items[Index];
      Item.Caption:=IntToStr(Index+1);
      Item.SubItems.Text:=Format('%.7f x %.7f',[Point.Latitude,Point.Longitude]);
    end;
    // после удаления точки
    pctDeleted:begin
      lvPoints.Items.Delete(Index);
    end;
  end;}
end;

// запрос обратного геокодирования
function TfmMaps.DoReverseGeocodingRequest(Latitude: Double; Longitude: Double): string;

  procedure FillNode(Node: IXMLNode);
  var
    i: Integer;
    NodeText: String;
  begin
    NodeText := '';

    if Node.IsTextElement
    then NodeText := Node.NodeName + '=' + Node.Text
    else NodeText := Node.NodeName;

    if (Node.ChildNodes <> nil) and (Node.ChildNodes.Count > 0) then
      for i := 0 to Node.ChildNodes.Count - 1 do begin
        FillNode(Node.ChildNodes.Nodes[i]);
        // самый первый адрес наиболее подробный, берем его
        if (Result = '') and (Node.NodeName = 'address')
        then Result := Node.Text;
      end;
  end;

var
  Node: IXMLNode;
  FileOnNet: String;
  Stream: TMemoryStream;
begin
  Result := '';

  // создаем поток
  Stream := TMemoryStream.Create;
  try
    // формируем url для запроса
    FileOnNet := 'http://maps.google.ru/maps/geo?ll=%.6f,%.6f&output=xml&key=%s&gl=ru';
    FileOnNet := Format(FileOnNet, [Latitude, Longitude, GoogleKey]);
    // получение потока с данными ответа
    if GetInetFile(FileOnNet, Stream) then begin
      StreamToUtf8Stream(Stream);
      // заполняем XMLDocument
      FXMLDocument.LoadFromStream(Stream);

      Node := FXMLDocument.DocumentElement;
      FillNode(Node);
    end;
  finally
    Stream.Free;
  end;
end;

// обработка нажатия кнопки "Обновить"
procedure TfmMaps.btnSearchClick(Sender: TObject);
var
  pt: TMapPoint;
  pt_name: String;
begin
  case RadioGroup1.ItemIndex of
    0: begin
      pt := TMapPoint.Create;
      pt.Lat := StrToFloat(edLatitude.Text);
      pt.Lon := StrToFloat(edLongitude.Text);
      pt.Address := DoReverseGeocodingRequest(pt.Lat, pt.Lon);
      FMapPoints.Add(pt);
    end;
    1: begin
      pt_name := StringReplace(Trim(edSearch.Text), ' ', '+', [rfReplaceAll]);
      DoGeocodingRequest(pt_name, FMapPoints);
    end;
  end;

  PreparePointsList(FMapPoints);
end;

procedure TfmMaps.PreparePointsList(aPoints: TObjectList);
var
  //OleGraphic: TOleGraphic;
  i: Integer;
  pt: TMapPoint;
  rt: TCoordsRect;
begin
  try
    // заполнение списка точек
    FillGeocodingListView(aPoints);
    // запрос карты
    {OleGraphic := GetMap(aPoints);
    if (OleGraphic <> nil) then begin
      // рисуем карту
      ImageGeocoding.Picture.Assign(OleGraphic);
      OleGraphic.Free;
    end;}

    //FMapFrame.BeginUpdate;
    //FMapFrame.ClearPoints();
    for i := 0 to aPoints.Count - 1 do begin
      pt := TMapPoint(aPoints[i]);
      TGoogleMapFrame(FMapFrame).AddMarker(pt.Lat, pt.Lon, pt.Address);
    end;

    if (aPoints.Count > 1)
    then rt := GetPointsFrame()
    else begin
      //pt := TMapPoint(aPoints[0]);

      rt.MinLon := pt.Lon - 20;
      rt.MaxLon := pt.Lon + 20;
      rt.MinLat := pt.Lat - 20;
      rt.MaxLat := pt.Lat + 20;
    end;

    TGoogleMapFrame(FMapFrame).ZoomToBounds(rt);
    //FMapFrame.EndUpdate;
  finally
  end;
end;

// получение "прямоугольника", в который вписан маршрут
function TfmMaps.GetPointsFrame(): TCoordsRect;
var
  i: Integer;
  Point: TMapPoint;
begin
  FillChar(Result, SizeOf(Result), 0);

  if (FMapPoints.Count) > 0 then begin
    Point := TMapPoint(FMapPoints[0]);
    Result.MinLon := Point.Lon;
    Result.MinLat := Point.Lat;
    Result.MaxLon := Point.Lon;
    Result.MaxLat := Point.Lat;

    for i := 0 to FMapPoints.Count - 1 do begin
      Point := TMapPoint(FMapPoints[i]);

      if Result.MinLon > Point.Lon
      then Result.MinLon := Point.Lon
      else
      if Result.MaxLon < Point.Lon
      then Result.MaxLon := Point.Lon;

      if Result.MinLat > Point.Lat
      then Result.MinLat := Point.Lat
      else
      if Result.MaxLat < Point.Lat
      then Result.MaxLat := Point.Lat;
    end;
  end;
end;

// получение списка точек по запросу
procedure TfmMaps.DoGeocodingRequest(SearchString: String; aPoints: TObjectList);
var
  Point: TMapPoint;
  FileOnNet: String;
  Stream: TMemoryStream;
  Node: IXMLNode;
  PlacemarkNode, PointNode, AddressNode: IXMLNode;
  i: Integer;
  sCoordinates: String;
  StringList: TStringList;
begin
  if (aPoints = nil) then Exit;

  // создаем поток
  Stream := TMemoryStream.Create;
  StringList := TStringList.Create;
  try
    // формируем url для запроса
    FileOnNet := 'http://maps.google.ru/maps/geo?q=%s&output=xml&key=%s&gl=ru';
    FileOnNet := Format(FileOnNet, [SearchString, GoogleKey]);
    // получение потока с данными ответа
    if GetInetFile(FileOnNet, Stream) then begin
      StreamToUtf8Stream(Stream);
      // заполняем XMLDocument
      FXMLDocument.LoadFromStream(Stream);
      // формируем содержимое списка точек
      Node := FXMLDocument.DocumentElement;
      Node := Node.ChildNodes.FindNode('Response');
      if (Node<>nil) and (Node.ChildNodes.Count>0) then
        for i := 0 to Node.ChildNodes.Count-1 do
          if Node.ChildNodes[i].NodeName='Placemark' then begin
            // находим узел точки
            PlacemarkNode := Node.ChildNodes[i];
            // получаем узел адреса
            AddressNode := PlacemarkNode.ChildNodes.FindNode('address');
            //получаем узел координат
            PointNode := PlacemarkNode.ChildNodes.FindNode('Point');
            PointNode := PointNode.ChildNodes.FindNode('coordinates');
            if (AddressNode <> nil) and (PointNode <> nil) then begin
              Point := TMapPoint.Create;
              // получаем адрес
              Point.Address := AddressNode.Text;
              // получаем координаты
              sCoordinates := PointNode.Text;
              // разбираем координаты
              ExtractStrings([','], [], PChar(sCoordinates), StringList);
              if (StringList.Count > 1) then begin
                // Формируем точку
                Point.Lon := StrToFloatDef(StringList[0], -1);
                Point.Lat := StrToFloatDef(StringList[1], -1);
                // добавляем точку в список
                if (Point.Lat <> -1) and (Point.Lon <> -1)
                then aPoints.Add(Point);
                StringList.Clear;
              end else
                Point.Free;
            end;
          end;
    end;
  finally
    StringList.Free;
    Stream.Free;
  end;
end;

// заполнение ListView со списком точек
procedure TfmMaps.FillGeocodingListView(Points: TObjectList);
var
  i: Integer;
  Item: TListItem;
  Point: TMapPoint;
begin
  ListView.Items.BeginUpdate;
  try
    ListView.Items.Clear;

    for i := 0 to Points.Count - 1 do
      if (Points[i] is TMapPoint) then begin
        Point := TMapPoint(Points[i]);
        Item := ListView.Items.Add;
        Item.Caption := Point.Address;
        Item.SubItems.Text := Format('%.6f'#13#10'%.6f',[Point.Lat,Point.Lon]);
      end;
  finally
    ListView.Items.EndUpdate;
  end;
end;

procedure TfmMaps.PlacesLoad();
var
  root: TTreeNode;

  function FindTreeNode(aPlace: string): TTreeNode;
  var
    idx: Integer;
  begin
    Result := nil;

    for idx := 0 to TreeView1.Items.Count - 1 do
      if (TreeView1.Items[idx].Text = aPlace) then begin
        Result := TreeView1.Items[idx];
        Exit;
      end;
  end;

  procedure PreparePlace(aNode: TTreeNode; aPlace: TPlace);
  var
    SearchString, pt_title: String;
    pt: TMapPoint;
    k: Integer;
  begin
    try
      SearchString := StringReplace(Trim(aPlace.Name), ' ', '+', [rfReplaceAll]);
      DoGeocodingRequest(SearchString, aPlace.Points);

      for k := 0 to aPlace.Points.Count - 1 do
        if (aPlace.Points[k] is TMapPoint) then begin
          pt := TMapPoint(aPlace.Points[k]);

          pt_title := pt.Address + Format(' [%.6f, %.6f]', [pt.Lat, pt.Lon]);
          TreeView1.Items.AddChildFirst(aNode, pt_title);
        end;
    finally
    end;
  end;

  procedure AddPlace(aPlace: string; aRef: TGEDCOMObject);
  var
    node: TTreeNode;
    place: TPlace;
  begin
    node := FindTreeNode(aPlace);
    if (node = nil) then begin
      node := TreeView1.Items.AddChild(root, aPlace);

      place := TPlace.Create;
      place.Name := aPlace;
      FPlaces.Add(place);

      node.Data := place;

      PreparePlace(node, place); 
    end else begin
      place := TPlace(node.Data);
    end;

    place.Refs.Add(aRef);
  end;

var
  i, k: Integer;
  rec: TGEDCOMRecord;
  ind: TGEDCOMIndividualRecord;
  i_ev: TGEDCOMIndividualEvent;
  i_att: TGEDCOMIndividualAttribute;
  has_places: Boolean;
begin
  ComboPersons.Items.BeginUpdate;
  TreeView1.Items.BeginUpdate;
  ProgressInit(FTree.Count, 'Загрузка и поиск мест');
  try
    FPlaces.Clear;
    TreeView1.Items.Clear;
    root := TreeView1.Items.AddChild(nil, 'Места');

    ComboPersons.Clear;
    ComboPersons.Sorted := False;
    ComboPersons.Items.AddObject('( не выбран )', nil);

    for i := 0 to FTree.Count - 1 do begin
      rec := FTree.Records[i];

      if (rec is TGEDCOMIndividualRecord) then begin
        ind := rec as TGEDCOMIndividualRecord;
        has_places := False;

        for k := 0 to ind.IndividualEventsCount - 1 do begin
          i_ev := ind.IndividualEvents[k];
          if (i_ev.Detail.Place <> '') then begin
            AddPlace(i_ev.Detail.Place, i_ev);
            has_places := True;
          end;
        end;

        for k := 0 to ind.IndividualAttributesCount - 1 do begin
          i_att := ind.IndividualAttributes[k];

          if (i_att.Name = 'RESI') and (i_att.StringValue <> '') then begin
            AddPlace(i_att.StringValue, i_att);
            has_places := True;
          end;
        end;

        if has_places
        then ComboPersons.Items.AddObject(GetNameStr(ind), ind);
      end;

      ProgressStep();
    end;

    TreeView1.AlphaSort(True);
    TreeView1.Items[0].Expand(True);

    ComboPersons.Sorted := True;
  finally
    ProgressDone();
    TreeView1.Items.EndUpdate;
    ComboPersons.Items.EndUpdate;
  end;
end;

procedure TfmMaps.btnSelectPlacesClick(Sender: TObject);

  procedure CopyPoint(aPt: TMapPoint);
  var
    pt: TMapPoint;
    i: Integer;
  begin
    for i := 0 to FMapPoints.Count - 1 do begin
      pt := TMapPoint(FMapPoints[i]);
      if (pt.Address = aPt.Address)
      then Exit;
    end;

    pt := TMapPoint.Create;
    pt.Lat := aPt.Lat;
    pt.Lon := aPt.Lon;
    pt.Address := aPt.Address;
    FMapPoints.Add(pt);
  end;

var
  i, k: Integer;
  place: TPlace;
  ref: TGEDCOMObject;
  cond: set of (pcBirth, pcDeath, pcResidence);
  ind: TGEDCOMIndividualRecord;
begin
  cond := [];
  if CheckBirth.Checked then Include(cond, pcBirth);
  if CheckDeath.Checked then Include(cond, pcDeath);
  if CheckResidence.Checked then Include(cond, pcResidence);

  ind := nil;
  if (ComboPersons.ItemIndex >= 0)
  then ind := TGEDCOMIndividualRecord(ComboPersons.Items.Objects[ComboPersons.ItemIndex]);

  FMapPoints.Clear;
  FMapFrame.ClearPoints();

  for i := 0 to FPlaces.Count - 1 do begin
    place := TPlace(FPlaces[i]);
    if (place.Points.Count < 1) then Continue;

    for k := 0 to place.Refs.Count - 1 do begin
      ref := TGEDCOMObject(place.Refs[k]);

      if (ref is TGEDCOMIndividualEvent) then begin
        if ((ind <> nil) and (TGEDCOMIndividualEvent(ref).Parent = ind))
        or ((pcBirth in cond) and (TGEDCOMIndividualEvent(ref).Name = 'BIRT'))
        or ((pcDeath in cond) and (TGEDCOMIndividualEvent(ref).Name = 'DEAT'))
        then CopyPoint(TMapPoint(place.Points[0]));
      end
      else
      if (ref is TGEDCOMIndividualAttribute) then begin
        if ((ind <> nil) and (TGEDCOMIndividualAttribute(ref).Parent = ind))
        or ((pcResidence in cond) and (TGEDCOMIndividualAttribute(ref).Name = 'RESI'))
        then CopyPoint(TMapPoint(place.Points[0]));
      end;
    end;
  end;

  PreparePointsList(FMapPoints);
end;

procedure TfmMaps.ListViewSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
var
  Index: Integer;
  Point: TMapPoint;
begin
  if Selected and (Item <> nil) then begin
    Index := Item.Index;
    Point := TMapPoint(FMapPoints[Index]); //FMapFrame[Index];
    FMapFrame.SetCenter(Point.Lat, Point.Lon);
  end;
end;

procedure TfmMaps.btnSaveImageClick(Sender: TObject);
var
  img: TBitmap;
  OleGraphic: TOleGraphic;
begin
  if not(SaveDialog1.Execute) then Exit;

  img := TBitmap.Create();
  try
    OleGraphic := GetMap(FMapPoints);
    if (OleGraphic <> nil) then begin
      //img.Picture.Assign(OleGraphic);
      //OleGraphic.SaveToFile(SaveDialog1.FileName);

      img.Width := OleGraphic.Width;
      img.Height := OleGraphic.Height;
      img.Canvas.Draw(0, 0, OleGraphic);
      img.SaveToFile(SaveDialog1.FileName);

      OleGraphic.Free;
    end;

    //img.Canvas.Draw(0, 0, OleGraphic);

    {img.Width := FMapFrame.Width;
    img.Height := FMapFrame.Height;
    BitBlt(img.Canvas.Handle, 0, 0, img.Width, img.Height, GetDC(FMapFrame.Handle), 0, 0, SRCCOPY);
    img.SaveToFile(SaveDialog1.FileName);}
  finally
    img.Destroy;
  end;
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

procedure TfmMaps.cbMapTypeChange(Sender: TObject);
begin
  SetMapFile();
end;

end.
