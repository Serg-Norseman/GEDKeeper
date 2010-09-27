unit GKMaps;

{$I GEDKeeper.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, ComCtrls, XMLDoc, XMLIntf, Contnrs,
  OleCtrls, SHDocVw, GedCom551;

type
  // класс точки на карте
  TGMapPoint = class(TObject)
  private
    FLatitude: Double;
    FLongitude: Double;
    FHint: string;
  public
    constructor Create(); overload;
    constructor Create(aLatitude, aLongitude: Double; aHint: string = ''); overload;

    property Latitude: Double read FLatitude write FLatitude;    // широта
    property Longitude: Double read FLongitude write FLongitude; // долгота
    property Hint: string read FHint write FHint;                // подсказка
  end;

  // "прямоугольник" координат
  TCoordsRect = record
    MinLon, MinLat, MaxLon, MaxLat: Double;
  end;

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
    CheckResidence: TCheckBox;
    CheckDeath: TCheckBox;
    CheckBirth: TCheckBox;
    btnSelectPlaces: TButton;
    Label2: TLabel;
    btnSaveImage: TButton;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    btnSearch: TButton;
    edSearch: TEdit;
    WebBrowser: TWebBrowser;
    procedure WebBrowserStatusTextChange(Sender: TObject; const Text: Widestring);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnSearchClick(Sender: TObject);
    procedure btnSelectPlacesClick(Sender: TObject);
    procedure btnSaveImageClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure TreePlacesDblClick(Sender: TObject);
  private
    FMapFile: string;
    FMapPoints: TObjectList;
    FPlaces: TObjectList;
    FTree: TGEDCOMTree;
    FBaseRoot, FSearchRoot: TTreeNode;

    FGMapPoints: TObjectList;
    FGMapInitialized: Boolean;
    FGMapUpdateCount: Integer;
    FShowPoints: Boolean;
    FShowLines: Boolean;

    procedure gm_ClearPoints();
    procedure gm_ExecScript(Script: string);
    function GetGMapPoint(Index: Integer): TGMapPoint;
    function GetGMapPointsCount: Integer;
    procedure SetVisibleElementes(Index: Integer; Value: Boolean);

    procedure PlacesLoad();
    procedure PreparePointsList(aPoints: TObjectList);
    procedure SetMapFile();
  protected
    function  map_AddPoint(aLatitude, aLongitude: Double; aHint: string = ''): Integer;
    procedure map_BeginUpdate();
    procedure map_ClearPoints();
    procedure map_DeletePoint(aIndex: Integer);
    procedure map_EndUpdate();
    function  map_GetPointsFrame(): TCoordsRect;
    procedure map_RefreshPoints();
    procedure map_SetCenter(aLatitude, aLongitude: Double; aScale: Integer = -1);
    procedure map_SetLocation(aLocation: string);
    procedure map_ZoomToBounds();

    procedure map_GenSnapshot(browser: iWebBrowser2; jpegFQFilename: string;
      srcHeight, srcWidth, tarHeight, tarWidth: Integer);
    procedure map_SaveSnapshot(const aFileName: string);

    property GMapPointsCount: Integer read GetGMapPointsCount;
    property GMapPoints[Index: Integer]: TGMapPoint read GetGMapPoint; default;
    property ShowPoints: Boolean index 0 read FShowPoints write SetVisibleElementes default True;
    property ShowLines: Boolean index 1 read FShowLines write SetVisibleElementes default False;
  public
    property Tree: TGEDCOMTree read FTree write FTree;
  end;

var
  fmMaps: TfmMaps;

// получение списка точек по запросу
procedure RequestGeoCoords(SearchString: string; aPoints: TObjectList);

// запрос обратного геокодирования
function RequestGeoAddress(aLatitude, aLongitude: Double): string;

implementation

{$R *.dfm}

uses
  bsComUtils, bsWinUtils, GKCommon, HTTPSend, Types, Jpeg, ActiveX, ComObj,
  MSHTML, GKProgress, GKMain;

const
  GoogleKey = 'ABQIAAAAIcIQgkzLQ27NamNDh2wULxTh9o9-e_HqfKVqUrQPniGEP9J6uhSJmXGEipvip6lxpu_ZXrXaeHwWgQ';

var
  xmlDocument: TXMLDocument;

procedure GeoInit();
begin
  DecimalSeparator := '.';
  xmlDocument := TXMLDocument.Create(Application);
end;

procedure GeoDone();
begin
  //xmlDocument.Free; // interfaced object
end;

// отправка запроса и получение ответа
function GetInetFile(const FileURL: string; Stream: TMemoryStream): Boolean;
var
  HTTP: THTTPSend;
  proxy: TProxy;
begin
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
    // переводим в принимаемую Гуглем кодировку
    HTTP.HTTPMethod('GET', AnsiToUtf8(FileURL));

    Stream.LoadFromStream(HTTP.Document);

    Result := True;
  finally
    HTTP.Free;
  end;
end;

// получение списка точек по запросу
procedure RequestGeoCoords(SearchString: string; aPoints: TObjectList);
var
  Point: TGMapPoint;
  FileOnNet, sCoordinates: string;
  Stream: TMemoryStream;
  Node, PlacemarkNode, PointNode, AddressNode: IXMLNode;
  i: Integer;
  StringList: TStringList;
begin
  SearchString := StringReplace(Trim(SearchString), ' ', '+', [rfReplaceAll]);

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
      xmlDocument.LoadFromStream(Stream);
      // формируем содержимое списка точек
      Node := xmlDocument.DocumentElement;
      Node := Node.ChildNodes.FindNode('Response');
      if (Node <> nil) and (Node.ChildNodes.Count > 0) then
        for i := 0 to Node.ChildNodes.Count - 1 do
          if Node.ChildNodes[i].NodeName = 'Placemark' then begin
            // находим узел точки
            PlacemarkNode := Node.ChildNodes[i];
            // получаем узел адреса
            AddressNode := PlacemarkNode.ChildNodes.FindNode('address');
            //получаем узел координат
            PointNode := PlacemarkNode.ChildNodes.FindNode('Point');
            PointNode := PointNode.ChildNodes.FindNode('coordinates');
            if (AddressNode <> nil) and (PointNode <> nil) then begin
              Point := TGMapPoint.Create;
              // получаем адрес
              Point.Hint := Utf8ToAnsi(AddressNode.Text);
              // получаем координаты
              sCoordinates := PointNode.Text;
              // разбираем координаты
              ExtractStrings([','], [], PChar(sCoordinates), StringList);
              if (StringList.Count > 1) then begin
                // Формируем точку
                Point.Longitude := StrToFloatDef(StringList[0], -1);
                Point.Latitude := StrToFloatDef(StringList[1], -1);
                // добавляем точку в список
                if (Point.Latitude <> -1) and (Point.Longitude <> -1)
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

// запрос обратного геокодирования
function RequestGeoAddress(aLatitude, aLongitude: Double): string;

  procedure FillNode(Node: IXMLNode);
  var
    i: Integer;
    NodeText: string;
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
  FileOnNet: string;
  Stream: TMemoryStream;
begin
  Result := '';

  // создаем поток
  Stream := TMemoryStream.Create;
  try
    // формируем url для запроса
    FileOnNet := 'http://maps.google.ru/maps/geo?ll=%.6f,%.6f&output=xml&key=%s&gl=ru';
    FileOnNet := Format(FileOnNet, [aLatitude, aLongitude, GoogleKey]);
    // получение потока с данными ответа
    if GetInetFile(FileOnNet, Stream) then begin
      StreamToUtf8Stream(Stream);
      // заполняем XMLDocument
      xmlDocument.LoadFromStream(Stream);

      Node := xmlDocument.DocumentElement;
      FillNode(Node);
    end;
  finally
    Stream.Free;
  end;
end;

{ TGMapPoint }

constructor TGMapPoint.Create();
begin
  inherited Create;
end;

constructor TGMapPoint.Create(aLatitude, aLongitude: Double; aHint: string = '');
begin
  inherited Create;

  FLatitude := aLatitude;
  FLongitude := aLongitude;
  FHint := aHint;
end;

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
    '        map.addMapType(G_PHYSICAL_MAP);'#13#10+
    '        map.setCenter(new GLatLng(55.755786, 37.617633), 11, G_PHYSICAL_MAP);'#13#10+
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
  FGMapPoints := TObjectList.Create(True);
  FGMapUpdateCount := 0;
  FShowPoints := True;
  FShowLines := True;
  FGMapInitialized := False;

  SetMapFile();

  FMapPoints := TObjectList.Create(True);
  FPlaces := TObjectList.Create(True);

  FBaseRoot := TreePlaces.Items.AddChild(nil, 'Места');
  FSearchRoot := TreePlaces.Items.AddChild(nil, 'Поиск');
end;

procedure TfmMaps.FormDestroy(Sender: TObject);
begin
  FPlaces.Destroy;
  FMapPoints.Destroy;

  map_ClearPoints();
  FGMapPoints.Free;

  DeleteFile(FMapFile);
end;

procedure TfmMaps.SetMapFile();
var
  tf: TextFile;
begin
  FMapFile := GetTempDir() + 'GEDKeeperMap.html';
  AssignFile(tf, FMapFile); Rewrite(tf);
  Writeln(tf, MapContent);
  CloseFile(tf);

  map_SetLocation(FMapFile);
end;

function TfmMaps.GetGMapPointsCount(): Integer;
begin
  Result := FGMapPoints.Count;
end;

function TfmMaps.GetGMapPoint(Index: Integer): TGMapPoint;
begin
  Result := nil;
  if (Index >= 0) and (Index < FGMapPoints.Count)
  then Result := TGMapPoint(FGMapPoints[Index]);
end;

procedure TfmMaps.SetVisibleElementes(Index: Integer; Value: Boolean);
begin
  case Index of
    0: FShowPoints := Value;
    1: FShowLines := Value;
  end;
  map_RefreshPoints();
end;

procedure TfmMaps.gm_ExecScript(Script: string);
begin
  if (Trim(Script) = '') then Exit;

  try
    if WebBrowser.Document <> nil then
      IHTMLDocument2(WebBrowser.Document).parentWindow.execScript(Script, 'JavaScript');
  except
    on E: Exception do MessageDlg(E.Message, mtError, [mbOK], 0);
  end;
end;

procedure TfmMaps.gm_ClearPoints();
begin
  gm_ExecScript('map.clearOverlays();');
end;

procedure TfmMaps.WebBrowserStatusTextChange(Sender: TObject;
  const Text: Widestring);
begin
  if not FGMapInitialized and (WebBrowser.ReadyState = READYSTATE_COMPLETE) then begin
    FGMapInitialized := True;
  end;
end;

function TfmMaps.map_AddPoint(aLatitude, aLongitude: Double; aHint: string = ''): Integer;
var
  pt: TGMapPoint;
begin
  pt := TGMapPoint.Create(aLatitude, aLongitude, aHint);
  Result := FGMapPoints.Add(pt);
end;

procedure TfmMaps.map_DeletePoint(aIndex: Integer);
begin
  FGMapPoints.Delete(aIndex);
  map_RefreshPoints();
end;

procedure TfmMaps.map_BeginUpdate();
begin
  Inc(FGMapUpdateCount);
end;

procedure TfmMaps.map_EndUpdate();
begin
  Dec(FGMapUpdateCount);
  if (FGMapUpdateCount <= 0) then begin
    map_RefreshPoints();
    FGMapUpdateCount := 0;
  end;
end;

procedure TfmMaps.map_SetLocation(aLocation: string);
begin
  WebBrowser.Navigate(aLocation);
end;

procedure TfmMaps.map_RefreshPoints();
var
  PointsScript, PolylineScript: string;
  i: Integer;
  pt: TGMapPoint;
begin
  // очистка точек только на карте, из списка не убиваются
  gm_ClearPoints();

  if GMapPointsCount > 0 then begin
    for i := 0 to GMapPointsCount - 1 do begin
      pt := GMapPoints[i];
      // формируем скрипт создания точек
      PointsScript := PointsScript +
        Format('addMarker(%.6f, %.6f, "%s");', [pt.Latitude, pt.Longitude, pt.Hint]);
      // формируем скрипт создания линий маршрута
      PolylineScript := PolylineScript + 'new GLatLng(' +
        FloatToStr(pt.Latitude) + ',' + FloatToStr(pt.Longitude) + '),';
    end;

    // если поднят флаг рисовать точки, то рисуем
    if ShowPoints
    then gm_ExecScript(PointsScript);

    // если поднят флаг рисовать линии
    if ShowLines then begin
      // добиваем скрипт
      Delete(PolylineScript, Length(PolylineScript), 1);
      PolylineScript := 'var polyline = new GPolyline([' +
        PolylineScript + '],"#FF0000",3);map.addOverlay(polyline);';
      // выполняем
      gm_ExecScript(PolylineScript);
    end;
  end;
end;

procedure TfmMaps.map_SetCenter(aLatitude, aLongitude: Double; aScale: Integer = -1);
var
  Script: string;
begin
  if (aScale >= 0) then begin
    // если при вызове указан масштаб
    Script := 'var point = new GLatLng(' + FloatToStr(aLatitude) + ',' + FloatToStr(aLongitude) + '); ' +
      'map.setCenter(point, ' + IntToStr(aScale) + ')';
  end else begin
    // если не указан, то масштаб сохраняется
    Script := 'var point = new GLatLng(' + FloatToStr(aLatitude) + ',' + FloatToStr(aLongitude) + '); ' +
      'map.setCenter(point)';
  end;
  gm_ExecScript(Script);
end;

procedure TfmMaps.map_ClearPoints();
begin
  gm_ClearPoints();
  FGMapPoints.Clear;
end;

procedure TfmMaps.map_ZoomToBounds();
var
  Script: string;
  Center_Latitude, Center_Longtude: Double;
  rt: TCoordsRect;
begin
  rt := map_GetPointsFrame();

  if (rt.MinLon <> rt.MaxLon) and (rt.MinLat <> rt.MaxLat) then begin
    Center_Longtude := (rt.MaxLon + rt.MinLon) / 2;
    Center_Latitude := (rt.MaxLat + rt.MinLat) / 2;

    Script := 'var point1 = new GLatLng(%.7f, %.7f);'+
              'var point2 = new GLatLng(%.7f, %.7f);'+
              'var bounds = new GLatLngBounds(point1, point2);'+
              'var zoom = map.getBoundsZoomLevel(bounds);'+
              'map.setCenter(new GLatLng(%.7f, %.7f), zoom);';
    Script := Format(Script, [rt.MinLat, rt.MinLon,
       rt.MaxLat, rt.MaxLon, Center_Latitude, Center_Longtude]);
    gm_ExecScript(Script);
  end;
end;

function TfmMaps.map_GetPointsFrame(): TCoordsRect;
var
  i: Integer;
  Point: TGMapPoint;
begin
  FillChar(Result, SizeOf(Result), 0);

  if (FGMapPoints.Count) > 0 then begin
    Point := TGMapPoint(FGMapPoints[0]);

    Result.MinLon := Point.Longitude;
    Result.MaxLon := Point.Longitude;

    Result.MinLat := Point.Latitude;
    Result.MaxLat := Point.Latitude;

    if (FGMapPoints.Count = 1) then begin
      Result.MinLon := Result.MinLon - 20;
      Result.MaxLon := Result.MaxLon + 20;

      Result.MinLat := Result.MinLat - 20;
      Result.MaxLat := Result.MaxLat + 20;

      Exit;
    end;

    for i := 0 to FGMapPoints.Count - 1 do begin
      Point := TGMapPoint(FGMapPoints[i]);

      if (Result.MinLon > Point.Longitude) then Result.MinLon := Point.Longitude
      else
      if (Result.MaxLon < Point.Longitude) then Result.MaxLon := Point.Longitude;

      if (Result.MinLat > Point.Latitude) then Result.MinLat := Point.Latitude
      else
      if (Result.MaxLat < Point.Latitude) then Result.MaxLat := Point.Latitude;
    end;
  end;
end;

procedure TfmMaps.map_GenSnapshot(browser: iWebBrowser2; jpegFQFilename: string;
   srcHeight: Integer; srcWidth: Integer; tarHeight: Integer; tarWidth: Integer);
var
  sourceDrawRect, targetDrawRect: TRect;
  sourceBitmap, targetBitmap: TBitmap;
  jpeg: TJPEGImage;
  viewObject: IViewObject;
begin
  sourceBitmap := TBitmap.Create;
  targetBitmap := TBitmap.Create;
  jpeg := TJPEGImage.Create;
  try
    try
      sourceDrawRect := Rect(0, 0, srcWidth, srcHeight);
      sourceBitmap.Width := srcWidth;
      sourceBitmap.Height := srcHeight;

      viewObject := browser as IViewObject;

      if (viewObject = nil) then Exit;

      OleCheck(viewObject.Draw(DVASPECT_CONTENT, 1, nil, nil, Handle,
        sourceBitmap.Canvas.Handle, @sourceDrawRect, nil, nil, 0));

      // Resize the src bitmap to the target bitmap
      targetDrawRect := Rect(0, 0, tarWidth, tarHeight);
      targetBitmap.Height := tarHeight;
      targetBitmap.Width  := tarWidth;
      targetBitmap.Canvas.StretchDraw(targetDrawRect, sourceBitmap);

      // Create a JPEG from the Bitmap and save it
      jpeg.Assign(targetBitmap);
      jpeg.SaveToFile(jpegFQFilename);
    finally
      jpeg.Free;
      sourceBitmap.Free;
      targetBitmap.Free;
    end;
  except
    // Error Code
  end;
end;

procedure TfmMaps.map_SaveSnapshot(const aFileName: string);
var
  IDoc1: IHTMLDocument2;
  Web: IWebBrowser2;
  tmpX, tmpY: Integer;
begin
  with WebBrowser do begin
    Document.QueryInterface(IHTMLDocument2, iDoc1);
    Web := ControlInterface;
    tmpX := Height;
    tmpY := Width;
    TControl(WebBrowser).Visible := Boolean(0);
    Height := OleObject.Document.ParentWindow.Screen.Height;
    Width := OleObject.Document.ParentWindow.Screen.Width;
    map_GenSnapshot(Web, aFileName, Height, Width, Height, Width);
    Height := tmpX;
    Width := tmpY;
    TControl(WebBrowser).Visible := Boolean(1);
  end;
end;

procedure TfmMaps.PreparePointsList(aPoints: TObjectList);
var
  i: Integer;
  pt: TGMapPoint;
begin
  map_BeginUpdate();
  try
    map_ClearPoints();
    for i := 0 to aPoints.Count - 1 do begin
      pt := TGMapPoint(aPoints[i]);
      map_AddPoint(pt.Latitude, pt.Longitude, pt.Hint);
    end;

    map_ZoomToBounds();
  finally
    map_EndUpdate();
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

      if (rec is TGEDCOMIndividualRecord) then begin
        ind := rec as TGEDCOMIndividualRecord;
        p_cnt := 0;

        for k := 0 to ind.IndividualEventsCount - 1 do begin
          ev := ind.IndividualEvents[k];
          if (ev.Detail.Place.StringValue <> '') then begin
            AddPlace(ev.Detail.Place, ev);
            Inc(p_cnt);
          end;
        end;

        for k := 0 to ind.IndividualAttributesCount - 1 do begin
          ev := ind.IndividualAttributes[k];
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
  s: string;
begin
  cond := [];
  if CheckBirth.Checked then Include(cond, pcBirth);
  if CheckDeath.Checked then Include(cond, pcDeath);
  if CheckResidence.Checked then Include(cond, pcResidence);

  ind := nil;
  if (ComboPersons.ItemIndex >= 0)
  then ind := TGEDCOMIndividualRecord(ComboPersons.Items.Objects[ComboPersons.ItemIndex]);

  ShowLines := (ind <> nil);

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
procedure TfmMaps.btnSearchClick(Sender: TObject);
var
  pt_title: string;
  pt: TGMapPoint;
  k: Integer;
  points: TObjectList;
begin
  {fixme!!! память этиъ точек не будет освобождаться}
  points := TObjectList.Create(False);
  try
    RequestGeoCoords(edSearch.Text, points);

    for k := 0 to points.Count - 1 do
      if (points[k] is TGMapPoint) then begin
        pt := TGMapPoint(points[k]);

        pt_title := pt.Hint + Format(' [%.6f, %.6f]', [pt.Latitude, pt.Longitude]);
        TreePlaces.Items.AddChildObjectFirst(FSearchRoot, pt_title, pt);
      end;

    FSearchRoot.Expand(True);
    FSearchRoot.Selected := True;

    PreparePointsList(FMapPoints);
  finally
    points.Destroy;
  end;
end;

procedure TfmMaps.btnSaveImageClick(Sender: TObject);
begin
  if (SaveDialog1.Execute)
  then map_SaveSnapshot(SaveDialog1.FileName);
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
  then map_SetCenter(pt.Latitude, pt.Longitude);
end;

initialization
  GeoInit();

end.
