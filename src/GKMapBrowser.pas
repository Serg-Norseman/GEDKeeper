unit GKMapBrowser;

interface

uses
  Types, Classes, Contnrs, Graphics, Controls, Forms, SHDocVw;

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

  TMapBrowser = class(TWebBrowser)
  private
    FMapFile: string;
    FMapInitialized: Boolean;
    FMapPoints: TObjectList;
    FShowPoints: Boolean;
    FShowLines: Boolean;
    FUpdateCount: Integer;

    procedure gm_ClearPoints();
    procedure gm_ExecScript(Script: string);

    procedure BrowserStatusTextChange(Sender: TObject; const Text: Widestring);
    procedure GenSnapshot(browser: iWebBrowser2; jpegFQFilename: string;
      srcHeight, srcWidth, tarHeight, tarWidth: Integer);
    function GetMapPoint(Index: Integer): TGMapPoint;
    function GetMapPointsCount: Integer;
    function GetPointsFrame(): TCoordsRect;
    procedure SetVisibleElementes(Index: Integer; Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function  AddPoint(aLatitude, aLongitude: Double; aHint: string = ''): Integer;
    procedure BeginUpdate();
    procedure ClearPoints();
    procedure DeletePoint(aIndex: Integer);
    procedure EndUpdate();
    procedure InitMap();
    procedure RefreshPoints();
    procedure SaveSnapshot(const aFileName: string);
    procedure SetCenter(aLatitude, aLongitude: Double; aScale: Integer = -1);
    procedure ZoomToBounds();

    property MapPoints[Index: Integer]: TGMapPoint read GetMapPoint; default;
    property MapPointsCount: Integer read GetMapPointsCount;
    property ShowPoints: Boolean index 0 read FShowPoints write SetVisibleElementes default True;
    property ShowLines: Boolean index 1 read FShowLines write SetVisibleElementes default False;
  end;

const
  GoogleKey = 'ABQIAAAAIcIQgkzLQ27NamNDh2wULxTh9o9-e_HqfKVqUrQPniGEP9J6uhSJmXGEipvip6lxpu_ZXrXaeHwWgQ';

// получение списка точек по запросу
procedure RequestGeoCoords(SearchString: string; aPoints: TObjectList);

// запрос обратного геокодирования
function RequestGeoAddress(aLatitude, aLongitude: Double): string;

implementation

uses
  SysUtils, bsComUtils, bsWinUtils, Dialogs, Jpeg, MSHTML, ActiveX, ComObj,
  XMLDoc, XMLIntf, HTTPSend, GKUtils, GKEngine, GKCommon, GKMain;

{==============================================================================}

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
  try
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
      Result := HTTP.HTTPMethod('GET', AnsiToUtf8(FileURL));

      if (Result)
      then Stream.LoadFromStream(HTTP.Document);

      Result := True;
    finally
      HTTP.Free;
    end;
  except
    on E: Exception do begin
      LogWrite('GetInetFile(): ' + E.Message);
      Result := False;
    end;
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
  if (aPoints = nil) then Exit;

  SearchString := StringReplace(Trim(SearchString), ' ', '+', [rfReplaceAll]);

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

{==============================================================================}

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
//    '        map.addControl(new GSmallMapControl());'#13#10+
    '        map.addControl(new GMapTypeControl());'#13#10+
//    '        map.addControl(new GOverviewMapControl());'#13#10+
//    '        addMarker(59.944265, 30.319948, "This be standard marker without letter");'#13#10+
    '      }'#13#10+
    '    }'#13#10+
    '    </script>'#13#10+
    '  </head>'#13#10+
    '  <body onload="initialize()" onunload="GUnload()">'#13#10+
    '    <div id="map" style="position:absolute; width: 100%; height: 100%; left: 0px; top: 0px;"></div>'#13#10+
    '    <noscript><b style="font-family:Tahoma;">JavaScript must be switched on for use Google Maps.</b></noscript>'#13#10+
    '  </body>'#13#10+
    '</html>';

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

{ TMapBrowser }

constructor TMapBrowser.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FMapInitialized := False;
  OnStatusTextChange := BrowserStatusTextChange;

  FMapPoints := TObjectList.Create(True);
  FUpdateCount := 0;
  FShowPoints := True;
  FShowLines := True;
end;

destructor TMapBrowser.Destroy;
begin
  DeleteFile(FMapFile);

  ClearPoints();
  FMapPoints.Free;

  inherited Destroy;
end;

procedure TMapBrowser.InitMap();
var
  tf: TextFile;
begin
  FMapFile := GetTempDir() + 'GEDKeeperMap.html';
  AssignFile(tf, FMapFile); Rewrite(tf);
  Writeln(tf, MapContent);
  CloseFile(tf);

  Navigate(FMapFile);
end;

procedure TMapBrowser.BrowserStatusTextChange(Sender: TObject; const Text: Widestring);
begin
  if not FMapInitialized and (ReadyState = READYSTATE_COMPLETE) then begin
    FMapInitialized := True;
  end;
end;

function TMapBrowser.GetMapPointsCount(): Integer;
begin
  Result := FMapPoints.Count;
end;

function TMapBrowser.GetMapPoint(Index: Integer): TGMapPoint;
begin
  Result := nil;
  if (Index >= 0) and (Index < FMapPoints.Count)
  then Result := TGMapPoint(FMapPoints[Index]);
end;

procedure TMapBrowser.SetVisibleElementes(Index: Integer; Value: Boolean);
begin
  case Index of
    0: FShowPoints := Value;
    1: FShowLines := Value;
  end;
  RefreshPoints();
end;

procedure TMapBrowser.gm_ExecScript(Script: string);
begin
  if (Trim(Script) = '') then Exit;

  try
    if (Document <> nil) then
      IHTMLDocument2(Document).parentWindow.execScript(Script, 'JavaScript');
  except
    on E: Exception do LogWrite('TMapBrowser.gm_ExecScript(): '+E.Message);
  end;
end;

procedure TMapBrowser.gm_ClearPoints();
begin
  gm_ExecScript('map.clearOverlays();');
end;

function TMapBrowser.AddPoint(aLatitude, aLongitude: Double; aHint: string = ''): Integer;
var
  pt: TGMapPoint;
begin
  pt := TGMapPoint.Create(aLatitude, aLongitude, aHint);
  Result := FMapPoints.Add(pt);
end;

procedure TMapBrowser.DeletePoint(aIndex: Integer);
begin
  FMapPoints.Delete(aIndex);
  RefreshPoints();
end;

procedure TMapBrowser.BeginUpdate();
begin
  Inc(FUpdateCount);
end;

procedure TMapBrowser.EndUpdate();
begin
  Dec(FUpdateCount);
  if (FUpdateCount <= 0) then begin
    RefreshPoints();
    FUpdateCount := 0;
  end;
end;

procedure TMapBrowser.RefreshPoints();
var
  PointsScript, PolylineScript: string;
  i: Integer;
  pt: TGMapPoint;
begin
  // очистка точек только на карте, из списка не убиваются
  gm_ClearPoints();

  if MapPointsCount > 0 then begin
    for i := 0 to MapPointsCount - 1 do begin
      pt := MapPoints[i];
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

procedure TMapBrowser.SetCenter(aLatitude, aLongitude: Double; aScale: Integer = -1);
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

procedure TMapBrowser.ClearPoints();
begin
  gm_ClearPoints();
  FMapPoints.Clear;
end;

procedure TMapBrowser.ZoomToBounds();
var
  Script: string;
  Center_Latitude, Center_Longtude: Double;
  rt: TCoordsRect;
begin
  rt := GetPointsFrame();

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

function TMapBrowser.GetPointsFrame(): TCoordsRect;
var
  i: Integer;
  pt: TGMapPoint;
begin
  FillChar(Result, SizeOf(Result), 0);

  if (FMapPoints.Count) > 0 then begin
    pt := TGMapPoint(FMapPoints[0]);

    Result.MinLon := pt.Longitude;
    Result.MaxLon := pt.Longitude;

    Result.MinLat := pt.Latitude;
    Result.MaxLat := pt.Latitude;

    if (FMapPoints.Count = 1) then begin
      Result.MinLon := Result.MinLon - 20;
      Result.MaxLon := Result.MaxLon + 20;

      Result.MinLat := Result.MinLat - 20;
      Result.MaxLat := Result.MaxLat + 20;

      Exit;
    end;

    for i := 0 to FMapPoints.Count - 1 do begin
      pt := TGMapPoint(FMapPoints[i]);

      if (Result.MinLon > pt.Longitude) then Result.MinLon := pt.Longitude
      else
      if (Result.MaxLon < pt.Longitude) then Result.MaxLon := pt.Longitude;

      if (Result.MinLat > pt.Latitude) then Result.MinLat := pt.Latitude
      else
      if (Result.MaxLat < pt.Latitude) then Result.MaxLat := pt.Latitude;
    end;
  end;
end;

procedure TMapBrowser.GenSnapshot(browser: iWebBrowser2; jpegFQFilename: string;
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

procedure TMapBrowser.SaveSnapshot(const aFileName: string);
var
  IDoc1: IHTMLDocument2;
  Web: IWebBrowser2;
  tmpX, tmpY: Integer;
begin
  Document.QueryInterface(IHTMLDocument2, iDoc1);
  Web := ControlInterface;
  tmpX := Height;
  tmpY := Width;
  TControl(Self).Visible := Boolean(0);
  Height := OleObject.Document.ParentWindow.Screen.Height;
  Width := OleObject.Document.ParentWindow.Screen.Width;

  GenSnapshot(Web, aFileName, Height, Width, Height, Width);

  Height := tmpX;
  Width := tmpY;
  TControl(Self).Visible := Boolean(1);
end;

initialization
  GeoInit();

end.
