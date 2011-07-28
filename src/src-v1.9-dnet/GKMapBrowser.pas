unit GKMapBrowser; {trans:fin}

{$I GEDKeeper2.inc}

interface

uses
  System.Drawing, System.ComponentModel, System.Windows.Forms, System.Resources,
  System.IO, System.Xml, System.Net, System.Text,
  VCLStub, GKUtils, GKEngine, GKCommon, GKMain;

type
  TMapBrowser = class({$IFNDEF DELPHI_NET}TWebBrowser{$ELSE}System.Windows.Forms.Control{$ENDIF})
  public
  type
    // класс точки на карте
    TGMapPoint = class(System.Object)
    private
      FDate: DateTime;
      FLatitude: System.Double;
      FLongitude: System.Double;
      FHint: string;
    public
      constructor Create(); overload;
      constructor Create(aLatitude, aLongitude: Double; aHint: string = ''); overload;

      property Date: DateTime read FDate write FDate;
      property Latitude: System.Double read FLatitude write FLatitude;    // широта
      property Longitude: System.Double read FLongitude write FLongitude; // долгота
      property Hint: string read FHint write FHint;                // подсказка
    end;
  strict private
  type
    // "прямоугольник" координат
    TCoordsRect = record
      MinLon, MinLat, MaxLon, MaxLat: Double;
    end;

  const
    GoogleKey = 'ABQIAAAAIcIQgkzLQ27NamNDh2wULxTh9o9-e_HqfKVqUrQPniGEP9J6uhSJmXGEipvip6lxpu_ZXrXaeHwWgQ';

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

    var
      FMapFile: string;
      FMapPoints: TObjectList;
      FShowPoints: Boolean;
      FShowLines: Boolean;
      FUpdateCount: Integer;

    class var
      xmlDocument: System.Xml.XmlDocument;

    procedure gm_ClearPoints();
    procedure gm_ExecScript(Script: string);

    class function FillNode(Node: XMLNode): string;

    function GetMapPoint(Index: Integer): TGMapPoint;
    function GetMapPointsCount: Integer;
    function GetPointsFrame(): TCoordsRect;
    procedure SetVisibleElementes(Index: Integer; Value: Boolean);
  public
    constructor Create;
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

    class procedure GeoInit();
    class procedure GeoDone();
    class function GetInetFile(const FileURL: string; var Stream: System.IO.Stream): Boolean;
    class procedure RequestGeoCoords(SearchString: string; aPoints: TObjectList);
    class function RequestGeoAddress(aLatitude, aLongitude: Double): string;
  end;

implementation

{ TGMapPoint }

constructor TMapBrowser.TGMapPoint.Create();
begin
  inherited Create;
end;

constructor TMapBrowser.TGMapPoint.Create(aLatitude, aLongitude: Double; aHint: string = '');
begin
  inherited Create;

  FLatitude := aLatitude;
  FLongitude := aLongitude;
  FHint := aHint;
end;

{ TMapBrowser }

constructor TMapBrowser.Create;
begin
  inherited Create;

  FMapPoints := TObjectList.Create(True);
  FUpdateCount := 0;
  FShowPoints := True;
  FShowLines := True;
end;

destructor TMapBrowser.Destroy;
begin
  System.IO.File.Delete(FMapFile);

  ClearPoints();
  FMapPoints.Free;

  inherited Destroy;
end;

procedure TMapBrowser.InitMap();
var
  tf: System.IO.StreamWriter;
begin
  FMapFile := TGKUtils.GetTempDir() + 'GEDKeeperMap.html';

  tf := System.IO.StreamWriter.Create(FMapFile, False, Encoding.UTF8);
  tf.WriteLine(MapContent);
  tf.Close();

  {$IFNDEF DELPHI_NET}
  Navigate(FMapFile);
  {$ENDIF}
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
  if (Script.Trim() = '') then Exit;

  try
    {$IFNDEF DELPHI_NET}
    if (Document <> nil)
    then IHTMLDocument2(Document).parentWindow.execScript(Script, 'JavaScript');
    {$ENDIF}
  except
    on E: Exception do TGKUtils.LogWrite('TMapBrowser.gm_ExecScript(): '+E.Message);
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
        System.&String.Format('addMarker({0:0.000000}, {1:0.000000}, "{2}");', [pt.Latitude, pt.Longitude, pt.Hint]);
      // формируем скрипт создания линий маршрута
      PolylineScript := PolylineScript + 'new GLatLng(' +
        pt.Latitude.ToString() + ',' + pt.Longitude.ToString() + '),';
    end;

    // если поднят флаг рисовать точки, то рисуем
    if ShowPoints
    then gm_ExecScript(PointsScript);

    // если поднят флаг рисовать линии
    if ShowLines then begin
      // добиваем скрипт
      StrDelete(PolylineScript, Length(PolylineScript), 1);
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
    Script := 'var point = new GLatLng(' + aLatitude.ToString() + ',' + aLongitude.ToString() + '); ' +
      'map.setCenter(point, ' + aScale.ToString() + ')';
  end else begin
    // если не указан, то масштаб сохраняется
    Script := 'var point = new GLatLng(' + aLatitude.ToString() + ',' + aLongitude.ToString() + '); ' +
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

    Script := 'var point1 = new GLatLng({0:0.0000000}, {1:0.0000000});'+
              'var point2 = new GLatLng({2:0.0000000}, {3:0.0000000});'+
              'var bounds = new GLatLngBounds(point1, point2);'+
              'var zoom = map.getBoundsZoomLevel(bounds);'+
              'map.setCenter(new GLatLng({4:0.0000000}, {5:0.0000000}), zoom);';
    Script := System.&String.Format(Script, [rt.MinLat, rt.MinLon,
       rt.MaxLat, rt.MaxLon, Center_Latitude, Center_Longtude]);
    gm_ExecScript(Script);
  end;
end;

function TMapBrowser.GetPointsFrame(): TCoordsRect;
var
  i: Integer;
  pt: TGMapPoint;
begin
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

procedure TMapBrowser.SaveSnapshot(const aFileName: string);
begin
  //
end;

class procedure TMapBrowser.GeoInit();
begin
  xmlDocument := System.Xml.XmlDocument.Create();
end;

class procedure TMapBrowser.GeoDone();
begin
  xmlDocument.Free; // interfaced object
end;

// отправка запроса и получение ответа
class function TMapBrowser.GetInetFile(const FileURL: string; var Stream: System.IO.Stream): Boolean;
var
  request: HttpWebRequest;
  response: HttpWebResponse;
  web_proxy: IWebProxy;
  proxy: TProxy;
begin
  try
    request := HttpWebRequest(WebRequest.CreateDefault(Uri.Create(FileURL)));
    request.ContentType := 'application/x-www-form-urlencoded';

    proxy := fmGEDKeeper.Options.Proxy;
    if (proxy.UseProxy) then begin
      web_proxy := WebProxy.Create(proxy.Server+':'+proxy.Port, True);
      //web_proxy.Credentials := NetworkCredential.Create(proxy.Login, proxy.Password);
      web_proxy.Credentials := CredentialCache.DefaultCredentials;
      request.Proxy := web_proxy;
    end;

    // Get Default Proxy
    (*IWebProxy web_proxy := WebRequest.GetSystemWebProxy();
    if (web_proxy <> nil) and (web_proxy.Credentials <> nil)
    then request.Proxy := web_proxy
    else begin
      web_proxy := WebProxy.Create(proxy.Server+':'+proxy.Port {'myproxy:8080'});
      web_proxy.Credentials := NetworkCredential.Create(proxy.Login, proxy.Password);
      request.Proxy := web_proxy;
    end;*)

    response := HttpWebResponse(request.GetResponse());
    Stream := response.GetResponseStream();
    Result := True;
  except
    on E: Exception do begin
      TGKUtils.LogWrite('GetInetFile(): ' + E.Message);
      Stream := nil;
      Result := False;
    end;
  end;
end;

// получение списка точек по запросу
class procedure TMapBrowser.RequestGeoCoords(SearchString: string; aPoints: TObjectList);
var
  Point: TGMapPoint;
  FileOnNet, coords: string;
  Stream: System.IO.Stream;
  //strd: System.IO.StreamReader;
  //st: string;
  Node, PlacemarkNode, PointNode, AddressNode: XMLNode;
  i: Integer;
  coord_tokens: array of string;
begin
  if (aPoints = nil) then Exit;

  //StringList := TStringList.Create;
  try
    SearchString := StringReplace(SearchString.Trim(), ' ', '+', [rfReplaceAll]);
    FileOnNet := 'http://maps.google.ru/maps/geo?q={0}&output=xml&key={1}&gl=ru';
    FileOnNet := System.&String.Format(FileOnNet, [SearchString, GoogleKey]);
    if GetInetFile(FileOnNet, Stream) then begin
      xmlDocument.Load(Stream);

      {strd := StreamReader.Create(Stream);
      st := strd.ReadToEnd;
      TGKUtils.Hole(st);}

      Node := xmlDocument.DocumentElement;
      Node := Node.Item['Response'];
      if (Node <> nil) and (Node.ChildNodes.Count > 0) then
        for i := 0 to Node.ChildNodes.Count - 1 do
          if (Node.ChildNodes[i].Name = 'Placemark') then begin
            // находим узел точки
            PlacemarkNode := Node.ChildNodes[i];
            // получаем узел адреса
            AddressNode := PlacemarkNode.Item['address'];
            //получаем узел координат
            PointNode := PlacemarkNode.Item['Point'];
            PointNode := PointNode.Item['coordinates'];
            if (AddressNode <> nil) and (PointNode <> nil) then begin
              Point := TGMapPoint.Create;
              // получаем адрес
              Point.Hint := AddressNode.InnerText;
              // получаем координаты
              coords := PointNode.InnerText;
              // разбираем координаты
              coord_tokens := coords.Split([',']);
              if (Length(coord_tokens) > 1) then begin
                // Формируем точку
                Point.Longitude := TGKUtils.StrToFloatDef(coord_tokens[0], -1);
                Point.Latitude := TGKUtils.StrToFloatDef(coord_tokens[1], -1);
                // добавляем точку в список
                if (Point.Latitude <> -1) and (Point.Longitude <> -1)
                then aPoints.Add(Point);
              end else Point.Free;
            end;
          end;
    end;
  finally
    Stream.Free;
  end;
end;

class function TMapBrowser.FillNode(Node: XMLNode): string;
var
  i: Integer;
begin
  if (Node.ChildNodes <> nil) and (Node.ChildNodes.Count > 0) then
    for i := 0 to Node.ChildNodes.Count - 1 do begin
      FillNode(Node.ChildNodes[i]);
      // самый первый адрес наиболее подробный, берем его
      if (Node.Name = 'address') then begin
        Result := Node.InnerText;
        Break;
      end;
    end;
end;

// запрос обратного геокодирования
class function TMapBrowser.RequestGeoAddress(aLatitude, aLongitude: Double): string;
var
  FileOnNet: string;
  Stream: System.IO.Stream;
begin
  Result := '';
  try
    FileOnNet := 'http://maps.google.ru/maps/geo?ll={0:0.000000},{1:0.000000}&output=xml&key={2}&gl=ru';
    FileOnNet := System.&String.Format(FileOnNet, [aLatitude, aLongitude, GoogleKey]);
    if GetInetFile(FileOnNet, Stream) then begin
      xmlDocument.Load(Stream);
      Result := FillNode(xmlDocument.DocumentElement);
    end;
  finally
    Stream.Free;
  end;
end;

end.
