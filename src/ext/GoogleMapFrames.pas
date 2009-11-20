unit GoogleMapFrames;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseMapFrames, OleCtrls, SHDocVw, HTTPApp, HTTPProd, ActiveX, MSHTML;

type
  // класс фрейма карт Google
  TGoogleMapFrame = class(TBaseMapFrame)
    WebBrowser: TWebBrowser;
    procedure WebBrowserStatusTextChange(Sender: TObject; const Text: WideString);
  private
    FLocation: String;
    FInitialized: Boolean;
    function PointToAddString(MapPoint: TMapPoint): String;
  protected
    procedure Initialize(); override;
    procedure ExecScript(Script: String);
    procedure ClearPointsOnMap();
  public
    procedure RefreshPoints(); override;
    procedure ShowPoint(Index: Integer); override;
    procedure HidePoint(Index: Integer); override;

    procedure SetLocation(Location: String); override;
    procedure SetCenter(Latitude: Double; Longitude: Double; Scale: Integer = -1); override;
    procedure ZoomIn(); override;
    procedure ZoomOut(); override;
    procedure UnZoom(); override;

    procedure ClearPoints(); override;
    procedure ZoomToRoute(); override;

    procedure AddMarker(Lat, Lon: Double; Hint: String);
    procedure ZoomToBounds(aRect: TCoordsRect);
  end;

var
  GoogleMapFrame: TGoogleMapFrame;

implementation

{$R *.dfm}

// инициализация
procedure TGoogleMapFrame.Initialize();
begin
  inherited Initialize;
  // флаг "карта не загружена"
  FInitialized := False;
end;

procedure TGoogleMapFrame.SetLocation(Location: String);
begin
  // установка расположения карты
  FLocation := Location;
  WebBrowser.Navigate(FLocation);
end;

// отлавливаем событие загрузки карты
procedure TGoogleMapFrame.WebBrowserStatusTextChange(Sender: TObject;
  const Text: WideString);
begin
  if not FInitialized and (WebBrowser.ReadyState = READYSTATE_COMPLETE) then begin
    FInitialized := True;
    if Assigned(FOnMapLoaded) then FOnMapLoaded(Self);
  end;
end;

// принудительная перерисовка точек
procedure TGoogleMapFrame.RefreshPoints();
var
  PointsScript: String;
  PolylineScript: String;
  i: Integer;
  Point: TMapPoint;
begin
  // очистка точек только на карте, из списка не убиваются
  ClearPointsOnMap;

  if PointsCount > 0 then begin
    for i := 0 to PointsCount - 1 do begin
      Point := Points[i];
      // формируем скрипт создания точек
      PointsScript := PointsScript + PointToAddString(Point);
      // формируем скрипт создания линий маршрута
      PolylineScript := PolylineScript + 'new GLatLng(' +
        FloatToStr(Point.Latitude) + ',' + FloatToStr(Point.Longitude) + '),';
    end;

    // если поднят флаг рисовать точки, то рисуем
    if ShowPoints then
      ExecScript(PointsScript);

    // если поднят флаг рисовать линии
    if ShowLines then begin
      // добиваем скрипт
      Delete(PolylineScript, Length(PolylineScript), 1);
      PolylineScript := 'var polyline = new GPolyline([' +
        PolylineScript + '],"#FF0000",3);map.addOverlay(polyline);';
      // выполняем
      ExecScript(PolylineScript);
    end;
  end;
end;

// нарисовать точку с заданным индексом
procedure TGoogleMapFrame.ShowPoint(Index: Integer);
var
  Script: String;
begin
  // предварительно убить
  HidePoint(Index);
  // получаем скрипт
  Script := PointToAddString(Points[Index]);
  // выполним
  ExecScript(Script);
end;

procedure TGoogleMapFrame.AddMarker(Lat, Lon: Double; Hint: String);
var
  Script: String;
begin
  Script := Format('addMarker(%.6f, %.6f, "%s");', [Lat, Lon, Hint]);
  ExecScript(Script);
end;

// спрятать точку с заданным индексом
procedure TGoogleMapFrame.HidePoint(Index: Integer);
var
  Script: String;
begin
  if Index >= PointsCount then Exit;
  // формируем скрипт
  Script := 'map.removeTLabelById("' + Points[Index].Id + '");';
  ExecScript(Script);
end;

// формирование скрипта на добавление точки
function TGoogleMapFrame.PointToAddString(MapPoint: TMapPoint):String;
var
  ColorStr: String;
begin
  ColorStr := ColorToString(MapPoint.Color) + 'label';
  if (Copy(ColorStr, 1, 2) = 'cl') then Delete(ColorStr, 1, 2);
  Result := 'createTLabel("%s", new GLatLng(%.8f, %.8f), "%s", "%s");';
  Result := Format(Result, [MapPoint.Id, MapPoint.Latitude, MapPoint.Longitude, MapPoint.Caption, ColorStr]);
end;

// выполнение скрипта
procedure TGoogleMapFrame.ExecScript(Script: String);
begin
  if (Trim(Script) = '') then Exit;

  try
    if WebBrowser.Document <> nil then
      IHTMLDocument2(WebBrowser.Document).parentWindow.execScript(Script, 'JavaScript');
  except
    on E: Exception do begin
      MessageBox(Handle, PChar(E.Message), 'Ошибка исполнения', MB_OK or MB_ICONERROR);
    end;
  end;
end;

// центрирование карты
procedure TGoogleMapFrame.SetCenter(Latitude: Double; Longitude: Double; Scale: Integer = -1);
var
  Script: String;
begin
  // если при вызове указан масштаб
  if Scale >= 0 then begin
    Script := 'var point = new GLatLng(' + FloatToStr(Latitude) + ',' + FloatToStr(Longitude) + '); ' +
      'map.setCenter(point, ' + IntToStr(Scale) + ')';
  end else begin
  // если не указан, то масштаб сохраняется
    Script := 'var point = new GLatLng(' + FloatToStr(Latitude) + ',' + FloatToStr(Longitude) + '); ' +
      'map.setCenter(point)';
  end;
  ExecScript(Script);
end;

// приближение карты
procedure TGoogleMapFrame.ZoomIn();
begin
  ExecScript('map.zoomIn();');
end;

// отдаление карты
procedure TGoogleMapFrame.ZoomOut();
begin
  ExecScript('map.zoomOut();');
end;

// установка начального масштаба
procedure TGoogleMapFrame.UnZoom();
begin
  SetCenter(59.944265,30.319948,10);
end;

// очистка точек на карте без удаления из списка точек
procedure TGoogleMapFrame.ClearPointsOnMap();
var
  i: Integer;
  Script: String;
begin
  Script := 'map.clearOverlays();';
  for i := 0 to PointsCount - 1 do
    Script := Script + 'map.removeTLabelById("' + Points[i].Id + '");';
  ExecScript(Script);
end;

// очистка списка точек
procedure TGoogleMapFrame.ClearPoints();
begin
  ClearPointsOnMap;
  inherited ClearPoints;
end;

procedure TGoogleMapFrame.ZoomToBounds(aRect: TCoordsRect);
var
  Script: String;
  Center: TCoordinate;
begin
  if (aRect.MinLon <> aRect.MaxLon) and (aRect.MinLat <> aRect.MaxLat) then begin
    Center.Longtude := (aRect.MaxLon + aRect.MinLon) / 2;
    Center.Latitude := (aRect.MaxLat + aRect.MinLat) / 2;

    Script := 'var point1 = new GLatLng(%.7f, %.7f);'+
              'var point2 = new GLatLng(%.7f, %.7f);'+
              'var bounds = new GLatLngBounds(point1, point2);'+
              'var zoom = map.getBoundsZoomLevel(bounds);'+
              'map.setCenter(new GLatLng(%.7f, %.7f), zoom);';
    Script := Format(Script, [aRect.MinLat, aRect.MinLon,
       aRect.MaxLat, aRect.MaxLon, Center.Latitude, Center.Longtude]);
    ExecScript(Script);
  end;
end;

// масштабирование карты по маршруту
procedure TGoogleMapFrame.ZoomToRoute();
var
  Frame: TCoordsRect;
begin
  Frame := GetRouteFrame();
  ZoomToBounds(Frame);
end;

end.
