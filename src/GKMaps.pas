unit GKMaps;

// Partial Copyright by "Static Google Maps API & Geocoding Demo © Maxim Mazitov"

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, WinInet, Jpeg, AxCtrls, 
  ComCtrls, XMLDoc, XMLIntf, xmldom, msxmldom, Contnrs, GedCom551;

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
    PageControl: TPageControl;
    StatusBar1: TStatusBar;
    tsMap: TTabSheet;
    ScrollBoxGeocoding: TScrollBox;
    ImageGeocoding: TImage;
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
    btRefersh: TButton;
    TreeView1: TTreeView;
    Button1: TButton;
    ProgressBar1: TProgressBar;
    GroupBox2: TGroupBox;
    edSearch: TEdit;
    btSearch: TButton;
    ListView: TListView;
    CheckBirth: TCheckBox;
    CheckDeath: TCheckBox;
    CheckResidence: TCheckBox;
    Button2: TButton;
    ComboBox1: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btRefershClick(Sender: TObject);
    procedure btSearchClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    XMLDocument: TXMLDocument;
    FTree: TGEDCOMTree;
    FMapPoints: TObjectList;
    FPlaces: TObjectList;

    procedure DoGeocodingRequest(SearchString: String; aPoints: TObjectList);
    function DoReverseGeocodingRequest(Latitude: Double; Longitude: Double): string;
    procedure FillGeocodingListView(Points: TObjectList);
    procedure PreparePointsList(aPoints: TObjectList);
  public
    property Tree: TGEDCOMTree read FTree write FTree;
  end;

var
  fmMaps: TfmMaps;

implementation

uses GKCommon;

{$R *.dfm}

procedure TfmMaps.FormCreate(Sender: TObject);
begin
  FMapPoints := TObjectList.Create(True);
  FPlaces := TObjectList.Create(True);

  DecimalSeparator := '.';
  XMLDocument := TXMLDocument.Create(Self);
  btRefersh.Click;
end;

procedure TfmMaps.FormDestroy(Sender: TObject);
begin
  FPlaces.Destroy;
  FMapPoints.Destroy;
end;

// отправка запроса и получение ответа
function GetInetFile(const FileURL: String; Stream: TMemoryStream): Boolean;
const
  BufferSize = 1024;
var
  hSession, hURL: HInternet;
  Buffer: array [1..BufferSize] of Byte;
  BufferLen: DWORD;
  sAppName: string;
  Utf8FileUrl: UTF8String; // Гугл принимает и отдает UTF8 кодировку!!!
  Headers: String;
begin
  // переводим в принимаемую Гуглем кодировку
  Utf8FileUrl := AnsiToUtf8(FileURL);
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
    InternetCloseHandle(hSession)
  end;
end;

// замена данных в потоке с кодировки 1251 на UTF-8
function StreamToUtf8Stream(Stream: TStream): UTF8String;
var
  s: String;
begin
  SetLength(s, Stream.Size);
  Stream.Seek(0, 0);
  Stream.Read(s[1], Stream.Size);
  Result := AnsiToUtf8(s);
  Stream.Size := 0;
  Stream.Write(Result[1], Length(Result));
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
    FileOnNet := 'http://maps.google.com/staticmap?center=%.6f,%.6f&zoom=%d&size=640x640'
      +'&markers=%.6f,%.6f,blues'
      +'&maptype=mobile&key=ABQIAAAAIcIQgkzLQ27NamNDh2wULxTh9o9-e_HqfKVqUrQPniGEP9J6uhSJmXGEipvip6lxpu_ZXrXaeHwWgQ';
    FileOnNet := Format(FileOnNet, [Latitude, Longitude, Scale, Latitude, Longitude]);
    // получение потока с данными ответа
    if GetInetFile(FileOnNet, Stream) = True then begin
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
    FileOnNet := 'http://maps.google.com/staticmap?size=640x640'
      +'&markers=%s'
      +'&maptype=mobile&key=ABQIAAAAIcIQgkzLQ27NamNDh2wULxTh9o9-e_HqfKVqUrQPniGEP9J6uhSJmXGEipvip6lxpu_ZXrXaeHwWgQ';
    FileOnNet := Format(FileOnNet, [Markers]);
    // получение потока с данными ответа
    if GetInetFile(FileOnNet, Stream) = True then begin
      // создаем графический объект
      Stream.Position := 0;
      Result := TOleGraphic.Create;
      Result.LoadFromStream(Stream);
    end else Result := nil;
  finally
    Stream.Free;
  end;
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
    FileOnNet := 'http://maps.google.com/maps/geo?ll=%.6f,%.6f&output=xml&key=abcdefg&gl=ru';
    FileOnNet := Format(FileOnNet, [Latitude, Longitude]);
    // получение потока с данными ответа
    if GetInetFile(FileOnNet, Stream) = True then begin
      StreamToUtf8Stream(Stream);
      // заполняем XMLDocument
      XMLDocument.LoadFromStream(Stream);

      Node := XMLDocument.DocumentElement;
      FillNode(Node);
    end;
  finally
    Stream.Free;
  end;
end;

// обработка нажатия кнопки "Обновить"
procedure TfmMaps.btRefershClick(Sender: TObject);
var
  pt: TMapPoint;
begin
  pt := TMapPoint.Create;
  pt.Lat := StrToFloat(edLatitude.Text);
  pt.Lon := StrToFloat(edLongitude.Text);
  pt.Address := DoReverseGeocodingRequest(pt.Lat, pt.Lon);
  FMapPoints.Add(pt);

  PreparePointsList(FMapPoints);
end;

procedure TfmMaps.PreparePointsList(aPoints: TObjectList);
var
  OleGraphic: TOleGraphic;
begin
  try
    // заполнение списка точек
    FillGeocodingListView(aPoints);
    // запрос карты
    OleGraphic := GetMap(aPoints);
    if (OleGraphic <> nil) then begin
      // рисуем карту
      ImageGeocoding.Picture.Assign(OleGraphic);
      OleGraphic.Free;
    end;
  finally
  end;
end;

// нажатие кнопки поиска геокодинга
procedure TfmMaps.btSearchClick(Sender: TObject);
var
  SearchString: String;
begin
  SearchString := StringReplace(Trim(edSearch.Text), ' ', '+', [rfReplaceAll]);
  DoGeocodingRequest(SearchString, FMapPoints);
  PreparePointsList(FMapPoints);
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
    FileOnNet := 'http://maps.google.com/maps/geo?q=%s&output=xml&key=abcdefg&gl=ru';
    FileOnNet := Format(FileOnNet,[SearchString]);
    // получение потока с данными ответа
    if GetInetFile(FileOnNet,Stream) = True then begin
      // кому надо - расскоментировать и смотреть содержимое при отладке
      //Stream.SaveToFile('c: \geo.xml');

      StreamToUtf8Stream(Stream);
      // заполняем XMLDocument
      XMLDocument.LoadFromStream(Stream);
      // формируем содержимое списка точек
      Node := XMLDocument.DocumentElement;
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
            if (AddressNode<>nil) and (PointNode<>nil) then begin
              Point := TMapPoint.Create;
              // получаем адрес
              Point.Address := AddressNode.Text;
              // получаем координаты
              sCoordinates := PointNode.Text;
              // разбираем координаты
              ExtractStrings([','],[],PChar(sCoordinates),StringList);
              if StringList.Count>1 then begin
                // Формируем точку
                Point.Lon := StrToFloatDef(StringList[0],-1);
                Point.Lat := StrToFloatDef(StringList[1],-1);
                // добавляем точку в список 
                if (Point.Lat<>-1) and (Point.Lon<>-1) then
                  aPoints.Add(Point);
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

procedure TfmMaps.Button1Click(Sender: TObject);
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
  //fam: TGEDCOMFamilyRecord;
  //f_ev: TGEDCOMFamilyEvent;
begin
  TreeView1.Items.BeginUpdate;
  TreeView1.Items.Clear;

  FPlaces.Clear;

  root := TreeView1.Items.AddChild(nil, 'Места');

  ProgressBar1.Min := 0;
  ProgressBar1.Max := FTree.Count;

  for i := 0 to FTree.Count - 1 do begin
    rec := FTree.Records[i];

    if (rec is TGEDCOMIndividualRecord) then begin
      ind := rec as TGEDCOMIndividualRecord;

      for k := 0 to ind.IndividualEventsCount - 1 do begin
        i_ev := ind.IndividualEvents[k];
        if (i_ev.Detail.Place <> '')
        then AddPlace(i_ev.Detail.Place, i_ev);
      end;

      for k := 0 to ind.IndividualAttributesCount - 1 do begin
        i_att := ind.IndividualAttributes[k];

        if (i_att.Name = 'RESI') and (i_att.StringValue <> '')
        then AddPlace(i_att.StringValue, i_att);
      end;
    end;

    ProgressBar1.Position := i + 1;
  end;

  TreeView1.AlphaSort(True);
  TreeView1.Items[0].Expand(True);
  TreeView1.Items.EndUpdate;
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

procedure TfmMaps.Button2Click(Sender: TObject);

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
begin
  cond := [];
  if CheckBirth.Checked then Include(cond, pcBirth);
  if CheckDeath.Checked then Include(cond, pcDeath);
  if CheckResidence.Checked then Include(cond, pcResidence);

  FMapPoints.Clear;

  for i := 0 to FPlaces.Count - 1 do begin
    place := TPlace(FPlaces[i]);
    if (place.Points.Count < 1) then Continue;

    for k := 0 to place.Refs.Count - 1 do begin
      ref := TGEDCOMObject(place.Refs[k]);

      if (ref is TGEDCOMIndividualEvent) then begin
        if (pcBirth in cond) and (TGEDCOMIndividualEvent(ref).Name = 'BIRT')
        then CopyPoint(TMapPoint(place.Points[0]))
        else
        if (pcDeath in cond) and (TGEDCOMIndividualEvent(ref).Name = 'DEAT')
        then CopyPoint(TMapPoint(place.Points[0]));
      end
      else
      if (ref is TGEDCOMIndividualAttribute) then begin
        if (pcResidence in cond) and (TGEDCOMIndividualAttribute(ref).Name = 'RESI')
        then CopyPoint(TMapPoint(place.Points[0]));
      end;
    end;
  end;

  PreparePointsList(FMapPoints); 
end;

end.
