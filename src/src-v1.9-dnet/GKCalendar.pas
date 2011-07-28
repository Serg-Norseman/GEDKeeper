unit GKCalendar; {trans:fin}

{$I GEDKeeper2.inc}

interface

uses
  System.Drawing, System.ComponentModel, System.Windows.Forms,
  System.Globalization, System.Threading,
  VCLStub, GKCalendarCore, GKLangs;

type
  TfmCalendar = class(System.Windows.Forms.Form)
  strict private
    lvDates: System.Windows.Forms.ListView;
    qtc: System.Windows.Forms.MonthCalendar;
    ColumnHeader1: System.Windows.Forms.ColumnHeader;
    ColumnHeader2: System.Windows.Forms.ColumnHeader;

    procedure InitializeComponent;
    procedure qtc_DateSelected(sender: System.Object; e: System.Windows.Forms.DateRangeEventArgs);
    procedure TfmCalendar_Closed(sender: System.Object; e: System.EventArgs);
  public
    constructor Create;
  end;

implementation

uses GKMain;

procedure TfmCalendar.InitializeComponent;
type
  TArrayOfSystem_Windows_Forms_ColumnHeader = array of System.Windows.Forms.ColumnHeader;
begin
  Self.lvDates := System.Windows.Forms.ListView.Create;
  Self.ColumnHeader1 := System.Windows.Forms.ColumnHeader.Create;
  Self.ColumnHeader2 := System.Windows.Forms.ColumnHeader.Create;
  Self.qtc := System.Windows.Forms.MonthCalendar.Create;
  Self.SuspendLayout;
  // 
  // lvDates
  // 
  Self.lvDates.Columns.AddRange(TArrayOfSystem_Windows_Forms_ColumnHeader.Create(Self.ColumnHeader1, 
          Self.ColumnHeader2));
  Self.lvDates.FullRowSelect := True;
  Self.lvDates.Location := System.Drawing.Point.Create(8, 184);
  Self.lvDates.Name := 'lvDates';
  Self.lvDates.Size := System.Drawing.Size.Create(258, 121);
  Self.lvDates.TabIndex := 1;
  Self.lvDates.View := System.Windows.Forms.View.Details;
  // 
  // ColumnHeader1
  // 
  Self.ColumnHeader1.Text := 'Календарь';
  Self.ColumnHeader1.Width := 120;
  // 
  // ColumnHeader2
  // 
  Self.ColumnHeader2.Text := 'Дата';
  Self.ColumnHeader2.Width := 120;
  // 
  // qtc
  // 
  Self.qtc.FirstDayOfWeek := System.Windows.Forms.Day.Monday;
  Self.qtc.Location := System.Drawing.Point.Create(8, 8);
  Self.qtc.Name := 'qtc';
  Self.qtc.ShowWeekNumbers := True;
  Self.qtc.TabIndex := 0;
  Include(Self.qtc.DateSelected, Self.qtc_DateSelected);
  // 
  // TfmCalendar
  // 
  Self.AutoScaleBaseSize := System.Drawing.Size.Create(5, 14);
  Self.ClientSize := System.Drawing.Size.Create(274, 313);
  Self.Controls.Add(Self.qtc);
  Self.Controls.Add(Self.lvDates);
  Self.Font := System.Drawing.Font.Create('Tahoma', 8.25, System.Drawing.FontStyle.Regular, 
      System.Drawing.GraphicsUnit.Point, (Byte(204)));
  Self.FormBorderStyle := System.Windows.Forms.FormBorderStyle.FixedToolWindow;
  Self.Name := 'TfmCalendar';
  Self.ShowInTaskbar := False;
  Self.StartPosition := System.Windows.Forms.FormStartPosition.Manual;
  Self.Text := 'Календарь';
  Self.TopMost := True;
  Include(Self.Closed, Self.TfmCalendar_Closed);
  Self.ResumeLayout(False);
end;

constructor TfmCalendar.Create;
begin
  inherited Create;
  InitializeComponent;

  qtc.SelectionStart := DateTime.Now;
  qtc_DateSelected(nil, nil);

  /// SetLang
  Text := LSList[LSID_MICalendar];
  ColumnHeader1.Text := LSList[LSID_MICalendar];
  ColumnHeader2.Text := LSList[LSID_Date];
end;

procedure TfmCalendar.qtc_DateSelected(sender: System.Object; e: System.Windows.Forms.DateRangeEventArgs);

  procedure AddItem(aCalendar, aDate: string);
  var
    item: System.Windows.Forms.ListViewItem;
  begin
    item := lvDates.Items.Add(aCalendar);
    item.SubItems.Add(aDate);
  end;

var
  gdt: DateTime;
  jd: Double;
  year, month, day, major, cycle: Longint;
  s: string;
  DateTimeInfo: System.Globalization.DateTimeFormatInfo;
  cal: TCalendarConv;
begin
  DateTimeInfo := System.Threading.Thread.CurrentThread.CurrentCulture.DateTimeFormat;

  cal := TCalendarConv.Create;
  lvDates.BeginUpdate;
  try
    lvDates.Items.Clear;

    gdt := qtc.SelectionStart;

    s := cal.date_to_str(gdt.Year, gdt.Month, gdt.Day, AD) + ', ' + DateTimeInfo.DayNames[Ord(gdt.DayOfWeek)];
    AddItem(LSList[LSID_Cal_Gregorian], s);

    jd := cal.gregorian_to_jd(gdt.Year, gdt.Month, gdt.Day);
    cal.jd_to_julian(jd, year, month, day);
    AddItem(LSList[LSID_Cal_Julian], cal.date_to_str(year, month, day, AD));

    cal.jd_to_hebrew(jd, year, month, day);
    s := day.ToString() + ' ';
    s := s + TCalendarConv.HebrewMonths[month];
    s := s + ' ' + year.ToString() + ', ' + TCalendarConv.HebrewWeekdays[cal.jwday(jd)];
    AddItem(LSList[LSID_Cal_Hebrew], s);

    cal.jd_to_islamic(jd, year, month, day);
    s := day.ToString() + ' ';
    s := s + TCalendarConv.IslamicMonths[month];
    s := s + ' ' + year.ToString() + ', йаум ' + TCalendarConv.IslamicWeekdays[cal.jwday(jd)];
    AddItem(LSList[LSID_Cal_Islamic], s);

    cal.jd_to_persian(jd, year, month, day);
    s := day.ToString() + ' ';
    s := s + TCalendarConv.PersianMonths[month];
    s := s + ' ' + year.ToString() + ', ' + TCalendarConv.PersianWeekdays[cal.jwday(jd)];
    AddItem(LSList[LSID_Cal_Persian], s);

    cal.jd_to_indian_civil(jd, year, month, day);
    s := day.ToString() + ' ';
    s := s + TCalendarConv.IndianCivilMonths[month];
    s := s + ' ' + year.ToString() + ', ' + TCalendarConv.IndianCivilWeekdays[cal.jwday(jd)];
    AddItem(LSList[LSID_Cal_Indian], s);

    cal.jd_to_bahai(jd, major, cycle, year, month, day);
    s := 'Кулл-и Шай'' ' + major.ToString() + ', Вахид ' + cycle.ToString() + ', ';
    s := s + day.ToString() + ' ';
    s := s + TCalendarConv.BahaiMonths[month];
    s := s + ' ' + year.ToString() + ', ' + TCalendarConv.BahaiWeekdays[cal.jwday(jd)];
    AddItem(LSList[LSID_Cal_Bahai], s);
  finally
    lvDates.EndUpdate;
    cal.Free;
  end;
end;

procedure TfmCalendar.TfmCalendar_Closed(sender: System.Object; e: System.EventArgs);
begin
  fmGEDKeeper.miCalendar.Checked := False;
  fmGEDKeeper.fmCalendar := nil;
end;

end.
