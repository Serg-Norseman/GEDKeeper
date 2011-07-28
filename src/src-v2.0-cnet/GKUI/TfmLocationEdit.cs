using GedCom551;
using GKCore;
using GKSys;
using GKUI.Controls;
using GKUI.Lists;
using System;
using System.ComponentModel;
using System.Drawing;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Windows.Forms;

namespace GKUI
{
	public class TfmLocationEdit : Form
	{
		private Button btnAccept;
		private Button btnCancel;
		private TabControl PagesData;
		private TabPage SheetNotes;
		private TabPage SheetMultimedia;
		private TabPage SheetCommon;
		private Label Label1;
		private TextBox EditName;
		private Label Label2;
		private TextBox EditLatitude;
		private Label Label3;
		private TextBox EditLongitude;
		private GroupBox GroupBox1;
		private ListView ListGeoCoords;
		private Button btnSearch;
		private Button btnSelect;
		private Button btnSelectName;
		private Button btnShowOnMap;
		private Panel panMap;
		private ColumnHeader ColumnHeader1;
		private ColumnHeader ColumnHeader2;
		private ColumnHeader ColumnHeader3;
		private TfmBase FBase;
		private TGEDCOMLocationRecord FLocationRecord;
		private TMapBrowser FMapBrowser;
		private TSheetList FMediaList;
		private TSheetList FNotesList;
		private TObjectList FSearchPoints;

		[Browsable(false)]
		public TfmBase Base
		{
			get
			{
				return this.FBase;
			}
		}
		[Browsable(false)]
		public TGEDCOMLocationRecord LocationRecord
		{
			get
			{
				return this.FLocationRecord;
			}
			set
			{
				this.SetLocationRecord(value);
			}
		}
		private void ControlsRefresh()
		{
			this.Base.RecListNotesRefresh(this.FLocationRecord, this.FNotesList.List, null);
			this.Base.RecListMediaRefresh(this.FLocationRecord, this.FMediaList.List, null);
		}
		private void ListModify(object Sender, object ItemData, TGenEngine.TRecAction Action)
		{
			if (object.Equals(Sender, this.FNotesList))
			{
				if (this.Base.ModifyRecNote(this, this.FLocationRecord, ItemData as TGEDCOMNotes, Action))
				{
					this.ControlsRefresh();
				}
			}
			else
			{
				if (object.Equals(Sender, this.FMediaList) && this.Base.ModifyRecMultimedia(this, this.FLocationRecord, ItemData as TGEDCOMMultimediaLink, Action))
				{
					this.ControlsRefresh();
				}
			}
		}
		private void SetLocationRecord([In] TGEDCOMLocationRecord Value)
		{
			this.FLocationRecord = Value;
			this.EditName.Text = this.FLocationRecord.Name;
			this.EditLatitude.Text = this.FLocationRecord.Map.Lati;
			this.EditLongitude.Text = this.FLocationRecord.Map.Long;
			this.ControlsRefresh();
		}
		private void InitializeComponent()
		{
			this.btnAccept = new Button();
			this.btnCancel = new Button();
			this.PagesData = new TabControl();
			this.SheetCommon = new TabPage();
			this.Label1 = new Label();
			this.Label2 = new Label();
			this.Label3 = new Label();
			this.EditName = new TextBox();
			this.EditLatitude = new TextBox();
			this.EditLongitude = new TextBox();
			this.GroupBox1 = new GroupBox();
			this.ListGeoCoords = new ListView();
			this.ColumnHeader1 = new ColumnHeader();
			this.ColumnHeader2 = new ColumnHeader();
			this.ColumnHeader3 = new ColumnHeader();
			this.btnSearch = new Button();
			this.btnSelect = new Button();
			this.btnSelectName = new Button();
			this.panMap = new Panel();
			this.btnShowOnMap = new Button();
			this.SheetNotes = new TabPage();
			this.SheetMultimedia = new TabPage();
			this.PagesData.SuspendLayout();
			this.SheetCommon.SuspendLayout();
			this.GroupBox1.SuspendLayout();
			base.SuspendLayout();
			this.btnAccept.ImageAlign = ContentAlignment.MiddleLeft;
			this.btnAccept.Location = new Point(384, 440);
			this.btnAccept.Name = "btnAccept";
			this.btnAccept.Size = new Size(81, 25);
			this.btnAccept.TabIndex = 2;
			this.btnAccept.Text = "Принять";
			this.btnAccept.TextAlign = ContentAlignment.MiddleRight;
			this.btnAccept.Click += new EventHandler(this.btnAccept_Click);
			this.btnCancel.DialogResult = DialogResult.Cancel;
			this.btnCancel.ImageAlign = ContentAlignment.MiddleLeft;
			this.btnCancel.Location = new Point(472, 440);
			this.btnCancel.Name = "btnCancel";
			this.btnCancel.Size = new Size(81, 25);
			this.btnCancel.TabIndex = 1;
			this.btnCancel.Text = "Отменить";
			this.btnCancel.TextAlign = ContentAlignment.MiddleRight;
			this.PagesData.Controls.Add(this.SheetCommon);
			this.PagesData.Controls.Add(this.SheetNotes);
			this.PagesData.Controls.Add(this.SheetMultimedia);
			this.PagesData.Location = new Point(0, 0);
			this.PagesData.Name = "PagesData";
			this.PagesData.SelectedIndex = 0;
			this.PagesData.Size = new Size(561, 425);
			this.PagesData.TabIndex = 0;
			this.SheetCommon.Controls.Add(this.Label1);
			this.SheetCommon.Controls.Add(this.Label2);
			this.SheetCommon.Controls.Add(this.Label3);
			this.SheetCommon.Controls.Add(this.EditName);
			this.SheetCommon.Controls.Add(this.EditLatitude);
			this.SheetCommon.Controls.Add(this.EditLongitude);
			this.SheetCommon.Controls.Add(this.GroupBox1);
			this.SheetCommon.Controls.Add(this.btnShowOnMap);
			this.SheetCommon.Location = new Point(4, 22);
			this.SheetCommon.Name = "SheetCommon";
			this.SheetCommon.Size = new Size(553, 399);
			this.SheetCommon.TabIndex = 0;
			this.SheetCommon.Text = "Основное";
			this.Label1.Location = new Point(16, 8);
			this.Label1.Name = "Label1";
			this.Label1.Size = new Size(60, 13);
			this.Label1.TabIndex = 0;
			this.Label1.Text = "Название";
			this.Label2.Location = new Point(287, 8);
			this.Label2.Name = "Label2";
			this.Label2.Size = new Size(50, 13);
			this.Label2.TabIndex = 1;
			this.Label2.Text = "Широта";
			this.Label3.Location = new Point(375, 8);
			this.Label3.Name = "Label3";
			this.Label3.Size = new Size(50, 13);
			this.Label3.TabIndex = 2;
			this.Label3.Text = "Долгота";
			this.EditName.Location = new Point(16, 24);
			this.EditName.Name = "EditName";
			this.EditName.Size = new Size(265, 21);
			this.EditName.TabIndex = 0;
			this.EditName.Text = "";
			this.EditName.KeyDown += new KeyEventHandler(this.EditName_KeyDown);
			this.EditName.TextChanged += new EventHandler(this.EditName_TextChanged);
			this.EditLatitude.Location = new Point(287, 24);
			this.EditLatitude.Name = "EditLatitude";
			this.EditLatitude.Size = new Size(81, 21);
			this.EditLatitude.TabIndex = 1;
			this.EditLatitude.Text = "";
			this.EditLongitude.Location = new Point(375, 24);
			this.EditLongitude.Name = "EditLongitude";
			this.EditLongitude.Size = new Size(81, 21);
			this.EditLongitude.TabIndex = 2;
			this.EditLongitude.Text = "";
			this.GroupBox1.Controls.Add(this.ListGeoCoords);
			this.GroupBox1.Controls.Add(this.btnSearch);
			this.GroupBox1.Controls.Add(this.btnSelect);
			this.GroupBox1.Controls.Add(this.btnSelectName);
			this.GroupBox1.Controls.Add(this.panMap);
			this.GroupBox1.Location = new Point(0, 51);
			this.GroupBox1.Name = "GroupBox1";
			this.GroupBox1.Size = new Size(553, 346);
			this.GroupBox1.TabIndex = 3;
			this.GroupBox1.TabStop = false;
			this.GroupBox1.Text = "Поиск координат (Google Maps)";
			ListView.ColumnHeaderCollection arg_714_0 = this.ListGeoCoords.Columns;
			ColumnHeader[] array = null;
			ColumnHeader[] array2 = array;
			ColumnHeader[] array3;
			ColumnHeader[] expr_6DD = array3 = new ColumnHeader[3];
			if (array2 != null)
			{
				int num;
				if ((num = array2.Length) > 3)
				{
					num = 3;
				}
				if (num > 0)
				{
					Array.Copy(array2, array3, num);
				}
			}
			array = expr_6DD;
			array[0] = this.ColumnHeader1;
			array[1] = this.ColumnHeader2;
			array[2] = this.ColumnHeader3;
			arg_714_0.AddRange(array);
			this.ListGeoCoords.FullRowSelect = true;
			this.ListGeoCoords.Location = new Point(16, 16);
			this.ListGeoCoords.Name = "ListGeoCoords";
			this.ListGeoCoords.Size = new Size(402, 89);
			this.ListGeoCoords.TabIndex = 0;
			this.ListGeoCoords.View = View.Details;
			this.ListGeoCoords.Click += new EventHandler(this.ListGeoCoords_Click);
			this.ColumnHeader1.Text = "Название";
			this.ColumnHeader1.Width = 200;
			this.ColumnHeader2.Text = "Широта";
			this.ColumnHeader2.Width = 80;
			this.ColumnHeader3.Text = "Долгота";
			this.ColumnHeader3.Width = 80;
			this.btnSearch.Location = new Point(429, 16);
			this.btnSearch.Name = "btnSearch";
			this.btnSearch.Size = new Size(105, 25);
			this.btnSearch.TabIndex = 1;
			this.btnSearch.Text = "Поиск";
			this.btnSearch.Click += new EventHandler(this.btnSearch_Click);
			this.btnSelect.Location = new Point(429, 48);
			this.btnSelect.Name = "btnSelect";
			this.btnSelect.Size = new Size(105, 25);
			this.btnSelect.TabIndex = 2;
			this.btnSelect.Text = "Выбрать коорд.";
			this.btnSelect.Click += new EventHandler(this.btnSelect_Click);
			this.btnSelectName.Location = new Point(429, 80);
			this.btnSelectName.Name = "btnSelectName";
			this.btnSelectName.Size = new Size(105, 25);
			this.btnSelectName.TabIndex = 3;
			this.btnSelectName.Text = "Выбрать название";
			this.btnSelectName.Click += new EventHandler(this.btnSelectName_Click);
			this.panMap.Location = new Point(2, 111);
			this.panMap.Name = "panMap";
			this.panMap.Size = new Size(549, 233);
			this.panMap.TabIndex = 4;
			this.btnShowOnMap.AccessibleDescription = "Показать на карте";
			this.btnShowOnMap.Location = new Point(464, 24);
			this.btnShowOnMap.Name = "btnShowOnMap";
			this.btnShowOnMap.Size = new Size(70, 21);
			this.btnShowOnMap.TabIndex = 4;
			this.btnShowOnMap.Text = "Показать";
			this.btnShowOnMap.Click += new EventHandler(this.btnShowOnMap_Click);
			this.SheetNotes.Location = new Point(4, 22);
			this.SheetNotes.Name = "SheetNotes";
			this.SheetNotes.Size = new Size(553, 399);
			this.SheetNotes.TabIndex = 1;
			this.SheetNotes.Text = "Заметки";
			this.SheetMultimedia.Location = new Point(4, 22);
			this.SheetMultimedia.Name = "SheetMultimedia";
			this.SheetMultimedia.Size = new Size(553, 399);
			this.SheetMultimedia.TabIndex = 2;
			this.SheetMultimedia.Text = "Мультимедиа";
			base.AcceptButton = this.btnAccept;
			this.AutoScaleBaseSize = new Size(5, 14);
			base.CancelButton = this.btnCancel;
			base.ClientSize = new Size(561, 473);
			base.Controls.Add(this.PagesData);
			base.Controls.Add(this.btnAccept);
			base.Controls.Add(this.btnCancel);
			this.Font = new Font("Tahoma", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 204);
			base.FormBorderStyle = FormBorderStyle.FixedDialog;
			base.MaximizeBox = false;
			base.MinimizeBox = false;
			base.Name = "TfmLocationEdit";
			base.ShowInTaskbar = false;
			base.StartPosition = FormStartPosition.CenterScreen;
			this.Text = "Местоположение";
			this.PagesData.ResumeLayout(false);
			this.SheetCommon.ResumeLayout(false);
			this.GroupBox1.ResumeLayout(false);
			base.ResumeLayout(false);
		}
		private void EditName_KeyDown(object sender, KeyEventArgs e)
		{
			if (e.KeyCode == Keys.Down && e.Control)
			{
				this.EditName.Text = this.EditName.Text.ToLower();
			}
		}
		private void btnAccept_Click(object sender, EventArgs e)
		{
			this.FLocationRecord.Name = this.EditName.Text;
			this.FLocationRecord.Map.Lati = this.EditLatitude.Text;
			this.FLocationRecord.Map.Long = this.EditLongitude.Text;
			this.Base.ChangeRecord(this.FLocationRecord);
			base.DialogResult = DialogResult.OK;
		}
		private void btnSearch_Click(object sender, EventArgs e)
		{
			this.ListGeoCoords.BeginUpdate();
			this.FMapBrowser.BeginUpdate();
			try
			{
				this.FSearchPoints.Clear();
				TMapBrowser.RequestGeoCoords(this.EditName.Text, this.FSearchPoints);
				this.ListGeoCoords.Items.Clear();
				this.FMapBrowser.ClearPoints();
				int arg_66_0 = 0;
				int num = this.FSearchPoints.Count - 1;
				int i = arg_66_0;
				if (num >= i)
				{
					num++;
					do
					{
						if (this.FSearchPoints[i] is TMapBrowser.TGMapPoint)
						{
							TMapBrowser.TGMapPoint pt = this.FSearchPoints[i] as TMapBrowser.TGMapPoint;
							TExtListItem item = new TExtListItem();
							item.Text = pt.Hint;
							item.Data = pt;
							item.SubItems.Add(pt.Latitude.ToString("0.000000"));
							item.SubItems.Add(pt.Longitude.ToString("0.000000"));
							this.ListGeoCoords.Items.Add(item);
							this.FMapBrowser.AddPoint(pt.Latitude, pt.Longitude, pt.Hint);
						}
						i++;
					}
					while (i != num);
				}
				this.FMapBrowser.ZoomToBounds();
			}
			finally
			{
				this.FMapBrowser.EndUpdate();
				this.ListGeoCoords.EndUpdate();
			}
		}
		private void btnSelect_Click(object sender, EventArgs e)
		{
			if (this.ListGeoCoords.SelectedItems.Count > 0)
			{
				TExtListItem item = this.ListGeoCoords.SelectedItems[0] as TExtListItem;
				this.EditLatitude.Text = item.SubItems[1].Text;
				this.EditLongitude.Text = item.SubItems[2].Text;
			}
		}
		private void btnSelectName_Click(object sender, EventArgs e)
		{
			if (this.ListGeoCoords.SelectedItems.Count > 0)
			{
				this.EditName.Text = (this.ListGeoCoords.SelectedItems[0] as TExtListItem).Text;
			}
		}
		private void EditName_TextChanged(object sender, EventArgs e)
		{
			this.Text = GKL.LSList[170] + " \"" + this.EditName.Text + "\"";
		}

		private void btnShowOnMap_Click(object sender, EventArgs e)
		{
			if (BDSSystem.WStrCmp(this.EditLatitude.Text, "") != 0 && BDSSystem.WStrCmp(this.EditLongitude.Text, "") != 0)
			{
				this.FMapBrowser.SetCenter(double.Parse(this.EditLatitude.Text), double.Parse(this.EditLongitude.Text), -1);
			}
		}

		private void ListGeoCoords_Click(object sender, EventArgs e)
		{
			if (this.ListGeoCoords.SelectedItems.Count > 0)
			{
				TExtListItem item = this.ListGeoCoords.SelectedItems[0] as TExtListItem;
				TMapBrowser.TGMapPoint pt = item.Data as TMapBrowser.TGMapPoint;
				if (pt != null)
				{
					this.FMapBrowser.SetCenter(pt.Latitude, pt.Longitude, -1);
				}
			}
		}
		protected override void Dispose(bool Disposing)
		{
			if (Disposing)
			{
				this.FSearchPoints.Free();
			}
			base.Dispose(Disposing);
		}
		public TfmLocationEdit(TfmBase aBase)
		{
			this.InitializeComponent();
			this.FBase = aBase;
			this.FMapBrowser = new TMapBrowser();
			this.FMapBrowser.InitMap();
			this.FMapBrowser.Dock = DockStyle.Fill;
			this.panMap.Controls.Add(this.FMapBrowser);
			this.FNotesList = new TSheetList(this.SheetNotes);
			this.FNotesList.set_OnModify(new TSheetList.TModifyEvent(this.ListModify));
			this.Base.SetupRecNotesList(this.FNotesList);
			this.FMediaList = new TSheetList(this.SheetMultimedia);
			this.FMediaList.set_OnModify(new TSheetList.TModifyEvent(this.ListModify));
			this.Base.SetupRecMediaList(this.FMediaList);
			this.FSearchPoints = new TObjectList(true);
			this.SetLang();
		}

		public void SetLang()
		{
			this.btnAccept.Text = GKL.LSList[97];
			this.btnCancel.Text = GKL.LSList[98];
			this.SheetCommon.Text = GKL.LSList[144];
			this.SheetNotes.Text = GKL.LSList[54];
			this.SheetMultimedia.Text = GKL.LSList[55];
			this.Label1.Text = GKL.LSList[125];
			this.Label2.Text = GKL.LSList[171];
			this.Label3.Text = GKL.LSList[172];
			this.ListGeoCoords.Columns[0].Text = GKL.LSList[125];
			this.ListGeoCoords.Columns[1].Text = GKL.LSList[171];
			this.ListGeoCoords.Columns[2].Text = GKL.LSList[172];
			this.btnShowOnMap.Text = GKL.LSList[173];
			this.GroupBox1.Text = GKL.LSList[174];
			this.btnSearch.Text = GKL.LSList[175];
			this.btnSelect.Text = GKL.LSList[176];
			this.btnSelectName.Text = GKL.LSList[177];
		}

	}
}
