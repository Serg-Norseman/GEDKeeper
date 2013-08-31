using System;
using System.Windows.Forms;

using Ext.Utils;
using GedCom551;
using GKCore;
using GKUI.Controls;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKUI
{
	public partial class TfmLocationEdit : Form
	{
		private TfmBase FBase;
		private TGEDCOMLocationRecord FLocationRecord;
		private GKMapBrowser FMapBrowser;
		private GKSheetList FMediaList;
		private GKSheetList FNotesList;
		private TList FSearchPoints;

		public TfmBase Base
		{
			get { return this.FBase; }
		}

		public TGEDCOMLocationRecord LocationRecord
		{
			get { return this.FLocationRecord; }
			set { this.SetLocationRecord(value); }
		}

		private void ControlsRefresh()
		{
			this.Base.RecListNotesRefresh(this.FLocationRecord, this.FNotesList.List, null);
			this.Base.RecListMediaRefresh(this.FLocationRecord, this.FMediaList.List, null);
		}

		private void ListModify(object Sender, object ItemData, TRecAction Action)
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

		private void SetLocationRecord(TGEDCOMLocationRecord Value)
		{
			this.FLocationRecord = Value;
			this.EditName.Text = this.FLocationRecord.LocationName;
			this.EditLatitude.Text = GKMapBrowser.CoordToStr(this.FLocationRecord.Map.Lati);
			this.EditLongitude.Text = GKMapBrowser.CoordToStr(this.FLocationRecord.Map.Long);
			this.ControlsRefresh();
			this.ActiveControl = this.EditName;
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
			try
			{
				this.FLocationRecord.LocationName = this.EditName.Text;
				this.FLocationRecord.Map.Lati = SysUtils.ParseFloat(this.EditLatitude.Text, 0.0);
				this.FLocationRecord.Map.Long = SysUtils.ParseFloat(this.EditLongitude.Text, 0.0);
				this.Base.ChangeRecord(this.FLocationRecord);
				base.DialogResult = DialogResult.OK;
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("TfmLocationEdit.Accept(): " + E.Message);
				base.DialogResult = DialogResult.None;
			}
		}

		private void btnSearch_Click(object sender, EventArgs e)
		{
			this.ListGeoCoords.BeginUpdate();
			this.FMapBrowser.BeginUpdate();
			try
			{
				this.FSearchPoints.Clear();
				GKMapBrowser.RequestGeoCoords(this.EditName.Text, this.FSearchPoints);
				this.ListGeoCoords.Items.Clear();
				this.FMapBrowser.ClearPoints();

				int num = this.FSearchPoints.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					if (this.FSearchPoints[i] is GKMapBrowser.TGMapPoint)
					{
						GKMapBrowser.TGMapPoint pt = this.FSearchPoints[i] as GKMapBrowser.TGMapPoint;
						GKListItem item = new GKListItem();
						item.Text = pt.Hint;
						item.Data = pt;
						item.SubItems.Add(GKMapBrowser.CoordToStr(pt.Latitude));
						item.SubItems.Add(GKMapBrowser.CoordToStr(pt.Longitude));
						this.ListGeoCoords.Items.Add(item);
						this.FMapBrowser.AddPoint(pt.Latitude, pt.Longitude, pt.Hint);
					}
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
				GKListItem item = this.ListGeoCoords.SelectedItems[0] as GKListItem;
				this.EditLatitude.Text = item.SubItems[1].Text;
				this.EditLongitude.Text = item.SubItems[2].Text;
			}
		}

		private void btnSelectName_Click(object sender, EventArgs e)
		{
			if (this.ListGeoCoords.SelectedItems.Count > 0)
			{
				this.EditName.Text = (this.ListGeoCoords.SelectedItems[0] as GKListItem).Text;
			}
		}

		private void EditName_TextChanged(object sender, EventArgs e)
		{
			this.Text = LangMan.LSList[170] + " \"" + this.EditName.Text + "\"";
		}

		private void btnShowOnMap_Click(object sender, EventArgs e)
		{
			if (this.EditLatitude.Text != "" && this.EditLongitude.Text != "")
			{
				this.FMapBrowser.SetCenter(double.Parse(this.EditLatitude.Text), double.Parse(this.EditLongitude.Text), -1);
			}
		}

		private void ListGeoCoords_Click(object sender, EventArgs e)
		{
			if (this.ListGeoCoords.SelectedItems.Count > 0)
			{
				GKListItem item = this.ListGeoCoords.SelectedItems[0] as GKListItem;
				GKMapBrowser.TGMapPoint pt = item.Data as GKMapBrowser.TGMapPoint;
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
				this.FSearchPoints.Dispose();
			}
			base.Dispose(Disposing);
		}

		public TfmLocationEdit(TfmBase aBase)
		{
			this.InitializeComponent();
			this.FBase = aBase;
			this.FMapBrowser = new GKMapBrowser();
			this.FMapBrowser.InitMap();
			this.FMapBrowser.Dock = DockStyle.Fill;
			this.panMap.Controls.Add(this.FMapBrowser);

			this.FNotesList = new GKSheetList(this.SheetNotes);
			this.FNotesList.OnModify += new GKSheetList.TModifyEvent(this.ListModify);
			this.Base.SetupRecNotesList(this.FNotesList);

			this.FMediaList = new GKSheetList(this.SheetMultimedia);
			this.FMediaList.OnModify += new GKSheetList.TModifyEvent(this.ListModify);
			this.Base.SetupRecMediaList(this.FMediaList);

			this.FSearchPoints = new TList(true);
			this.SetLang();
		}

		public void SetLang()
		{
			this.btnAccept.Text = LangMan.LSList[97];
			this.btnCancel.Text = LangMan.LSList[98];
			this.SheetCommon.Text = LangMan.LSList[144];
			this.SheetNotes.Text = LangMan.LSList[54];
			this.SheetMultimedia.Text = LangMan.LSList[55];
			this.Label1.Text = LangMan.LSList[125];
			this.Label2.Text = LangMan.LSList[171];
			this.Label3.Text = LangMan.LSList[172];
			this.ListGeoCoords.Columns[0].Text = LangMan.LSList[125];
			this.ListGeoCoords.Columns[1].Text = LangMan.LSList[171];
			this.ListGeoCoords.Columns[2].Text = LangMan.LSList[172];
			this.btnShowOnMap.Text = LangMan.LSList[173];
			this.GroupBox1.Text = LangMan.LSList[174];
			this.btnSearch.Text = LangMan.LSList[175];
			this.btnSelect.Text = LangMan.LSList[176];
			this.btnSelectName.Text = LangMan.LSList[177];
		}

	}
}
