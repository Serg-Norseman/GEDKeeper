using System;
using System.Windows.Forms;

using ExtUtils;
using GedCom551;
using GKCore;
using GKCore.Interfaces;
using GKUI.Controls;
using GKUI.Sheets;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKUI.Dialogs
{
	public partial class TfmLocationEdit : Form, IBaseEditor
	{
		private readonly IBase fBase;
		private readonly GKMapBrowser fMapBrowser;
        private readonly GKMediaSheet fMediaList;
		private readonly GKNotesSheet fNotesList;
		private readonly ExtList fSearchPoints;

        private TGEDCOMLocationRecord fLocationRecord;
        
		public TGEDCOMLocationRecord LocationRecord
		{
			get { return this.fLocationRecord; }
			set { this.SetLocationRecord(value); }
		}

		public IBase Base
		{
			get { return this.fBase; }
		}

		public TfmLocationEdit(IBase aBase)
		{
			this.InitializeComponent();
			this.fBase = aBase;
			this.fMapBrowser = new GKMapBrowser();
			this.fMapBrowser.InitMap();
			this.fMapBrowser.Dock = DockStyle.Fill;
			this.panMap.Controls.Add(this.fMapBrowser);

			this.fNotesList = new GKNotesSheet(this, this.SheetNotes);
            this.fMediaList = new GKMediaSheet(this, this.SheetMultimedia);

			this.fSearchPoints = new ExtList(true);
			this.SetLang();
		}

		protected override void Dispose(bool disposing)
		{
			if (disposing)
			{
				this.fSearchPoints.Dispose();
			}
			base.Dispose(disposing);
		}

		private void SetLocationRecord(TGEDCOMLocationRecord value)
		{
			this.fLocationRecord = value;
			this.EditName.Text = this.fLocationRecord.LocationName;
			this.EditLatitude.Text = GKMapBrowser.CoordToStr(this.fLocationRecord.Map.Lati);
			this.EditLongitude.Text = GKMapBrowser.CoordToStr(this.fLocationRecord.Map.Long);

			this.fNotesList.DataList = this.fLocationRecord.Notes.GetEnumerator();
		    this.fMediaList.DataList = this.fLocationRecord.MultimediaLinks.GetEnumerator();

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
				this.fLocationRecord.LocationName = this.EditName.Text;
				this.fLocationRecord.Map.Lati = SysUtils.ParseFloat(this.EditLatitude.Text, 0.0);
				this.fLocationRecord.Map.Long = SysUtils.ParseFloat(this.EditLongitude.Text, 0.0);
				this.fBase.ChangeRecord(this.fLocationRecord);
				base.DialogResult = DialogResult.OK;
			}
			catch (Exception ex)
			{
				this.fBase.Host.LogWrite("TfmLocationEdit.Accept(): " + ex.Message);
				base.DialogResult = DialogResult.None;
			}
		}

		private void btnSearch_Click(object sender, EventArgs e)
		{
			this.fMapBrowser.ShowLines = false;
			
			this.ListGeoCoords.BeginUpdate();
			this.fMapBrowser.BeginUpdate();
			try
			{
				this.fSearchPoints.Clear();
				GKMapBrowser.RequestGeoCoords(this.EditName.Text, this.fSearchPoints);
				this.ListGeoCoords.Items.Clear();
				this.fMapBrowser.ClearPoints();

				int num = this.fSearchPoints.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					if (this.fSearchPoints[i] is GKMapBrowser.GMapPoint)
					{
						GKMapBrowser.GMapPoint pt = this.fSearchPoints[i] as GKMapBrowser.GMapPoint;
						GKListItem item = new GKListItem();
						item.Text = pt.Hint;
						item.Data = pt;
						item.SubItems.Add(GKMapBrowser.CoordToStr(pt.Latitude));
						item.SubItems.Add(GKMapBrowser.CoordToStr(pt.Longitude));
						this.ListGeoCoords.Items.Add(item);
						this.fMapBrowser.AddPoint(pt.Latitude, pt.Longitude, pt.Hint);
					}
				}
				this.fMapBrowser.ZoomToBounds();
			}
			finally
			{
				this.fMapBrowser.EndUpdate();
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
			this.Text = LangMan.LS(LSID.LSID_Location) + " \"" + this.EditName.Text + "\"";
		}

		private void btnShowOnMap_Click(object sender, EventArgs e)
		{
			if (this.EditLatitude.Text != "" && this.EditLongitude.Text != "")
			{
				this.fMapBrowser.SetCenter(SysUtils.ParseFloat(this.EditLatitude.Text, 0), SysUtils.ParseFloat(this.EditLongitude.Text, 0), -1);
			}
		}

		private void ListGeoCoords_Click(object sender, EventArgs e)
		{
			if (this.ListGeoCoords.SelectedItems.Count > 0)
			{
				GKListItem item = this.ListGeoCoords.SelectedItems[0] as GKListItem;
				GKMapBrowser.GMapPoint pt = item.Data as GKMapBrowser.GMapPoint;
				if (pt != null)
				{
					this.fMapBrowser.SetCenter(pt.Latitude, pt.Longitude, -1);
				}
			}
		}

		public void SetLang()
		{
			this.btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
			this.btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
			this.SheetCommon.Text = LangMan.LS(LSID.LSID_Common);
			this.SheetNotes.Text = LangMan.LS(LSID.LSID_RPNotes);
			this.SheetMultimedia.Text = LangMan.LS(LSID.LSID_RPMultimedia);
			this.Label1.Text = LangMan.LS(LSID.LSID_Title);
			this.Label2.Text = LangMan.LS(LSID.LSID_Latitude);
			this.Label3.Text = LangMan.LS(LSID.LSID_Longitude);
			this.ListGeoCoords.Columns[0].Text = LangMan.LS(LSID.LSID_Title);
			this.ListGeoCoords.Columns[1].Text = LangMan.LS(LSID.LSID_Latitude);
			this.ListGeoCoords.Columns[2].Text = LangMan.LS(LSID.LSID_Longitude);
			this.btnShowOnMap.Text = LangMan.LS(LSID.LSID_Show);
			this.GroupBox1.Text = LangMan.LS(LSID.LSID_SearchCoords);
			this.btnSearch.Text = LangMan.LS(LSID.LSID_Search);
			this.btnSelect.Text = LangMan.LS(LSID.LSID_SelectCoords);
			this.btnSelectName.Text = LangMan.LS(LSID.LSID_SelectName);
		}

	}
}
