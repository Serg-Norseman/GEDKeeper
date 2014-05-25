using System;
using System.Windows.Forms;

using ExtUtils;
using GedCom551;
using GKCore;
using GKCore.Interfaces;
using GKUI.Controls;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKUI
{
    public sealed partial class TfmMaps : Form, ILocalization
	{
		private class TPlaceRef
		{
			public DateTime Date;
			public TGEDCOMCustomEvent Event;
		}

		private class MapPlace : IDisposable
		{
			public string Name;
			public readonly ExtList Points;
			public readonly ExtList PlaceRefs;

            private bool fDisposed;

            public MapPlace()
			{
				this.Points = new ExtList(true);
				this.PlaceRefs = new ExtList(false);
			}

			public void Dispose()
			{
				if (!this.fDisposed)
				{
					this.PlaceRefs.Dispose();
					this.Points.Dispose();
					this.fDisposed = true;
				}
			}
		}

		private readonly TreeNode fBaseRoot;
		private readonly GKMapBrowser fMapBrowser;
		private readonly ExtList fMapPoints;
		private readonly ExtList fPlaces;
		private readonly ExtList fSelectedPersons;
		private readonly IBase fBase;
		private readonly TGEDCOMTree fTree;

		private bool IsSelected(TGEDCOMRecord iRec)
		{
			bool res = (this.fSelectedPersons == null || (this.fSelectedPersons != null && this.fSelectedPersons.IndexOf(iRec) >= 0));
			return res;
		}
		
		private void PlacesLoad()
		{
			this.ComboPersons.BeginUpdate();
			this.TreePlaces.BeginUpdate();
			this.fBase.ProgressInit(LangMan.LS(LSID.LSID_LoadingLocations), this.fTree.RecordsCount);
			try
			{
				this.fPlaces.Clear();
				this.ComboPersons.Items.Clear();
				this.ComboPersons.Sorted = false;
				this.ComboPersons.Items.Add(new GKComboItem(LangMan.LS(LSID.LSID_NotSelected), null));

				int num = this.fTree.RecordsCount - 1;
				for (int i = 0; i <= num; i++) {
					TGEDCOMRecord rec = this.fTree[i];
					bool res = rec is TGEDCOMIndividualRecord && this.IsSelected(rec);

					if (res) {
						TGEDCOMIndividualRecord ind = rec as TGEDCOMIndividualRecord;
						int p_cnt = 0;

						int num2 = ind.IndividualEvents.Count - 1;
						for (int j = 0; j <= num2; j++)
						{
							TGEDCOMCustomEvent ev = ind.IndividualEvents[j];
							if (ev.Detail.Place.StringValue != "") {
								AddPlace(ev.Detail.Place, ev);
								p_cnt++;
							}
						}

						if (p_cnt > 0) {
							this.ComboPersons.Items.Add(new GKComboItem(ind.aux_GetNameStr(true, false) + " [" + p_cnt.ToString() + "]", ind));
						}
					}

					this.fBase.ProgressStep();
				}

				this.fBaseRoot.ExpandAll();
				this.ComboPersons.Sorted = true;
			}
			finally
			{
				this.fBase.ProgressDone();
				this.TreePlaces.EndUpdate();
				this.ComboPersons.EndUpdate();
			}
		}

		private void PreparePointsList(ExtList aPoints, bool byPerson)
		{
			this.fMapBrowser.BeginUpdate();
			try
			{
				this.fMapBrowser.ClearPoints();
				int num = aPoints.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					GKMapBrowser.GMapPoint pt = aPoints[i] as GKMapBrowser.GMapPoint;
					string stHint = pt.Hint;
					if (byPerson)
					{
						stHint = stHint + " [" + pt.Date.ToString() + "]";
					}
					this.fMapBrowser.AddPoint(pt.Latitude, pt.Longitude, stHint);
				}
				this.fMapBrowser.ZoomToBounds();
			}
			finally
			{
				this.fMapBrowser.EndUpdate();
			}
		}

		private void radTotal_Click(object sender, EventArgs e)
		{
			this.chkBirth.Enabled = this.radTotal.Checked;
			this.chkDeath.Enabled = this.radTotal.Checked;
			this.chkResidence.Enabled = this.radTotal.Checked;
			this.ComboPersons.Enabled = this.radSelected.Checked;
			this.chkLinesVisible.Enabled = this.radSelected.Checked;
		}

		private void TfmMaps_KeyDown(object sender, KeyEventArgs e)
		{
			if (e.KeyCode == Keys.Escape) base.Close();
		}

		private void btnSaveImage_Click(object sender, EventArgs e)
		{
			if (this.SaveDialog1.ShowDialog() == DialogResult.OK)
			{
				this.fMapBrowser.SaveSnapshot(this.SaveDialog1.FileName);
			}
		}

		private void btnSelectPlaces_Click(object sender, EventArgs e)
		{
			TGEDCOMIndividualRecord ind = null;

			bool condBirth = false;
			bool condDeath = false;
			bool condResidence = false;

			if (this.radTotal.Checked) {
				condBirth = this.chkBirth.Checked;
				condDeath = this.chkDeath.Checked;
				condResidence = this.chkResidence.Checked;
			} else if (this.radSelected.Checked) {
				if (this.ComboPersons.SelectedIndex >= 0)
				{
					ind = ((this.ComboPersons.Items[this.ComboPersons.SelectedIndex] as GKComboItem).Data as TGEDCOMIndividualRecord);
				}
			}

			this.fMapBrowser.ShowLines = (ind != null && this.chkLinesVisible.Checked);
			this.fMapPoints.Clear();

			int num = this.fPlaces.Count - 1;
			for (int i = 0; i <= num; i++)
			{
                MapPlace place = this.fPlaces[i] as MapPlace;

				if (place.Points.Count >= 1)
				{
					int num2 = place.PlaceRefs.Count - 1;
					for (int j = 0; j <= num2; j++)
					{
						TGEDCOMCustomEvent evt = (place.PlaceRefs[j] as TPlaceRef).Event;

						if ((ind != null && (evt.Parent == ind)) || (condBirth && evt.Name == "BIRT") || (condDeath && evt.Name == "DEAT") || (condResidence && evt.Name == "RESI"))
						{
							this.CopyPoint(place.Points[0] as GKMapBrowser.GMapPoint, place.PlaceRefs[j] as TPlaceRef);
						}
					}
				}
			}

			if (ind != null)
			{
				this.SortPointsByDate();
			}

			this.PreparePointsList(this.fMapPoints, ind != null);
		}

		private void TreePlaces_DoubleClick(object sender, EventArgs e)
		{
			GKTreeNode node = this.TreePlaces.SelectedNode as GKTreeNode;
			if (node != null)
			{
				GKMapBrowser.GMapPoint pt = node.Data as GKMapBrowser.GMapPoint;
				if (pt != null)
				{
					this.fMapBrowser.SetCenter(pt.Latitude, pt.Longitude, -1);
				}
			}
		}

		private void TfmMaps_Load(object sender, EventArgs e)
		{
			this.PlacesLoad();
			this.btnSelectPlaces.Enabled = true;
		}

		public TfmMaps(IBase aBase)
		{
			this.InitializeComponent();

			this.fBase = aBase;
			this.fTree = aBase.Tree;
			this.fSelectedPersons = aBase.GetContentList(TGEDCOMRecordType.rtIndividual);
			this.fMapBrowser = new GKMapBrowser();
			this.fMapBrowser.Dock = DockStyle.Fill;
			this.fMapBrowser.InitMap();
			this.Panel1.Controls.Add(this.fMapBrowser);
			this.fMapPoints = new ExtList(true);
			this.fPlaces = new ExtList(true);
			this.fBaseRoot = this.TreePlaces.Nodes.Add(LangMan.LS(LSID.LSID_RPLocations));
			this.radTotal.Checked = true;

			(this as ILocalization).SetLang();
		}

		void ILocalization.SetLang()
		{
			this.Text = LangMan.LS(LSID.LSID_MIMap);
			this.tsPlaces.Text = LangMan.LS(LSID.LSID_RPLocations);
			this.GroupBox2.Text = LangMan.LS(LSID.LSID_MapSelection);
			this.radTotal.Text = LangMan.LS(LSID.LSID_MapSelOnAll);
			this.chkBirth.Text = LangMan.LS(LSID.LSID_MSBirthPlaces);
			this.chkDeath.Text = LangMan.LS(LSID.LSID_MSDeathPlaces);
			this.chkResidence.Text = LangMan.LS(LSID.LSID_MSResiPlace);
			this.radSelected.Text = LangMan.LS(LSID.LSID_MapSelOnSelected);
			this.btnSaveImage.Text = LangMan.LS(LSID.LSID_SaveImage);
			this.btnSelectPlaces.Text = LangMan.LS(LSID.LSID_Show);
		}

		private TreeNode FindTreeNode(string aPlace)
		{
			TreeNode result = null;			
			for (int idx = 0; idx <= this.fBaseRoot.Nodes.Count - 1; idx++) {
				if (this.fBaseRoot.Nodes[idx].Text == aPlace)
				{
					result = this.fBaseRoot.Nodes[idx];
					break;
				}
			}
			return result;
		}

		private void AddPlace(TGEDCOMPlace aPlace, TGEDCOMCustomEvent aRef)
		{
			TGEDCOMLocationRecord locRec = aPlace.Location.Value as TGEDCOMLocationRecord;

            string place_name = (locRec != null) ? locRec.LocationName : aPlace.StringValue;

			TreeNode node = this.FindTreeNode(place_name);
            MapPlace place;

			if (node == null) {
                place = new MapPlace();
				place.Name = place_name;
				this.fPlaces.Add(place);

				node = new GKTreeNode(place_name, place);
				this.fBaseRoot.Nodes.Add(node);

				if (locRec == null) {
					GKMapBrowser.RequestGeoCoords(place_name, place.Points);

					int num = place.Points.Count - 1;
					for (int i = 0; i <= num; i++) {
						if (place.Points[i] is GKMapBrowser.GMapPoint) {
							GKMapBrowser.GMapPoint pt = place.Points[i] as GKMapBrowser.GMapPoint;
							string pt_title = pt.Hint + string.Format(" [{0:0.000000}, {1:0.000000}]", pt.Latitude, pt.Longitude);
							node.Nodes.Add(new GKTreeNode(pt_title, pt));
						}
					}
				} else {
					GKMapBrowser.GMapPoint pt = new GKMapBrowser.GMapPoint(locRec.Map.Lati, locRec.Map.Long, place_name);
					place.Points.Add(pt);

                    string ptTitle = pt.Hint + string.Format(" [{0:0.000000}, {1:0.000000}]", pt.Latitude, pt.Longitude);
					node.Nodes.Add(new GKTreeNode(ptTitle, pt));
				}
			} else {
                place = ((node as GKTreeNode).Data as MapPlace);
			}

			TPlaceRef pRef = new TPlaceRef();
			pRef.Date = GKUtils.GEDCOMDateToDate(aRef.Detail.Date);
			pRef.Event = aRef;
			place.PlaceRefs.Add(pRef);
		}

		private void CopyPoint(GKMapBrowser.GMapPoint aPt, TPlaceRef aRef)
		{
			GKMapBrowser.GMapPoint pt;
			int num = this.fMapPoints.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				pt = (this.fMapPoints[i] as GKMapBrowser.GMapPoint);
				if (pt.Hint == aPt.Hint)
				{
					return;
				}
			}

			pt = new GKMapBrowser.GMapPoint(aPt.Latitude, aPt.Longitude, aPt.Hint);
			pt.Date = aRef.Date;
			this.fMapPoints.Add(pt);
		}

		private void SortPointsByDate()
		{
			int num = this.fMapPoints.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				int num2 = this.fMapPoints.Count - 1;
				for (int j = i + 1; j <= num2; j++)
				{
					GKMapBrowser.GMapPoint pt = this.fMapPoints[i] as GKMapBrowser.GMapPoint;
					GKMapBrowser.GMapPoint pt2 = this.fMapPoints[j] as GKMapBrowser.GMapPoint;

					if (pt.Date > pt2.Date)
					{
						this.fMapPoints.Exchange(i, j);
					}
				}
			}
		}
	}
}
