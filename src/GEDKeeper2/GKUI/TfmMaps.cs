using System;
using System.Collections.Generic;
using System.Windows.Forms;

using GKCommon;
using GKCommon.GEDCOM;
using GKCommon.GEDCOM.Enums;
using GKCore;
using GKCore.Interfaces;
using GKUI.Controls;

namespace GKUI
{
    /// <summary>
    /// 
    /// </summary>
    public sealed partial class TfmMaps : Form, ILocalization
	{
		private class TPlaceRef
		{
			public DateTime Date;
			public GEDCOMCustomEvent Event;
		}

		private class MapPlace : IDisposable
		{
			public string Name;
			public readonly ExtList<GKMapBrowser.GMapPoint> Points;
			public readonly ExtList<TPlaceRef> PlaceRefs;

            private bool fDisposed;

            public MapPlace()
			{
				this.Points = new ExtList<GKMapBrowser.GMapPoint>(true);
				this.PlaceRefs = new ExtList<TPlaceRef>(false);
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
		private readonly ExtList<GKMapBrowser.GMapPoint> fMapPoints;
		private readonly ExtList<MapPlace> fPlaces;
		private readonly List<GEDCOMRecord> fSelectedPersons;
		private readonly IBase fBase;
		private readonly GEDCOMTree fTree;

		private bool IsSelected(GEDCOMRecord iRec)
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

				int num = this.fTree.RecordsCount;
				for (int i = 0; i < num; i++) {
					GEDCOMRecord rec = this.fTree[i];
					bool res = rec is GEDCOMIndividualRecord && this.IsSelected(rec);

					if (res) {
						GEDCOMIndividualRecord ind = rec as GEDCOMIndividualRecord;
						int p_cnt = 0;

						int num2 = ind.IndividualEvents.Count;
						for (int j = 0; j < num2; j++)
						{
							GEDCOMCustomEvent ev = ind.IndividualEvents[j];
							if (ev.Detail.Place.StringValue != "") {
								AddPlace(ev.Detail.Place, ev);
								p_cnt++;
							}
						}

						if (p_cnt > 0) {
							this.ComboPersons.Items.Add(new GKComboItem(ind.GetNameString(true, false) + " [" + p_cnt.ToString() + "]", ind));
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

		private void PreparePointsList(ExtList<GKMapBrowser.GMapPoint> aPoints, bool byPerson)
		{
			this.fMapBrowser.BeginUpdate();
			try
			{
				this.fMapBrowser.ClearPoints();
				int num = aPoints.Count;
				for (int i = 0; i < num; i++)
				{
					GKMapBrowser.GMapPoint pt = aPoints[i];
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
			GEDCOMIndividualRecord ind = null;

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
					ind = ((this.ComboPersons.Items[this.ComboPersons.SelectedIndex] as GKComboItem).Data as GEDCOMIndividualRecord);
				}
			}

			this.fMapBrowser.ShowLines = (ind != null && this.chkLinesVisible.Checked);
			this.fMapPoints.Clear();

			int num = this.fPlaces.Count;
			for (int i = 0; i < num; i++)
			{
                MapPlace place = this.fPlaces[i];

				if (place.Points.Count >= 1)
				{
					int num2 = place.PlaceRefs.Count;
					for (int j = 0; j < num2; j++)
					{
						GEDCOMCustomEvent evt = place.PlaceRefs[j].Event;

						if ((ind != null && (evt.Parent == ind)) || (condBirth && evt.Name == "BIRT") || (condDeath && evt.Name == "DEAT") || (condResidence && evt.Name == "RESI"))
						{
							this.CopyPoint(place.Points[0], place.PlaceRefs[j]);
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
			this.fSelectedPersons = aBase.GetContentList(GEDCOMRecordType.rtIndividual);
			this.fMapBrowser = new GKMapBrowser();
			this.fMapBrowser.Dock = DockStyle.Fill;
			this.fMapBrowser.InitMap();
			this.Panel1.Controls.Add(this.fMapBrowser);
			this.fMapPoints = new ExtList<GKMapBrowser.GMapPoint>(true);
			this.fPlaces = new ExtList<MapPlace>(true);
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

		private TreeNode FindTreeNode(string place)
		{
			int num = this.fBaseRoot.Nodes.Count;
			for (int i = 0; i < num; i++) {
				TreeNode node = this.fBaseRoot.Nodes[i];

				if (node.Text == place) {
					return node;
				}
			}

			return null;
		}

		private void AddPlace(GEDCOMPlace place, GEDCOMCustomEvent placeEvent)
		{
			GEDCOMLocationRecord locRec = place.Location.Value as GEDCOMLocationRecord;

            string placeName = (locRec != null) ? locRec.LocationName : place.StringValue;

			TreeNode node = this.FindTreeNode(placeName);
            MapPlace mapPlace;

			if (node == null) {
                mapPlace = new MapPlace();
				mapPlace.Name = placeName;
				this.fPlaces.Add(mapPlace);

				node = new GKTreeNode(placeName, mapPlace);
				this.fBaseRoot.Nodes.Add(node);

				if (locRec == null) {
					GKMapBrowser.RequestGeoCoords(placeName, mapPlace.Points);

					int num = mapPlace.Points.Count;
					for (int i = 0; i < num; i++) {
						if (mapPlace.Points[i] is GKMapBrowser.GMapPoint) {
							GKMapBrowser.GMapPoint pt = mapPlace.Points[i] as GKMapBrowser.GMapPoint;
							string ptTitle = pt.Hint + string.Format(" [{0:0.000000}, {1:0.000000}]", pt.Latitude, pt.Longitude);
							node.Nodes.Add(new GKTreeNode(ptTitle, pt));
						}
					}
				} else {
					GKMapBrowser.GMapPoint pt = new GKMapBrowser.GMapPoint(locRec.Map.Lati, locRec.Map.Long, placeName);
					mapPlace.Points.Add(pt);

                    string ptTitle = pt.Hint + string.Format(" [{0:0.000000}, {1:0.000000}]", pt.Latitude, pt.Longitude);
					node.Nodes.Add(new GKTreeNode(ptTitle, pt));
				}
			} else {
                mapPlace = ((node as GKTreeNode).Data as MapPlace);
			}

			TPlaceRef pRef = new TPlaceRef();
			pRef.Date = placeEvent.GetIndependentDate();
			pRef.Event = placeEvent;
			mapPlace.PlaceRefs.Add(pRef);
		}

		private void CopyPoint(GKMapBrowser.GMapPoint aPt, TPlaceRef aRef)
		{
			GKMapBrowser.GMapPoint pt;
			int num = this.fMapPoints.Count;
			for (int i = 0; i < num; i++) {
				pt = this.fMapPoints[i];
				if (pt.Hint == aPt.Hint) {
					return;
				}
			}

			pt = new GKMapBrowser.GMapPoint(aPt.Latitude, aPt.Longitude, aPt.Hint);
			pt.Date = aRef.Date;
			this.fMapPoints.Add(pt);
		}

		private void SortPointsByDate()
		{
			int num = this.fMapPoints.Count;
			for (int i = 0; i < num; i++) {
				for (int j = i + 1; j < num; j++) {
					GKMapBrowser.GMapPoint pt = this.fMapPoints[i];
					GKMapBrowser.GMapPoint pt2 = this.fMapPoints[j];

					if (pt.Date > pt2.Date) {
						this.fMapPoints.Exchange(i, j);
					}
				}
			}
		}
	}
}
