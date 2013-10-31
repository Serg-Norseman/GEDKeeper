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
	public partial class TfmMaps : Form, ILocalization
	{
		// FIXME: реализация непрозрачная; оптимизировать, вычистить, оттестировать
		private class TPlaceRef
		{
			public DateTime Date;
			public TGEDCOMCustomEvent Event;
		}

		private class TPlace : IDisposable
		{
			public string Name;
			public TList Points;
			public TList PlaceRefs;
			protected bool Disposed_;

			public TPlace()
			{
				this.Points = new TList(true);
				this.PlaceRefs = new TList(false);
			}

			public void Dispose()
			{
				if (!this.Disposed_)
				{
					this.PlaceRefs.Dispose();
					this.Points.Dispose();
					this.Disposed_ = true;
				}
			}
		}

		private TreeNode FBaseRoot;
		private GKMapBrowser FMapBrowser;
		private TList FMapPoints;
		private TList FPlaces;
		private TList FSelectedPersons;
		private TGEDCOMTree FTree;

		private void PlacesLoad()
		{
			this.ComboPersons.BeginUpdate();
			this.TreePlaces.BeginUpdate();
			TfmProgress.ProgressInit(this.FTree.RecordsCount, LangMan.LSList[386]);
			try
			{
				this.FPlaces.Clear();
				this.ComboPersons.Items.Clear();
				this.ComboPersons.Sorted = false;
				this.ComboPersons.Items.Add(new GKComboItem(LangMan.LSList[387], null));

				int num = this.FTree.RecordsCount - 1;
				for (int i = 0; i <= num; i++) {
					TGEDCOMRecord rec = this.FTree[i];
					bool res = rec is TGEDCOMIndividualRecord && (this.FSelectedPersons == null || (this.FSelectedPersons != null && this.FSelectedPersons.IndexOf(rec) >= 0));

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
					TfmProgress.ProgressStep();
				}

				this.FBaseRoot.ExpandAll();
				this.ComboPersons.Sorted = true;
			}
			finally
			{
				TfmProgress.ProgressDone();
				this.TreePlaces.EndUpdate();
				this.ComboPersons.EndUpdate();
			}
		}

		private void PreparePointsList(TList aPoints, bool ByPerson)
		{
			this.FMapBrowser.BeginUpdate();
			try
			{
				this.FMapBrowser.ClearPoints();
				int num = aPoints.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					GKMapBrowser.TGMapPoint pt = aPoints[i] as GKMapBrowser.TGMapPoint;
					string stHint = pt.Hint;
					if (ByPerson)
					{
						stHint = stHint + " [" + pt.Date.ToString() + "]";
					}
					this.FMapBrowser.AddPoint(pt.Latitude, pt.Longitude, stHint);
				}
				this.FMapBrowser.ZoomToBounds();
			}
			finally
			{
				this.FMapBrowser.EndUpdate();
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
				this.FMapBrowser.SaveSnapshot(this.SaveDialog1.FileName);
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
				condBirth = false;
				condDeath = false;
				condResidence = false;

				if (this.ComboPersons.SelectedIndex >= 0)
				{
					ind = ((this.ComboPersons.Items[this.ComboPersons.SelectedIndex] as GKComboItem).Data as TGEDCOMIndividualRecord);
				}
			}

			this.FMapBrowser.ShowLines = (ind != null && this.chkLinesVisible.Checked);
			this.FMapPoints.Clear();

			int num = this.FPlaces.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				TPlace place = this.FPlaces[i] as TPlace;

				if (place.Points.Count >= 1)
				{
					int num2 = place.PlaceRefs.Count - 1;
					for (int j = 0; j <= num2; j++)
					{
						TGEDCOMCustomEvent evt = (place.PlaceRefs[j] as TPlaceRef).Event;

						if ((ind != null && (evt.Parent == ind)) || (condBirth && evt.Name == "BIRT") || (condDeath && evt.Name == "DEAT") || (condResidence && evt.Name == "RESI"))
						{
							this.CopyPoint(place.Points[0] as GKMapBrowser.TGMapPoint, place.PlaceRefs[j] as TPlaceRef);
						}
					}
				}
			}

			if (ind != null)
			{
				this.SortPointsByDate();
			}

			this.PreparePointsList(this.FMapPoints, ind != null);
		}

		private void TreePlaces_DoubleClick(object sender, EventArgs e)
		{
			GKTreeNode node = this.TreePlaces.SelectedNode as GKTreeNode;
			if (node != null)
			{
				GKMapBrowser.TGMapPoint pt = node.Data as GKMapBrowser.TGMapPoint;
				if (pt != null)
				{
					this.FMapBrowser.SetCenter(pt.Latitude, pt.Longitude, -1);
				}
			}
		}

		private void TfmMaps_Load(object sender, EventArgs e)
		{
			this.PlacesLoad();
			this.btnSelectPlaces.Enabled = true;
		}

		public TfmMaps(TGEDCOMTree aTree, TList aSelectedPersons)
		{
			this.InitializeComponent();
			this.FTree = aTree;
			this.FSelectedPersons = aSelectedPersons;
			this.FMapBrowser = new GKMapBrowser();
			this.FMapBrowser.Dock = DockStyle.Fill;
			this.FMapBrowser.InitMap();
			this.Panel1.Controls.Add(this.FMapBrowser);
			this.FMapPoints = new TList(true);
			this.FPlaces = new TList(true);
			this.FBaseRoot = this.TreePlaces.Nodes.Add(LangMan.LSList[62]);
			this.radTotal.Checked = true;

			(this as ILocalization).SetLang();
		}

		void ILocalization.SetLang()
		{
			this.Text = LangMan.LSList[28];
			this.tsPlaces.Text = LangMan.LSList[62];
			this.GroupBox2.Text = LangMan.LSList[388];
			this.radTotal.Text = LangMan.LSList[389];
			this.chkBirth.Text = LangMan.LSList[390];
			this.chkDeath.Text = LangMan.LSList[391];
			this.chkResidence.Text = LangMan.LSList[392];
			this.radSelected.Text = LangMan.LSList[393];
			this.btnSaveImage.Text = LangMan.LSList[394];
			this.btnSelectPlaces.Text = LangMan.LSList[173];
		}

		private TreeNode FindTreeNode(string aPlace)
		{
			TreeNode Result = null;			
			for (int idx = 0; idx <= this.FBaseRoot.Nodes.Count - 1; idx++) {
				if (this.FBaseRoot.Nodes[idx].Text == aPlace)
				{
					Result = this.FBaseRoot.Nodes[idx];
					break;
				}
			}
			return Result;
		}

		private void AddPlace(TGEDCOMPlace aPlace, TGEDCOMCustomEvent aRef)
		{
			TGEDCOMLocationRecord locRec = aPlace.Location.Value as TGEDCOMLocationRecord;
			string place_name;
			if (locRec != null) {
				place_name = locRec.LocationName;
			} else {
				place_name = aPlace.StringValue;
			}

			TreeNode node = this.FindTreeNode(place_name);
			TPlace place;

			if (node == null) {
				place = new TPlace();
				place.Name = place_name;
				this.FPlaces.Add(place);

				node = new GKTreeNode(place_name, place);
				this.FBaseRoot.Nodes.Add(node);

				if (locRec == null) {
					GKMapBrowser.RequestGeoCoords(place_name, place.Points);

					int num = place.Points.Count - 1;
					for (int i = 0; i <= num; i++) {
						if (place.Points[i] is GKMapBrowser.TGMapPoint) {
							GKMapBrowser.TGMapPoint pt = place.Points[i] as GKMapBrowser.TGMapPoint;
							string pt_title = pt.Hint + string.Format(" [{0:0.000000}, {1:0.000000}]", pt.Latitude, pt.Longitude);
							node.Nodes.Add(new GKTreeNode(pt_title, pt));
						}
					}
				} else {
					GKMapBrowser.TGMapPoint pt = new GKMapBrowser.TGMapPoint();
					pt.Hint = place_name;
					pt.Longitude = locRec.Map.Long;
					pt.Latitude = locRec.Map.Lati;
					place.Points.Add(pt);
					string pt_title = pt.Hint + string.Format(" [{0:0.000000}, {1:0.000000}]", pt.Latitude, pt.Longitude);
					node.Nodes.Add(new GKTreeNode(pt_title, pt));
				}
			} else {
				place = ((node as GKTreeNode).Data as TfmMaps.TPlace);
			}

			TPlaceRef pRef = new TPlaceRef();
			pRef.Date = GKUtils.GEDCOMDateToDate(aRef.Detail.Date);
			pRef.Event = aRef;
			place.PlaceRefs.Add(pRef);
		}

		private void CopyPoint(GKMapBrowser.TGMapPoint aPt, TPlaceRef aRef)
		{
			GKMapBrowser.TGMapPoint pt;
			int num = this.FMapPoints.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				pt = (this.FMapPoints[i] as GKMapBrowser.TGMapPoint);
				if (pt.Hint == aPt.Hint)
				{
					return;
				}
			}

			pt = new GKMapBrowser.TGMapPoint();
			pt.Latitude = aPt.Latitude;
			pt.Longitude = aPt.Longitude;
			pt.Hint = aPt.Hint;
			pt.Date = aRef.Date;
			this.FMapPoints.Add(pt);
		}

		private void SortPointsByDate()
		{
			int num = this.FMapPoints.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				int num2 = this.FMapPoints.Count - 1;
				for (int j = i + 1; j <= num2; j++)
				{
					GKMapBrowser.TGMapPoint pt = this.FMapPoints[i] as GKMapBrowser.TGMapPoint;
					GKMapBrowser.TGMapPoint pt2 = this.FMapPoints[j] as GKMapBrowser.TGMapPoint;

					if (pt.Date > pt2.Date)
					{
						this.FMapPoints.Exchange(i, j);
					}
				}
			}
		}
	}
}
