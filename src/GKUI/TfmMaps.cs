using System;
using System.Runtime.InteropServices;
using System.Windows.Forms;

using GedCom551;
using GKCore;
using GKCore.Sys;
using GKUI.Controls;

namespace GKUI
{
	public partial class TfmMaps : Form
	{
		private class TPlaceRef
		{
			public DateTime DateTime;
			public TGEDCOMCustomEvent Ref;

			public void Free()
			{
				TObjectHelper.Free(this);
			}
		}

		private class TPlace : IDisposable
		{
			public string Name;
			public TObjectList Points;
			public TObjectList PlaceRefs;
			protected bool Disposed_;

			public TPlace()
			{
				this.Points = new TObjectList(true);
				this.PlaceRefs = new TObjectList(false);
			}

			public void Dispose()
			{
				if (!this.Disposed_)
				{
					this.PlaceRefs.Free();
					this.Points.Free();
					this.Disposed_ = true;
				}
			}

			public void Free()
			{
				TObjectHelper.Free(this);
			}
		}

		private TreeNode FBaseRoot;
		private TMapBrowser FMapBrowser;
		private TObjectList FMapPoints;
		private TObjectList FPlaces;
		private TList FSelectedPersons;
		private TGEDCOMTree FTree;

		private void PlacesLoad()
		{
			this.ComboPersons.BeginUpdate();
			this.TreePlaces.BeginUpdate();
			TfmProgress.ProgressInit(this.FTree.RecordsCount, GKL.LSList[386]);
			try
			{
				this.FPlaces.Clear();
				this.ComboPersons.Items.Clear();
				this.ComboPersons.Sorted = false;
				this.ComboPersons.Items.Add(new TComboItem(GKL.LSList[387], null));

				int num = this.FTree.RecordsCount - 1;
				for (int i = 0; i <= num; i++) {
					TGEDCOMRecord rec = this.FTree.GetRecord(i);
					bool res = rec is TGEDCOMIndividualRecord && (this.FSelectedPersons == null || (this.FSelectedPersons != null && this.FSelectedPersons.IndexOf(rec) >= 0));

					if (res) {
						TGEDCOMIndividualRecord ind = (TGEDCOMIndividualRecord)rec;
						int p_cnt = 0;

						int num2 = ind.IndividualEvents.Count - 1;
						for (int j = 0; j <= num2; j++)
						{
							TGEDCOMCustomEvent ev = ind.IndividualEvents[j];
							if (ev.Detail.Place.StringValue != "") {
								_PlacesLoad_AddPlace(this, ev.Detail.Place, ev);
								p_cnt++;
							}
						}
						if (p_cnt > 0) {
							this.ComboPersons.Items.Add(new TComboItem(TGenEngine.GetNameStr(ind, true, false) + " [" + p_cnt.ToString() + "]", ind));
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

		private void PreparePointsList(TObjectList aPoints, bool ByPerson)
		{
			this.FMapBrowser.BeginUpdate();
			try
			{
				this.FMapBrowser.ClearPoints();
				int num = aPoints.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					TMapBrowser.TGMapPoint pt = aPoints[i] as TMapBrowser.TGMapPoint;
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
			if (this.radTotal.Checked)
			{
				condBirth = this.chkBirth.Checked;
				condDeath = this.chkDeath.Checked;
				condResidence = this.chkResidence.Checked;
			}
			else
			{
				if (this.radSelected.Checked)
				{
					condBirth = false;
					condDeath = false;
					condResidence = false;
					if (this.ComboPersons.SelectedIndex >= 0)
					{
						ind = ((this.ComboPersons.Items[this.ComboPersons.SelectedIndex] as TComboItem).Data as TGEDCOMIndividualRecord);
					}
				}
			}
			this.FMapBrowser.ShowLines = (ind != null && this.chkLinesVisible.Checked);
			this.FMapPoints.Clear();

			int num = this.FPlaces.Count - 1;
			int i = 0;
			if (num >= i)
			{
				num++;
				do
				{
					TfmMaps.TPlace place = this.FPlaces[i] as TfmMaps.TPlace;
					if (place.Points.Count >= 1)
					{
						int num2 = place.PlaceRefs.Count - 1;
						int j = 0;
						if (num2 >= j)
						{
							num2++;
							do
							{
								TGEDCOMCustomEvent @ref = (place.PlaceRefs[j] as TfmMaps.TPlaceRef).Ref;
								if ((ind != null && object.Equals(@ref.Parent, ind)) || (condBirth && @ref.Name == "BIRT") || (condDeath && @ref.Name == "DEAT") || (condResidence && @ref.Name == "RESI"))
								{
									TfmMaps._btnSelectPlaces_Click_CopyPoint(this, place.Points[0] as TMapBrowser.TGMapPoint, place.PlaceRefs[j] as TfmMaps.TPlaceRef);
								}
								j++;
							}
							while (j != num2);
						}
					}
					i++;
				}
				while (i != num);
			}
			if (ind != null)
			{
				TfmMaps._btnSelectPlaces_Click_SortPointsByDate(this);
			}
			this.PreparePointsList(this.FMapPoints, ind != null);
		}

		private void TreePlaces_DoubleClick(object sender, EventArgs e)
		{
			TGKTreeNode node = this.TreePlaces.SelectedNode as TGKTreeNode;
			if (node != null)
			{
				TMapBrowser.TGMapPoint pt = node.Data as TMapBrowser.TGMapPoint;
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
			this.FMapBrowser = new TMapBrowser();
			this.FMapBrowser.Dock = DockStyle.Fill;
			this.FMapBrowser.InitMap();
			this.Panel1.Controls.Add(this.FMapBrowser);
			this.FMapPoints = new TObjectList(true);
			this.FPlaces = new TObjectList(true);
			this.FBaseRoot = this.TreePlaces.Nodes.Add(GKL.LSList[62]);
			this.radTotal.Checked = true;
			this.SetLang();
		}

		public void SetLang()
		{
			this.Text = GKL.LSList[28];
			this.tsPlaces.Text = GKL.LSList[62];
			this.GroupBox2.Text = GKL.LSList[388];
			this.radTotal.Text = GKL.LSList[389];
			this.chkBirth.Text = GKL.LSList[390];
			this.chkDeath.Text = GKL.LSList[391];
			this.chkResidence.Text = GKL.LSList[392];
			this.radSelected.Text = GKL.LSList[393];
			this.btnSaveImage.Text = GKL.LSList[394];
			this.btnSelectPlaces.Text = GKL.LSList[173];
		}

		private static TreeNode _PlacesLoad_FindTreeNode([In] TfmMaps Self, string aPlace)
		{
			TreeNode Result = null;			
			for (int idx = 0; idx <= Self.FBaseRoot.Nodes.Count - 1; idx++) {
				if (Self.FBaseRoot.Nodes[idx].Text == aPlace)
				{
					Result = Self.FBaseRoot.Nodes[idx];
					break;
				}
			}
			return Result;
		}

		private static void _PlacesLoad_AddPlace([In] TfmMaps Self, TGEDCOMPlace aPlace, TGEDCOMCustomEvent aRef)
		{
			TGEDCOMLocationRecord locRec = aPlace.Location.Value as TGEDCOMLocationRecord;
			string place_name;
			if (locRec != null) {
				place_name = locRec.LocationName;
			} else {
				place_name = aPlace.StringValue;
			}

			TreeNode node = TfmMaps._PlacesLoad_FindTreeNode(Self, place_name);
			TfmMaps.TPlace place;

			if (node == null) {
				place = new TfmMaps.TPlace();
				place.Name = place_name;
				Self.FPlaces.Add(place);

				node = new TGKTreeNode(place_name, place);
				Self.FBaseRoot.Nodes.Add(node);

				if (locRec == null) {
					TMapBrowser.RequestGeoCoords(place_name, place.Points);

					int num = place.Points.Count - 1;
					for (int i = 0; i <= num; i++) {
						if (place.Points[i] is TMapBrowser.TGMapPoint) {
							TMapBrowser.TGMapPoint pt = place.Points[i] as TMapBrowser.TGMapPoint;
							string pt_title = pt.Hint + string.Format(" [{0:0.000000}, {1:0.000000}]", new object[]
							{ pt.Latitude, pt.Longitude });
							node.Nodes.Add(new TGKTreeNode(pt_title, pt));
						}
					}
				} else {
					TMapBrowser.TGMapPoint pt = new TMapBrowser.TGMapPoint();
					pt.Hint = place_name;
					pt.Longitude = SysUtils.DoubleParse(locRec.Map.Long);
					pt.Latitude = SysUtils.DoubleParse(locRec.Map.Lati);
					place.Points.Add(pt);
					string pt_title = pt.Hint + string.Format(" [{0:0.000000}, {1:0.000000}]", new object[]
					{
						pt.Latitude, pt.Longitude
					});
					node.Nodes.Add(new TGKTreeNode(pt_title, pt));
				}
			} else {
				place = ((node as TGKTreeNode).Data as TfmMaps.TPlace);
			}

			TfmMaps.TPlaceRef pRef = new TfmMaps.TPlaceRef();
			pRef.DateTime = TGenEngine.GEDCOMDateToDate(aRef.Detail.Date.Value);
			pRef.Ref = aRef;
			place.PlaceRefs.Add(pRef);
		}

		private static void _btnSelectPlaces_Click_CopyPoint([In] TfmMaps Self, TMapBrowser.TGMapPoint aPt, TfmMaps.TPlaceRef aRef)
		{
			TMapBrowser.TGMapPoint pt;
			int num = Self.FMapPoints.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				pt = (Self.FMapPoints[i] as TMapBrowser.TGMapPoint);
				if (pt.Hint == aPt.Hint)
				{
					return;
				}
			}
			pt = new TMapBrowser.TGMapPoint();
			pt.Latitude = aPt.Latitude;
			pt.Longitude = aPt.Longitude;
			pt.Hint = aPt.Hint;
			pt.Date = aRef.DateTime;
			Self.FMapPoints.Add(pt);
		}

		private static void _btnSelectPlaces_Click_SortPointsByDate([In] TfmMaps Self)
		{
			int num = Self.FMapPoints.Count - 1;
			int i = 0;
			if (num >= i)
			{
				num++;
				do
				{
					int arg_2D_0 = i + 1;
					int num2 = Self.FMapPoints.Count - 1;
					int j = arg_2D_0;
					if (num2 >= j)
					{
						num2++;
						do
						{
							TMapBrowser.TGMapPoint pt = Self.FMapPoints[i] as TMapBrowser.TGMapPoint;
							TMapBrowser.TGMapPoint pt2 = Self.FMapPoints[j] as TMapBrowser.TGMapPoint;
							if (pt.Date > pt2.Date)
							{
								Self.FMapPoints.Exchange(i, j);
							}
							j++;
						}
						while (j != num2);
					}
					i++;
				}
				while (i != num);
			}
		}
	}
}
