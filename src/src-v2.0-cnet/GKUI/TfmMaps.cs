using GedCom551;
using GKCore;
using GKUI.Controls;
using GKSys;
using System;
using System.Drawing;
using System.Globalization;
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Windows.Forms;

namespace GKUI
{
	public class TfmMaps : Form
	{
		internal class TPlaceRef
		{
			public DateTime DateTime;
			public TGEDCOMCustomEvent Ref;

			public void Free()
			{
				TObjectHelper.Free(this);
			}
		}

		internal class TPlace : IDisposable
		{
			public string Name;
			public TObjectList Points;
			public TObjectList PlaceRefs;
			protected internal bool Disposed_;

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

		private StatusBar StatusBar1;
		private TabControl PageControl1;
		private TabPage tsPlaces;
		private TreeView TreePlaces;
		private SaveFileDialog SaveDialog1;
		private Panel Panel1;
		private GroupBox GroupBox2;
		private ComboBox ComboPersons;
		private CheckBox chkResidence;
		private CheckBox chkDeath;
		private CheckBox chkBirth;
		private Button btnSelectPlaces;
		private Button btnSaveImage;
		private RadioButton radTotal;
		private RadioButton radSelected;
		private CheckBox chkLinesVisible;
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

				for (int i = 0; i <= this.FTree.RecordsCount - 1; i++) {
					TGEDCOMRecord rec = this.FTree.GetRecord(i);
					bool res = rec is TGEDCOMIndividualRecord && (this.FSelectedPersons == null || (this.FSelectedPersons != null && this.FSelectedPersons.IndexOf(rec) >= 0));

					if (res) {
						TGEDCOMIndividualRecord ind = (TGEDCOMIndividualRecord)rec;
						int p_cnt = 0;
						for (int j = 0; j <= ind.IndividualEventsCount - 1; j++)
						{
							TGEDCOMCustomEvent ev = ind.GetIndividualEvent(j);
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
				int i = 0;
				if (num >= i)
				{
					num++;
					do
					{
						TMapBrowser.TGMapPoint pt = aPoints[i] as TMapBrowser.TGMapPoint;
						string stHint = pt.Hint;
						if (ByPerson)
						{
							stHint = stHint + " [" + pt.Date.ToString() + "]";
						}
						this.FMapBrowser.AddPoint(pt.Latitude, pt.Longitude, stHint);
						i++;
					}
					while (i != num);
				}
				this.FMapBrowser.ZoomToBounds();
			}
			finally
			{
				this.FMapBrowser.EndUpdate();
			}
		}

		private void InitializeComponent()
		{
			this.StatusBar1 = new StatusBar();
			this.PageControl1 = new TabControl();
			this.tsPlaces = new TabPage();
			this.TreePlaces = new TreeView();
			this.GroupBox2 = new GroupBox();
			this.ComboPersons = new ComboBox();
			this.chkResidence = new CheckBox();
			this.chkDeath = new CheckBox();
			this.chkBirth = new CheckBox();
			this.btnSelectPlaces = new Button();
			this.btnSaveImage = new Button();
			this.radTotal = new RadioButton();
			this.radSelected = new RadioButton();
			this.chkLinesVisible = new CheckBox();
			this.SaveDialog1 = new SaveFileDialog();
			this.Panel1 = new Panel();
			this.PageControl1.SuspendLayout();
			this.tsPlaces.SuspendLayout();
			this.GroupBox2.SuspendLayout();
			base.SuspendLayout();
			this.StatusBar1.Location = new Point(0, 485);
			this.StatusBar1.Name = "StatusBar1";
			this.StatusBar1.Size = new Size(829, 19);
			this.StatusBar1.TabIndex = 3;
			this.PageControl1.Controls.Add(this.tsPlaces);
			this.PageControl1.Dock = DockStyle.Left;
			this.PageControl1.Location = new Point(0, 0);
			this.PageControl1.Name = "PageControl1";
			this.PageControl1.SelectedIndex = 0;
			this.PageControl1.Size = new Size(289, 485);
			this.PageControl1.TabIndex = 1;
			this.tsPlaces.Controls.Add(this.TreePlaces);
			this.tsPlaces.Controls.Add(this.GroupBox2);
			this.tsPlaces.Location = new Point(4, 22);
			this.tsPlaces.Name = "tsPlaces";
			this.tsPlaces.Size = new Size(281, 459);
			this.tsPlaces.TabIndex = 0;
			this.tsPlaces.Text = "Места";
			this.TreePlaces.Dock = DockStyle.Fill;
			this.TreePlaces.ImageIndex = -1;
			this.TreePlaces.Location = new Point(0, 185);
			this.TreePlaces.Name = "TreePlaces";
			this.TreePlaces.SelectedImageIndex = -1;
			this.TreePlaces.Size = new Size(281, 274);
			this.TreePlaces.TabIndex = 0;
			this.TreePlaces.DoubleClick += new EventHandler(this.TreePlaces_DoubleClick);
			this.GroupBox2.Controls.Add(this.ComboPersons);
			this.GroupBox2.Controls.Add(this.chkResidence);
			this.GroupBox2.Controls.Add(this.chkDeath);
			this.GroupBox2.Controls.Add(this.chkBirth);
			this.GroupBox2.Controls.Add(this.btnSelectPlaces);
			this.GroupBox2.Controls.Add(this.btnSaveImage);
			this.GroupBox2.Controls.Add(this.radTotal);
			this.GroupBox2.Controls.Add(this.radSelected);
			this.GroupBox2.Controls.Add(this.chkLinesVisible);
			this.GroupBox2.Dock = DockStyle.Top;
			this.GroupBox2.Location = new Point(0, 0);
			this.GroupBox2.Name = "GroupBox2";
			this.GroupBox2.Size = new Size(281, 185);
			this.GroupBox2.TabIndex = 1;
			this.GroupBox2.TabStop = false;
			this.GroupBox2.Text = "Выборка";
			this.ComboPersons.DropDownStyle = ComboBoxStyle.DropDownList;
			this.ComboPersons.Location = new Point(8, 104);
			this.ComboPersons.Name = "ComboPersons";
			this.ComboPersons.Size = new Size(265, 21);
			this.ComboPersons.TabIndex = 5;
			this.chkResidence.Location = new Point(19, 64);
			this.chkResidence.Name = "chkResidence";
			this.chkResidence.Size = new Size(129, 17);
			this.chkResidence.TabIndex = 3;
			this.chkResidence.Text = "Места проживания";
			this.chkDeath.Location = new Point(19, 48);
			this.chkDeath.Name = "chkDeath";
			this.chkDeath.Size = new Size(129, 17);
			this.chkDeath.TabIndex = 2;
			this.chkDeath.Text = "Места смерти";
			this.chkBirth.Location = new Point(19, 32);
			this.chkBirth.Name = "chkBirth";
			this.chkBirth.Size = new Size(129, 17);
			this.chkBirth.TabIndex = 1;
			this.chkBirth.Text = "Места рождения";
			this.btnSelectPlaces.Enabled = false;
			this.btnSelectPlaces.Location = new Point(198, 152);
			this.btnSelectPlaces.Name = "btnSelectPlaces";
			this.btnSelectPlaces.Size = new Size(75, 25);
			this.btnSelectPlaces.TabIndex = 6;
			this.btnSelectPlaces.Text = "Показать";
			this.btnSelectPlaces.Click += new EventHandler(this.btnSelectPlaces_Click);
			this.btnSaveImage.Location = new Point(8, 152);
			this.btnSaveImage.Name = "btnSaveImage";
			this.btnSaveImage.Size = new Size(121, 25);
			this.btnSaveImage.TabIndex = 7;
			this.btnSaveImage.Text = "Сохранить снимок...";
			this.btnSaveImage.Click += new EventHandler(this.btnSaveImage_Click);
			this.radTotal.Location = new Point(8, 16);
			this.radTotal.Name = "radTotal";
			this.radTotal.Size = new Size(198, 17);
			this.radTotal.TabIndex = 0;
			this.radTotal.Text = "По всем людям";
			this.radTotal.Click += new EventHandler(this.radTotal_Click);
			this.radSelected.Location = new Point(8, 87);
			this.radSelected.Name = "radSelected";
			this.radSelected.Size = new Size(198, 17);
			this.radSelected.TabIndex = 4;
			this.radSelected.Text = "Только по выбранному";
			this.radSelected.Click += new EventHandler(this.radTotal_Click);
			this.chkLinesVisible.Checked = true;
			this.chkLinesVisible.CheckState = CheckState.Checked;
			this.chkLinesVisible.Location = new Point(8, 128);
			this.chkLinesVisible.Name = "chkLinesVisible";
			this.chkLinesVisible.Size = new Size(265, 17);
			this.chkLinesVisible.TabIndex = 8;
			this.chkLinesVisible.Text = "Отображать линии";
			this.SaveDialog1.DefaultExt = "jpg";
			this.SaveDialog1.Filter = "Image files|*.jpg";
			this.Panel1.Dock = DockStyle.Fill;
			this.Panel1.Location = new Point(289, 0);
			this.Panel1.Name = "Panel1";
			this.Panel1.Size = new Size(540, 485);
			this.Panel1.TabIndex = 4;
			this.AutoScaleBaseSize = new Size(5, 14);
			base.ClientSize = new Size(829, 504);
			base.Controls.Add(this.Panel1);
			base.Controls.Add(this.PageControl1);
			base.Controls.Add(this.StatusBar1);
			this.Font = new Font("Tahoma", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 204);
			base.FormBorderStyle = FormBorderStyle.FixedDialog;
			base.KeyPreview = true;
			base.Name = "TfmMaps";
			base.StartPosition = FormStartPosition.CenterScreen;
			this.Text = "Карты";
			base.KeyDown += new KeyEventHandler(this.TfmMaps_KeyDown);
			base.Load += new EventHandler(this.TfmMaps_Load);
			this.PageControl1.ResumeLayout(false);
			this.tsPlaces.ResumeLayout(false);
			this.GroupBox2.ResumeLayout(false);
			base.ResumeLayout(false);
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
								if ((ind != null && object.Equals(@ref.Parent, ind)) || (condBirth && BDSSystem.WStrCmp(@ref.Name, "BIRT") == 0) || (condDeath && BDSSystem.WStrCmp(@ref.Name, "DEAT") == 0) || (condResidence && BDSSystem.WStrCmp(@ref.Name, "RESI") == 0))
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

		protected override void Dispose(bool Disposing)
		{
			if (Disposing)
			{
				this.FPlaces.Free();
				this.FMapPoints.Free();
			}
			base.Dispose(Disposing);
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
				place_name = locRec.Name;
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

					for (int i = 0; i <= place.Points.Count - 1; i++) {
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
					pt.Longitude = TGKSys.DoubleParse(locRec.Map.Long);
					pt.Latitude = TGKSys.DoubleParse(locRec.Map.Lati);
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
			int num = Self.FMapPoints.Count - 1;
			int i = 0;
			TMapBrowser.TGMapPoint pt;
			if (num >= i)
			{
				num++;
				do
				{
					pt = (Self.FMapPoints[i] as TMapBrowser.TGMapPoint);
					if (BDSSystem.WStrCmp(pt.Hint, aPt.Hint) == 0)
					{
						return;
					}
					i++;
				}
				while (i != num);
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
