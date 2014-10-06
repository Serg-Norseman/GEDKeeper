using System;
using System.Collections.Generic;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Drawing.Imaging;
using System.IO;
using System.Windows.Forms;

using ExtUtils;
using ExtUtils.Graph;
using GedCom551;
using GKCore;
using GKCore.Interfaces;
using GKCore.Options;

/// <summary>
/// Localization: dirty
/// </summary>

namespace GKUI.Charts
{
	public delegate void ThumbMoved(int position);
	
	public class TScaleControl
	{
		#region Private fields
		
		private static Rectangle ControlsScaleRect = new Rectangle(0, 0, 26, 320);
		private static Rectangle ControlsThumbRect = new Rectangle(0, 322, 26, 11);

        private const int ScaleY1 = 22;
	    private const int ScaleY2 = 297;

	    private readonly Bitmap fControlsImage;
		private readonly TTreeChartBox fChart;

        private int fDCount = 11;
		private int fThumbPos = 5;
		private bool fVisible;
		private bool fThumbCaptured;
		private string fTip;
		private Rectangle fDestRect;

		#endregion
		
		#region Public properties
		
		public int Width
		{
			get { return 26; }
		}

		public int Height
		{
			get { return 320; }
		}

		public int DCount
		{
			get { return this.fDCount; }
			set { this.fDCount = value; }
		}

		public int ThumbPos
		{
			get { return this.fThumbPos; }
			set { this.fThumbPos = value; }
		}
		
		public string Tip
		{
			get { return this.fTip; }
			set { this.fTip = value; }
		}
		
		public bool Visible
		{
			get { return this.fVisible; }
			set { 
				this.fVisible = value;
				this.fChart.Invalidate();
			}
		}

		#endregion
		
		public TScaleControl(TTreeChartBox chart)
		{
			this.fChart = chart;
			this.fControlsImage = GKResources.iChartControls;
		}

		public void Update()
		{
			Rectangle cr = fChart.ClientRectangle;
			this.fDestRect = new Rectangle(cr.Right - (10 + this.Width), 10, this.Width, this.Height);
		}

        public void Draw(Graphics gfx)
        {
            if (gfx == null) return;

            gfx.InterpolationMode = InterpolationMode.HighQualityBicubic;
            gfx.SmoothingMode = SmoothingMode.HighQuality;
            gfx.PixelOffsetMode = PixelOffsetMode.HighQuality;
            gfx.CompositingQuality = CompositingQuality.HighQuality;
            gfx.DrawImage(fControlsImage, fDestRect, ControlsScaleRect, GraphicsUnit.Pixel);

			if (this.fDCount == 0) return;
            gfx.DrawImage(fControlsImage, this.GetDRect(fThumbPos), ControlsThumbRect, GraphicsUnit.Pixel);
		}

		private Rectangle GetDRect(int d)
		{
			int dH = ((ScaleY2 - ScaleY1) - ControlsThumbRect.Height) / (this.fDCount - 1);
			int thumbY = fDestRect.Top + ScaleY1 + (d - 1) * dH;
			return new Rectangle(fDestRect.Left, thumbY, fDestRect.Width, ControlsThumbRect.Height);
		}

		public bool Contains(int X, int Y)
		{
			return fDestRect.Contains(X, Y);
		}

		public void MouseDown(int X, int Y)
		{
			fThumbCaptured = (this.GetDRect(fThumbPos).Contains(X, Y) && !fThumbCaptured);
		}

		public void MouseMove(int X, int Y, ThumbMoved thumbMoved)
		{
			if (!fThumbCaptured) return;
			
			for (int i = 1; i <= fDCount; i++) {
				Rectangle r = GetDRect(i);
				if (r.Contains(X, Y)) {
					fThumbPos = i;
					fChart.Invalidate();
					if (thumbMoved != null) thumbMoved(i);
					break;
				}
			}
		}

		public void MouseUp(int X, int Y)
		{
			if (fThumbCaptured) fThumbCaptured = false;
		}
	}


	public class PersonModifyEventArgs : EventArgs
	{
	    public TreeChartPerson Person { get; set; }

	    public PersonModifyEventArgs(TreeChartPerson person)
		{
			this.Person = person;
		}
	}

	public delegate void PersonModifyEventHandler(object sender, PersonModifyEventArgs eArgs);
	
	public class TTreeChartBox : Panel
	{
		#region Subtypes
		
        private enum ChartControlMode
        {
            ccmDefault,
            ccmDragImage,
            ccmControlsVisible
        }

		public enum TChartKind
		{
			ckAncestors,
			ckDescendants,
			ckBoth
		}

		public enum TDrawMode
		{
			dmDefault,
			dmToFile
		}

		#endregion
		
		#region Private fields
		
		private int fHMax;
		private int fWMax;

		private IBase fBase;
		private int fBranchDistance;
		private int fDepthLimit;
		private Font fDrawFont;
		private TChartFilter fFilter;
		private TGraph fGraph;
		private TChartKind fKind;
		private TreeChartPerson fKinRoot;
		private int fLevelDistance;
		private int fMargins;
		private TreeChartOptions fOptions;
		private bool fPathDebug;
		private TPersonList fPersons;
		private List<string> fPreparedFamilies = new List<string>();
		internal List<string> fPreparedIndividuals = new List<string>(); // FIXME
		private TreeChartPerson fRoot;
		private float fScale;
		private TScaleControl fScaleControl;
		private TreeChartPerson fSelected;
		private ShieldState fShieldState;
		//private string[] FSignsData;
		private Bitmap[] fSignsPic;
		private int fSpouseDistance;
		private bool fTraceKinships;
		private bool fTraceSelected;
		private TGEDCOMTree fTree;
		private ToolTip fToolTip;
		
		private int fBorderWidth;
		private int fLeftPos;
		private int fTopPos;
		private Point fRange;

		protected int fImageHeight;
		protected int fImageWidth;
		protected int fSPX;
		protected int fSPY;
		protected ExtRect fVisibleArea;

		//private int FGensCount;

		private Pen fLinePen;
		private Pen fDecorativeLinePen;
		private SolidBrush fSolidBlack;

		private int fMouseX;
		private int fMouseY;
		private ChartControlMode fMode = ChartControlMode.ccmDefault;

        private static readonly object EventPersonModify;
        private static readonly object EventPersonProperties;

		#endregion

		#region Public properties
		
		public event PersonModifyEventHandler PersonModify
		{
			add { base.Events.AddHandler(TTreeChartBox.EventPersonModify, value); }
			remove { base.Events.RemoveHandler(TTreeChartBox.EventPersonModify, value); }
		}

		public event MouseEventHandler PersonProperties
		{
			add { base.Events.AddHandler(TTreeChartBox.EventPersonProperties, value); }
			remove { base.Events.RemoveHandler(TTreeChartBox.EventPersonProperties, value); }
		}

		public IBase Base
		{
			get { return this.fBase; }
			set { this.fBase = value; }
		}

		public int BorderWidth
		{
			get { return this.fBorderWidth; }
			set { this.SetBorderWidth(value); }
		}

		public int BranchDistance
		{
			get { return this.fBranchDistance; }
			set { this.fBranchDistance = value; }
		}

		public int DepthLimit
		{
			get { return this.fDepthLimit; }
			set { this.fDepthLimit = value; }
		}

		public Font DrawFont
		{
			get { return this.fDrawFont; }
		}

		public TChartFilter Filter
		{
			get { return this.fFilter; }
		}

//		public int GensCount
//		{
//			get { return this.FGensCount; }
//		}

		public int IndividualsCount
		{
			get { return this.fPreparedIndividuals.Count; }
		}

		public TChartKind Kind
		{
			get { return fKind; }
			set { fKind = value; }
		}

		public int LeftPos
		{
			get { return this.fLeftPos; }
			set { this.SetLeftPos(value); }
		}

		public int Margins
		{
			get { return this.fMargins; }
			set { this.fMargins = value; }
		}

		public TreeChartOptions Options
		{
			get { return this.fOptions; }
			set { this.fOptions = value; }
		}

		public bool PathDebug
		{
			get { return this.fPathDebug; }
			set { this.fPathDebug = value; }
		}

		public TreeChartPerson Root
		{
			get { return this.fRoot; }
		}

		public new float Scale
		{
			get { return this.fScale; }
			set {
				this.fScale = value;

				fScaleControl.ThumbPos = (int)Math.Round((value -  0.4f) / 0.1f);

				this.Predef();
				this.RecalcChart();
				this.ScrollRange();

				if (this.fTraceSelected && this.Selected != null) {
					this.CenterPerson(this.Selected, false);
				}
			}
		}

		public new TScaleControl ScaleControl
		{
			get { return this.fScaleControl; }
		}

		public TreeChartPerson Selected
		{
			get { return this.fSelected; }
			set { this.SetSelected(value); }
		}

		public ShieldState ShieldState
		{
			get { return this.fShieldState; }
			set { this.fShieldState = value; }
		}

		public int TopPos
		{
			get { return this.fTopPos; }
			set { this.SetTopPos(value); }
		}

		public bool TraceSelected
		{
			get { return this.fTraceSelected; }
			set { this.fTraceSelected = value; }
		}

		public bool TraceKinships
		{
			get { return this.fTraceKinships; }
			set { this.fTraceKinships = value; }
		}

		public TGEDCOMTree Tree
		{
			get { return this.fTree; }
			set { this.fTree = value; }
		}

		#endregion
		
		#region Instance control
		
        static TTreeChartBox()
        {
            TTreeChartBox.EventPersonModify = new object();
            TTreeChartBox.EventPersonProperties = new object();
        }

		public TTreeChartBox()
		{
			base.BorderStyle = BorderStyle.Fixed3D;
			base.DoubleBuffered = true;
			base.TabStop = true;
			base.BackColor = Color.White;

			this.InitSigns();

			this.TopPos = 0;
			this.LeftPos = 0;

			this.fPersons = new TPersonList(true);
			this.fFilter = new TChartFilter();
			this.fSpouseDistance = 10;
			this.fBranchDistance = 40;
			this.fLevelDistance = 46;
			this.fMargins = 40;
			this.fDepthLimit = -1;
			this.fSelected = null;
			this.fScale = 1.0f;
			this.fTraceSelected = true;
			this.fGraph = new TGraph();

			this.fScaleControl = new TScaleControl(this);
			this.fToolTip = new ToolTip();

			//this.AutoScroll = false;
			//this.HorizontalScroll.Visible = true;
			//this.VerticalScroll.Visible = true;

			//Win32Native.EnableScrollBar(this.Handle, Win32Native.SB_HORZ, Win32Native.ESB_ENABLE_BOTH);
			//Win32Native.EnableScrollBar(this.Handle, Win32Native.SB_VERT, Win32Native.ESB_ENABLE_BOTH);

			this.InitGraphics();
		}

		protected override void Dispose(bool disposing)
		{
			if (disposing)
			{
				this.DoneGraphics();

				this.fGraph.Dispose();
				this.fFilter.Dispose();
				this.fPersons.Dispose();
				this.DoneSigns();

                if (fDrawFont != null) fDrawFont.Dispose();
			}
			base.Dispose(disposing);
		}

		#endregion
		
		private void InitSigns()
		{
			//FSignsData = new string[] { "GEORGE_CROSS", "SOLDIER", "SOLDIER_FALL", "VETERAN_REAR" };

			fSignsPic = new Bitmap[4];

			fSignsPic[0] = GKResources.iTGGeorgeCross;
			fSignsPic[0].MakeTransparent(this.fSignsPic[0].GetPixel(0, 0));

			fSignsPic[1] = GKResources.iTGSoldier;
			fSignsPic[1].MakeTransparent(this.fSignsPic[1].GetPixel(0, 0));

			fSignsPic[2] = GKResources.iTGSoldierFall;
			fSignsPic[2].MakeTransparent(this.fSignsPic[2].GetPixel(0, 0));

			fSignsPic[3] = GKResources.iTGVeteranRear;
			fSignsPic[3].MakeTransparent(this.fSignsPic[3].GetPixel(0, 0));
		}

        // FIXME
		private void DoneSigns()
		{
			// dummy
		}

		private bool hasMediaFail = false;

		private TreeChartPerson AddDescPerson(TreeChartPerson aParent, TGEDCOMIndividualRecord iRec, TreeChartPerson.TPersonKind aKind, int aGeneration)
		{
			try
			{
				TreeChartPerson result;
				if (this.fRoot != null && this.fRoot.Rec == iRec) {
					result = this.fRoot;
					result.Parent = aParent;
					result.Kind = aKind;
				} else {
					result = new TreeChartPerson(this);
					result.BuildBy(iRec, ref hasMediaFail);
					result.Generation = aGeneration;
					result.Parent = aParent;
					result.Kind = aKind;
					this.fPersons.Add(result);

					if (this.fOptions.Kinship) {
						result.Node = this.fGraph.AddVertex(result);
					}

					if (aKind != TreeChartPerson.TPersonKind.pkSpouse && aParent != null) {
						aParent.AddChild(result);
					}
				}
				return result;
			}
			catch (Exception ex)
			{
				this.fBase.Host.LogWrite("TTreeChartBox.AddDescPerson(): " + ex.Message);
				throw ex;
			}
		}

		private TreeChartPerson DoAncestorsStep(TreeChartPerson aChild, TGEDCOMIndividualRecord aPerson, int aGeneration, bool dup_flag)
		{
			try
			{
				TreeChartPerson result = null;

				if (aPerson != null)
				{
					result = new TreeChartPerson(this);
					result.BuildBy(aPerson, ref hasMediaFail);
					result.Generation = aGeneration;
					this.fPersons.Add(result);

					if (aChild != null)
					{
						result.AddChild(aChild);
					}

					if (this.fOptions.Kinship)
					{
						result.Node = this.fGraph.AddVertex(result);
					}

					if ((this.fDepthLimit <= -1 || aGeneration != this.fDepthLimit) && aPerson.ChildToFamilyLinks.Count > 0 && !dup_flag)
					{
						TGEDCOMFamilyRecord family = aPerson.ChildToFamilyLinks[0].Family;

						bool is_dup = (this.fPreparedFamilies.IndexOf(family.XRef) >= 0);
						if (!is_dup) this.fPreparedFamilies.Add(family.XRef);

						if (GKUtils.IsRecordAccess(family.Restriction, this.fShieldState))
						{
							TGEDCOMIndividualRecord iFather = family.Husband.Value as TGEDCOMIndividualRecord;
							TGEDCOMIndividualRecord iMother = family.Wife.Value as TGEDCOMIndividualRecord;
							bool divorced = (family.GetTagStringValue("_STAT") == "NOTMARR");

							if (iFather != null && GKUtils.IsRecordAccess(iFather.Restriction, this.fShieldState))
							{
								result.Father = this.DoAncestorsStep(result, iFather, aGeneration + 1, is_dup);
								if (result.Father != null)
								{
									result.Father.Divorced = divorced;
									result.Father.IsDup = is_dup;
									if (this.fOptions.Kinship)
									{
										this.fGraph.AddUndirectedEdge(result.Node, result.Father.Node, 1, (int)TRelationKind.rkParent, (int)TRelationKind.rkChild);
									}
								}
							} else {
								result.Father = null;
							}

							if (iMother != null && GKUtils.IsRecordAccess(iMother.Restriction, this.fShieldState))
							{
								result.Mother = this.DoAncestorsStep(result, iMother, aGeneration + 1, is_dup);
								if (result.Mother != null)
								{
									result.Mother.Divorced = divorced;
									result.Mother.IsDup = is_dup;
									if (this.fOptions.Kinship)
									{
										this.fGraph.AddUndirectedEdge(result.Node, result.Mother.Node, 1, (int)TRelationKind.rkParent, (int)TRelationKind.rkChild);
									}
								}
							} else {
								result.Mother = null;
							}

							if (result.Father != null && result.Mother != null && this.fOptions.Kinship)
							{
								this.fGraph.AddUndirectedEdge(result.Father.Node, result.Mother.Node, 1, (int)TRelationKind.rkSpouse, (int)TRelationKind.rkSpouse);
							}
						}
					}
				}

				return result;
			}
			catch (Exception ex)
			{
				this.fBase.Host.LogWrite("TTreeChartBox.DoAncestorsStep(): " + ex.Message);
				throw ex;
			}
		}

		private TreeChartPerson DoDescendantsStep(TreeChartPerson aParent, TGEDCOMIndividualRecord aPerson, int aLevel)
		{
			try
			{
				TreeChartPerson result = null;
				if (aPerson != null && (!this.fOptions.ChildlessExclude || aLevel <= 1 || aPerson.SpouseToFamilyLinks.Count != 0 || !this.fBase.Context.IsChildless(aPerson)))
				{
					TGroupMode sourceMode = this.fFilter.SourceMode;

					switch (sourceMode)
					{
						case TGroupMode.gmAll:
							break;

						case TGroupMode.gmNone:
							if (aPerson.SourceCitations.Count != 0) {
								return result;
							}
							break;
							
						case TGroupMode.gmAny:
							if (aPerson.SourceCitations.Count == 0) {
								return result;
							}
							break;

						case TGroupMode.gmSelected:
							TGEDCOMSourceRecord filterSource;
							if (this.fFilter.SourceRef == "") {
								filterSource = null;
							} else {
								filterSource = this.fTree.XRefIndex_Find(this.fFilter.SourceRef) as TGEDCOMSourceRecord;
							}
							if (aPerson.IndexOfSource(filterSource) < 0) {
								return result;
							}
							break;
					}

					TChartFilter.TBranchCut branchCut = this.fFilter.BranchCut;
					if (branchCut != TChartFilter.TBranchCut.bcNone)
					{
						if (!(bool)aPerson.ExtData)
						{
							return result;
						}
					}

					TreeChartPerson res = this.AddDescPerson(aParent, aPerson, TreeChartPerson.TPersonKind.pkDefault, aLevel);
					result = res;

					int num = aPerson.SpouseToFamilyLinks.Count;
					for (int i = 0; i < num; i++)
					{
						TGEDCOMFamilyRecord family = aPerson.SpouseToFamilyLinks[i].Family;

						bool is_dup = (this.fPreparedFamilies.IndexOf(family.XRef) >= 0);
						if (!is_dup) this.fPreparedFamilies.Add(family.XRef);

						if (GKUtils.IsRecordAccess(family.Restriction, this.fShieldState))
						{
							TreeChartPerson res_parent = null;
							TGEDCOMSex sex = aPerson.Sex;
							TreeChartPerson ft = null;
							TreeChartPerson mt = null;
							TreeChartPerson.TPersonFlag desc_flag = TreeChartPerson.TPersonFlag.pfDescByFather;

							switch (sex) {
								case TGEDCOMSex.svFemale:
									{
										TGEDCOMIndividualRecord sp = family.Husband.Value as TGEDCOMIndividualRecord;
										res_parent = this.AddDescPerson(null, sp, TreeChartPerson.TPersonKind.pkSpouse, aLevel);
										res_parent.Sex = TGEDCOMSex.svMale;
										ft = res_parent;
										mt = res;
										desc_flag = TreeChartPerson.TPersonFlag.pfDescByFather;
										break;
									}

								case TGEDCOMSex.svMale:
									{
										TGEDCOMIndividualRecord sp = family.Wife.Value as TGEDCOMIndividualRecord;
										res_parent = this.AddDescPerson(null, sp, TreeChartPerson.TPersonKind.pkSpouse, aLevel);
										res_parent.Sex = TGEDCOMSex.svFemale;
										ft = res;
										mt = res_parent;
										desc_flag = TreeChartPerson.TPersonFlag.pfDescByMother;
										break;
									}
							}

							if (this.fOptions.Kinship)
							{
								this.fGraph.AddUndirectedEdge(res.Node, res_parent.Node, 1, (int)TRelationKind.rkSpouse, (int)TRelationKind.rkSpouse);
							}

							if (res_parent != null)
							{
								res.AddSpouse(res_parent);
								res_parent.BaseSpouse = res;
								res_parent.BaseFamily = family;
							}
							else
							{
								res_parent = res;
							}

							ft.IsDup = is_dup;
							mt.IsDup = is_dup;

							if ((this.fDepthLimit <= -1 || aLevel != this.fDepthLimit) && (!is_dup))
							{
								int num2 = family.Childrens.Count;
								for (int j = 0; j < num2; j++)
								{
									TGEDCOMIndividualRecord child_rec = family.Childrens[j].Value as TGEDCOMIndividualRecord;
									if (GKUtils.IsRecordAccess(child_rec.Restriction, this.fShieldState))
									{
										TreeChartPerson child = this.DoDescendantsStep(res_parent, child_rec, aLevel + 1);
										if (child != null)
										{
											child.Father = ft;
											child.Mother = mt;
											//int d = (int)desc_flag;
											child.Flags.Include(desc_flag);
											if (this.fOptions.Kinship)
											{
												this.fGraph.AddUndirectedEdge(child.Node, ft.Node, 1, (int)TRelationKind.rkParent, (int)TRelationKind.rkChild);
												this.fGraph.AddUndirectedEdge(child.Node, mt.Node, 1, (int)TRelationKind.rkParent, (int)TRelationKind.rkChild);
											}
										}
									}
								}
							}
						}
					}
				}
				
				return result;
			}
			catch (Exception ex)
			{
				this.fBase.Host.LogWrite("TTreeChartBox.DoDescendantsStep(): " + ex.Message);
				throw ex;
			}
		}

		private void FindRelationship(TreeChartPerson target)
		{
			if (target == null) return;

			if (target.Node == null) {
				target.Kinship = "";
				return;
			}

			string result = "";
			try
			{
				IEnumerable<IEdge> edgesPath = this.fGraph.GetPath(target.Node);

				string tmp = "";
				TRelationKind prevRel = TRelationKind.rkNone;
				TRelationKind finRel = TRelationKind.rkNone;
				int great = 0;

				foreach (IEdge edge in edgesPath)
				{
					TreeChartPerson xFrom = edge.Source.Value as TreeChartPerson;
					TreeChartPerson xTo = edge.Target.Value as TreeChartPerson;
					TRelationKind curRel = FixLink(xFrom, xTo, (TRelationKind)((int)edge.Value));

					if (this.fPathDebug) {
						if (tmp != "") tmp += ", ";
						if (xFrom.Rec != null) tmp += (xFrom.Rec.XRef + ">" + GKData.RelationSigns[(int)curRel] + ">");
						if (xTo.Rec != null) tmp += xTo.Rec.XRef;
					}

                    if (prevRel != TRelationKind.rkUndefined)
					{
						int g;
						int lev;
						finRel = KinshipsMan.FindKinship(prevRel, curRel, out g, out lev);
						great += g;
						prevRel = finRel;
					}
				}

				if (this.fPathDebug) {
					if (target.Rec != null) target.PathDebug = target.Rec.XRef + " ";

					target.PathDebug = target.PathDebug + " [" + tmp + "]";
				}

				result = "[" + FixRelation(target, finRel, great) + "]";
				target.Kinship = result;
			}
			finally
			{
			}
		}

		private static TRelationKind FixLink(TreeChartPerson xFrom, TreeChartPerson xTo, TRelationKind rel)
		{
			TRelationKind xRel = rel;

			switch (rel)
			{
				case TRelationKind.rkParent:
					switch (xTo.Sex)
					{
						case TGEDCOMSex.svMale:
							xRel = TRelationKind.rkFather;
							break;
						case TGEDCOMSex.svFemale:
							xRel = TRelationKind.rkMother;
							break;
					}
					break;

				case TRelationKind.rkSpouse:
					switch (xTo.Sex)
					{
						case TGEDCOMSex.svMale:
							xRel = TRelationKind.rkHusband;
							break;
						case TGEDCOMSex.svFemale:
							xRel = TRelationKind.rkWife;
							break;
					}
					break;

				case TRelationKind.rkChild:
					switch (xTo.Sex)
					{
						case TGEDCOMSex.svMale:
							xRel = TRelationKind.rkSon;
							break;
						case TGEDCOMSex.svFemale:
							xRel = TRelationKind.rkDaughter;
							break;
					}
					break;

				default:
					xRel = rel;
					break;
			}

			return xRel;
		}

		private static string FixRelation(TreeChartPerson aTarget, TRelationKind Rel, int Great)
		{
			string tmp = "";
			if (Great != 0)
			{
				if (Rel >= TRelationKind.rkUncle && Rel < TRelationKind.rkNephew)
				{
					tmp = GKData.Numerals[Great] + GKData.NumKinship[(int)aTarget.Sex] + " ";
					if (Rel == TRelationKind.rkUncle)
					{
						Rel = TRelationKind.rkGrandfather;
					}
					if (Rel == TRelationKind.rkAunt)
					{
						Rel = TRelationKind.rkGrandmother;
					}
				}
				else
				{
					if (Rel != TRelationKind.rkUndefined)
					{
						tmp = GetGreat(Great);
					}
				}
			}
			else
			{
				tmp = "";
			}
			return tmp + LangMan.LS(GKData.RelationKinds[(int)Rel]);
		}

		private static string GetGreat(int n)
		{
			string result = "";
            for (int i = 1; i <= n; i++)
            {
                result += "пра";
            }
            return result;
		}

		private static void InitEdges(ref int[] edges)
		{
			for (int i = 0; i <= 255; i++) edges[i] = 0;
		}

		private bool PersonIsVisible(ExtRect pn_rect)
		{
		    int pL = pn_rect.Left;
			int pR = pn_rect.Right;
			int pT = pn_rect.Top;
			int pB = pn_rect.Bottom;

			return (fVisibleArea.Contains(pL, pT) || fVisibleArea.Contains(pR, pT) ||
			        fVisibleArea.Contains(pL, pB) || fVisibleArea.Contains(pR, pB));
		}

		/*private bool LineIsVisible(int X1, int Y1, int X2, int Y2)
		{
			return (FVisibleArea.Contains(X1, Y1) || FVisibleArea.Contains(X2, Y2));
		}*/
		
		private void DrawLine(Graphics aCanvas, int X1, int Y1, int X2, int Y2)
		{
			//if (!LineIsVisible(X1, Y1, X2, Y2)) return;
			
			int sX = this.fSPX + X1;
			int sX2 = this.fSPX + X2;
			int sY = this.fSPY + Y1;
			int sY2 = this.fSPY + Y2;
			aCanvas.DrawLine(fLinePen, sX, sY, sX2, sY2);

			if (this.fOptions.Decorative) {
				if (sX == sX2) {
					aCanvas.DrawLine(fDecorativeLinePen, sX + 1, sY + 1, sX2 + 1, sY2 - 1);
				} else {
					if (sY == sY2) {
						aCanvas.DrawLine(fDecorativeLinePen, sX + 1, sY + 1, sX2 + 0, sY2 + 1);
					}
				}
			}
		}

		private void InitGraphics()
		{
			fLinePen = new Pen(Color.Black, 1f);
			fDecorativeLinePen = new Pen(Color.Silver, 1f);
			fSolidBlack = new SolidBrush(Color.Black);
		}

		private void DoneGraphics()
		{
			this.fLinePen.Dispose();
			this.fDecorativeLinePen.Dispose();
			this.fSolidBlack.Dispose();
		}

		private void TextOut(Graphics aCanvas, ExtRect rt, string s, int h, ref int line)
		{
			int stw = aCanvas.MeasureString(s, this.DrawFont).ToSize().Width;
			int rx = rt.Left + ((rt.Right - rt.Left + 1) - stw) / 2;
			int ry = rt.Top + (10 + (h * line));
			aCanvas.DrawString(s, this.DrawFont, fSolidBlack, (float)rx, (float)ry);
			line++;
		}

		private static GraphicsPath CreateRoundedRectangle(int x, int y, int width, int height, int radius)
	    {
			int xw = x + width;
			int yh = y + height;
			int xwr = xw - radius;
			int yhr = yh - radius;
			int xr = x + radius;
			int yr = y + radius;
			int r2 = radius * 2;
			int xwr2 = xw - r2;
			int yhr2 = yh - r2;

			GraphicsPath p = new GraphicsPath();
			p.StartFigure();

			//Top Left Corner
			p.AddArc(x, y, r2, r2, 180, 90);

			//Top Edge
			p.AddLine(xr, y, xwr, y);

			//Top Right Corner
			p.AddArc(xwr2, y, r2, r2, 270, 90);

			//Right Edge
			p.AddLine(xw, yr, xw, yhr);

			//Bottom Right Corner
			p.AddArc(xwr2, yhr2, r2, r2, 0, 90);

			//Bottom Edge
			p.AddLine(xwr, yh, xr, yh);

			//Bottom Left Corner
			p.AddArc(x, yhr2, r2, r2, 90, 90);

			//Left Edge
			p.AddLine(x, yhr, x, yr);

			p.CloseFigure();
			return p;
		}

		/*private static void DrawPathWithFuzzyLine(GraphicsPath path, Graphics gr, Color base_color, int max_opacity, int width, int opaque_width)
		{
			int num_steps = width - opaque_width + 1;       // Number of pens we will use.
			float delta = (float)max_opacity / num_steps / num_steps;   // Change in alpha between pens.
			float alpha = delta;                            // Initial alpha.
			for (int thickness = width; thickness >= opaque_width; thickness--)
			{
				Color color = Color.FromArgb(
					(int)alpha,
					base_color.R,
					base_color.G,
					base_color.B);
				using (Pen pen = new Pen(color, thickness))
				{
					pen.EndCap = LineCap.Round;
					pen.StartCap = LineCap.Round;
					gr.DrawPath(pen, path);
				}
				alpha += delta;
			}
		}*/

		private void DrawBorder(Graphics aCanvas, Pen xpen, ExtRect rt, bool dead, TreeChartPerson person)
		{
			Rectangle rect = rt.ToRectangle();
			Color b_color;
			switch (person.Sex) {
				case TGEDCOMSex.svMale:
				{
					if (!dead) {
						if (person.IsDup) {
							b_color = Color.FromArgb(192, 192, 192);
						} else {
							b_color = person.Divorced ? this.Options.UnHusbandColor : this.Options.MaleColor;
						}
					} else {
						b_color = Color.Black;
					}
					aCanvas.FillRectangle(new SolidBrush(b_color), rect.Left, rect.Top, rect.Width, rect.Height);
					aCanvas.DrawRectangle(xpen, rect.Left, rect.Top, rect.Width, rect.Height);
					break;
				}

				case TGEDCOMSex.svFemale:
				{
					if (!dead) {
						if (person.IsDup) {
							b_color = Color.FromArgb(192, 192, 192);
						} else {
							b_color = person.Divorced ? this.Options.UnWifeColor : this.Options.FemaleColor;
						}
					} else {
						b_color = Color.Black;
					}

					GraphicsPath path = CreateRoundedRectangle(rect.Left, rect.Top, rect.Width, rect.Height, 5);
					
					//aCanvas.TranslateTransform(3, 3);
					//DrawPathWithFuzzyLine(path, aCanvas, Color.Black, 200, 20, 2);
					//aCanvas.ResetTransform();

					aCanvas.FillPath(new SolidBrush(b_color), path);
					aCanvas.DrawPath(xpen, path);
					break;
				}

				default: 
				{
					b_color = !dead ? this.Options.UnkSexColor : Color.Black;
                    aCanvas.FillRectangle(new SolidBrush(b_color), rect.Left, rect.Top, rect.Width, rect.Height);
					aCanvas.DrawRectangle(xpen, rect.Left, rect.Top, rect.Width, rect.Height);
					break;
				}
			}
		}

		private void DrawPerson(Graphics aCanvas, int SPX, int SPY, TreeChartPerson person)
		{
			ExtRect rt = person.Rect;
			if (!this.PersonIsVisible(rt)) return;

			rt = rt.GetOffset(SPX, SPY);

			int h = aCanvas.MeasureString("A", this.DrawFont).ToSize().Height;
			bool has_port = this.Options.PortraitsVisible && person.Portrait != null;

			if (person.IsDead) {
				ExtRect dt = rt;
				dt = dt.GetOffset(-2, -2);
				this.DrawBorder(aCanvas, fLinePen, dt, true, person);
			}

			Pen xpen = null;
			try
			{
				if (person.Selected) {
					const int penWidth = 2;

                    Color penColor;
					switch (person.Sex) {
						case TGEDCOMSex.svMale:
							penColor = Color.Blue;
							break;

						case TGEDCOMSex.svFemale:
							penColor = Color.Red;
							break;

						default:
							penColor = Color.Black;
							break;
					}
					xpen = new Pen(penColor, ((float)((double)penWidth)));
				} else {
					xpen = new Pen(Color.Black, 1f);
				}

				this.DrawBorder(aCanvas, xpen, rt, false, person);
			}
			finally
			{
				xpen.Dispose();
			}

			if (has_port) {
				ExtRect port_rt = rt;
				port_rt.Right = port_rt.Left + person.PortraitWidth;
				port_rt.OffsetEx(3, 3);
				aCanvas.DrawImage(person.Portrait, person.GetDestRect(port_rt, person.Portrait));
				rt.Left += person.PortraitWidth;
			}

			int line = 0;

			if (this.Options.FamilyVisible) {
				this.TextOut(aCanvas, rt, person.Family, h, ref line);
			}

			if (!this.Options.DiffLines) {
				this.TextOut(aCanvas, rt, person.GetFullName(), h, ref line);
			} else {
				this.TextOut(aCanvas, rt, person.Name, h, ref line);
				this.TextOut(aCanvas, rt, person.Patronymic, h, ref line);
			}

			if (!this.Options.OnlyYears) {
				if (this.Options.BirthDateVisible) {
					this.TextOut(aCanvas, rt, person.BirthDate, h, ref line);
				}

				if (this.Options.DeathDateVisible) {
					this.TextOut(aCanvas, rt, person.DeathDate, h, ref line);
				}
			} else {
				this.TextOut(aCanvas, rt, person.GetLifeYears(), h, ref line);
			}

			if (this.Options.Kinship) {
				this.TextOut(aCanvas, rt, person.Kinship, h, ref line);
			}

			if (this.Options.SignsVisible && !person.Signs.IsEmpty())
			{
				int i = 0;
				for (TChartPersonSign cps = TChartPersonSign.urRI_StGeorgeCross; cps <= TChartPersonSign.urUSSR_RearVeteran; cps++)
				{
					if (person.Signs.Contains(cps)) {
						Bitmap pic = this.fSignsPic[(int)cps - 1];
						aCanvas.DrawImage(pic, rt.Right, rt.Top - 21 + i * pic.Height);
						i++;
					}
				}
			}

			if (this.PathDebug)
			{
				this.TextOut(aCanvas, rt, person.PathDebug, h, ref line);
			}
		}

		private void Predef()
		{
			double sc = (double)(this.fScale);
			int fsz = (int)checked((long)Math.Round(unchecked((double)this.fOptions.DefFont_Size * sc)));

		    string fontName = (fsz <= 7) ? "Small Fonts" : this.fOptions.DefFont_Name;

			this.fDrawFont = new Font(fontName, ((float)fsz), FontStyle.Regular, GraphicsUnit.Point);
			this.fSpouseDistance = (int)checked((long)Math.Round(10.0 * sc));
			this.fBranchDistance = (int)checked((long)Math.Round(40.0 * sc));
			this.fLevelDistance = (int)checked((long)Math.Round(46.0 * sc));
			this.fMargins = (int)checked((long)Math.Round(40.0 * sc));
		}

		private void RecalcAncestorsChart()
		{
			int[] edges = new int[256];
			InitEdges(ref edges);
			ExtList<TreeChartPerson> prev = new ExtList<TreeChartPerson>();
			try
			{
				this.RecalcAnc(prev, ref edges, this.fRoot, new Point(this.fMargins, this.fMargins));
			}
			finally
			{
				prev.Dispose();
			}
		}

		private void RecalcDescendantsChart(bool aPreDef)
		{
			int[] edges = new int[256];
			InitEdges(ref edges);
			this.RecalcDesc(ref edges, this.fRoot, new Point(this.fMargins, this.fMargins), aPreDef);
		}

		private void RecalcChart()
		{
			if (this.fOptions.Kinship && this.fKinRoot != null) {
				this.fGraph.FindPathTree(this.fKinRoot.Node);
			}

			int num = this.fPersons.Count;
			for (int i = 0; i < num; i++) {
				TreeChartPerson p = this.fPersons[i];

				if (this.fOptions.Kinship) {
					this.FindRelationship(p);
				}

				p.CalcBounds();
			}

			this.fHMax = 0;
			this.fWMax = 0;
			TChartKind fKind = this.fKind;

			switch (fKind) {
				case TChartKind.ckAncestors:
					this.RecalcAncestorsChart();
					break;

				case TChartKind.ckDescendants:
					this.RecalcDescendantsChart(true);
					break;

				case TChartKind.ckBoth:
					this.RecalcAncestorsChart();
					this.RecalcDescendantsChart(false);
					break;
			}

			this.fHMax = this.fHMax + this.fMargins - 1;
			this.fWMax = this.fWMax + this.fMargins - 1;
			this.fImageHeight = this.fHMax;
			this.fImageWidth = this.fWMax;
		}

		private void ShiftAnc(ref int[] edges, TreeChartPerson aPerson, int aOffset)
		{
			TreeChartPerson pp = aPerson;
			if (pp != null)
			{
				do
				{
					pp.PtX += aOffset;
					edges[pp.Generation] = pp.Rect.Right;

                    pp = (pp.GetChildsCount() < 1) ? null : pp.GetChild(0);
				}
				while (pp != null);
			}
		}

		private void RecalcAnc(ExtList<TreeChartPerson> prev, ref int[] edges, TreeChartPerson aPerson, Point aPt)
		{
			if (aPerson != null) {
				aPerson.Pt = aPt;
				int gen = aPerson.Generation;

			    int offset = (edges[gen] > 0) ? this.fBranchDistance : this.fMargins;

                if (aPerson.Rect.Left <= edges[gen] + offset) {
					this.ShiftAnc(ref edges, aPerson, edges[gen] + offset - aPerson.Rect.Left);
				}
				edges[gen] = aPerson.Rect.Right;
				prev.Add(aPerson);
				if (aPerson.Rect.Top < 0)
				{
					offset = 0 - aPerson.Rect.Top + this.fMargins;
					int num = prev.Count;
					for (int i = 0; i < num; i++) {
						TreeChartPerson pp = prev[i];
						pp.PtY += offset;
					}
				}

				if (aPerson.Father != null && aPerson.Mother != null)
				{
					Point xpt = new Point(aPerson.PtX - (this.fSpouseDistance + aPerson.Father.Width / 2), aPerson.PtY - this.fLevelDistance - aPerson.Height);
					this.RecalcAnc(prev, ref edges, aPerson.Father, xpt);
					xpt = new Point(aPerson.PtX + (this.fSpouseDistance + aPerson.Mother.Width / 2), aPerson.PtY - this.fLevelDistance - aPerson.Height);
					this.RecalcAnc(prev, ref edges, aPerson.Mother, xpt);
					aPerson.PtX = (aPerson.Father.PtX + aPerson.Mother.PtX) / 2;
					edges[aPerson.Generation] = aPerson.Rect.Right;
				}
				else
				{
					Point xpt = new Point(aPerson.PtX, aPerson.PtY - this.fLevelDistance - aPerson.Height);
					if (aPerson.Father != null) {
						this.RecalcAnc(prev, ref edges, aPerson.Father, xpt);
					} else {
						if (aPerson.Mother != null) {
							this.RecalcAnc(prev, ref edges, aPerson.Mother, xpt);
						}
					}
				}

				if (this.fWMax < aPerson.Rect.Right) this.fWMax = aPerson.Rect.Right;
				if (this.fHMax < aPerson.Rect.Bottom) this.fHMax = aPerson.Rect.Bottom;
			}
		}

		private void ShiftDesc(TreeChartPerson aPerson, int aOffset, bool single)
		{
			if (aPerson != null) {
				if (aPerson == this.fRoot) {
					single = false;
				}

                aPerson.PtX += aOffset;

                if (aPerson.BaseSpouse != null && (aPerson.BaseSpouse.Sex == TGEDCOMSex.svFemale || aPerson.BaseSpouse.GetSpousesCount() == 1))
				{
					this.ShiftDesc(aPerson.BaseSpouse, aOffset, single);
				} else {
					if (!single) {
						this.ShiftDesc(aPerson.Father, aOffset, single);
						this.ShiftDesc(aPerson.Mother, aOffset, single);
					} else {
						if (aPerson.Flags.Contains(TreeChartPerson.TPersonFlag.pfDescByFather)) {
							this.ShiftDesc(aPerson.Father, aOffset, single);
						} else {
							if (aPerson.Flags.Contains(TreeChartPerson.TPersonFlag.pfDescByMother)) {
								this.ShiftDesc(aPerson.Mother, aOffset, single);
							}
						}
					}
				}
			}
		}

		private void RecalcDescChilds(ref int[] edges, TreeChartPerson aPerson)
		{
			//edges = (int[])edges.Clone();
			if (aPerson.GetChildsCount() != 0)
			{
				bool fix_pair = aPerson.BaseSpouse != null && aPerson.BaseSpouse.GetSpousesCount() == 1;
				int cent_x = 0;

				if (fix_pair) {
					TGEDCOMSex sex = aPerson.Sex;
					switch (sex) {
						case TGEDCOMSex.svMale:
							cent_x = (aPerson.Rect.Right + aPerson.BaseSpouse.Rect.Left) / 2;
							break;

						case TGEDCOMSex.svFemale:
							cent_x = (aPerson.BaseSpouse.Rect.Right + aPerson.Rect.Left) / 2;
							break;
					}
				} else {
					cent_x = aPerson.PtX;
				}

				int cur_y = aPerson.PtY + this.fLevelDistance + aPerson.Height;
				int childs_width = (aPerson.GetChildsCount() - 1) * this.fBranchDistance;

				int num = aPerson.GetChildsCount();
				for (int i = 0; i < num; i++) {
					childs_width += aPerson.GetChild(i).Width;
				}

				int cur_x = cent_x - childs_width / 2;

				int num2 = aPerson.GetChildsCount();
				for (int i = 0; i < num2; i++) {
					TreeChartPerson child = aPerson.GetChild(i);
					this.RecalcDesc(ref edges, child, new Point(cur_x + child.Width / 2, cur_y), true);
					cur_x = child.Rect.Right + this.fBranchDistance;
				}

				cur_x = aPerson.GetChild(0).PtX;
				if (aPerson.GetChildsCount() > 1) {
					cur_x += (aPerson.GetChild(aPerson.GetChildsCount() - 1).PtX - cur_x) / 2;
				}

				if (fix_pair) {
					switch (aPerson.Sex) {
						case TGEDCOMSex.svMale:
							this.ShiftDesc(aPerson, cur_x - (this.BranchDistance + aPerson.Width) / 2 + 1 - aPerson.PtX, true);
							this.ShiftDesc(aPerson.BaseSpouse, cur_x + (this.BranchDistance + aPerson.BaseSpouse.Width) / 2 - aPerson.BaseSpouse.PtX, true);
							break;

						case TGEDCOMSex.svFemale:
							this.ShiftDesc(aPerson, cur_x + (this.BranchDistance + aPerson.Width) / 2 - aPerson.PtX, true);
							this.ShiftDesc(aPerson.BaseSpouse, cur_x - (this.BranchDistance + aPerson.BaseSpouse.Width) / 2 + 1 - aPerson.BaseSpouse.PtX, true);
							break;
					}
				} else {
					this.ShiftDesc(aPerson, cur_x - aPerson.PtX, true);
				}
			}
		}

		private void RecalcDesc(ref int[] edges, TreeChartPerson aPerson, Point aPt, bool aPreDef)
		{
			//edges = (int[])edges.Clone();
			if (aPerson != null) {
				int gen = aPerson.Generation;
				if (aPreDef) {
					aPerson.Pt = aPt;
				}

			    int offset = (edges[gen] > 0) ? this.fBranchDistance : this.fMargins;
				
                if (aPerson.Rect.Left <= edges[gen] + offset) {
					this.ShiftDesc(aPerson, edges[gen] + offset - aPerson.Rect.Left, true);
				}

                if (aPerson.Sex == TGEDCOMSex.svMale) {
					this.RecalcDescChilds(ref edges, aPerson);
					edges[gen] = aPerson.Rect.Right;
				}

				if (aPerson.GetSpousesCount() > 0) {
					TreeChartPerson prev = aPerson;
					int num = aPerson.GetSpousesCount();
					for (int i = 0; i < num; i++) {
						TreeChartPerson sp = aPerson.GetSpouse(i);
						Point sp_pt = new Point();

						switch (aPerson.Sex) {
							case TGEDCOMSex.svMale:
								sp_pt = new Point(prev.Rect.Right + (this.fBranchDistance + sp.Width / 2), aPerson.PtY);
								break;

							case TGEDCOMSex.svFemale:
								sp_pt = new Point(prev.Rect.Left - (this.fBranchDistance + sp.Width / 2), aPerson.PtY);
								break;
						}

						this.RecalcDesc(ref edges, sp, sp_pt, true);

						if (sp.Sex != TGEDCOMSex.svMale) {
							prev = sp;
						}
					}
				}

				if (aPerson.Sex == TGEDCOMSex.svFemale) {
					this.RecalcDescChilds(ref edges, aPerson);
					edges[gen] = aPerson.Rect.Right;
				}

				if (this.fWMax < aPerson.Rect.Right) {
					this.fWMax = aPerson.Rect.Right;
				}

				if (this.fHMax < aPerson.Rect.Bottom) {
					this.fHMax = aPerson.Rect.Bottom;
				}
			}
		}

		private void DrawAncestors(Graphics aCanvas, TreeChartPerson aPerson)
		{
			this.Draw(aCanvas, aPerson.Father, TChartKind.ckAncestors);
			this.Draw(aCanvas, aPerson.Mother, TChartKind.ckAncestors);
			int cr_y = aPerson.PtY - this.fLevelDistance / 2;

			if (aPerson.Father != null) {
				this.DrawLine(aCanvas, aPerson.Father.PtX, cr_y, aPerson.PtX, cr_y);
				this.DrawLine(aCanvas, aPerson.Father.PtX, aPerson.Father.PtY + aPerson.Father.Height, aPerson.Father.PtX, cr_y);
			}

			if (aPerson.Mother != null) {
				this.DrawLine(aCanvas, aPerson.PtX, cr_y, aPerson.Mother.PtX, cr_y);
				this.DrawLine(aCanvas, aPerson.Mother.PtX, aPerson.Mother.PtY + aPerson.Mother.Height, aPerson.Mother.PtX, cr_y);
			}

			if (aPerson.Father != null || aPerson.Mother != null) {
				this.DrawLine(aCanvas, aPerson.PtX, cr_y, aPerson.PtX, aPerson.PtY);
			}
		}

		private void DrawDescendants(Graphics aCanvas, TreeChartPerson aPerson)
		{
			int num = aPerson.GetChildsCount();
			for (int i = 0; i < num; i++) {
				this.Draw(aCanvas, aPerson.GetChild(i), TChartKind.ckDescendants);
			}

			int spb_ofs = (aPerson.Height - 10) / (aPerson.GetSpousesCount() + 1);
			int spb_beg = aPerson.PtY + (aPerson.Height - spb_ofs * (aPerson.GetSpousesCount() - 1)) / 2;

			switch (aPerson.Sex) {
				case TGEDCOMSex.svMale:
					int num3 = aPerson.GetSpousesCount();
					for (int i = 0; i < num3; i++) {
						int spb_v = spb_beg + spb_ofs * i;
						this.DrawLine(aCanvas, aPerson.Rect.Right + 1, spb_v, aPerson.GetSpouse(i).Rect.Left, spb_v);
					}
					break;

				case TGEDCOMSex.svFemale:
					int num2 = aPerson.GetSpousesCount();
					for (int i = 0; i < num2; i++) {
						int spb_v = spb_beg + spb_ofs * i;
						this.DrawLine(aCanvas, aPerson.GetSpouse(i).Rect.Right + 1, spb_v, aPerson.Rect.Left, spb_v);
					}
					break;
			}

			int num4 = aPerson.GetSpousesCount();
			for (int i = 0; i < num4; i++) {
				this.Draw(aCanvas, aPerson.GetSpouse(i), TChartKind.ckDescendants);
			}

			int cr_y = aPerson.PtY + aPerson.Height + this.fLevelDistance / 2;
			int cx = 0;
			if (aPerson.BaseSpouse == null || (aPerson.BaseSpouse != null && aPerson.BaseSpouse.GetSpousesCount() > 1))
			{
				cx = aPerson.PtX;
				spb_beg = aPerson.PtY + aPerson.Height - 1;
			}
			else
			{
				switch (aPerson.Sex) {
					case TGEDCOMSex.svMale:
						cx = (aPerson.Rect.Right + aPerson.BaseSpouse.Rect.Left) / 2;
						break;

					case TGEDCOMSex.svFemale:
						cx = (aPerson.BaseSpouse.Rect.Right + aPerson.Rect.Left) / 2;
						break;
				}

				spb_beg -= spb_ofs / 2;
			}

			if (aPerson.GetChildsCount() != 0)
			{
				this.DrawLine(aCanvas, cx, spb_beg, cx, cr_y);
				if (aPerson.GetChildsCount() == 1)
				{
					Point child_pt = aPerson.GetChild(0).Pt;
					this.DrawLine(aCanvas, child_pt.X, cr_y, child_pt.X, child_pt.Y);
				}
				else
				{
					int bpx = aPerson.GetChild(0).PtX;
					int epx = aPerson.GetChild(aPerson.GetChildsCount() - 1).PtX;
					this.DrawLine(aCanvas, bpx, cr_y, epx, cr_y);
					int num5 = aPerson.GetChildsCount();
					for (int i = 0; i < num5; i++) {
						Point child_pt = aPerson.GetChild(i).Pt;
						this.DrawLine(aCanvas, child_pt.X, cr_y, child_pt.X, child_pt.Y);
					}
				}
			}
		}

		private void ScrollRange(bool noRedraw = false)
		{
			if (this.fImageWidth < base.ClientRectangle.Width) {
				this.fRange.X = 0;
				this.LeftPos = (base.ClientRectangle.Width - this.fImageWidth) / 2;
			} else {
				this.fRange.X = this.fImageWidth - base.ClientRectangle.Width;
			}

			if (this.fImageHeight < base.ClientRectangle.Height) {
				this.fRange.Y = 0;
				this.TopPos = (base.ClientRectangle.Height - this.fImageHeight) / 2;
			} else {
				this.fRange.Y = this.fImageHeight - base.ClientRectangle.Height;
			}

            Win32Native.SetScrollRange(this.Handle, 0, 0, this.fRange.X, false);
            Win32Native.SetScrollRange(this.Handle, 1, 0, this.fRange.Y, false);

            if (!noRedraw) base.Invalidate();
		}

		private void SetBorderWidth(int value)
		{
			if (this.fBorderWidth != value) {
				this.fBorderWidth = value;
				base.Invalidate();
			}
		}

		private void SetLeftPos(int value)
		{
			if (value < 0) value = 0;
			if (value > this.fRange.X) value = this.fRange.X;

			if (this.fLeftPos != value) {
				ExtRect dummy = ExtRect.Empty();
				ExtRect R;
                Win32Native.ScrollWindowEx(this.Handle, this.fLeftPos - value, 0, ref dummy, ref dummy, 0, out R, 0u);
                Win32Native.SetScrollPos(this.Handle, 0, this.fLeftPos, true);
				base.Invalidate();
				this.fLeftPos = value;
				
				this.ResetView();
			}
		}

		private void SetTopPos(int value)
		{
			if (value < 0) value = 0;
			if (value > this.fRange.Y) value = this.fRange.Y;

			if (this.fTopPos != value) {
				ExtRect dummy = ExtRect.Empty();
				ExtRect R;
                Win32Native.ScrollWindowEx(this.Handle, 0, this.fTopPos - value, ref dummy, ref dummy, 0, out R, 0u);
                Win32Native.SetScrollPos(this.Handle, 1, this.fTopPos, true);
				base.Invalidate();
				this.fTopPos = value;
				
				this.ResetView();
			}
		}

		private void ResetView()
		{
			Size sz = this.ClientSize;
			fVisibleArea = ExtRect.Bounds(fLeftPos, fTopPos, sz.Width, sz.Height);
		}
		
		private void InternalDraw(Graphics aCanvas, TDrawMode mode)
		{
			Rectangle imgRect = new Rectangle(0, 0, fImageWidth, fImageHeight);
			if (this.BackgroundImage == null) {
				using (Brush brush = new SolidBrush(this.BackColor)) {
					aCanvas.FillRectangle(brush, imgRect);
				}
			} else {
				//ControlPaint.DrawBackgroundImage( aCanvas, this.BackgroundImage, BackColor, this.BackgroundImageLayout, CR, CR);
				using (TextureBrush textureBrush = new TextureBrush(this.BackgroundImage, WrapMode.Tile))
				{
					aCanvas.FillRectangle(textureBrush, imgRect);
				}
			}

			Rectangle CR = base.ClientRectangle;

			if (mode == TDrawMode.dmDefault)
			{
				this.fSPX = this.fBorderWidth - this.fLeftPos;
				this.fSPY = this.fBorderWidth - this.fTopPos;
				if (this.fImageWidth < CR.Width)
				{
					this.fSPX += (CR.Width - this.fImageWidth) / 2;
				}
				if (this.fImageHeight < CR.Height)
				{
					this.fSPY += (CR.Height - this.fImageHeight) / 2;
				}
			}
			else
			{
				this.fSPX = 0;
				this.fSPY = 0;
			}

			// this overrided routines

			this.Draw(aCanvas, this.fRoot, this.fKind);

			if (fScaleControl.Visible) fScaleControl.Draw(aCanvas);
		}

		protected void Draw(Graphics aCanvas, TreeChartPerson aPerson, TChartKind aDirKind)
		{
			if (aPerson != null) {
				switch (this.fKind) {
					case TChartKind.ckAncestors:
						this.DrawAncestors(aCanvas, aPerson);
						break;

					case TChartKind.ckDescendants:
						this.DrawDescendants(aCanvas, aPerson);
						break;

					case TChartKind.ckBoth:
						if (aPerson == this.fRoot || aDirKind == TChartKind.ckAncestors) this.DrawAncestors(aCanvas, aPerson);
						if (aPerson == this.fRoot || aDirKind == TChartKind.ckDescendants) this.DrawDescendants(aCanvas, aPerson);
						break;
				}

				this.DrawPerson(aCanvas, this.fSPX, this.fSPY, aPerson);
			}
		}

		public void GenChart(TGEDCOMIndividualRecord person, TChartKind kind)
		{
			if (person == null) return;
			
			this.InternalGenChart(person, kind);
			this.ScrollRange();
		}

		private void InternalGenChart(TGEDCOMIndividualRecord aPerson, TChartKind aKind)
		{
			try
			{
				this.fKind = aKind;
				this.fSelected = null;
				this.fPersons.Clear();
				this.Predef();
				this.fGraph.Clear();
				this.DoFilter(aPerson);
				this.fRoot = null;
				TChartKind fKind = this.fKind;

				fPreparedIndividuals.Clear();

				switch (fKind) {
					case TChartKind.ckAncestors:
						this.fPreparedFamilies.Clear();
						this.fRoot = this.DoAncestorsStep(null, aPerson, 1, false);
						break;
						
					case TChartKind.ckDescendants:
						this.fPreparedFamilies.Clear();
						this.fRoot = this.DoDescendantsStep(null, aPerson, 1);
						break;

					case TChartKind.ckBoth:
						this.fPreparedFamilies.Clear();
						this.fRoot = this.DoAncestorsStep(null, aPerson, 1, false);
						this.fPreparedFamilies.Clear();
						this.DoDescendantsStep(null, aPerson, 1);
						break;
				}

				this.fKinRoot = this.fRoot;
				this.RecalcChart();
			}
			catch (Exception ex)
			{
				this.fBase.Host.LogWrite("TTreeChartBox.InternalGenChart(): " + ex.Message);
			}
		}

		public void DoFilter(TGEDCOMIndividualRecord aRoot)
		{
			if (this.fFilter.BranchCut != TChartFilter.TBranchCut.bcNone) {
				TreeStats.InitExtCounts(this.fTree, 0);
				this.DoDescendantsFilter(aRoot);
				aRoot.ExtData = true;
			}
		}

		public EnumSet<TChartPersonSign> GetPersonSign(TGEDCOMIndividualRecord iRec)
		{
			EnumSet<TChartPersonSign> result = EnumSet<TChartPersonSign>.Create();
			
			int num = iRec.UserReferences.Count;
			for (int i = 0; i < num; i++)
			{
				string rs = iRec.UserReferences[i].StringValue;
				for (TChartPersonSign cps = TChartPersonSign.urRI_StGeorgeCross; cps <= TChartPersonSign.urLast; cps++)
				{
					if (rs == GKData.UserRefs[(int)cps]) result.Include(cps);
				}
			}
			
			return result;
		}

		public void RebuildKinships(bool noRedraw = false)
		{
			try
			{
				if (this.fOptions.Kinship) {
					TreeChartPerson p = this.fSelected;
					if (p != null) {
						this.fKinRoot = p;
						this.RecalcChart();

						this.ScrollRange(noRedraw);
					}
				}
			}
			catch (Exception ex)
			{
				this.fBase.Host.LogWrite("TTreeChartBox.RebuildKinships(): " + ex.Message);
			}
		}

		public void SaveSnapshot(string aFileName)
		{
			string ext = Path.GetExtension(aFileName).ToLowerInvariant();

			if ((ext == ".bmp" || ext == ".jpg") && this.fImageWidth >= 65535)
			{
				GKUtils.ShowError(LangMan.LS(LSID.LSID_TooMuchWidth));
			}
			else
			{
				Image pic = new Bitmap(this.fImageWidth, this.fImageHeight, PixelFormat.Format24bppRgb);
				Graphics canv = Graphics.FromImage(pic);
				try
				{
					try
					{
						this.Predef();
						this.InternalDraw(canv, TDrawMode.dmToFile);
					}
					finally
					{
						canv.Dispose();
					}

					if (ext == ".bmp") pic.Save(aFileName, ImageFormat.Bmp);
					else
					if (ext == ".emf") pic.Save(aFileName, ImageFormat.Emf);
					else 
					if (ext == ".jpg") pic.Save(aFileName, ImageFormat.Jpeg);
				}
				finally
				{
					SysUtils.Free(pic);
				}
			}
		}

		private void SetSelected(TreeChartPerson value)
		{
			if (this.fSelected != null) this.fSelected.Selected = false;
			this.fSelected = value;
			if (this.fSelected != null) this.fSelected.Selected = true;

			base.Invalidate();
		}

		private TreeChartPerson FindPersonByCoords(int aX, int aY)
		{
			TreeChartPerson result = null;
			
			aX -= this.fSPX;
			aY -= this.fSPY;
			int num = this.fPersons.Count;
			for (int i = 0; i < num; i++) {
				TreeChartPerson p = this.fPersons[i];
				if (p.Rect.Contains(aX, aY)) {
					result = p;
					break;
				}
			}

			return result;
		}

		private void SelectBy(int aX, int aY, bool needCenter)
		{
			TreeChartPerson p = this.FindPersonByCoords(aX, aY);
			this.SetSelected(p);

			//if (this.FTraceKinships && this.fOptions.Kinship) this.RebuildKinships(true);
			
			if (p != null && needCenter && this.fTraceSelected) CenterPerson(p);
		}

		private void SelectByRec(TGEDCOMIndividualRecord iRec)
		{
			int num = this.fPersons.Count;
			for (int i = 0; i < num; i++) {
				TreeChartPerson p = this.fPersons[i];
				if (p.Rec == iRec) {
					this.SetSelected(p);

					if (this.fTraceSelected) CenterPerson(p);

					return;
				}
			}

			this.SetSelected(null);
		}

		private void CenterPerson(TreeChartPerson p, bool animation = true)
		{
		    if (p == null) return;

			Point pt = p.Pt;
			int dstX = (pt.X) - (this.ClientSize.Width / 2);
			int dstY = (pt.Y + (p.Height / 2)) - (this.ClientSize.Height / 2);

			if (dstX < 0) dstX = dstX + (0 - dstX);
			if (dstY < 0) dstY = dstY + (0 - dstY);

			if ((this.LeftPos == dstX) && (this.TopPos == dstY)) return;

			if (animation) {
					TweenLibrary tween = new TweenLibrary();
					tween.StartTween(delegate(int newX, int newY) { this.LeftPos = newX; this.TopPos = newY; },
                                          this.LeftPos, this.TopPos, dstX, dstY, TweenAnimation.EaseInOutQuad, 20);
			} else {
				this.LeftPos = dstX; 
				this.TopPos = dstY;
			}
		}

		private bool DoDescendantsFilter(TGEDCOMIndividualRecord aPerson)
		{
			bool result = false;
			if (aPerson != null)
			{
				TChartFilter.TBranchCut branchCut = this.fFilter.BranchCut;
				switch (branchCut) {
					case TChartFilter.TBranchCut.bcYears:
						int year = GKUtils.GetIndependentYear(aPerson, "BIRT");
						result = (year >= this.fFilter.BranchYear);
						break;

					case TChartFilter.TBranchCut.bcPersons:
						result = (this.fFilter.BranchPersons.IndexOf(aPerson.XRef + ";") >= 0);
						break;
				}

				int num = aPerson.SpouseToFamilyLinks.Count;
				for (int i = 0; i < num; i++)
				{
					TGEDCOMFamilyRecord family = aPerson.SpouseToFamilyLinks[i].Family;

					int num2 = family.Childrens.Count;
					for (int j = 0; j < num2; j++)
					{
						TGEDCOMIndividualRecord child = family.Childrens[j].Value as TGEDCOMIndividualRecord;
						bool res_child = DoDescendantsFilter(child);
						result |= res_child;
					}
				}
				aPerson.ExtData = result;
			}
			return result;
		}
		
        private void DoPersonModify(PersonModifyEventArgs eArgs)
        {
            PersonModifyEventHandler eventHandler = (PersonModifyEventHandler)base.Events[TTreeChartBox.EventPersonModify];
            if (eventHandler == null) return;

            eventHandler(this, eArgs);
        }
		
        private void DoPersonProperties(MouseEventArgs eArgs)
        {
            MouseEventHandler eventHandler = (MouseEventHandler)base.Events[TTreeChartBox.EventPersonProperties];
            if (eventHandler == null) return;

            eventHandler(this, eArgs);
        }

		#region Protected methods
		
		protected override void OnResize(EventArgs e)
		{
			base.OnResize(e);

			this.ResetView();
			fScaleControl.Update();
		}

		protected override void WndProc(ref Message m)
		{
			base.WndProc(ref m);

			if (m.Msg == Win32Native.WM_SIZE)
			{
				this.ScrollRange();
			}
			else if (m.Msg == Win32Native.WM_GETDLGCODE)
			{
				m.Result = (IntPtr)(m.Result.ToInt32() | 
				                    Win32Native.DLGC_WANTARROWS | Win32Native.DLGC_WANTTAB | 
				                    Win32Native.DLGC_WANTCHARS | Win32Native.DLGC_WANTALLKEYS);
			}
			else if (m.Msg == Win32Native.WM_HSCROLL)
			{
				int page = this.ClientSize.Width / 10;
				uint wParam = (uint)m.WParam.ToInt32();
				int new_pos = SysUtils.DoScroll(this.Handle, wParam, 0, this.LeftPos, 0, this.fRange.X, 1, page);
				this.SetLeftPos(new_pos);
			}
			else if (m.Msg == Win32Native.WM_VSCROLL)
			{
				int page = this.ClientSize.Height / 10;
				uint wParam = (uint)m.WParam.ToInt32();
				int new_pos = SysUtils.DoScroll(this.Handle, wParam, 1, this.TopPos, 0, this.fRange.Y, 1, page);
				this.SetTopPos(new_pos);
			}
		}

		protected override void OnPaint(PaintEventArgs e)
		{
			this.InternalDraw(e.Graphics, TDrawMode.dmDefault);
		}

		protected override void OnDoubleClick(EventArgs e)
		{
			TreeChartPerson p = this.Selected;
			
			this.DoPersonModify(new PersonModifyEventArgs(p));
		}

		protected override void OnMouseWheel(MouseEventArgs e)
		{
			base.OnMouseWheel(e);

			if (ModifierKeys == Keys.Control) {
				float newScale = this.Scale;
				
				if (e.Delta > 0) {
					newScale -= 0.05f;
				} else {
					newScale += 0.05f;
				}
				
				if (newScale < 0.5 || newScale > 1.5) return;
				this.Scale = newScale;
			} else {
				var direction = e.Delta > 0 ? Win32Native.SB_PAGELEFT : Win32Native.SB_PAGERIGHT;
				uint msg = (ModifierKeys == Keys.Shift) ? Win32Native.WM_HSCROLL : Win32Native.WM_VSCROLL;

				Win32Native.SendMessage(this.Handle, msg, (IntPtr)direction, IntPtr.Zero);
			}
		}

		protected override void OnMouseDown(MouseEventArgs e)
		{
			base.OnMouseDown(e);
			if (!this.Focused) base.Focus();

			this.fMouseX = e.X;
			this.fMouseY = e.Y;

			switch (this.fMode) {
				case ChartControlMode.ccmDefault:
					this.SelectBy(e.X, e.Y, (e.Button == MouseButtons.Left));

					if (this.fSelected == null && e.Button == MouseButtons.Right)
					{
						this.Cursor = Cursors.SizeAll;
						this.fMode = ChartControlMode.ccmDragImage;
					}
					break;

				case ChartControlMode.ccmDragImage:
					break;

				case ChartControlMode.ccmControlsVisible:
					this.fScaleControl.MouseDown(e.X, e.Y);
					break;
			}
		}

		protected override void OnMouseMove(MouseEventArgs e)
		{
			switch (this.fMode)
			{
				case ChartControlMode.ccmDefault:
					if (this.fScaleControl.Contains(e.X, e.Y))
					{
						this.fMode = ChartControlMode.ccmControlsVisible;
						this.fScaleControl.Visible = true;
						this.fScaleControl.MouseMove(e.X, e.Y, ThumbMoved);

						Point pt = new Point(e.X, e.Y);
						pt.Offset(+this.Left, +this.Top);
						this.fToolTip.Show(this.fScaleControl.Tip, this, pt, 1500);
					} else {
//						TreeChartPerson p = this.fTreeBox.FindPersonByCoords(e.X, e.Y);
//
//						if (p != null && e.Button == MouseButtons.Left)
//						{
//							this.fTreeBox.DoDragDrop(p.Rec.XRef, DragDropEffects.Move);
//						}
					}
					break;

				case ChartControlMode.ccmDragImage:
					this.LeftPos = this.LeftPos - (e.X - this.fMouseX);
					this.TopPos = this.TopPos - (e.Y - this.fMouseY);
					this.fMouseX = e.X;
					this.fMouseY = e.Y;
					break;

				case ChartControlMode.ccmControlsVisible:
					if (!this.fScaleControl.Contains(e.X, e.Y)) {
						this.fMode = ChartControlMode.ccmDefault;
						this.fScaleControl.Visible = false;
						this.fToolTip.Hide(this);
					} else {
						this.fScaleControl.MouseMove(e.X, e.Y, ThumbMoved);
					}
					break;
			}
		}

		private void ThumbMoved(int position)
		{
			this.Scale = 0.4f + (position * 0.1f);
		}

		protected override void OnMouseUp(MouseEventArgs e)
		{
			switch (this.fMode) {
				case ChartControlMode.ccmDefault:
					if (this.fSelected != null && this.fSelected.Rec != null)
					{
						switch (e.Button) {
							case MouseButtons.Left:
								break;

							case MouseButtons.Right:
								this.DoPersonProperties(new MouseEventArgs(e.Button, 1, e.X, e.Y, 0));
								break;
						}
					}
					break;

				case ChartControlMode.ccmDragImage:
					this.Cursor = Cursors.Default;
					this.fMode = ChartControlMode.ccmDefault;
					break;

				case ChartControlMode.ccmControlsVisible:
					this.fScaleControl.MouseUp(e.X, e.Y);
					break;
			}
		}

		#endregion

		/*protected override CreateParams CreateParams
		{ 
			get {
				CreateParams cp = base.CreateParams;
				cp.Style |= Win32Native.WS_HSCROLL | Win32Native.WS_VSCROLL;
				return cp;
			}
		}*/

	}
}
