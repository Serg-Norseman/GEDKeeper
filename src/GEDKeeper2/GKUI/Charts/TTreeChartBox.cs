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
		
		private int FHMax;
		private int FWMax;

		private IBase fBase;
		private int FBranchDistance;
		private int FDepthLimit;
		private Font FDrawFont;
		private TChartFilter FFilter;
		private TGraph FGraph;
		private TChartKind FKind;
		private TreeChartPerson fKinRoot;
		private int FLevelDistance;
		private int FMargins;
		private TreeChartOptions fOptions;
		private bool FPathDebug;
		private TPersonList FPersons;
		private List<string> FPreparedFamilies = new List<string>();
		internal List<string> FPreparedIndividuals = new List<string>(); // FIXME
		private TreeChartPerson FRoot;
		private float FScale;
		private TScaleControl fScaleControl;
		private TreeChartPerson fSelected;
		private ShieldState FShieldState;
		//private string[] FSignsData;
		private Bitmap[] FSignsPic;
		private int FSpouseDistance;
		private bool FTraceKinships;
		private bool FTraceSelected;
		private TGEDCOMTree FTree;
		private ToolTip fToolTip;
		
		private int FBorderWidth;
		private int FLeftPos;
		private int FTopPos;
		private Point FRange;

		protected int FImageHeight;
		protected int FImageWidth;
		protected int FSPX;
		protected int FSPY;
		protected ExtRect FVisibleArea;

		//private int FGensCount;

		private Pen FLinePen;
		private Pen FDecorativeLinePen;
		private SolidBrush FSolidBlack;

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
			get { return this.FBorderWidth; }
			set { this.SetBorderWidth(value); }
		}

		public int BranchDistance
		{
			get { return this.FBranchDistance; }
			set { this.FBranchDistance = value; }
		}

		public int DepthLimit
		{
			get { return this.FDepthLimit; }
			set { this.FDepthLimit = value; }
		}

		public Font DrawFont
		{
			get { return this.FDrawFont; }
		}

		public TChartFilter Filter
		{
			get { return this.FFilter; }
		}

//		public int GensCount
//		{
//			get { return this.FGensCount; }
//		}

		public int IndividualsCount
		{
			get { return this.FPreparedIndividuals.Count; }
		}

		public TChartKind Kind
		{
			get { return FKind; }
			set { FKind = value; }
		}

		public int LeftPos
		{
			get { return this.FLeftPos; }
			set { this.SetLeftPos(value); }
		}

		public int Margins
		{
			get { return this.FMargins; }
			set { this.FMargins = value; }
		}

		public TreeChartOptions Options
		{
			get { return this.fOptions; }
			set { this.fOptions = value; }
		}

		public bool PathDebug
		{
			get { return this.FPathDebug; }
			set { this.FPathDebug = value; }
		}

		public TreeChartPerson Root
		{
			get { return this.FRoot; }
		}

		public new float Scale
		{
			get { return this.FScale; }
			set {
				this.FScale = value;

				fScaleControl.ThumbPos = (int)Math.Round((value -  0.4f) / 0.1f);

				this.Predef();
				this.RecalcChart();
				this.ScrollRange();

				if (this.FTraceSelected && this.Selected != null) {
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
			get { return this.FShieldState; }
			set { this.FShieldState = value; }
		}

		public int TopPos
		{
			get { return this.FTopPos; }
			set { this.SetTopPos(value); }
		}

		public bool TraceSelected
		{
			get { return this.FTraceSelected; }
			set { this.FTraceSelected = value; }
		}

		public bool TraceKinships
		{
			get { return this.FTraceKinships; }
			set { this.FTraceKinships = value; }
		}

		public TGEDCOMTree Tree
		{
			get { return this.FTree; }
			set { this.FTree = value; }
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

			this.FPersons = new TPersonList(true);
			this.FFilter = new TChartFilter();
			this.FSpouseDistance = 10;
			this.FBranchDistance = 40;
			this.FLevelDistance = 46;
			this.FMargins = 40;
			this.FDepthLimit = -1;
			this.fSelected = null;
			this.FScale = 1.0f;
			this.FTraceSelected = true;
			this.FGraph = new TGraph();

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

				this.FGraph.Dispose();
				this.FFilter.Dispose();
				this.FPersons.Dispose();
				this.DoneSigns();

                if (FDrawFont != null) FDrawFont.Dispose();
			}
			base.Dispose(disposing);
		}

		#endregion
		
		private void InitSigns()
		{
			//FSignsData = new string[] { "GEORGE_CROSS", "SOLDIER", "SOLDIER_FALL", "VETERAN_REAR" };

			FSignsPic = new Bitmap[4];

			FSignsPic[0] = GKResources.iTGGeorgeCross;
			FSignsPic[0].MakeTransparent(this.FSignsPic[0].GetPixel(0, 0));

			FSignsPic[1] = GKResources.iTGSoldier;
			FSignsPic[1].MakeTransparent(this.FSignsPic[1].GetPixel(0, 0));

			FSignsPic[2] = GKResources.iTGSoldierFall;
			FSignsPic[2].MakeTransparent(this.FSignsPic[2].GetPixel(0, 0));

			FSignsPic[3] = GKResources.iTGVeteranRear;
			FSignsPic[3].MakeTransparent(this.FSignsPic[3].GetPixel(0, 0));
		}

        // FIXME
		private void DoneSigns()
		{
			// dummy
		}

		private bool hasMediaFail = false;

		private TreeChartPerson AddDescPerson(TreeChartPerson aParent, TGEDCOMIndividualRecord iRec, TreeChartPerson.TPersonKind aKind, int aGeneration)
		{
			TreeChartPerson result;
			if (this.FRoot != null && this.FRoot.Rec == iRec)
			{
				result = this.FRoot;
				result.Parent = aParent;
				result.Kind = aKind;
			} else {
				result = new TreeChartPerson(this);
				result.BuildBy(iRec, ref hasMediaFail);
				result.Generation = aGeneration;
				result.Parent = aParent;
				result.Kind = aKind;
				this.FPersons.Add(result);

				if (this.fOptions.Kinship)
				{
					result.Node = this.FGraph.AddVertex(result);
				}

				if (aKind != TreeChartPerson.TPersonKind.pkSpouse && aParent != null)
				{
					aParent.AddChild(result);
				}
			}
			return result;
		}

		private TreeChartPerson DoAncestorsStep(TreeChartPerson aChild, TGEDCOMIndividualRecord aPerson, int aGeneration, bool dup_flag)
		{
			TreeChartPerson result = null;

			if (aPerson != null)
			{
				result = new TreeChartPerson(this);
				result.BuildBy(aPerson, ref hasMediaFail);
				result.Generation = aGeneration;
				this.FPersons.Add(result);

				if (aChild != null)
				{
					result.AddChild(aChild);
				}

				if (this.fOptions.Kinship)
				{
					result.Node = this.FGraph.AddVertex(result);
				}

				if ((this.FDepthLimit <= -1 || aGeneration != this.FDepthLimit) && aPerson.ChildToFamilyLinks.Count > 0 && !dup_flag)
				{
					TGEDCOMFamilyRecord family = aPerson.ChildToFamilyLinks[0].Family;

					bool is_dup = (this.FPreparedFamilies.IndexOf(family.XRef) >= 0);
					if (!is_dup) this.FPreparedFamilies.Add(family.XRef);

					if (GKUtils.IsRecordAccess(family.Restriction, this.FShieldState))
					{
						TGEDCOMIndividualRecord iFather = family.Husband.Value as TGEDCOMIndividualRecord;
						TGEDCOMIndividualRecord iMother = family.Wife.Value as TGEDCOMIndividualRecord;
						bool divorced = (family.GetTagStringValue("_STAT") == "NOTMARR");

						if (iFather != null && GKUtils.IsRecordAccess(iFather.Restriction, this.FShieldState))
						{
							result.Father = this.DoAncestorsStep(result, iFather, aGeneration + 1, is_dup);
							if (result.Father != null)
							{
								result.Father.Divorced = divorced;
								result.Father.IsDup = is_dup;
								if (this.fOptions.Kinship)
								{
									this.FGraph.AddUndirectedEdge(result.Node, result.Father.Node, 1, (int)TRelationKind.rkParent, (int)TRelationKind.rkChild);
								}
							}
						} else {
							result.Father = null;
						}

						if (iMother != null && GKUtils.IsRecordAccess(iMother.Restriction, this.FShieldState))
						{
							result.Mother = this.DoAncestorsStep(result, iMother, aGeneration + 1, is_dup);
							if (result.Mother != null)
							{
								result.Mother.Divorced = divorced;
								result.Mother.IsDup = is_dup;
								if (this.fOptions.Kinship)
								{
									this.FGraph.AddUndirectedEdge(result.Node, result.Mother.Node, 1, (int)TRelationKind.rkParent, (int)TRelationKind.rkChild);
								}
							}
						} else {
							result.Mother = null;
						}

						if (result.Father != null && result.Mother != null && this.fOptions.Kinship)
						{
							this.FGraph.AddUndirectedEdge(result.Father.Node, result.Mother.Node, 1, (int)TRelationKind.rkSpouse, (int)TRelationKind.rkSpouse);
						}
					}
				}
			}

			return result;
		}

		private TreeChartPerson DoDescendantsStep(TreeChartPerson aParent, TGEDCOMIndividualRecord aPerson, int aLevel)
		{
			TreeChartPerson result = null;
			if (aPerson != null && (!this.fOptions.ChildlessExclude || aLevel <= 1 || aPerson.SpouseToFamilyLinks.Count != 0 || !this.fBase.Context.IsChildless(aPerson)))
			{
				TGroupMode sourceMode = this.FFilter.SourceMode;

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
                        if (this.FFilter.SourceRef == "") {
                            filterSource = null;
                        } else {
                            filterSource = this.FTree.XRefIndex_Find(this.FFilter.SourceRef) as TGEDCOMSourceRecord;
                        }
                        if (aPerson.IndexOfSource(filterSource) < 0) {
                            return result;
                        }
                        break;
                }

				TChartFilter.TBranchCut branchCut = this.FFilter.BranchCut;
				if (branchCut != TChartFilter.TBranchCut.bcNone)
				{
					if (!(bool)aPerson.ExtData)
					{
						return result;
					}
				}

				TreeChartPerson res = this.AddDescPerson(aParent, aPerson, TreeChartPerson.TPersonKind.pkDefault, aLevel);
				result = res;

				int num = aPerson.SpouseToFamilyLinks.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMFamilyRecord family = aPerson.SpouseToFamilyLinks[i].Family;

					bool is_dup = (this.FPreparedFamilies.IndexOf(family.XRef) >= 0);
					if (!is_dup) this.FPreparedFamilies.Add(family.XRef);

					if (GKUtils.IsRecordAccess(family.Restriction, this.FShieldState))
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
							this.FGraph.AddUndirectedEdge(res.Node, res_parent.Node, 1, (int)TRelationKind.rkSpouse, (int)TRelationKind.rkSpouse);
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

						if ((this.FDepthLimit <= -1 || aLevel != this.FDepthLimit) && (!is_dup))
						{
							int num2 = family.Childrens.Count - 1;
							for (int j = 0; j <= num2; j++)
							{
								TGEDCOMIndividualRecord child_rec = family.Childrens[j].Value as TGEDCOMIndividualRecord;
								if (GKUtils.IsRecordAccess(child_rec.Restriction, this.FShieldState))
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
											this.FGraph.AddUndirectedEdge(child.Node, ft.Node, 1, (int)TRelationKind.rkParent, (int)TRelationKind.rkChild);
											this.FGraph.AddUndirectedEdge(child.Node, mt.Node, 1, (int)TRelationKind.rkParent, (int)TRelationKind.rkChild);
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
				IEnumerable<IEdge> edgesPath = this.FGraph.GetPath(target.Node);

				string tmp = "";
				TRelationKind prevRel = TRelationKind.rkNone;
				TRelationKind finRel = TRelationKind.rkNone;
				int great = 0;

				foreach (IEdge edge in edgesPath)
				{
					TreeChartPerson xFrom = edge.Source.Value as TreeChartPerson;
					TreeChartPerson xTo = edge.Target.Value as TreeChartPerson;
					TRelationKind curRel = FixLink(xFrom, xTo, (TRelationKind)((int)edge.Value));

					if (this.FPathDebug) {
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

				if (this.FPathDebug) {
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

			return (FVisibleArea.Contains(pL, pT) || FVisibleArea.Contains(pR, pT) ||
			        FVisibleArea.Contains(pL, pB) || FVisibleArea.Contains(pR, pB));
		}

		/*private bool LineIsVisible(int X1, int Y1, int X2, int Y2)
		{
			return (FVisibleArea.Contains(X1, Y1) || FVisibleArea.Contains(X2, Y2));
		}*/
		
		private void DrawLine(Graphics aCanvas, int X1, int Y1, int X2, int Y2)
		{
			//if (!LineIsVisible(X1, Y1, X2, Y2)) return;
			
			int sX = this.FSPX + X1;
			int sX2 = this.FSPX + X2;
			int sY = this.FSPY + Y1;
			int sY2 = this.FSPY + Y2;
			aCanvas.DrawLine(FLinePen, sX, sY, sX2, sY2);

			if (this.fOptions.Decorative) {
				if (sX == sX2) {
					aCanvas.DrawLine(FDecorativeLinePen, sX + 1, sY + 1, sX2 + 1, sY2 - 1);
				} else {
					if (sY == sY2) {
						aCanvas.DrawLine(FDecorativeLinePen, sX + 1, sY + 1, sX2 + 0, sY2 + 1);
					}
				}
			}
		}

		private void InitGraphics()
		{
			FLinePen = new Pen(Color.Black, 1f);
			FDecorativeLinePen = new Pen(Color.Silver, 1f);
			FSolidBlack = new SolidBrush(Color.Black);
		}

		private void DoneGraphics()
		{
			this.FLinePen.Dispose();
			this.FDecorativeLinePen.Dispose();
			this.FSolidBlack.Dispose();
		}

		private void TextOut(Graphics aCanvas, ExtRect rt, string s, int h, ref int line)
		{
			int stw = aCanvas.MeasureString(s, this.DrawFont).ToSize().Width;
			int rx = rt.Left + ((rt.Right - rt.Left + 1) - stw) / 2;
			int ry = rt.Top + (10 + (h * line));
			aCanvas.DrawString(s, this.DrawFont, FSolidBlack, (float)rx, (float)ry);
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
				this.DrawBorder(aCanvas, FLinePen, dt, true, person);
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

			if (has_port)
			{
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

			if (this.Options.Kinship)
			{
				this.TextOut(aCanvas, rt, person.Kinship, h, ref line);
			}

			if (this.Options.SignsVisible && !person.Signs.IsEmpty())
			{
				int i = 0;
				for (TChartPersonSign cps = TChartPersonSign.urRI_StGeorgeCross;
				     cps <= TChartPersonSign.urUSSR_RearVeteran; cps++)
				{
					if (person.Signs.Contains(cps))
					{
						Bitmap pic = this.FSignsPic[(int)cps - 1];
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
			double sc = (double)(this.FScale);
			int fsz = (int)checked((long)Math.Round(unchecked((double)this.fOptions.DefFont_Size * sc)));

		    string fontName = (fsz <= 7) ? "Small Fonts" : this.fOptions.DefFont_Name;

			this.FDrawFont = new Font(fontName, ((float)fsz), FontStyle.Regular, GraphicsUnit.Point);
			this.FSpouseDistance = (int)checked((long)Math.Round(10.0 * sc));
			this.FBranchDistance = (int)checked((long)Math.Round(40.0 * sc));
			this.FLevelDistance = (int)checked((long)Math.Round(46.0 * sc));
			this.FMargins = (int)checked((long)Math.Round(40.0 * sc));
		}

		private void RecalcAncestorsChart()
		{
			int[] edges = new int[256];
			InitEdges(ref edges);
			ExtList<TreeChartPerson> prev = new ExtList<TreeChartPerson>();
			try
			{
				this.RecalcAnc(prev, ref edges, this.FRoot, new Point(this.FMargins, this.FMargins));
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
			this.RecalcDesc(ref edges, this.FRoot, new Point(this.FMargins, this.FMargins), aPreDef);
		}

		private void RecalcChart()
		{
			if (this.fOptions.Kinship && this.fKinRoot != null)
			{
				this.FGraph.FindPathTree(this.fKinRoot.Node);
			}

			int num = this.FPersons.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				TreeChartPerson p = this.FPersons[i];

				if (this.fOptions.Kinship) {
					this.FindRelationship(p);
				}

				p.CalcBounds();
			}

			this.FHMax = 0;
			this.FWMax = 0;
			TChartKind fKind = this.FKind;

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

			this.FHMax = this.FHMax + this.FMargins - 1;
			this.FWMax = this.FWMax + this.FMargins - 1;
			this.FImageHeight = this.FHMax;
			this.FImageWidth = this.FWMax;
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
			if (aPerson != null)
			{
				aPerson.Pt = aPt;
				int gen = aPerson.Generation;

			    int offset = (edges[gen] > 0) ? this.FBranchDistance : this.FMargins;

                if (aPerson.Rect.Left <= edges[gen] + offset) {
					this.ShiftAnc(ref edges, aPerson, edges[gen] + offset - aPerson.Rect.Left);
				}
				edges[gen] = aPerson.Rect.Right;
				prev.Add(aPerson);
				if (aPerson.Rect.Top < 0)
				{
					offset = 0 - aPerson.Rect.Top + this.FMargins;
					int num = prev.Count - 1;
					for (int i = 0; i <= num; i++)
					{
						TreeChartPerson pp = prev[i] as TreeChartPerson;
						pp.PtY += offset;
					}
				}

				if (aPerson.Father != null && aPerson.Mother != null)
				{
					Point xpt = new Point(aPerson.PtX - (this.FSpouseDistance + aPerson.Father.Width / 2), aPerson.PtY - this.FLevelDistance - aPerson.Height);
					this.RecalcAnc(prev, ref edges, aPerson.Father, xpt);
					xpt = new Point(aPerson.PtX + (this.FSpouseDistance + aPerson.Mother.Width / 2), aPerson.PtY - this.FLevelDistance - aPerson.Height);
					this.RecalcAnc(prev, ref edges, aPerson.Mother, xpt);
					aPerson.PtX = (aPerson.Father.PtX + aPerson.Mother.PtX) / 2;
					edges[aPerson.Generation] = aPerson.Rect.Right;
				}
				else
				{
					Point xpt = new Point(aPerson.PtX, aPerson.PtY - this.FLevelDistance - aPerson.Height);
					if (aPerson.Father != null)
					{
						this.RecalcAnc(prev, ref edges, aPerson.Father, xpt);
					}
					else
					{
						if (aPerson.Mother != null)
						{
							this.RecalcAnc(prev, ref edges, aPerson.Mother, xpt);
						}
					}
				}

				if (this.FWMax < aPerson.Rect.Right) this.FWMax = aPerson.Rect.Right;
				if (this.FHMax < aPerson.Rect.Bottom) this.FHMax = aPerson.Rect.Bottom;
			}
		}

		private void ShiftDesc(TreeChartPerson aPerson, int aOffset, bool single)
		{
			if (aPerson != null)
			{
				if (aPerson == this.FRoot)
				{
					single = false;
				}

                aPerson.PtX += aOffset;
				
                if (aPerson.BaseSpouse != null && (aPerson.BaseSpouse.Sex == TGEDCOMSex.svFemale || aPerson.BaseSpouse.GetSpousesCount() == 1))
				{
					this.ShiftDesc(aPerson.BaseSpouse, aOffset, single);
				}
				else
				{
					if (!single)
					{
						this.ShiftDesc(aPerson.Father, aOffset, single);
						this.ShiftDesc(aPerson.Mother, aOffset, single);
					}
					else
					{
						if (aPerson.Flags.Contains(TreeChartPerson.TPersonFlag.pfDescByFather))
						{
							this.ShiftDesc(aPerson.Father, aOffset, single);
						}
						else
						{
							if (aPerson.Flags.Contains(TreeChartPerson.TPersonFlag.pfDescByMother))
							{
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
				if (fix_pair)
				{
					TGEDCOMSex sex = aPerson.Sex;
					if (sex != TGEDCOMSex.svMale)
					{
						if (sex == TGEDCOMSex.svFemale)
						{
							cent_x = (aPerson.BaseSpouse.Rect.Right + aPerson.Rect.Left) / 2;
						}
					}
					else
					{
						cent_x = (aPerson.Rect.Right + aPerson.BaseSpouse.Rect.Left) / 2;
					}
				}
				else
				{
					cent_x = aPerson.PtX;
				}
				int cur_y = aPerson.PtY + this.FLevelDistance + aPerson.Height;
				int childs_width = (aPerson.GetChildsCount() - 1) * this.FBranchDistance;

				int num = aPerson.GetChildsCount() - 1;
				for (int i = 0; i <= num; i++)
				{
					childs_width += aPerson.GetChild(i).Width;
				}

				int cur_x = cent_x - childs_width / 2;

				int num2 = aPerson.GetChildsCount() - 1;
				for (int i = 0; i <= num2; i++)
				{
					TreeChartPerson child = aPerson.GetChild(i);
					this.RecalcDesc(ref edges, child, new Point(cur_x + child.Width / 2, cur_y), true);
					cur_x = child.Rect.Right + this.FBranchDistance;
				}

				cur_x = aPerson.GetChild(0).PtX;
				if (aPerson.GetChildsCount() > 1)
				{
					cur_x += (aPerson.GetChild(aPerson.GetChildsCount() - 1).PtX - cur_x) / 2;
				}
				if (fix_pair)
				{
					TGEDCOMSex sex2 = aPerson.Sex;
					if (sex2 != TGEDCOMSex.svMale)
					{
						if (sex2 == TGEDCOMSex.svFemale)
						{
							this.ShiftDesc(aPerson, cur_x + (this.BranchDistance + aPerson.Width) / 2 - aPerson.PtX, true);
							this.ShiftDesc(aPerson.BaseSpouse, cur_x - (this.BranchDistance + aPerson.BaseSpouse.Width) / 2 + 1 - aPerson.BaseSpouse.PtX, true);
						}
					}
					else
					{
						this.ShiftDesc(aPerson, cur_x - (this.BranchDistance + aPerson.Width) / 2 + 1 - aPerson.PtX, true);
						this.ShiftDesc(aPerson.BaseSpouse, cur_x + (this.BranchDistance + aPerson.BaseSpouse.Width) / 2 - aPerson.BaseSpouse.PtX, true);
					}
				}
				else
				{
					this.ShiftDesc(aPerson, cur_x - aPerson.PtX, true);
				}
			}
		}

		private void RecalcDesc(ref int[] edges, TreeChartPerson aPerson, Point aPt, bool aPreDef)
		{
			//edges = (int[])edges.Clone();
			if (aPerson != null)
			{
				int gen = aPerson.Generation;
				if (aPreDef)
				{
					aPerson.Pt = aPt;
				}

			    int offset = (edges[gen] > 0) ? this.FBranchDistance : this.FMargins;
				
                if (aPerson.Rect.Left <= edges[gen] + offset)
				{
					this.ShiftDesc(aPerson, edges[gen] + offset - aPerson.Rect.Left, true);
				}
				if (aPerson.Sex == TGEDCOMSex.svMale)
				{
					this.RecalcDescChilds(ref edges, aPerson);
					edges[gen] = aPerson.Rect.Right;
				}

				if (aPerson.GetSpousesCount() > 0)
				{
					TreeChartPerson prev = aPerson;
					int num = aPerson.GetSpousesCount() - 1;
					for (int i = 0; i <= num; i++)
					{
						TreeChartPerson sp = aPerson.GetSpouse(i);
						TGEDCOMSex sex = aPerson.Sex;
						Point sp_pt = new Point();
						if (sex != TGEDCOMSex.svMale)
						{
							if (sex == TGEDCOMSex.svFemale)
							{
								sp_pt = new Point(prev.Rect.Left - (this.FBranchDistance + sp.Width / 2), aPerson.PtY);
							}
						}
						else
						{
							sp_pt = new Point(prev.Rect.Right + (this.FBranchDistance + sp.Width / 2), aPerson.PtY);
						}
						this.RecalcDesc(ref edges, sp, sp_pt, true);
						if (sp.Sex != TGEDCOMSex.svMale)
						{
							prev = sp;
						}
					}
				}

				if (aPerson.Sex == TGEDCOMSex.svFemale)
				{
					this.RecalcDescChilds(ref edges, aPerson);
					edges[gen] = aPerson.Rect.Right;
				}
				if (this.FWMax < aPerson.Rect.Right)
				{
					this.FWMax = aPerson.Rect.Right;
				}
				if (this.FHMax < aPerson.Rect.Bottom)
				{
					this.FHMax = aPerson.Rect.Bottom;
				}
			}
		}

		private void DrawAncestors(Graphics aCanvas, TreeChartPerson aPerson)
		{
			this.Draw(aCanvas, aPerson.Father, TChartKind.ckAncestors);
			this.Draw(aCanvas, aPerson.Mother, TChartKind.ckAncestors);
			int cr_y = aPerson.PtY - this.FLevelDistance / 2;
			if (aPerson.Father != null)
			{
				this.DrawLine(aCanvas, aPerson.Father.PtX, cr_y, aPerson.PtX, cr_y);
				this.DrawLine(aCanvas, aPerson.Father.PtX, aPerson.Father.PtY + aPerson.Father.Height, aPerson.Father.PtX, cr_y);
			}
			if (aPerson.Mother != null)
			{
				this.DrawLine(aCanvas, aPerson.PtX, cr_y, aPerson.Mother.PtX, cr_y);
				this.DrawLine(aCanvas, aPerson.Mother.PtX, aPerson.Mother.PtY + aPerson.Mother.Height, aPerson.Mother.PtX, cr_y);
			}
			if (aPerson.Father != null || aPerson.Mother != null)
			{
				this.DrawLine(aCanvas, aPerson.PtX, cr_y, aPerson.PtX, aPerson.PtY);
			}
		}

		private void DrawDescendants(Graphics aCanvas, TreeChartPerson aPerson)
		{
			int num = aPerson.GetChildsCount() - 1;
			for (int i = 0; i <= num; i++)
			{
				this.Draw(aCanvas, aPerson.GetChild(i), TChartKind.ckDescendants);
			}

			int spb_ofs = (aPerson.Height - 10) / (aPerson.GetSpousesCount() + 1);
			int spb_beg = aPerson.PtY + (aPerson.Height - spb_ofs * (aPerson.GetSpousesCount() - 1)) / 2;

			TGEDCOMSex sex = aPerson.Sex;
			if (sex != TGEDCOMSex.svMale)
			{
				if (sex == TGEDCOMSex.svFemale)
				{
					int num2 = aPerson.GetSpousesCount() - 1;
					for (int i = 0; i <= num2; i++)
					{
						int spb_v = spb_beg + spb_ofs * i;
						this.DrawLine(aCanvas, aPerson.GetSpouse(i).Rect.Right + 1, spb_v, aPerson.Rect.Left, spb_v);
					}
				}
			}
			else
			{
				int num3 = aPerson.GetSpousesCount() - 1;
				for (int i = 0; i <= num3; i++)
				{
					int spb_v = spb_beg + spb_ofs * i;
					this.DrawLine(aCanvas, aPerson.Rect.Right + 1, spb_v, aPerson.GetSpouse(i).Rect.Left, spb_v);
				}
			}

			int num4 = aPerson.GetSpousesCount() - 1;
			for (int i = 0; i <= num4; i++)
			{
				this.Draw(aCanvas, aPerson.GetSpouse(i), TChartKind.ckDescendants);
			}

			int cr_y = aPerson.PtY + aPerson.Height + this.FLevelDistance / 2;
			int cx = 0;
			if (aPerson.BaseSpouse == null || (aPerson.BaseSpouse != null && aPerson.BaseSpouse.GetSpousesCount() > 1))
			{
				cx = aPerson.PtX;
				spb_beg = aPerson.PtY + aPerson.Height - 1;
			}
			else
			{
				TGEDCOMSex sex2 = aPerson.Sex;
				if (sex2 != TGEDCOMSex.svMale)
				{
					if (sex2 == TGEDCOMSex.svFemale)
					{
						cx = (aPerson.BaseSpouse.Rect.Right + aPerson.Rect.Left) / 2;
					}
				}
				else
				{
					cx = (aPerson.Rect.Right + aPerson.BaseSpouse.Rect.Left) / 2;
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
					int num5 = aPerson.GetChildsCount() - 1;
					for (int i = 0; i <= num5; i++)
					{
						Point child_pt = aPerson.GetChild(i).Pt;
						this.DrawLine(aCanvas, child_pt.X, cr_y, child_pt.X, child_pt.Y);
					}
				}
			}
		}

		private void ScrollRange(bool noRedraw = false)
		{
			if (this.FImageWidth < base.ClientRectangle.Width) {
				this.FRange.X = 0;
				this.LeftPos = (base.ClientRectangle.Width - this.FImageWidth) / 2;
			} else {
				this.FRange.X = this.FImageWidth - base.ClientRectangle.Width;
			}

			if (this.FImageHeight < base.ClientRectangle.Height) {
				this.FRange.Y = 0;
				this.TopPos = (base.ClientRectangle.Height - this.FImageHeight) / 2;
			} else {
				this.FRange.Y = this.FImageHeight - base.ClientRectangle.Height;
			}

            Win32Native.SetScrollRange(this.Handle, 0, 0, this.FRange.X, false);
            Win32Native.SetScrollRange(this.Handle, 1, 0, this.FRange.Y, false);

            if (!noRedraw) base.Invalidate();
		}

		private void SetBorderWidth(int value)
		{
			if (this.FBorderWidth != value)
			{
				this.FBorderWidth = value;
				base.Invalidate();
			}
		}

		private void SetLeftPos(int value)
		{
			if (value < 0) value = 0;
			if (value > this.FRange.X) value = this.FRange.X;

			if (this.FLeftPos != value)
			{
				ExtRect dummy = ExtRect.Empty();
				ExtRect R;
                Win32Native.ScrollWindowEx(this.Handle, this.FLeftPos - value, 0, ref dummy, ref dummy, 0, out R, 0u);
                Win32Native.SetScrollPos(this.Handle, 0, this.FLeftPos, true);
				base.Invalidate();
				this.FLeftPos = value;
				
				this.ResetView();
			}
		}

		private void SetTopPos(int value)
		{
			if (value < 0) value = 0;
			if (value > this.FRange.Y) value = this.FRange.Y;

			if (this.FTopPos != value)
			{
				ExtRect dummy = ExtRect.Empty();
				ExtRect R;
                Win32Native.ScrollWindowEx(this.Handle, 0, this.FTopPos - value, ref dummy, ref dummy, 0, out R, 0u);
                Win32Native.SetScrollPos(this.Handle, 1, this.FTopPos, true);
				base.Invalidate();
				this.FTopPos = value;
				
				this.ResetView();
			}
		}

		private void ResetView()
		{
			Size sz = this.ClientSize;
			FVisibleArea = ExtRect.Bounds(FLeftPos, FTopPos, sz.Width, sz.Height);
		}
		
		private void InternalDraw(Graphics aCanvas, TDrawMode mode)
		{
			Rectangle imgRect = new Rectangle(0, 0, FImageWidth, FImageHeight);
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
				this.FSPX = this.FBorderWidth - this.FLeftPos;
				this.FSPY = this.FBorderWidth - this.FTopPos;
				if (this.FImageWidth < CR.Width)
				{
					this.FSPX += (CR.Width - this.FImageWidth) / 2;
				}
				if (this.FImageHeight < CR.Height)
				{
					this.FSPY += (CR.Height - this.FImageHeight) / 2;
				}
			}
			else
			{
				this.FSPX = 0;
				this.FSPY = 0;
			}

			// this overrided routines

			this.Draw(aCanvas, this.FRoot, this.FKind);

			if (fScaleControl.Visible) fScaleControl.Draw(aCanvas);
		}

		protected void Draw(Graphics aCanvas, TreeChartPerson aPerson, TChartKind aDirKind)
		{
			if (aPerson != null)
			{
				switch (this.FKind) {
					case TChartKind.ckAncestors:
						this.DrawAncestors(aCanvas, aPerson);
						break;

					case TChartKind.ckDescendants:
						this.DrawDescendants(aCanvas, aPerson);
						break;

					case TChartKind.ckBoth:
						if (aPerson == this.FRoot || aDirKind == TChartKind.ckAncestors) this.DrawAncestors(aCanvas, aPerson);
						if (aPerson == this.FRoot || aDirKind == TChartKind.ckDescendants) this.DrawDescendants(aCanvas, aPerson);
						break;
				}

				this.DrawPerson(aCanvas, this.FSPX, this.FSPY, aPerson);
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
				this.FKind = aKind;
				this.fSelected = null;
				this.FPersons.Clear();
				this.Predef();
				this.FGraph.Clear();
				this.DoFilter(aPerson);
				this.FRoot = null;
				TChartKind fKind = this.FKind;

				FPreparedIndividuals.Clear();

				switch (fKind) {
					case TChartKind.ckAncestors:
						this.FPreparedFamilies.Clear();
						this.FRoot = this.DoAncestorsStep(null, aPerson, 1, false);
						break;
						
					case TChartKind.ckDescendants:
						this.FPreparedFamilies.Clear();
						this.FRoot = this.DoDescendantsStep(null, aPerson, 1);
						break;

					case TChartKind.ckBoth:
						this.FPreparedFamilies.Clear();
						this.FRoot = this.DoAncestorsStep(null, aPerson, 1, false);
						this.FPreparedFamilies.Clear();
						this.DoDescendantsStep(null, aPerson, 1);
						break;
				}

				this.fKinRoot = this.FRoot;
				this.RecalcChart();
			}
			catch (Exception ex)
			{
				this.fBase.Host.LogWrite("TAncestryChartBox.InternalGenChart(): " + ex.Message);
			}
		}

		public void DoFilter(TGEDCOMIndividualRecord aRoot)
		{
			if (this.FFilter.BranchCut != TChartFilter.TBranchCut.bcNone)
			{
				TreeStats.InitExtCounts(this.FTree, 0);
				this.DoDescendantsFilter(aRoot);
				aRoot.ExtData = true;
			}
		}

		public EnumSet<TChartPersonSign> GetPersonSign(TGEDCOMIndividualRecord iRec)
		{
			EnumSet<TChartPersonSign> result = new EnumSet<TChartPersonSign>();
			
			int num = iRec.UserReferences.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				string rs = iRec.UserReferences[i].StringValue;
				for (TChartPersonSign cps = TChartPersonSign.urRI_StGeorgeCross; 
				     cps <= TChartPersonSign.urLast; cps++)
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
				if (this.fOptions.Kinship)
				{
					TreeChartPerson p = this.fSelected;
					if (p != null)
					{
						this.fKinRoot = p;
						this.RecalcChart();

						this.ScrollRange(noRedraw);
					}
				}
			}
			catch (Exception ex)
			{
				this.fBase.Host.LogWrite("TAncestryChartBox.RebuildKinships(): " + ex.Message);
			}
		}

		public void SaveSnapshot(string aFileName)
		{
			string ext = Path.GetExtension(aFileName).ToLowerInvariant();

			if ((ext == ".bmp" || ext == ".jpg") && this.FImageWidth >= 65535)
			{
				GKUtils.ShowError(LangMan.LS(LSID.LSID_TooMuchWidth));
			}
			else
			{
				Image pic = new Bitmap(this.FImageWidth, this.FImageHeight, PixelFormat.Format24bppRgb);
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
			
			aX -= this.FSPX;
			aY -= this.FSPY;
			int num = this.FPersons.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				TreeChartPerson p = this.FPersons[i];
				if (p.Rect.Contains(aX, aY))
				{
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
			
			if (p != null && needCenter && this.FTraceSelected) CenterPerson(p);
		}

		private void SelectByRec(TGEDCOMIndividualRecord iRec)
		{
			int num = this.FPersons.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				TreeChartPerson p = this.FPersons[i];
				if (p.Rec == iRec)
				{
					this.SetSelected(p);

					if (this.FTraceSelected) CenterPerson(p);

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
				TChartFilter.TBranchCut branchCut = this.FFilter.BranchCut;
				switch (branchCut) {
					case TChartFilter.TBranchCut.bcYears:
						int year = GKUtils.GetIndependentYear(aPerson, "BIRT");
						result = (year >= this.FFilter.BranchYear);
						break;

					case TChartFilter.TBranchCut.bcPersons:
						result = (this.FFilter.BranchPersons.IndexOf(aPerson.XRef + ";") >= 0);
						break;
				}

				int num = aPerson.SpouseToFamilyLinks.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMFamilyRecord family = aPerson.SpouseToFamilyLinks[i].Family;

					int num2 = family.Childrens.Count - 1;
					for (int j = 0; j <= num2; j++)
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
				int new_pos = SysUtils.DoScroll(this.Handle, wParam, 0, this.LeftPos, 0, this.FRange.X, 1, page);
				this.SetLeftPos(new_pos);
			}
			else if (m.Msg == Win32Native.WM_VSCROLL)
			{
				int page = this.ClientSize.Height / 10;
				uint wParam = (uint)m.WParam.ToInt32();
				int new_pos = SysUtils.DoScroll(this.Handle, wParam, 1, this.TopPos, 0, this.FRange.Y, 1, page);
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
