using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Drawing.Imaging;
using System.IO;
using System.Windows.Forms;

using GKCommon;
using GKCommon.GEDCOM;
using GKCommon.GEDCOM.Enums;
using GKCommon.Graph;
using GKCore;
using GKCore.Interfaces;
using GKCore.Kinships;
using GKCore.Options;
using GKCore.Types;

namespace GKUI.Charts
{
	public delegate void ThumbMoved(int position);

    public delegate void PersonModifyEventHandler(object sender, PersonModifyEventArgs eArgs);
    
    public delegate void RootChangedEventHandler(object sender, TreeChartPerson person);

    /// <summary>
    /// Localization: dirty
    /// </summary>
    public class TreeChartBox : Panel
	{
    	public const int DefMargins = 24;
		public const int DefSpouseDistance = 10;
		public const int DefBranchDistance = 40;
		public const int DefLevelDistance = 46;
    	
		private const bool DEBUG_IMAGE = false;
		
		#region Subtypes
		
        private enum ChartControlMode
        {
            ccmDefault,
            ccmDragImage,
            ccmControlsVisible
        }

		public enum ChartKind
		{
			ckAncestors,
			ckDescendants,
			ckBoth
		}

		public enum DrawMode
		{
			dmScreen,
			dmFile
		}

		public enum MouseAction
		{
			maNone,
			maProperties,
			maExpand
		}
		
		#endregion
		
		#region Private fields
		
		private IBase fBase;
		private int fBranchDistance;
		private int fDepthLimit;
		private Font fDrawFont;
		private ChartFilter fFilter;
		private TGraph fGraph;
		private ChartKind fKind;
		private TreeChartPerson fKinRoot;
		private int fLevelDistance;
		private int fMargins;
		private TreeChartOptions fOptions;
		private bool fPathDebug;
		private PersonList fPersons;
		private List<string> fPreparedFamilies = new List<string>();
		internal List<string> fPreparedIndividuals = new List<string>(); // FIXME
		private TreeChartPerson fRoot;
		private float fScale;
		private ScaleControl fScaleControl;
		private TreeChartPerson fSelected;
		private GEDCOMIndividualRecord fSaveSelection;
		private ShieldState fShieldState;
		//private string[] FSignsData;
		private Bitmap[] fSignsPic;
		private int fSpouseDistance;
		private bool fTraceKinships;
		private bool fTraceSelected;
		private GEDCOMTree fTree;
		private ToolTip fToolTip;
		
		private int fBorderWidth;
		private int fLeftPos; // scroll position inside the tree
		private int fTopPos;
		private Size fScrollRange;
		private ExtRect fTreeBounds;

		protected int fImageHeight;
		protected int fImageWidth;
		internal int fSPX; // drawing relative offset of tree on graphics
		internal int fSPY;
		protected ExtRect fVisibleArea;

		private Pen fLinePen;
		private Pen fDecorativeLinePen;
		private SolidBrush fSolidBlack;

		private int fMouseX;
		private int fMouseY;
		private ChartControlMode fMode = ChartControlMode.ccmDefault;
		
		/*private TreeChartPerson fHighlightedPerson;
		private long fHighlightedStart;
		private PersonControl fPersonControl;*/
		
		private Bitmap fExpPic;

        private static readonly object EventPersonModify;
        private static readonly object EventRootChanged;
        private static readonly object EventPersonProperties;

        private IContainer fComponents;
        private System.Windows.Forms.Timer fTimer;
        
		#endregion

		#region Public properties
		
		public event PersonModifyEventHandler PersonModify
		{
			add { base.Events.AddHandler(TreeChartBox.EventPersonModify, value); }
			remove { base.Events.RemoveHandler(TreeChartBox.EventPersonModify, value); }
		}

		public event RootChangedEventHandler RootChanged
		{
			add { base.Events.AddHandler(TreeChartBox.EventRootChanged, value); }
			remove { base.Events.RemoveHandler(TreeChartBox.EventRootChanged, value); }
		}

		public event MouseEventHandler PersonProperties
		{
			add { base.Events.AddHandler(TreeChartBox.EventPersonProperties, value); }
			remove { base.Events.RemoveHandler(TreeChartBox.EventPersonProperties, value); }
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

		public ChartFilter Filter
		{
			get { return this.fFilter; }
		}

		public int IndividualsCount
		{
			get { return this.fPreparedIndividuals.Count; }
		}

		public ChartKind Kind
		{
			get { return fKind; }
			set { fKind = value; }
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

				if (this.fTraceSelected && this.Selected != null) {
					this.CenterPerson(this.Selected, false);
				}
			}
		}

		public new ScaleControl ScaleControl
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

		public GEDCOMTree Tree
		{
			get { return this.fTree; }
			set { this.fTree = value; }
		}

		#endregion
		
		#region Instance control
		
        static TreeChartBox()
        {
            TreeChartBox.EventPersonModify = new object();
            TreeChartBox.EventPersonProperties = new object();
        }

		public TreeChartBox()
		{
			base.BorderStyle = BorderStyle.Fixed3D;
			base.DoubleBuffered = true;
			base.TabStop = true;
			base.BackColor = Color.White;

			this.InitSigns();

			this.fLeftPos = 0;
			this.fTopPos = 0;
			this.fPersons = new PersonList(true);
			this.fFilter = new ChartFilter();
			this.fSpouseDistance = DefSpouseDistance;
			this.fBranchDistance = DefBranchDistance;
			this.fLevelDistance = DefLevelDistance;
			this.fMargins = DefMargins;
			this.fDepthLimit = -1;
			this.fSelected = null;
			this.fScale = 1.0f;
			this.fTraceSelected = true;
			this.fGraph = new TGraph();

			this.fScaleControl = new ScaleControl(this);
			//this.fPersonControl = new PersonControl(this);
			this.fToolTip = new ToolTip();

			//this.AutoScroll = false;
			//this.HorizontalScroll.Visible = true;
			//this.VerticalScroll.Visible = true;

			//Win32Native.EnableScrollBar(this.Handle, Win32Native.SB_HORZ, Win32Native.ESB_ENABLE_BOTH);
			//Win32Native.EnableScrollBar(this.Handle, Win32Native.SB_VERT, Win32Native.ESB_ENABLE_BOTH);

			this.InitTimer();
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
			
			fExpPic = GKResources.iExpand;
			fExpPic.MakeTransparent(this.fExpPic.GetPixel(0, 0));
		}

        // FIXME
		private void DoneSigns()
		{
			// dummy
		}

		private bool hasMediaFail = false;

		private TreeChartPerson AddDescPerson(TreeChartPerson parent, GEDCOMIndividualRecord iRec, PersonKind aKind, int generation)
		{
			try
			{
				TreeChartPerson result;
				if (this.fRoot != null && this.fRoot.Rec == iRec) {
					result = this.fRoot;
					result.Parent = parent;
					result.Kind = aKind;
				} else {
					result = new TreeChartPerson(this);
					result.BuildBy(iRec, ref hasMediaFail);
					result.Generation = generation;
					result.Parent = parent;
					result.Kind = aKind;
					this.fPersons.Add(result);

					if (this.fOptions.Kinship) {
						result.Node = this.fGraph.AddVertex(result);
					}

					if (aKind != PersonKind.pkSpouse && parent != null) {
						parent.AddChild(result);
					}
				}
				result.Flags.Include(PersonFlag.pfDescWalk);
				return result;
			}
			catch (Exception ex)
			{
				this.fBase.Host.LogWrite("TreeChartBox.AddDescPerson(): " + ex.Message);
				throw;
			}
		}

		private TreeChartPerson DoAncestorsStep(TreeChartPerson aChild, GEDCOMIndividualRecord aPerson, int aGeneration, bool dupFlag)
		{
			try
			{
				TreeChartPerson result = null;

				if (aPerson != null)
				{
					result = new TreeChartPerson(this);
					result.BuildBy(aPerson, ref hasMediaFail);
					result.Generation = aGeneration;
					result.Flags.Include(PersonFlag.pfAncWalk);
					this.fPersons.Add(result);

					if (aChild != null)
					{
						result.AddChild(aChild);
					}

					if (this.fOptions.Kinship)
					{
						result.Node = this.fGraph.AddVertex(result);
					}

					if ((this.fDepthLimit <= -1 || aGeneration != this.fDepthLimit) && aPerson.ChildToFamilyLinks.Count > 0 && !dupFlag)
					{
						GEDCOMFamilyRecord family = aPerson.ChildToFamilyLinks[0].Family;

						bool isDup = (this.fPreparedFamilies.IndexOf(family.XRef) >= 0);
						if (!isDup) this.fPreparedFamilies.Add(family.XRef);

						if (GKUtils.IsRecordAccess(family.Restriction, this.fShieldState))
						{
							GEDCOMIndividualRecord iFather = family.Husband.Value as GEDCOMIndividualRecord;
							GEDCOMIndividualRecord iMother = family.Wife.Value as GEDCOMIndividualRecord;
							bool divorced = (family.GetTagStringValue("_STAT") == "NOTMARR");

							if (iFather != null && GKUtils.IsRecordAccess(iFather.Restriction, this.fShieldState))
							{
								result.Father = this.DoAncestorsStep(result, iFather, aGeneration + 1, isDup);
								if (result.Father != null)
								{
									result.Father.Divorced = divorced;
									result.Father.IsDup = isDup;
									if (this.fOptions.Kinship)
									{
										this.fGraph.AddUndirectedEdge(result.Node, result.Father.Node, 1, (int)RelationKind.rkParent, (int)RelationKind.rkChild);
									}
								}
							} else {
								result.Father = null;
							}

							if (iMother != null && GKUtils.IsRecordAccess(iMother.Restriction, this.fShieldState))
							{
								result.Mother = this.DoAncestorsStep(result, iMother, aGeneration + 1, isDup);
								if (result.Mother != null)
								{
									result.Mother.Divorced = divorced;
									result.Mother.IsDup = isDup;
									if (this.fOptions.Kinship)
									{
										this.fGraph.AddUndirectedEdge(result.Node, result.Mother.Node, 1, (int)RelationKind.rkParent, (int)RelationKind.rkChild);
									}
								}
							} else {
								result.Mother = null;
							}

							if (result.Father != null && result.Mother != null && this.fOptions.Kinship)
							{
								this.fGraph.AddUndirectedEdge(result.Father.Node, result.Mother.Node, 1, (int)RelationKind.rkSpouse, (int)RelationKind.rkSpouse);
							}
						}
					} else {
						if (aPerson.ChildToFamilyLinks.Count > 0) {
							result.Flags.Include(PersonFlag.pfHasInvAnc);
						}
					}
					
					if (aPerson.aux_GetChildsCount() > 1 || aPerson.SpouseToFamilyLinks.Count > 1) {
						result.Flags.Include(PersonFlag.pfHasInvDesc);
					}
				}

				return result;
			}
			catch (Exception ex)
			{
				this.fBase.Host.LogWrite("TreeChartBox.DoAncestorsStep(): " + ex.Message);
				throw;
			}
		}

		private TreeChartPerson DoDescendantsStep(TreeChartPerson parent, GEDCOMIndividualRecord person, int level)
		{
			try
			{
				TreeChartPerson result = null;
				if (person != null && (!this.fOptions.ChildlessExclude || level <= 1 || person.SpouseToFamilyLinks.Count != 0 || !this.fBase.Context.IsChildless(person)))
				{
					FilterGroupMode sourceMode = this.fFilter.SourceMode;

					switch (sourceMode)
					{
						case FilterGroupMode.gmAll:
							break;

						case FilterGroupMode.gmNone:
							if (person.SourceCitations.Count != 0) {
								return result;
							}
							break;
							
						case FilterGroupMode.gmAny:
							if (person.SourceCitations.Count == 0) {
								return result;
							}
							break;

						case FilterGroupMode.gmSelected:
							GEDCOMSourceRecord filterSource;
							if (this.fFilter.SourceRef == "") {
								filterSource = null;
							} else {
								filterSource = this.fTree.XRefIndex_Find(this.fFilter.SourceRef) as GEDCOMSourceRecord;
							}
							if (person.IndexOfSource(filterSource) < 0) {
								return result;
							}
							break;
					}

					ChartFilter.TBranchCut branchCut = this.fFilter.BranchCut;
					if (branchCut != ChartFilter.TBranchCut.bcNone)
					{
						if (!(bool)person.ExtData)
						{
							return result;
						}
					}

					TreeChartPerson res = this.AddDescPerson(parent, person, PersonKind.pkDefault, level);
					result = res;

					int num = person.SpouseToFamilyLinks.Count;
					for (int i = 0; i < num; i++)
					{
						GEDCOMFamilyRecord family = person.SpouseToFamilyLinks[i].Family;

						bool isDup = (this.fPreparedFamilies.IndexOf(family.XRef) >= 0);
						if (!isDup) this.fPreparedFamilies.Add(family.XRef);

						if (GKUtils.IsRecordAccess(family.Restriction, this.fShieldState))
						{
							TreeChartPerson resParent = null;
							GEDCOMSex sex = person.Sex;
							TreeChartPerson ft = null;
							TreeChartPerson mt = null;
							PersonFlag descFlag = PersonFlag.pfDescByFather;

							switch (sex) {
								case GEDCOMSex.svFemale:
									{
										GEDCOMIndividualRecord sp = family.Husband.Value as GEDCOMIndividualRecord;
										resParent = this.AddDescPerson(null, sp, PersonKind.pkSpouse, level);
										resParent.Sex = GEDCOMSex.svMale;
										ft = resParent;
										mt = res;
										descFlag = PersonFlag.pfDescByFather;
										break;
									}

								case GEDCOMSex.svMale:
									{
										GEDCOMIndividualRecord sp = family.Wife.Value as GEDCOMIndividualRecord;
										resParent = this.AddDescPerson(null, sp, PersonKind.pkSpouse, level);
										resParent.Sex = GEDCOMSex.svFemale;
										ft = res;
										mt = resParent;
										descFlag = PersonFlag.pfDescByMother;
										break;
									}
							}

							if (this.fOptions.Kinship)
							{
								this.fGraph.AddUndirectedEdge(res.Node, resParent.Node, 1, (int)RelationKind.rkSpouse, (int)RelationKind.rkSpouse);
							}

							if (resParent != null)
							{
								res.AddSpouse(resParent);
								resParent.BaseSpouse = res;
								resParent.BaseFamily = family;
								
								if (resParent.Rec != null) {
									if (resParent.Rec.ChildToFamilyLinks.Count > 0) {
										resParent.Flags.Include(PersonFlag.pfHasInvAnc);
									}

									if (resParent.Rec.SpouseToFamilyLinks.Count > 1) {
										resParent.Flags.Include(PersonFlag.pfHasInvDesc);
									}
								}
							}
							else
							{
								resParent = res;
							}

							ft.IsDup = isDup;
							mt.IsDup = isDup;

							if ((this.fDepthLimit <= -1 || level != this.fDepthLimit) && (!isDup))
							{
								int num2 = family.Childrens.Count;
								for (int j = 0; j < num2; j++)
								{
									GEDCOMIndividualRecord child_rec = family.Childrens[j].Value as GEDCOMIndividualRecord;
									if (GKUtils.IsRecordAccess(child_rec.Restriction, this.fShieldState))
									{
										TreeChartPerson child = this.DoDescendantsStep(resParent, child_rec, level + 1);
										if (child != null)
										{
											child.Father = ft;
											child.Mother = mt;
											//int d = (int)desc_flag;
											child.Flags.Include(descFlag);
											if (this.fOptions.Kinship)
											{
												this.fGraph.AddUndirectedEdge(child.Node, ft.Node, 1, (int)RelationKind.rkParent, (int)RelationKind.rkChild);
												this.fGraph.AddUndirectedEdge(child.Node, mt.Node, 1, (int)RelationKind.rkParent, (int)RelationKind.rkChild);
											}
										}
									}
								}
							} else {
								if (family.Childrens.Count > 0) {
									ft.Flags.Include(PersonFlag.pfHasInvDesc);
									mt.Flags.Include(PersonFlag.pfHasInvDesc);
								}
							}
						}
					}
				}
				
				return result;
			}
			catch (Exception ex)
			{
				this.fBase.Host.LogWrite("TreeChartBox.DoDescendantsStep(): " + ex.Message);
				throw;
			}
		}

		private void FindRelationship(TreeChartPerson target)
		{
			if (target == null) return;

			if (target.Node == null) {
				target.Kinship = "";
				return;
			}

		    try
			{
				IEnumerable<IEdge> edgesPath = this.fGraph.GetPath(target.Node);

				string tmp = "";
				RelationKind prevRel = RelationKind.rkNone;
				RelationKind finRel = RelationKind.rkNone;
				int great = 0;

				foreach (IEdge edge in edgesPath)
				{
					TreeChartPerson xFrom = edge.Source.Value as TreeChartPerson;
					TreeChartPerson xTo = edge.Target.Value as TreeChartPerson;
					RelationKind curRel = FixLink(xFrom, xTo, (RelationKind)((int)edge.Value));

					if (this.fPathDebug) {
						if (tmp != "") tmp += ", ";
						if (xFrom.Rec != null) tmp += (xFrom.Rec.XRef + ">" + GKData.RelationSigns[(int)curRel] + ">");
						if (xTo.Rec != null) tmp += xTo.Rec.XRef;
					}

                    if (prevRel != RelationKind.rkUndefined)
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

				string result = "[" + FixRelation(target, finRel, great) + "]";
				target.Kinship = result;
			}
			finally
			{
			}
		}

		private static RelationKind FixLink(TreeChartPerson xFrom, TreeChartPerson xTo, RelationKind rel)
		{
			RelationKind xRel = rel;

			switch (rel)
			{
				case RelationKind.rkParent:
					switch (xTo.Sex)
					{
						case GEDCOMSex.svMale:
							xRel = RelationKind.rkFather;
							break;
						case GEDCOMSex.svFemale:
							xRel = RelationKind.rkMother;
							break;
					}
					break;

				case RelationKind.rkSpouse:
					switch (xTo.Sex)
					{
						case GEDCOMSex.svMale:
							xRel = RelationKind.rkHusband;
							break;
						case GEDCOMSex.svFemale:
							xRel = RelationKind.rkWife;
							break;
					}
					break;

				case RelationKind.rkChild:
					switch (xTo.Sex)
					{
						case GEDCOMSex.svMale:
							xRel = RelationKind.rkSon;
							break;
						case GEDCOMSex.svFemale:
							xRel = RelationKind.rkDaughter;
							break;
					}
					break;

				default:
					xRel = rel;
					break;
			}

			return xRel;
		}

		private static string FixRelation(TreeChartPerson target, RelationKind rel, int great)
		{
			string tmp = "";
			if (great != 0)
			{
				if (rel >= RelationKind.rkUncle && rel < RelationKind.rkNephew)
				{
					tmp = GKData.Numerals[great] + GKData.NumKinship[(int)target.Sex] + " ";
					if (rel == RelationKind.rkUncle)
					{
						rel = RelationKind.rkGrandfather;
					}
					if (rel == RelationKind.rkAunt)
					{
						rel = RelationKind.rkGrandmother;
					}
				}
				else
				{
					if (rel != RelationKind.rkUndefined)
					{
						tmp = GetGreat(great);
					}
				}
			}
			else
			{
				tmp = "";
			}
			return tmp + LangMan.LS(GKData.RelationKinds[(int)rel]);
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

		private bool PersonIsVisible(ExtRect pnRect)
		{
		    int pL = pnRect.Left;
			int pR = pnRect.Right;
			int pT = pnRect.Top;
			int pB = pnRect.Bottom;

			return (fVisibleArea.Contains(pL, pT) || fVisibleArea.Contains(pR, pT) ||
			        fVisibleArea.Contains(pL, pB) || fVisibleArea.Contains(pR, pB));
		}

		/*private bool LineIsVisible(int X1, int Y1, int X2, int Y2)
		{
			return (FVisibleArea.Contains(X1, Y1) || FVisibleArea.Contains(X2, Y2));
		}*/
		
		private void DrawLine(Graphics gfx, int x1, int y1, int x2, int y2)
		{
			//if (!LineIsVisible(X1, Y1, X2, Y2)) return;
			
			int sX = this.fSPX + x1;
			int sX2 = this.fSPX + x2;
			int sY = this.fSPY + y1;
			int sY2 = this.fSPY + y2;
			gfx.DrawLine(fLinePen, sX, sY, sX2, sY2);

			if (this.fOptions.Decorative) {
				if (sX == sX2) {
					gfx.DrawLine(fDecorativeLinePen, sX + 1, sY + 1, sX2 + 1, sY2 - 1);
				} else {
					if (sY == sY2) {
						gfx.DrawLine(fDecorativeLinePen, sX + 1, sY + 1, sX2 + 0, sY2 + 1);
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

		private void DrawText(Graphics gfx, ExtRect rt, string s, int h, int line)
		{
			int stw = gfx.MeasureString(s, this.DrawFont).ToSize().Width;
			int rx = rt.Left + ((rt.Right - rt.Left + 1) - stw) / 2;
			int ry = rt.Top + (10 + (h * line));
			gfx.DrawString(s, this.DrawFont, fSolidBlack, rx, ry);
		}
		
		private void DrawBorder(Graphics gfx, Pen xpen, ExtRect rt, bool dead, TreeChartPerson person)
		{
			Rectangle rect = rt.ToRectangle();
			Color bColor;
			bool highlighted = false;//(this.fHighlightedPerson == person);
			
			switch (person.Sex) {
				case GEDCOMSex.svMale:
				{
					if (!dead) {
						if (person.IsDup) {
							bColor = Color.FromArgb(192, 192, 192);
						} else {
							bColor = person.Divorced ? this.Options.UnHusbandColor : this.Options.MaleColor;
						}
					} else {
						bColor = Color.Black;
					}
					
					if (highlighted) bColor = GKUtils.lighter(bColor, 0.2f);
					gfx.FillRectangle(new SolidBrush(bColor), rect.Left, rect.Top, rect.Width, rect.Height);
					gfx.DrawRectangle(xpen, rect.Left, rect.Top, rect.Width, rect.Height);
					break;
				}

				case GEDCOMSex.svFemale:
				{
					if (!dead) {
						if (person.IsDup) {
							bColor = Color.FromArgb(192, 192, 192);
						} else {
							bColor = person.Divorced ? this.Options.UnWifeColor : this.Options.FemaleColor;
						}
					} else {
						bColor = Color.Black;
					}

					if (highlighted) bColor = GKUtils.lighter(bColor, 0.2f);
					GraphicsPath path = GKUtils.CreateRoundedRectangle(rect.Left, rect.Top, rect.Width, rect.Height, 6);
					
					/*gfx.TranslateTransform(3, 3);
					GKUtils.DrawPathWithFuzzyLine(path, gfx, Color.Black, 200, 20, 2);
					gfx.ResetTransform();*/

					gfx.FillPath(new SolidBrush(bColor), path);
					gfx.DrawPath(xpen, path);
					break;
				}

				default: 
				{
					bColor = !dead ? this.Options.UnkSexColor : Color.Black;
                    gfx.FillRectangle(new SolidBrush(bColor), rect.Left, rect.Top, rect.Width, rect.Height);
					gfx.DrawRectangle(xpen, rect.Left, rect.Top, rect.Width, rect.Height);
					break;
				}
			}
		}

		private void DrawPerson(Graphics gfx, int spx, int spy, TreeChartPerson person, DrawMode drawMode)
		{
			ExtRect rt = person.Rect;
			if (drawMode == DrawMode.dmScreen && !this.PersonIsVisible(rt)) return;

			rt = rt.GetOffset(spx, spy);

			int h = gfx.MeasureString("A", this.DrawFont).ToSize().Height;
			bool hasPort = this.Options.PortraitsVisible && person.Portrait != null;

			if (person.IsDead) {
				ExtRect dt = rt;
				dt = dt.GetOffset(-2, -2);
				this.DrawBorder(gfx, fLinePen, dt, true, person);
			}

			Pen xpen = null;
			try
			{
				if (drawMode == DrawMode.dmScreen && person.Selected) {
					const int penWidth = 2;

                    Color penColor;
					switch (person.Sex) {
						case GEDCOMSex.svMale:
							penColor = Color.Blue;
							break;

						case GEDCOMSex.svFemale:
							penColor = Color.Red;
							break;

						default:
							penColor = Color.Black;
							break;
					}
					xpen = new Pen(penColor, (float)penWidth);
				} else {
					xpen = new Pen(Color.Black, 1f);
				}

				this.DrawBorder(gfx, xpen, rt, false, person);
			}
			finally
			{
				xpen.Dispose();
			}
 
			if (drawMode == DrawMode.dmScreen && person.CanExpand) {
				ExtRect expRt = this.GetExpanderRect(rt);
				gfx.DrawImage(fExpPic, expRt.Left, expRt.Top);
			}

			if (hasPort) {
				ExtRect portRt = rt;
				portRt.Right = portRt.Left + person.PortraitWidth;
				portRt.OffsetEx(3, 3);
				gfx.DrawImage(person.Portrait, person.GetDestRect(portRt, person.Portrait));
				rt.Left += person.PortraitWidth;
			}

			int lines = person.Lines.Length;
			for (int k = 0; k < lines; k++) {
				this.DrawText(gfx, rt, person.Lines[k], h, k);
			}

			if (this.Options.SignsVisible && !person.Signs.IsEmpty())
			{
				int i = 0;
				for (ChartPersonSign cps = ChartPersonSign.urRI_StGeorgeCross; cps <= ChartPersonSign.urUSSR_RearVeteran; cps++)
				{
					if (person.Signs.Contains(cps)) {
						Bitmap pic = this.fSignsPic[(int)cps - 1];
						gfx.DrawImage(pic, rt.Right, rt.Top - 21 + i * pic.Height);
						i++;
					}
				}
			}
		}

		private ExtRect GetExpanderRect(ExtRect personRect)
		{
			ExtRect expRt = ExtRect.Create(personRect.Left, personRect.Top - 18, personRect.Left + 16 - 1, personRect.Top - 2);
			return expRt;
		}

		private void Predef()
		{
			double sc = this.fScale;
			int fsz = (int)Math.Round(this.fOptions.DefFontSize * sc);

		    string fontName = (fsz <= 7) ? "Small Fonts" : this.fOptions.DefFontName;

			this.fDrawFont = new Font(fontName, fsz, FontStyle.Regular, GraphicsUnit.Point);
			this.fSpouseDistance = (int)Math.Round(DefSpouseDistance * sc);
			this.fBranchDistance = (int)Math.Round(DefBranchDistance * sc);
			this.fLevelDistance = (int)Math.Round(DefLevelDistance * sc);
			this.fMargins = (int)Math.Round(DefMargins * sc);
		}
		
		private int InitInfoSize()
		{
			int lines = 0;

			if (this.fOptions.FamilyVisible) {
				lines++;
			}

			if (!this.fOptions.DiffLines) {
				lines++;
			} else {
				lines++;
				lines++;
			}

			if (!this.fOptions.OnlyYears) {
				if (this.fOptions.BirthDateVisible) {
					lines++;
				}
				if (this.fOptions.DeathDateVisible) {
					lines++;
				}
			} else {
				lines++;
			}

			if (this.fOptions.Kinship) {
				lines++;
			}

			if (this.fPathDebug) {
				lines++;
			}

			return lines;
		}
		
		private void RecalcChart(bool noRedraw = false)
		{
			if (this.fOptions.Kinship && this.fKinRoot != null) {
				this.fGraph.FindPathTree(this.fKinRoot.Node);
			}

			int lines = this.InitInfoSize();
			
			using (Graphics gfx = this.CreateGraphics()) {
				int num = this.fPersons.Count;
				for (int i = 0; i < num; i++) {
					TreeChartPerson p = this.fPersons[i];

					p.DefineExpands();
					
					if (this.fOptions.Kinship) {
						this.FindRelationship(p);
					}

					p.CalcBounds(lines, gfx);
				}
			}

			switch (this.fKind) {
				case ChartKind.ckAncestors:
					this.RecalcAncestorsChart();
					break;

				case ChartKind.ckDescendants:
					this.RecalcDescendantsChart(true);
					break;

				case ChartKind.ckBoth:
					this.RecalcAncestorsChart();
					this.RecalcDescendantsChart(false);
					break;
			}

			// search bounds
			this.fTreeBounds = ExtRect.Create(int.MaxValue, int.MaxValue, 0, 0);
			int num2 = this.fPersons.Count;
			for (int i = 0; i < num2; i++) {
				TreeChartPerson p = this.fPersons[i];
				this.AdjustTreeBounds(p);
			}
			// adjust bounds
			int offsetX = 0 + this.fMargins - this.fTreeBounds.Left;
			int offsetY = 0 + this.fMargins - this.fTreeBounds.Top;
			this.fTreeBounds = ExtRect.Create(int.MaxValue, int.MaxValue, 0, 0);
			for (int i = 0; i < num2; i++) {
				TreeChartPerson p = this.fPersons[i];
				p.PtX += offsetX;
				p.PtY += offsetY;
				this.AdjustTreeBounds(p);
			}

			this.fImageHeight = this.fTreeBounds.GetHeight() + this.fMargins * 2;
			this.fImageWidth = this.fTreeBounds.GetWidth() + this.fMargins * 2;

			this.SetScrollRange(noRedraw);
		}

		private void AdjustTreeBounds(TreeChartPerson person)
		{
			if (person == null) return;
			ExtRect prt = person.Rect;
			
			if (this.fTreeBounds.Left > prt.Left) this.fTreeBounds.Left = prt.Left;
			if (this.fTreeBounds.Top > prt.Top) this.fTreeBounds.Top = prt.Top;
			
			if (this.fTreeBounds.Right < prt.Right) this.fTreeBounds.Right = prt.Right;
			if (this.fTreeBounds.Bottom < prt.Bottom) this.fTreeBounds.Bottom = prt.Bottom;
		}
		
		private void ShiftAnc(ref int[] edges, TreeChartPerson person, int offset)
		{
			TreeChartPerson pp = person;
			if (pp == null) return;

			do
			{
				pp.PtX += offset;
				edges[pp.Generation] = pp.Rect.Right;

				pp = (pp.GetChildsCount() < 1) ? null : pp.GetChild(0);
			}
			while (pp != null);
		}

		private void RecalcAnc(ExtList<TreeChartPerson> prev, ref int[] edges, TreeChartPerson person, int ptX, int ptY)
		{
			if (person == null) return;

			person.PtX = ptX;
			person.PtY = ptY;

			int gen = person.Generation;

			int offset = (edges[gen] > 0) ? this.fBranchDistance : this.fMargins;
			int bound = edges[gen] + offset;
			if (person.Rect.Left <= bound) {
				this.ShiftAnc(ref edges, person, bound - person.Rect.Left);
			}

			edges[gen] = person.Rect.Right;

			prev.Add(person);
			if (person.Rect.Top < 0)
			{
				offset = 0 - person.Rect.Top + this.fMargins;
				int num = prev.Count;
				for (int i = 0; i < num; i++) {
					prev[i].PtY += offset;
				}
			}

			if (person.Father != null && person.Mother != null)
			{
				this.RecalcAnc(prev, ref edges, person.Father, person.PtX - (this.fSpouseDistance + person.Father.Width / 2), person.PtY - this.fLevelDistance - person.Height);
				this.RecalcAnc(prev, ref edges, person.Mother, person.PtX + (this.fSpouseDistance + person.Mother.Width / 2), person.PtY - this.fLevelDistance - person.Height);

				person.PtX = (person.Father.PtX + person.Mother.PtX) / 2;
				edges[gen] = person.Rect.Right;
			}
			else
			{
				TreeChartPerson anc = null;
				if (person.Father != null) {
					anc = person.Father;
				} else if (person.Mother != null) {
					anc = person.Mother;
				}
				
				if (anc != null) {
					this.RecalcAnc(prev, ref edges, anc, person.PtX, person.PtY - this.fLevelDistance - person.Height);
				}
			}
		}

		private void RecalcAncestorsChart()
		{
			int[] edges = new int[256];
			InitEdges(ref edges);

			ExtList<TreeChartPerson> prev = new ExtList<TreeChartPerson>();
			try
			{
				this.RecalcAnc(prev, ref edges, this.fRoot, this.fMargins, this.fMargins);
			}
			finally
			{
				prev.Dispose();
			}
		}

		private void ShiftDesc(TreeChartPerson person, int offset, bool isSingle)
		{
			if (person == null) return;

			if (person == this.fRoot) {
				isSingle = false;
			}

			person.PtX += offset;

			if (person.BaseSpouse != null && (person.BaseSpouse.Sex == GEDCOMSex.svFemale || person.BaseSpouse.GetSpousesCount() == 1))
			{
				this.ShiftDesc(person.BaseSpouse, offset, isSingle);
			} else {
				if (!isSingle) {
					this.ShiftDesc(person.Father, offset, isSingle);
					this.ShiftDesc(person.Mother, offset, isSingle);
				} else {
					if (person.Flags.Contains(PersonFlag.pfDescByFather)) {
						this.ShiftDesc(person.Father, offset, isSingle);
					} else {
						if (person.Flags.Contains(PersonFlag.pfDescByMother)) {
							this.ShiftDesc(person.Mother, offset, isSingle);
						}
					}
				}
			}
		}

		private void RecalcDescChilds(ref int[] edges, TreeChartPerson person)
		{
			if (person.GetChildsCount() == 0) return;

			bool fixPair = person.BaseSpouse != null && person.BaseSpouse.GetSpousesCount() == 1;
			int centX = 0;

			if (fixPair) {
				GEDCOMSex sex = person.Sex;
				switch (sex) {
					case GEDCOMSex.svMale:
						centX = (person.Rect.Right + person.BaseSpouse.Rect.Left) / 2;
						break;

					case GEDCOMSex.svFemale:
						centX = (person.BaseSpouse.Rect.Right + person.Rect.Left) / 2;
						break;
				}
			} else {
				centX = person.PtX;
			}

			int curY = person.PtY + this.fLevelDistance + person.Height;
			int childsWidth = (person.GetChildsCount() - 1) * this.fBranchDistance;

			int num = person.GetChildsCount();
			for (int i = 0; i < num; i++) {
				childsWidth += person.GetChild(i).Width;
			}

			int curX = centX - childsWidth / 2;

			int num2 = person.GetChildsCount();
			for (int i = 0; i < num2; i++) {
				TreeChartPerson child = person.GetChild(i);
				this.RecalcDesc(ref edges, child, new Point(curX + child.Width / 2, curY), true);
				curX = child.Rect.Right + this.fBranchDistance;
			}

			curX = person.GetChild(0).PtX;
			if (person.GetChildsCount() > 1) {
				curX += (person.GetChild(person.GetChildsCount() - 1).PtX - curX) / 2;
			}

			if (fixPair) {
				switch (person.Sex) {
					case GEDCOMSex.svMale:
						this.ShiftDesc(person, curX - (this.BranchDistance + person.Width) / 2 + 1 - person.PtX, true);
						this.ShiftDesc(person.BaseSpouse, curX + (this.BranchDistance + person.BaseSpouse.Width) / 2 - person.BaseSpouse.PtX, true);
						break;

					case GEDCOMSex.svFemale:
						this.ShiftDesc(person, curX + (this.BranchDistance + person.Width) / 2 - person.PtX, true);
						this.ShiftDesc(person.BaseSpouse, curX - (this.BranchDistance + person.BaseSpouse.Width) / 2 + 1 - person.BaseSpouse.PtX, true);
						break;
				}
			} else {
				this.ShiftDesc(person, curX - person.PtX, true);
			}
		}

		private void RecalcDesc(ref int[] edges, TreeChartPerson person, Point aPt, bool predef)
		{
			if (person == null) return;
			
			int gen = person.Generation;
			if (predef) {
				person.PtX = aPt.X;
				person.PtY = aPt.Y;
			}

			int offset = (edges[gen] > 0) ? this.fBranchDistance : this.fMargins;
			int bound = edges[gen] + offset;
			if (person.Rect.Left <= bound) {
				this.ShiftDesc(person, bound - person.Rect.Left, true);
			}

			if (person.Sex == GEDCOMSex.svMale) {
				this.RecalcDescChilds(ref edges, person);
				edges[gen] = person.Rect.Right;
			}

			if (person.GetSpousesCount() > 0) {
				TreeChartPerson prev = person;
				int num = person.GetSpousesCount();
				for (int i = 0; i < num; i++) {
					TreeChartPerson sp = person.GetSpouse(i);
					Point spPt = new Point();

					switch (person.Sex) {
						case GEDCOMSex.svMale:
							spPt = new Point(prev.Rect.Right + (this.fBranchDistance + sp.Width / 2), person.PtY);
							break;

						case GEDCOMSex.svFemale:
							spPt = new Point(prev.Rect.Left - (this.fBranchDistance + sp.Width / 2), person.PtY);
							break;
					}

					this.RecalcDesc(ref edges, sp, spPt, true);

					if (sp.Sex != GEDCOMSex.svMale) {
						prev = sp;
					}
				}
			}

			if (person.Sex == GEDCOMSex.svFemale) {
				this.RecalcDescChilds(ref edges, person);
				edges[gen] = person.Rect.Right;
			}
		}

		private void RecalcDescendantsChart(bool predef)
		{
			int[] edges = new int[256];
			InitEdges(ref edges);

			this.RecalcDesc(ref edges, this.fRoot, new Point(this.fMargins, this.fMargins), predef);
		}

		private void DrawAncestors(Graphics gfx, TreeChartPerson person, DrawMode drawMode)
		{
			this.Draw(gfx, person.Father, ChartKind.ckAncestors, drawMode);
			this.Draw(gfx, person.Mother, ChartKind.ckAncestors, drawMode);
			int crY = person.PtY - this.fLevelDistance / 2;

			if (person.Father != null) {
				this.DrawLine(gfx, person.Father.PtX, crY, person.PtX, crY);
				this.DrawLine(gfx, person.Father.PtX, person.Father.PtY + person.Father.Height, person.Father.PtX, crY);
			}

			if (person.Mother != null) {
				this.DrawLine(gfx, person.PtX, crY, person.Mother.PtX, crY);
				this.DrawLine(gfx, person.Mother.PtX, person.Mother.PtY + person.Mother.Height, person.Mother.PtX, crY);
			}

			if (person.Father != null || person.Mother != null) {
				this.DrawLine(gfx, person.PtX, crY, person.PtX, person.PtY);
			}
		}

		private void DrawDescendants(Graphics gfx, TreeChartPerson person, DrawMode drawMode)
		{
			int num = person.GetChildsCount();
			for (int i = 0; i < num; i++) {
				this.Draw(gfx, person.GetChild(i), ChartKind.ckDescendants, drawMode);
			}

			int spbOfs = (person.Height - 10) / (person.GetSpousesCount() + 1);
			int spbBeg = person.PtY + (person.Height - spbOfs * (person.GetSpousesCount() - 1)) / 2;

			switch (person.Sex) {
				case GEDCOMSex.svMale:
					int num3 = person.GetSpousesCount();
					for (int i = 0; i < num3; i++) {
						int spbV = spbBeg + spbOfs * i;
						this.DrawLine(gfx, person.Rect.Right + 1, spbV, person.GetSpouse(i).Rect.Left, spbV);
					}
					break;

				case GEDCOMSex.svFemale:
					int num2 = person.GetSpousesCount();
					for (int i = 0; i < num2; i++) {
						int spbV = spbBeg + spbOfs * i;
						this.DrawLine(gfx, person.GetSpouse(i).Rect.Right + 1, spbV, person.Rect.Left, spbV);
					}
					break;
			}

			int num4 = person.GetSpousesCount();
			for (int i = 0; i < num4; i++) {
				this.Draw(gfx, person.GetSpouse(i), ChartKind.ckDescendants, drawMode);
			}

			int crY = person.PtY + person.Height + this.fLevelDistance / 2;
			int cx = 0;
			if (person.BaseSpouse == null || (person.BaseSpouse != null && person.BaseSpouse.GetSpousesCount() > 1))
			{
				cx = person.PtX;
				spbBeg = person.PtY + person.Height - 1;
			}
			else
			{
				switch (person.Sex) {
					case GEDCOMSex.svMale:
						cx = (person.Rect.Right + person.BaseSpouse.Rect.Left) / 2;
						break;

					case GEDCOMSex.svFemale:
						cx = (person.BaseSpouse.Rect.Right + person.Rect.Left) / 2;
						break;
				}

				spbBeg -= spbOfs / 2;
			}

			if (person.GetChildsCount() != 0)
			{
				this.DrawLine(gfx, cx, spbBeg, cx, crY);
				if (person.GetChildsCount() == 1)
				{
					TreeChartPerson child = person.GetChild(0);
					this.DrawLine(gfx, child.PtX, crY, child.PtX, child.PtY);
				}
				else
				{
					int bpx = person.GetChild(0).PtX;
					int epx = person.GetChild(person.GetChildsCount() - 1).PtX;
					this.DrawLine(gfx, bpx, crY, epx, crY);
					int num5 = person.GetChildsCount();
					for (int i = 0; i < num5; i++) {
						TreeChartPerson child = person.GetChild(i);
						this.DrawLine(gfx, child.PtX, crY, child.PtX, child.PtY);
					}
				}
			}
		}

		private void SetScrollRange(bool noRedraw = false)
		{
			Size client = base.ClientSize;
			
			if (this.fImageWidth < client.Width) {
				this.fScrollRange.Width = 0;
				this.SetLeftPos(0);
			} else {
				this.fScrollRange.Width = this.fImageWidth - client.Width;
			}

			if (this.fImageHeight < client.Height) {
				this.fScrollRange.Height = 0;
				this.SetTopPos(0);
			} else {
				this.fScrollRange.Height = this.fImageHeight - client.Height;
			}

            Win32Native.SetScrollRange(this.Handle, Win32Native.SB_HORZ, 0, this.fScrollRange.Width, false);
            Win32Native.SetScrollRange(this.Handle, Win32Native.SB_VERT, 0, this.fScrollRange.Height, false);

            if (!noRedraw) this.Invalidate();
		}

		private void SetBorderWidth(int value)
		{
			if (this.fBorderWidth != value) {
				this.fBorderWidth = value;
				this.Invalidate();
			}
		}

		private void SetLeftPos(int value)
		{
			if (value < 0) value = 0;
			if (value > this.fScrollRange.Width) value = this.fScrollRange.Width;

			if (this.fLeftPos != value) {
				ExtRect dummy = ExtRect.Empty();
				ExtRect rt;
                Win32Native.ScrollWindowEx(this.Handle, this.fLeftPos - value, 0, ref dummy, ref dummy, 0, out rt, 0u);
                Win32Native.SetScrollPos(this.Handle, 0, this.fLeftPos, true);

                this.fLeftPos = value;
				this.ResetView();
				this.Invalidate();
			}
		}

		private void SetTopPos(int value)
		{
			if (value < 0) value = 0;
			if (value > this.fScrollRange.Height) value = this.fScrollRange.Height;

			if (this.fTopPos != value) {
				ExtRect dummy = ExtRect.Empty();
				ExtRect rt;
                Win32Native.ScrollWindowEx(this.Handle, 0, this.fTopPos - value, ref dummy, ref dummy, 0, out rt, 0u);
                Win32Native.SetScrollPos(this.Handle, 1, this.fTopPos, true);

				this.fTopPos = value;
				this.ResetView();
                this.Invalidate();
			}
		}

		private void ResetView()
		{
			Size sz = this.ClientSize;
			this.fVisibleArea = ExtRect.Bounds(this.fLeftPos, this.fTopPos, sz.Width, sz.Height);
		}
		
		private void InternalDraw(Graphics gfx, DrawMode drawMode)
		{
			Rectangle imgRect = new Rectangle(0, 0, fImageWidth, fImageHeight);
			if (this.BackgroundImage == null) {
				using (Brush brush = new SolidBrush(this.BackColor)) {
					gfx.FillRectangle(brush, imgRect);
				}
			} else {
				using (TextureBrush textureBrush = new TextureBrush(this.BackgroundImage, WrapMode.Tile)) {
					gfx.FillRectangle(textureBrush, imgRect);
				}
			}

			this.fSPX = 0;
			this.fSPY = 0;

			if (drawMode == DrawMode.dmScreen) {
				this.fSPX += this.fBorderWidth - this.fLeftPos;
				this.fSPY += this.fBorderWidth - this.fTopPos;

				Size sz = this.ClientSize;

				if (this.fImageWidth < sz.Width) {
					this.fSPX += (sz.Width - this.fImageWidth) / 2;
				}

				if (this.fImageHeight < sz.Height) {
					this.fSPY += (sz.Height - this.fImageHeight) / 2;
				}
			} else {
				this.fSPX += 0;
				this.fSPY += 0;
			}

			if (DEBUG_IMAGE) {
				Rectangle irt = new Rectangle(this.fSPX, this.fSPY, this.fImageWidth - 1, this.fImageHeight - 1);
				using (Pen pen = new Pen(Color.Red)) {
					gfx.DrawRectangle(pen, irt);
				}
			}
			
			this.Draw(gfx, this.fRoot, this.fKind, drawMode);

			if (fScaleControl.Visible) fScaleControl.Draw(gfx);
			//if (fPersonControl.Visible) fPersonControl.Draw(gfx);
		}

		protected void Draw(Graphics gfx, TreeChartPerson person, ChartKind dirKind, DrawMode drawMode)
		{
			if (person != null) {
				switch (this.fKind) {
					case ChartKind.ckAncestors:
						this.DrawAncestors(gfx, person, drawMode);
						break;

					case ChartKind.ckDescendants:
						this.DrawDescendants(gfx, person, drawMode);
						break;

					case ChartKind.ckBoth:
						if (person == this.fRoot || dirKind == ChartKind.ckAncestors) this.DrawAncestors(gfx, person, drawMode);
						if (person == this.fRoot || dirKind == ChartKind.ckDescendants) this.DrawDescendants(gfx, person, drawMode);
						break;
				}

				this.DrawPerson(gfx, this.fSPX, this.fSPY, person, drawMode);
			}
		}

		public void GenChart(GEDCOMIndividualRecord iRec, ChartKind kind, bool center)
		{
			if (iRec == null) return;
			
			try
			{
				this.Predef();

				this.fKind = kind;
				this.fSelected = null;
				this.fPersons.Clear();
				this.fGraph.Clear();
				this.DoFilter(iRec);
				this.fRoot = null;
				this.fPreparedIndividuals.Clear();

				switch (this.fKind) {
					case ChartKind.ckAncestors:
						this.fPreparedFamilies.Clear();
						this.fRoot = this.DoAncestorsStep(null, iRec, 1, false);
						break;
						
					case ChartKind.ckDescendants:
						this.fPreparedFamilies.Clear();
						this.fRoot = this.DoDescendantsStep(null, iRec, 1);
						break;

					case ChartKind.ckBoth:
						this.fPreparedFamilies.Clear();
						this.fRoot = this.DoAncestorsStep(null, iRec, 1, false);
						this.fPreparedFamilies.Clear();
						this.DoDescendantsStep(null, iRec, 1);
						break;
				}

				this.fKinRoot = this.fRoot;

				this.RecalcChart();

				if (center) this.CenterPerson(this.fRoot);
			}
			catch (Exception ex)
			{
				this.fBase.Host.LogWrite("TreeChartBox.GenChart(): " + ex.Message);
			}
		}

		public void RefreshTree()
		{
			try {
				if (this.fRoot == null) return;

				GEDCOMIndividualRecord rootRec = this.fRoot.Rec;
				
				this.SaveSelection();
				this.GenChart(rootRec, this.fKind, false);
				this.RestoreSelection();
			}
			catch (Exception ex)
			{
				this.fBase.Host.LogWrite("TreeChartBox.RefreshTree(): " + ex.Message);
			}
		}

		public void SaveSelection()
		{
			this.fSaveSelection = (this.fSelected == null) ? null : this.fSelected.Rec;
		}
		
		public void RestoreSelection()
		{
			this.SelectByRec(this.fSaveSelection);
		}
		
		public void DoFilter(GEDCOMIndividualRecord root)
		{
			if (this.fFilter.BranchCut != ChartFilter.TBranchCut.bcNone) {
				TreeStats.InitExtCounts(this.fTree, 0);
				this.DoDescendantsFilter(root);
				root.ExtData = true;
			}
		}

		public void RebuildKinships(bool noRedraw = false)
		{
			try
			{
				if (this.fOptions.Kinship) {
					TreeChartPerson p = this.fSelected;
					if (p != null) {
						this.fKinRoot = p;
						this.RecalcChart(noRedraw);
					}
				}
			}
			catch (Exception ex)
			{
				this.fBase.Host.LogWrite("TreeChartBox.RebuildKinships(): " + ex.Message);
			}
		}

		public void SaveSnapshot(string fileName)
		{
			string ext = Path.GetExtension(fileName).ToLowerInvariant();

			if ((ext == ".bmp" || ext == ".jpg") && this.fImageWidth >= 65535)
			{
				GKUtils.ShowError(LangMan.LS(LSID.LSID_TooMuchWidth));
			}
			else
			{
				ImageFormat imFmt = ImageFormat.Png;
				if (ext == ".bmp") { imFmt = ImageFormat.Bmp; }
				else
					if (ext == ".emf") { imFmt = ImageFormat.Emf; }
				else
					if (ext == ".png") { imFmt = ImageFormat.Png; }
				else
					if (ext == ".gif") { imFmt = ImageFormat.Gif; }
				else
					if (ext == ".jpg") { imFmt = ImageFormat.Jpeg; }

				Image pic;
				if (imFmt == ImageFormat.Emf) {
					pic = new Metafile(fileName, this.CreateGraphics().GetHdc());
				} else {
					pic = new Bitmap(this.fImageWidth, this.fImageHeight, PixelFormat.Format24bppRgb);
				}
				
				try
				{
					using (Graphics gfx = Graphics.FromImage(pic)) {
						this.Predef();
						this.InternalDraw(gfx, DrawMode.dmFile);
					}

					pic.Save(fileName, imFmt);
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

			this.Invalidate();
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

		private void SelectByRec(GEDCOMIndividualRecord iRec)
		{
			if (iRec == null) return;
			
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

		private void CenterPerson(TreeChartPerson person, bool animation = true)
		{
		    if (person == null) return;

			int dstX = (person.PtX) - (this.ClientSize.Width / 2);
			int dstY = (person.PtY + (person.Height / 2)) - (this.ClientSize.Height / 2);

			if (dstX < 0) dstX = dstX + (0 - dstX);
			if (dstY < 0) dstY = dstY + (0 - dstY);

			if ((this.fLeftPos == dstX) && (this.fTopPos == dstY)) return;

			if (animation) {
					TweenLibrary tween = new TweenLibrary();
					tween.StartTween(delegate(int newX, int newY) { this.SetLeftPos(newX); this.SetTopPos(newY); },
                                          this.fLeftPos, this.fTopPos, dstX, dstY, TweenAnimation.EaseInOutQuad, 20);
			} else {
				this.SetLeftPos(dstX);
				this.SetTopPos(dstY);
			}
		}

		private bool DoDescendantsFilter(GEDCOMIndividualRecord person)
		{
			bool result = false;
			if (person != null)
			{
				ChartFilter.TBranchCut branchCut = this.fFilter.BranchCut;
				switch (branchCut) {
					case ChartFilter.TBranchCut.bcYears:
						int year = GKUtils.GetIndependentYear(person, "BIRT");
						result = (year >= this.fFilter.BranchYear);
						break;

					case ChartFilter.TBranchCut.bcPersons:
						result = (this.fFilter.BranchPersons.IndexOf(person.XRef + ";") >= 0);
						break;
				}

				int num = person.SpouseToFamilyLinks.Count;
				for (int i = 0; i < num; i++)
				{
					GEDCOMFamilyRecord family = person.SpouseToFamilyLinks[i].Family;

					int num2 = family.Childrens.Count;
					for (int j = 0; j < num2; j++)
					{
						GEDCOMIndividualRecord child = family.Childrens[j].Value as GEDCOMIndividualRecord;
						bool resChild = DoDescendantsFilter(child);
						result |= resChild;
					}
				}
				person.ExtData = result;
			}
			return result;
		}
		
        private void DoPersonModify(PersonModifyEventArgs eArgs)
        {
            PersonModifyEventHandler eventHandler = (PersonModifyEventHandler)base.Events[TreeChartBox.EventPersonModify];
            if (eventHandler == null) return;

            eventHandler(this, eArgs);
        }
		
        private void DoRootChanged(TreeChartPerson person)
        {
            RootChangedEventHandler eventHandler = (RootChangedEventHandler)base.Events[TreeChartBox.EventRootChanged];
            if (eventHandler == null) return;

            eventHandler(this, person);
        }
		
        private void DoPersonProperties(MouseEventArgs eArgs)
        {
            MouseEventHandler eventHandler = (MouseEventHandler)base.Events[TreeChartBox.EventPersonProperties];
            if (eventHandler == null) return;

            eventHandler(this, eArgs);
        }

		#region Protected methods
		
		protected override void WndProc(ref Message m)
		{
			base.WndProc(ref m);

			if (m.Msg == Win32Native.WM_GETDLGCODE)
			{
				m.Result = (IntPtr)(m.Result.ToInt32() | 
				                    Win32Native.DLGC_WANTARROWS | Win32Native.DLGC_WANTTAB | 
				                    Win32Native.DLGC_WANTCHARS | Win32Native.DLGC_WANTALLKEYS);
			}
			else if (m.Msg == Win32Native.WM_HSCROLL)
			{
				int page = this.ClientSize.Width / 10;
				uint wParam = (uint)m.WParam.ToInt32();
				int newPos = SysUtils.DoScroll(this.Handle, wParam, 0, this.fLeftPos, 0, this.fScrollRange.Width, 1, page);
				this.SetLeftPos(newPos);
			}
			else if (m.Msg == Win32Native.WM_VSCROLL)
			{
				int page = this.ClientSize.Height / 10;
				uint wParam = (uint)m.WParam.ToInt32();
				int newPos = SysUtils.DoScroll(this.Handle, wParam, 1, this.fTopPos, 0, this.fScrollRange.Height, 1, page);
				this.SetTopPos(newPos);
			}
		}

		protected override void OnResize(EventArgs e)
		{
			this.SaveSelection();
			
			this.SetScrollRange();
			this.ResetView();
			this.fScaleControl.Update();

			this.RestoreSelection();

			base.OnResize(e);
		}

		protected override void OnPaint(PaintEventArgs e)
		{
			this.InternalDraw(e.Graphics, DrawMode.dmScreen);
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

		private MouseAction GetMouseAction(MouseEventArgs e, bool isDown, out TreeChartPerson person)
		{
			MouseAction result = MouseAction.maNone;
			person = null;
			
			int aX = e.X - this.fSPX;
			int aY = e.Y - this.fSPY;
			int num = this.fPersons.Count;
			for (int i = 0; i < num; i++) {
				TreeChartPerson p = this.fPersons[i];
				ExtRect persRt = p.Rect;
				
				if ((e.Button == MouseButtons.Right) && persRt.Contains(aX, aY)) {
					person = p;
					result = MouseAction.maProperties;
					break;
				}
				
				ExtRect expRt = this.GetExpanderRect(persRt);
				if ((e.Button == MouseButtons.Left) && expRt.Contains(aX, aY)) {
					person = p;
					result = MouseAction.maExpand;
					break;
				}
			}

			return result;
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
					if (this.fScaleControl.Contains(e.X, e.Y)) {
						this.fMode = ChartControlMode.ccmControlsVisible;
						this.fScaleControl.Visible = true;
						this.fScaleControl.MouseMove(e.X, e.Y, ThumbMoved);

						Point pt = new Point(e.X, e.Y);
						pt.Offset(+this.Left, +this.Top);
						this.fToolTip.Show(this.fScaleControl.Tip, this, pt, 1500);
					} /*if (this.fPersonControl.Contains(e.X, e.Y)) {
						
					} */else {
						/*TreeChartPerson p = this.FindPersonByCoords(e.X, e.Y);
						if (this.fHighlightedPerson != p) {
							this.fHighlightedPerson = p;
							this.fHighlightedStart = DateTime.Now.ToBinary();

							if (p == null) {
								this.fPersonControl.Visible = false;
							} else {
								this.fPersonControl.SetPerson(p);
							}

							this.Invalidate();
						}*/
//
//						if (p != null && e.Button == MouseButtons.Left)
//						{
//							this.fTreeBox.DoDragDrop(p.Rec.XRef, DragDropEffects.Move);
//						}
					}
					break;

				case ChartControlMode.ccmDragImage:
					this.SetLeftPos(this.fLeftPos - (e.X - this.fMouseX));
					this.SetTopPos(this.fTopPos - (e.Y - this.fMouseY));
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
					TreeChartPerson mPers;
					MouseAction mAct = this.GetMouseAction(e, false, out mPers);

					switch (mAct) {
						case MouseAction.maNone:
							break;

						case MouseAction.maProperties:
							if (this.fSelected == mPers && this.fSelected.Rec != null)
							{
								this.DoPersonProperties(new MouseEventArgs(e.Button, 1, e.X, e.Y, 0));
							}
							break;

						case MouseAction.maExpand:
							this.DoRootChanged(mPers);
							this.GenChart(mPers.Rec, TreeChartBox.ChartKind.ckBoth, true);
							break;
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

		private void InitTimer()
		{
			/*this.fComponents = new System.ComponentModel.Container();
			this.fTimer = new System.Windows.Forms.Timer(this.fComponents);
			this.fTimer.Interval = 1;
			this.fTimer.Tick += this.timer_Tick;
            this.fTimer.Stop();
            this.fTimer.Enabled = false;
            this.fTimer.Enabled = true;*/
		}
		
		private void timer_Tick(object sender, System.EventArgs e)
		{
			/*if (this.fHighlightedPerson != null) {
				DateTime st = DateTime.FromBinary(this.fHighlightedStart);
				DateTime cur = DateTime.Now;
				TimeSpan d = cur - st;

				if (d.Seconds >= 1 && !this.fPersonControl.Visible) {
					this.fPersonControl.Visible = true;
					this.Invalidate();
				}
			}*/
		}
	}
}
