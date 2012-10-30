using System;
using System.Collections.Generic;
using System.Drawing;
using System.Drawing.Imaging;
using System.IO;
using System.Runtime.InteropServices;

using Ext.Utils;
using GedCom551;
using GKCore;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKUI.Charts
{
	/*public class TScaleControl
	{
		private Bitmap ControlsImage;
		private static Rectangle ControlsScaleRect = new Rectangle(0, 0, 26, 320);
		//private static Rectangle ControlsThumbRect = new Rectangle(0, 322, 26, 13);
		//private static int ControlsThumbHeight = 10;
		//private static int ScaleY1 = 22;
		//private static int ScaleY2 = 297;

		public int Width
		{
			get { return 26; }
		}

		public int Height
		{
			get { return 320; }
		}

		public TScaleControl()
		{
			this.ControlsImage = new Bitmap(SysUtils.GetAppPath() + "temp\\ch_controls.png");
		}

		public void Draw(Graphics aCanvas, Rectangle destRect)
		{
			aCanvas.InterpolationMode = InterpolationMode.HighQualityBicubic;
			aCanvas.SmoothingMode = SmoothingMode.HighQuality;
			aCanvas.PixelOffsetMode = PixelOffsetMode.HighQuality;
			aCanvas.CompositingQuality = CompositingQuality.HighQuality;
			aCanvas.DrawImage(ControlsImage, destRect, ControlsScaleRect, GraphicsUnit.Pixel);
		}
	}*/

	public class TTreeChartBox : TCustomChartBox
	{
		private class TRelLink
		{
			public TreeChartPerson xFrom;
			public TreeChartPerson xTo;
			public KinshipsMan.TRelationKind xRel;

			public void Free()
			{
				SysUtils.Free(this);
			}
		}

		public enum TChartKind : byte
		{
			ckAncestors,
			ckDescendants,
			ckBoth
		}

		private static readonly string[] SignsData;

		private int FBranchDistance;
		private int FDepthLimit;
		private int FHMax;
		private int FWMax;
		private TChartFilter FFilter;
		private TGraph FGraph;
		private TChartKind FKind;
		private TreeChartPerson FKinRoot;
		private int FLevelDistance;
		private int FMargins;
		private bool FPathDebug;
		private TPersonList FPersons;
		private List<string> FPreparedFamilies = new List<string>();
		internal List<string> FPreparedIndividuals = new List<string>();
		private TreeChartPerson FRoot;
		private int FScale;
		private int FSpouseDistance;
		private TreeChartPerson FSelected;

		public Bitmap[] SignsPic = new Bitmap[4];

		static TTreeChartBox()
		{
			TTreeChartBox.SignsData = new string[]
			{
				"GEORGE_CROSS", 
				"SOLDIER", 
				"SOLDIER_FALL", 
				"VETERAN_REAR"
			};
		}

		/*private TScaleControl FScaleControl = new TScaleControl();
		private bool FControlsVisible;

		public bool ControlsVisible
		{
			get { return this.FControlsVisible; }
			set {
				this.FControlsVisible = value;
				this.Invalidate();
			}
		}

		public Rectangle ControlsRect
		{
			get
			{
				Rectangle cr = this.ClientRectangle;
				return new Rectangle(cr.Right - (10 + FScaleControl.Width), 10, FScaleControl.Width, FScaleControl.Height);
			}
		}*/

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

		public TChartFilter Filter
		{
			get { return this.FFilter; }
		}

		public int IndividualsCount
		{
			get { return this.FPreparedIndividuals.Count; }
		}

		public TChartKind Kind
		{
			get { return FKind; }
			set { FKind = value; }
		}

		public int Margins
		{
			get { return this.FMargins; }
			set { this.FMargins = value; }
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

		public new int Scale
		{
			get { return this.FScale; }
			set { this.FScale = value; }
		}

		public TreeChartPerson Selected
		{
			get { return this.FSelected; }
			set { this.SetSelected(value); }
		}

		public TTreeChartBox()
		{
			this.InitSigns();
			this.FPersons = new TPersonList(true);
			this.FFilter = new TChartFilter();
			this.FSpouseDistance = 10;
			this.FBranchDistance = 40;
			this.FLevelDistance = 46;
			this.FMargins = 40;
			this.FDepthLimit = -1;
			this.FSelected = null;
			this.FGraph = new TGraph();
		}

		protected override void Dispose(bool Disposing)
		{
			if (Disposing)
			{
				this.FGraph.Dispose();
				this.FFilter.Free();
				this.FPersons.Dispose();
				this.DoneSigns();
			}
			base.Dispose(Disposing);
		}

		private void InitSigns()
		{
			SignsPic[0] = GKResources.iTGGeorgeCross;
			SignsPic[0].MakeTransparent(this.SignsPic[0].GetPixel(0, 0));

			SignsPic[1] = GKResources.iTGSoldier;
			SignsPic[1].MakeTransparent(this.SignsPic[1].GetPixel(0, 0));

			SignsPic[2] = GKResources.iTGSoldierFall;
			SignsPic[2].MakeTransparent(this.SignsPic[2].GetPixel(0, 0));

			SignsPic[3] = GKResources.iTGVeteranRear;
			SignsPic[3].MakeTransparent(this.SignsPic[3].GetPixel(0, 0));
		}

		private void DoneSigns()
		{
			// dummy
		}

		private bool IsChildless(TGEDCOMIndividualRecord iRec)
		{
			string exp = TGenEngine.GetLifeExpectancy(iRec);
			return (exp != "" && exp != "?" && int.Parse(exp) < 15);
		}

		private bool hasMediaFail = false;

		private TreeChartPerson AddDescPerson(TreeChartPerson aParent, TGEDCOMIndividualRecord iRec, TreeChartPerson.TPersonKind aKind, int aGeneration)
		{
			TreeChartPerson Result;
			if (this.FRoot != null && object.Equals(this.FRoot.Rec, iRec))
			{
				Result = this.FRoot;
				Result.Parent = aParent;
				Result.Kind = aKind;
			} else {
				Result = new TreeChartPerson(this);
				Result.BuildBy(iRec, ref hasMediaFail);
				Result.Generation = aGeneration;
				Result.Parent = aParent;
				Result.Kind = aKind;
				this.FPersons.Add(Result);

				if (this.FOptions.Kinship)
				{
					Result.Node = this.FGraph.CreateNode(Result);
				}

				if (aKind != TreeChartPerson.TPersonKind.pkSpouse && aParent != null)
				{
					aParent.AddChild(Result);
				}
			}
			return Result;
		}

		private TreeChartPerson DoAncestorsStep(TreeChartPerson aChild, TGEDCOMIndividualRecord aPerson, int aGeneration, bool dup_flag)
		{
			TreeChartPerson Result = null;

			if (aPerson != null)
			{
				Result = new TreeChartPerson(this);
				Result.BuildBy(aPerson, ref hasMediaFail);
				Result.Generation = aGeneration;
				this.FPersons.Add(Result);

				if (aChild != null)
				{
					Result.AddChild(aChild);
				}

				if (this.FOptions.Kinship)
				{
					Result.Node = this.FGraph.CreateNode(Result);
				}

				if ((this.FDepthLimit <= -1 || aGeneration != this.FDepthLimit) && aPerson.ChildToFamilyLinks.Count > 0 && !dup_flag)
				{
					TGEDCOMFamilyRecord family = aPerson.ChildToFamilyLinks[0].Family;

					bool is_dup = (this.FPreparedFamilies.IndexOf(family.XRef) >= 0);
					if (!is_dup) this.FPreparedFamilies.Add(family.XRef);

					if (TGenEngine.IsRecordAccess(family.Restriction, this.FShieldState))
					{
						TGEDCOMIndividualRecord iFather = family.Husband.Value as TGEDCOMIndividualRecord;
						TGEDCOMIndividualRecord iMother = family.Wife.Value as TGEDCOMIndividualRecord;
						bool divorced = (family.GetTagStringValue("_STAT") == "NOTMARR");

						if (iFather != null && TGenEngine.IsRecordAccess(iFather.Restriction, this.FShieldState))
						{
							Result.Father = this.DoAncestorsStep(Result, iFather, aGeneration + 1, is_dup);
							if (Result.Father != null)
							{
								Result.Father.Divorced = divorced;
								Result.Father.IsDup = is_dup;
								if (this.FOptions.Kinship)
								{
									this.FGraph.CreateLink(Result.Node, Result.Father.Node, 1, 1, 3);
								}
							}
						} else {
							Result.Father = null;
						}

						if (iMother != null && TGenEngine.IsRecordAccess(iMother.Restriction, this.FShieldState))
						{
							Result.Mother = this.DoAncestorsStep(Result, iMother, aGeneration + 1, is_dup);
							if (Result.Mother != null)
							{
								Result.Mother.Divorced = divorced;
								Result.Mother.IsDup = is_dup;
								if (this.FOptions.Kinship)
								{
									this.FGraph.CreateLink(Result.Node, Result.Mother.Node, 1, 1, 3);
								}
							}
						} else {
							Result.Mother = null;
						}

						if (Result.Father != null && Result.Mother != null && this.FOptions.Kinship)
						{
							this.FGraph.CreateLink(Result.Father.Node, Result.Mother.Node, 1, 2, 2);
						}
					}
				}
			}

			return Result;
		}

		private TreeChartPerson DoDescendantsStep(TreeChartPerson aParent, TGEDCOMIndividualRecord aPerson, int aLevel)
		{
			TreeChartPerson Result = null;
			if (aPerson != null && (!this.FOptions.ChildlessExclude || aLevel <= 1 || aPerson.SpouseToFamilyLinks.Count != 0 || !this.IsChildless(aPerson)))
			{
				CustomFilter.TGroupMode sourceMode = this.FFilter.SourceMode;

				if (sourceMode != CustomFilter.TGroupMode.gmNone)
				{
					if (sourceMode != CustomFilter.TGroupMode.gmAny)
					{
						if (sourceMode == CustomFilter.TGroupMode.gmSelected)
						{
							TGEDCOMSourceRecord filter_source;

							if (this.FFilter.SourceRef == "")
							{
								filter_source = null;
							} else {
								filter_source = (TGEDCOMSourceRecord)this.FTree.XRefIndex_Find(this.FFilter.SourceRef);
							}

							if (aPerson.IndexOfSource(filter_source) < 0)
							{
								return Result;
							}
						}
					} else {
						if (aPerson.SourceCitations.Count == 0)
						{
							return Result;
						}
					}
				} else {
					if (aPerson.SourceCitations.Count != 0)
					{
						return Result;
					}
				}

				TChartFilter.TBranchCut branchCut = this.FFilter.BranchCut;
				if (branchCut != TChartFilter.TBranchCut.bcNone)
				{
					if (!(bool)aPerson.ExtData)
					{
						return Result;
					}
				}

				TreeChartPerson res = this.AddDescPerson(aParent, aPerson, TreeChartPerson.TPersonKind.pkDefault, aLevel);
				Result = res;

				int num = aPerson.SpouseToFamilyLinks.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMFamilyRecord family = aPerson.SpouseToFamilyLinks[i].Family;

					bool is_dup = (this.FPreparedFamilies.IndexOf(family.XRef) >= 0);
					if (!is_dup) this.FPreparedFamilies.Add(family.XRef);

					if (TGenEngine.IsRecordAccess(family.Restriction, this.FShieldState))
					{
						TreeChartPerson res_parent = null;
						TGEDCOMSex sex = aPerson.Sex;
						TreeChartPerson ft = null;
						TreeChartPerson mt = null;
						TreeChartPerson.TPersonFlag desc_flag = TreeChartPerson.TPersonFlag.pfDescByFather;
						if (sex != TGEDCOMSex.svMale)
						{
							if (sex == TGEDCOMSex.svFemale)
							{
								TGEDCOMIndividualRecord sp = family.Husband.Value as TGEDCOMIndividualRecord;
								res_parent = this.AddDescPerson(null, sp, TreeChartPerson.TPersonKind.pkSpouse, aLevel);
								res_parent.Sex = TGEDCOMSex.svMale;
								ft = res_parent;
								mt = res;
								desc_flag = TreeChartPerson.TPersonFlag.pfDescByFather;
							}
						}
						else
						{
							TGEDCOMIndividualRecord sp = family.Wife.Value as TGEDCOMIndividualRecord;
							res_parent = this.AddDescPerson(null, sp, TreeChartPerson.TPersonKind.pkSpouse, aLevel);
							res_parent.Sex = TGEDCOMSex.svFemale;
							ft = res;
							mt = res_parent;
							desc_flag = TreeChartPerson.TPersonFlag.pfDescByMother;
						}

						if (this.FOptions.Kinship)
						{
							this.FGraph.CreateLink(res.Node, res_parent.Node, 1, 2, 2);
						}

						if (res_parent != null)
						{
							res.AddSpouse(res_parent);
							res_parent.BaseSpouse = res;
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
								if (TGenEngine.IsRecordAccess(child_rec.Restriction, this.FShieldState))
								{
									TreeChartPerson child = this.DoDescendantsStep(res_parent, child_rec, aLevel + 1);
									if (child != null)
									{
										child.Father = ft;
										child.Mother = mt;
										//int d = (int)desc_flag;
										child.FFlags.Include(desc_flag);
										if (this.FOptions.Kinship)
										{
											this.FGraph.CreateLink(child.Node, ft.Node, 1, 1, 3);
											this.FGraph.CreateLink(child.Node, mt.Node, 1, 1, 3);
										}
									}
								}
							}
						}
					}
				}
			}
			return Result;
		}

		private string FindRelationship(TreeChartPerson aTarget)
		{
			string Result = "";
			TList path = new TList(true);
			try
			{
				TGraph.TGraphLink link = aTarget.Node.LinkIn;
				if (link != null)
				{
					do
					{
						this.FixLink(path, link.Node1.ExtObj as TreeChartPerson, link.Node2.ExtObj as TreeChartPerson, (KinshipsMan.TRelationKind)link.ExtData);
						link = link.Node1.LinkIn;
					}
					while (link != null);
				}
				string tmp = "";
				KinshipsMan.TRelationKind prev_rel = KinshipsMan.TRelationKind.rkNone;
				KinshipsMan.TRelationKind fin_rel = KinshipsMan.TRelationKind.rkNone;
				int great = 0;

				for (int i = path.Count - 1; i >= 0; i--)
				{
					TTreeChartBox.TRelLink L = path[i] as TTreeChartBox.TRelLink;
					KinshipsMan.TRelationKind cur_rel = L.xRel;
					if (this.FPathDebug)
					{
						if (tmp != "")
						{
							tmp += ", ";
						}
						if (L.xFrom.Rec != null)
						{
							tmp = string.Concat(new string[]
							{
								tmp, L.xFrom.Rec.XRef, ">", TGenEngine.RelationSigns[(int)cur_rel], ">"
							});
						}
						if (L.xTo.Rec != null)
						{
							tmp += L.xTo.Rec.XRef;
						}
					}
					if (prev_rel != KinshipsMan.TRelationKind.rkUndefined)
					{
						int g;
						int lev;
						fin_rel = KinshipsMan.FindKinship(prev_rel, cur_rel, out g, out lev);
						great += g;
						prev_rel = fin_rel;
					}
				}

				if (this.FPathDebug)
				{
					if (aTarget.Rec != null)
					{
						aTarget.FPathDebug = aTarget.Rec.XRef + " ";
					}
					aTarget.FPathDebug = aTarget.FPathDebug + " [" + tmp + "]";
				}
				Result = "[" + this.FixRelation(aTarget, fin_rel, great) + "]";
			}
			finally
			{
				path.Dispose();
			}
			return Result;
		}

		private void FixLink(TList path, TreeChartPerson f, TreeChartPerson t, KinshipsMan.TRelationKind rel)
		{
			TTreeChartBox.TRelLink L = new TTreeChartBox.TRelLink();
			L.xFrom = f;
			L.xTo = t;
			if (rel != KinshipsMan.TRelationKind.rkParent)
			{
				if (rel != KinshipsMan.TRelationKind.rkSpouse)
				{
					if (rel != KinshipsMan.TRelationKind.rkChild)
					{
						L.xRel = rel;
					}
					else
					{
						TGEDCOMSex sex = L.xTo.Sex;
						if (sex != TGEDCOMSex.svMale)
						{
							if (sex == TGEDCOMSex.svFemale)
							{
								L.xRel = KinshipsMan.TRelationKind.rkDaughter;
							}
						}
						else
						{
							L.xRel = KinshipsMan.TRelationKind.rkSon;
						}
					}
				}
				else
				{
					TGEDCOMSex sex2 = L.xTo.Sex;
					if (sex2 != TGEDCOMSex.svMale)
					{
						if (sex2 == TGEDCOMSex.svFemale)
						{
							L.xRel = KinshipsMan.TRelationKind.rkWife;
						}
					}
					else
					{
						L.xRel = KinshipsMan.TRelationKind.rkHusband;
					}
				}
			}
			else
			{
				TGEDCOMSex sex3 = L.xTo.Sex;
				if (sex3 != TGEDCOMSex.svMale)
				{
					if (sex3 == TGEDCOMSex.svFemale)
					{
						L.xRel = KinshipsMan.TRelationKind.rkMother;
					}
				}
				else
				{
					L.xRel = KinshipsMan.TRelationKind.rkFather;
				}
			}
			path.Add(L);
		}

		private string FixRelation(TreeChartPerson aTarget, KinshipsMan.TRelationKind Rel, int Great)
		{
			string tmp = "";
			if (Great != 0)
			{
				if (Rel >= KinshipsMan.TRelationKind.rkUncle && Rel < KinshipsMan.TRelationKind.rkNephew)
				{
					tmp = TGenEngine.Numerals[Great] + TGenEngine.NumKinship[(int)aTarget.Sex] + " ";
					if (Rel == KinshipsMan.TRelationKind.rkUncle)
					{
						Rel = KinshipsMan.TRelationKind.rkGrandfather;
					}
					if (Rel == KinshipsMan.TRelationKind.rkAunt)
					{
						Rel = KinshipsMan.TRelationKind.rkGrandmother;
					}
				}
				else
				{
					if (Rel != KinshipsMan.TRelationKind.rkUndefined)
					{
						tmp = this.GetGreat(Great);
					}
				}
			}
			else
			{
				tmp = "";
			}
			return tmp + LangMan.LSList[(int)TGenEngine.RelationKinds[(int)Rel] - 1];
		}

		private string GetGreat(int n)
		{
			string Result = "";
			int num = n;
			int i = 1;
			num = num - i + 1;
			if (num >= i)
			{
				do
				{
					Result += "пра";
					num--;
				}
				while (num != 0);
			}
			return Result;
		}

		private void InitEdges(ref int[] edges)
		{
			for (int i = 0; i <= 255; i++) edges[i] = 0;
		}

		private void Line(Graphics aCanvas, int X1, int Y1, int X2, int Y2)
		{
			int sX = this.FSPX + X1;
			int sX2 = this.FSPX + X2;
			int sY = this.FSPY + Y1;
			int sY2 = this.FSPY + Y2;
			aCanvas.DrawLine(new Pen(Color.Black, 1f), sX, sY, sX2, sY2);

			if (this.FOptions.Decorative) {
				Pen xpen = new Pen(Color.Silver, 1f);
				try
				{
					if (sX == sX2) {
						aCanvas.DrawLine(xpen, sX + 1, sY + 1, sX2 + 1, sY2 - 1);
					} else {
						if (sY == sY2) {
							aCanvas.DrawLine(xpen, sX + 1, sY + 1, sX2 + 0, sY2 + 1);
						}
					}
				}
				finally
				{
					xpen.Dispose();
				}
			}
		}

		private void Predef()
		{
			double sc = (double)(this.FScale / 100.0);
			int fsz = (int)checked((long)Math.Round(unchecked((double)this.FOptions.DefFont_Size * sc)));
			string f_name;

			if (fsz <= 7) {
				f_name = "Small Fonts";
			} else {
				f_name = this.FOptions.DefFont_Name;
			}

			this.FDrawFont = new Font(f_name, ((float)fsz), FontStyle.Regular, GraphicsUnit.Point);
			this.FSpouseDistance = (int)checked((long)Math.Round(10.0 * sc));
			this.FBranchDistance = (int)checked((long)Math.Round(40.0 * sc));
			this.FLevelDistance = (int)checked((long)Math.Round(46.0 * sc));
			this.FMargins = (int)checked((long)Math.Round(40.0 * sc));
		}

		private void RecalcAncestorsChart()
		{
			int[] edges = new int[256];
			this.InitEdges(ref edges);
			TList prev = new TList();
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
			this.InitEdges(ref edges);
			this.RecalcDesc(ref edges, this.FRoot, new Point(this.FMargins, this.FMargins), aPreDef);
		}

		private void RecalcChart()
		{
			if (this.FOptions.Kinship)
			{
				this.FGraph.FindPathTree(this.FKinRoot.Node);
				int num = this.FPersons.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					TreeChartPerson p = this.FPersons[i];
					p.Kinship = this.FindRelationship(p);
				}
			}
			this.FHMax = 0;
			this.FWMax = 0;
			TTreeChartBox.TChartKind fKind = this.FKind;

			switch (fKind) {
				case TTreeChartBox.TChartKind.ckAncestors:
					this.RecalcAncestorsChart();
					break;
				case TTreeChartBox.TChartKind.ckDescendants:
					this.RecalcDescendantsChart(true);
					break;
				case TTreeChartBox.TChartKind.ckBoth:
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
					if (pp.ChildsCount < 1) {
						pp = null;
					} else {
						pp = pp.GetChild(0);
					}
				}
				while (pp != null);
			}
		}

		private void RecalcAnc(TList prev, ref int[] edges, TreeChartPerson aPerson, Point aPt)
		{
			if (aPerson != null)
			{
				aPerson.Pt = aPt;
				int gen = aPerson.Generation;
				int offset;
				if (edges[gen] > 0) {
					offset = this.FBranchDistance;
				} else {
					offset = this.FMargins;
				}
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

		private void ShiftDesc(TreeChartPerson aPerson, int aOffset, bool aSingle)
		{
			if (aPerson != null)
			{
				if (object.Equals(aPerson, this.FRoot))
				{
					aSingle = false;
				}
				aPerson.PtX += aOffset;
				if (aPerson.BaseSpouse != null && (aPerson.BaseSpouse.Sex == TGEDCOMSex.svFemale || aPerson.BaseSpouse.SpousesCount == 1))
				{
					this.ShiftDesc(aPerson.BaseSpouse, aOffset, aSingle);
				}
				else
				{
					if (!aSingle)
					{
						this.ShiftDesc(aPerson.Father, aOffset, aSingle);
						this.ShiftDesc(aPerson.Mother, aOffset, aSingle);
					}
					else
					{
						if (aPerson.FFlags.InSet(TreeChartPerson.TPersonFlag.pfDescByFather))
						{
							this.ShiftDesc(aPerson.Father, aOffset, aSingle);
						}
						else
						{
							if (aPerson.FFlags.InSet(TreeChartPerson.TPersonFlag.pfDescByMother))
							{
								this.ShiftDesc(aPerson.Mother, aOffset, aSingle);
							}
						}
					}
				}
			}
		}

		private void RecalcDescChilds(ref int[] edges, TreeChartPerson aPerson)
		{
			//edges = (int[])edges.Clone();
			if (aPerson.ChildsCount != 0)
			{
				bool fix_pair = aPerson.BaseSpouse != null && aPerson.BaseSpouse.SpousesCount == 1;
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
				int childs_width = (aPerson.ChildsCount - 1) * this.FBranchDistance;

				int num = aPerson.ChildsCount - 1;
				for (int i = 0; i <= num; i++)
				{
					childs_width += aPerson.GetChild(i).Width;
				}

				int cur_x = cent_x - childs_width / 2;

				int num2 = aPerson.ChildsCount - 1;
				for (int i = 0; i <= num2; i++)
				{
					TreeChartPerson child = aPerson.GetChild(i);
					this.RecalcDesc(ref edges, child, new Point(cur_x + child.Width / 2, cur_y), true);
					cur_x = child.Rect.Right + this.FBranchDistance;
				}

				cur_x = aPerson.GetChild(0).PtX;
				if (aPerson.ChildsCount > 1)
				{
					cur_x += (aPerson.GetChild(aPerson.ChildsCount - 1).PtX - cur_x) / 2;
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
				int offset;
				if (edges[gen] > 0)
				{
					offset = this.FBranchDistance;
				}
				else
				{
					offset = this.FMargins;
				}
				if (aPerson.Rect.Left <= edges[gen] + offset)
				{
					this.ShiftDesc(aPerson, edges[gen] + offset - aPerson.Rect.Left, true);
				}
				if (aPerson.Sex == TGEDCOMSex.svMale)
				{
					this.RecalcDescChilds(ref edges, aPerson);
					edges[gen] = aPerson.Rect.Right;
				}

				if (aPerson.SpousesCount > 0)
				{
					TreeChartPerson prev = aPerson;
					int num = aPerson.SpousesCount - 1;
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
			this.Draw(aCanvas, aPerson.Father, TTreeChartBox.TChartKind.ckAncestors);
			this.Draw(aCanvas, aPerson.Mother, TTreeChartBox.TChartKind.ckAncestors);
			int cr_y = aPerson.PtY - this.FLevelDistance / 2;
			if (aPerson.Father != null)
			{
				this.Line(aCanvas, aPerson.Father.PtX, cr_y, aPerson.PtX, cr_y);
				this.Line(aCanvas, aPerson.Father.PtX, aPerson.Father.PtY + aPerson.Father.Height, aPerson.Father.PtX, cr_y);
			}
			if (aPerson.Mother != null)
			{
				this.Line(aCanvas, aPerson.PtX, cr_y, aPerson.Mother.PtX, cr_y);
				this.Line(aCanvas, aPerson.Mother.PtX, aPerson.Mother.PtY + aPerson.Mother.Height, aPerson.Mother.PtX, cr_y);
			}
			if (aPerson.Father != null || aPerson.Mother != null)
			{
				this.Line(aCanvas, aPerson.PtX, cr_y, aPerson.PtX, aPerson.PtY);
			}
		}

		private void DrawDescendants(Graphics aCanvas, TreeChartPerson aPerson)
		{
			int num = aPerson.ChildsCount - 1;
			for (int i = 0; i <= num; i++)
			{
				this.Draw(aCanvas, aPerson.GetChild(i), TTreeChartBox.TChartKind.ckDescendants);
			}

			int spb_ofs = (aPerson.Height - 10) / (aPerson.SpousesCount + 1);
			int spb_beg = aPerson.PtY + (aPerson.Height - spb_ofs * (aPerson.SpousesCount - 1)) / 2;

			TGEDCOMSex sex = aPerson.Sex;
			if (sex != TGEDCOMSex.svMale)
			{
				if (sex == TGEDCOMSex.svFemale)
				{
					int num2 = aPerson.SpousesCount - 1;
					for (int i = 0; i <= num2; i++)
					{
						int spb_v = spb_beg + spb_ofs * i;
						this.Line(aCanvas, aPerson.GetSpouse(i).Rect.Right + 1, spb_v, aPerson.Rect.Left, spb_v);
					}
				}
			}
			else
			{
				int num3 = aPerson.SpousesCount - 1;
				for (int i = 0; i <= num3; i++)
				{
					int spb_v = spb_beg + spb_ofs * i;
					this.Line(aCanvas, aPerson.Rect.Right + 1, spb_v, aPerson.GetSpouse(i).Rect.Left, spb_v);
				}
			}

			int num4 = aPerson.SpousesCount - 1;
			for (int i = 0; i <= num4; i++)
			{
				this.Draw(aCanvas, aPerson.GetSpouse(i), TTreeChartBox.TChartKind.ckDescendants);
			}

			int cr_y = aPerson.PtY + aPerson.Height + this.FLevelDistance / 2;
			int cx = 0;
			if (aPerson.BaseSpouse == null || (aPerson.BaseSpouse != null && aPerson.BaseSpouse.SpousesCount > 1))
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

			if (aPerson.ChildsCount != 0)
			{
				this.Line(aCanvas, cx, spb_beg, cx, cr_y);
				if (aPerson.ChildsCount == 1)
				{
					Point child_pt = aPerson.GetChild(0).Pt;
					this.Line(aCanvas, child_pt.X, cr_y, child_pt.X, child_pt.Y);
				}
				else
				{
					int bpx = aPerson.GetChild(0).PtX;
					int epx = aPerson.GetChild(aPerson.ChildsCount - 1).PtX;
					this.Line(aCanvas, bpx, cr_y, epx, cr_y);
					int num5 = aPerson.ChildsCount - 1;
					for (int i = 0; i <= num5; i++)
					{
						Point child_pt = aPerson.GetChild(i).Pt;
						this.Line(aCanvas, child_pt.X, cr_y, child_pt.X, child_pt.Y);
					}
				}
			}
		}

		public override void InternalDraw(Graphics aCanvas, bool Default)
		{
			base.InternalDraw(aCanvas, Default);

			this.Draw(aCanvas, this.FRoot, this.FKind);

			//if (ControlsVisible) FScaleControl.Draw(aCanvas, this.ControlsRect);
		}

		protected void Draw(Graphics aCanvas, TreeChartPerson aPerson, TChartKind aDirKind)
		{
			if (aPerson != null)
			{
				switch (this.FKind) {
					case TTreeChartBox.TChartKind.ckAncestors:
						this.DrawAncestors(aCanvas, aPerson);
						break;

					case TTreeChartBox.TChartKind.ckDescendants:
						this.DrawDescendants(aCanvas, aPerson);
						break;

					case TTreeChartBox.TChartKind.ckBoth:
						if (aPerson == this.FRoot || aDirKind == TTreeChartBox.TChartKind.ckAncestors) this.DrawAncestors(aCanvas, aPerson);
						if (aPerson == this.FRoot || aDirKind == TTreeChartBox.TChartKind.ckDescendants) this.DrawDescendants(aCanvas, aPerson);
						break;
				}

				aPerson.Draw(aCanvas, this.FSPX, this.FSPY);
			}
		}

		public void GenChart(TGEDCOMIndividualRecord aPerson, TChartKind aKind)
		{
			this.InternalGenChart(aPerson, aKind);
			this.ScrollRange();
		}

		protected void InternalGenChart(TGEDCOMIndividualRecord aPerson, TChartKind aKind)
		{
			try
			{
				this.FKind = aKind;
				this.FSelected = null;
				this.FPersons.Clear();
				this.Predef();
				this.FGraph.Clear();
				this.DoFilter(aPerson);
				this.FRoot = null;
				TTreeChartBox.TChartKind fKind = this.FKind;

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

				this.FKinRoot = this.FRoot;
				this.RecalcChart();
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("TAncestryChartBox.InternalGenChart(): " + E.Message);
			}
		}

		public void DoFilter(TGEDCOMIndividualRecord aRoot)
		{
			if (this.FFilter.BranchCut != TChartFilter.TBranchCut.bcNone)
			{
				TGenEngine.InitExtCounts(this.FTree, 0);
				this.DoDescendantsFilter(aRoot);
				aRoot.ExtData = true;
			}
		}

		public EnumSet GetPersonSign(TGEDCOMIndividualRecord iRec)
		{
			EnumSet result = new EnumSet();
			int num = iRec.UserReferences.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				string rs = iRec.UserReferences[i].StringValue;
				for (TGenEngine.TChartPersonSign cps = TGenEngine.TChartPersonSign.urRI_StGeorgeCross; 
				     cps <= TGenEngine.TChartPersonSign.urLast; cps++)
				{
					if (rs == TGenEngine.UserRefs[(int)cps]) result.Include(cps);
				}
			}
			return result;
		}

		public void RebuildKinships()
		{
			try
			{
				if (this.FOptions.Kinship)
				{
					TreeChartPerson p = this.FSelected;
					if (p != null)
					{
						this.FKinRoot = p;
						this.RecalcChart();
						base.ScrollRange();
					}
				}
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("TAncestryChartBox.RebuildKinships(): " + E.Message);
			}
		}

		public void SaveSnapshot([In] string aFileName)
		{
			string ext = Path.GetExtension(aFileName).ToLower();

			if ((ext == ".bmp" || ext == ".jpg") && this.FImageWidth >= 65535)
			{
				TGenEngine.ShowError(LangMan.LSList[380]);
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
						this.InternalDraw(canv, false);
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

		private void SetSelected([In] TreeChartPerson Value)
		{
			if (this.FSelected != null) this.FSelected.Selected = false;
			this.FSelected = Value;
			if (this.FSelected != null) this.FSelected.Selected = true;

			base.Invalidate();
		}

		public void SelectBy(int aX, int aY)
		{
			aX -= this.FSPX;
			aY -= this.FSPY;
			int num = this.FPersons.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				TreeChartPerson p = this.FPersons[i];
				if (p.Rect.Contains(aX, aY))
				{
					this.SetSelected(p);
					return;
				}
			}
			this.SetSelected(null);
		}

		public void SelectByRec(TGEDCOMIndividualRecord iRec)
		{
			int num = this.FPersons.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				TreeChartPerson p = this.FPersons[i];
				if (p.Rec == iRec)
				{
					this.SetSelected(p);
					return;
				}
			}
			this.SetSelected(null);
		}

		private bool DoDescendantsFilter(TGEDCOMIndividualRecord aPerson)
		{
			bool Result = false;
			if (aPerson != null)
			{
				TChartFilter.TBranchCut branchCut = this.FFilter.BranchCut;
				switch (branchCut) {
					case TChartFilter.TBranchCut.bcYears:
						int year = TGenEngine.GetIndependentYear(aPerson, "BIRT");
						Result = (year >= this.FFilter.BranchYear);
						break;

					case TChartFilter.TBranchCut.bcPersons:
						Result = (this.FFilter.BranchPersons.IndexOf(aPerson.XRef + ";") >= 0);
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
						Result |= res_child;
					}
				}
				aPerson.ExtData = Result;
			}
			return Result;
		}
	}
}
