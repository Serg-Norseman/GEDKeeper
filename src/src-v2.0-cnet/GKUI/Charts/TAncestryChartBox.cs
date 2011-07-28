using GedCom551;
using GKCore;
using GKUI.Charts;
using GKUI.Lists;
using GKSys;
using System;
using System.Drawing;
using System.Drawing.Imaging;
using System.IO;
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GKUI.Charts
{
	public class TAncestryChartBox : TCustomChartBox
	{
		internal class TRelLink
		{
			public TPerson xFrom;
			public TPerson xTo;
			public TGenEngine.TRelationKind xRel;

			public void Free()
			{
				TObjectHelper.Free(this);
			}
		}

		public enum TChartKind : byte
		{
			ckAncestors,
			ckDescendants,
			ckBoth
		}

		internal TChartKind FKind;

		internal static readonly string[] SignsData;
		internal int FBranchDistance;
		internal int FDepthLimit;
		internal int FHMax;
		internal int FWMax;
		internal new TChartFilter FFilter;
		internal TGraph FGraph;
		internal TPerson FKinRoot;
		internal int FLevelDistance;
		internal int FMargin;
		internal bool FPathDebug;
		internal TPersonList FPersons;
		internal TPerson FRoot;
		internal int FScale;
		internal int FSpouseDistance;
		internal TPerson FSelected;

		public Bitmap[] SignsPic = new Bitmap[4];

		static TAncestryChartBox()
		{
			TAncestryChartBox.SignsData = new string[]
			{
				"GEORGE_CROSS", 
				"SOLDIER", 
				"SOLDIER_FALL", 
				"VETERAN_REAR"
			};
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

		public new TChartFilter Filter
		{
			get { return this.FFilter; }
		}

		public TChartKind Kind
		{
			get { return FKind; }
			set { FKind = value; }
		}

		public int Margin
		{
			get { return this.FMargin; }
			set { this.FMargin = value; }
		}

		public bool PathDebug
		{
			get { return this.FPathDebug; }
			set { this.FPathDebug = value; }
		}

		public TPerson Root
		{
			get { return this.FRoot; }
		}

		public new int Scale
		{
			get { return this.FScale; }
			set { this.FScale = value; }
		}

		public TPerson Selected
		{
			get { return this.FSelected; }
			set { this.SetSelected(value); }
		}

		internal void InitSigns()
		{
			//alert
			/*TGenEngine.TChartPersonSign ps = TGenEngine.TChartPersonSign.urRI_StGeorgeCross;
			do
			{
				try
				{
					this.SignsPic[(int)ps - 1] = Bitmap.FromResource((IntPtr)VCLUtils.HInstance(), TAncestryChartBox.SignsData[(int)ps - 1]);
					this.SignsPic[(int)ps - 1].MakeTransparent(this.SignsPic[(int)ps - 1].GetPixel(0, 0));
				}
				finally
				{
				}
				ps++;
			}
			while (ps != (TGenEngine.TChartPersonSign)5);*/
		}

		internal void DoneSigns()
		{
			/*TGenEngine.TChartPersonSign ps = TGenEngine.TChartPersonSign.urRI_StGeorgeCross;
			do
			{
				TObjectHelper.Free(TAncestryChartBox.SignsData[(int)ps - 1]);
				ps++;
			}
			while (ps != (TGenEngine.TChartPersonSign)5);*/
		}

		internal bool IsChildless(TGEDCOMIndividualRecord iRec)
		{
			string exp = TGenEngine.GetLifeExpectancy(iRec);
			return (exp != "" && exp != "?" && int.Parse(exp) < 15);
		}

		internal TPerson AddDescPerson(TPerson aParent, TGEDCOMIndividualRecord iRec, TPerson.TPersonKind aKind, int aGeneration)
		{
			TPerson Result;
			if (this.FRoot != null && object.Equals(this.FRoot.Rec, iRec))
			{
				Result = this.FRoot;
				Result.Parent = aParent;
				Result.Kind = aKind;
			} else {
				Result = new TPerson(this);
				Result.BuildBy(iRec);
				Result.Generation = aGeneration;
				Result.Parent = aParent;
				Result.Kind = aKind;
				this.FPersons.Add(Result);

				if (this.FOptions.Kinship)
				{
					Result.Node = this.FGraph.CreateNode(Result);
				}

				if (aKind != TPerson.TPersonKind.pkSpouse && aParent != null)
				{
					aParent.AddChild(Result);
				}
			}
			return Result;
		}

		internal TPerson DoAncestorsStep(TPerson aChild, TGEDCOMIndividualRecord aPerson, int aGeneration)
		{
			TPerson Result;
			if (aPerson == null) {
				Result = null;
			} else {
				Result = new TPerson(this);
				Result.BuildBy(aPerson);
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

				if ((this.FDepthLimit <= -1 || aGeneration != this.FDepthLimit) && aPerson.ChildToFamilyLinksCount > 0)
				{
					TGEDCOMFamilyRecord family = aPerson.GetChildToFamilyLink(0).Family;

					if (TGenEngine.IsRecordAccess(family.Restriction, this.FShieldState))
					{
						TGEDCOMIndividualRecord iFather = family.Husband.Value as TGEDCOMIndividualRecord;
						TGEDCOMIndividualRecord iMother = family.Wife.Value as TGEDCOMIndividualRecord;
						bool divorced = (family.GetTagStringValue("_STAT") == "NOTMARR");

						if (iFather != null && TGenEngine.IsRecordAccess(iFather.Restriction, this.FShieldState))
						{
							Result.Father = this.DoAncestorsStep(Result, iFather, aGeneration + 1);
							if (Result.Father != null)
							{
								Result.Father.Divorced = divorced;
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
							Result.Mother = this.DoAncestorsStep(Result, iMother, aGeneration + 1);
							if (Result.Mother != null)
							{
								Result.Mother.Divorced = divorced;
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

		internal TPerson DoDescendantsStep(TPerson aParent, TGEDCOMIndividualRecord aPerson, int aLevel)
		{
			TPerson Result = null;
			if (aPerson != null && (!this.FOptions.ChildlessExclude || aLevel <= 1 || aPerson.SpouseToFamilyLinksCount != 0 || !this.IsChildless(aPerson)))
			{
				TFilter.TGroupMode sourceMode = this.FFilter.SourceMode;

				if (sourceMode != TFilter.TGroupMode.gmNone)
				{
					if (sourceMode != TFilter.TGroupMode.gmAny)
					{
						if (sourceMode == TFilter.TGroupMode.gmSelected)
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
						if (aPerson.GetSourceCitationsCount() == 0)
						{
							return Result;
						}
					}
				} else {
					if (aPerson.GetSourceCitationsCount() != 0)
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

				TPerson res = this.AddDescPerson(aParent, aPerson, TPerson.TPersonKind.pkDefault, aLevel);
				Result = res;

				int num = aPerson.SpouseToFamilyLinksCount - 1;
				int i = 0;
				if (num >= i)
				{
					num++;
					do
					{
						TGEDCOMFamilyRecord family = aPerson.GetSpouseToFamilyLink(i).Family;
						if (TGenEngine.IsRecordAccess(family.Restriction, this.FShieldState))
						{
							TPerson res_parent = null;
							TGEDCOMObject.TGEDCOMSex sex = aPerson.Sex;
							TPerson ft = null;
							TPerson mt = null;
							TPerson.TPersonFlag desc_flag = TPerson.TPersonFlag.pfDescByFather;
							if (sex != TGEDCOMObject.TGEDCOMSex.svMale)
							{
								if (sex == TGEDCOMObject.TGEDCOMSex.svFemale)
								{
									TGEDCOMIndividualRecord sp = family.Husband.Value as TGEDCOMIndividualRecord;
									res_parent = this.AddDescPerson(null, sp, TPerson.TPersonKind.pkSpouse, aLevel);
									res_parent.Sex = TGEDCOMObject.TGEDCOMSex.svMale;
									ft = res_parent;
									mt = res;
									desc_flag = TPerson.TPersonFlag.pfDescByFather;
								}
							}
							else
							{
								TGEDCOMIndividualRecord sp = family.Wife.Value as TGEDCOMIndividualRecord;
								res_parent = this.AddDescPerson(null, sp, TPerson.TPersonKind.pkSpouse, aLevel);
								res_parent.Sex = TGEDCOMObject.TGEDCOMSex.svFemale;
								ft = res;
								mt = res_parent;
								desc_flag = TPerson.TPersonFlag.pfDescByMother;
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
							if (this.FDepthLimit <= -1 || aLevel != this.FDepthLimit)
							{
								int num2 = family.ChildrenCount - 1;
								int j = 0;
								if (num2 >= j)
								{
									num2++;
									do
									{
										TGEDCOMIndividualRecord child_rec = family.GetChildren(j).Value as TGEDCOMIndividualRecord;
										if (TGenEngine.IsRecordAccess(child_rec.Restriction, this.FShieldState))
										{
											TPerson child = this.DoDescendantsStep(res_parent, child_rec, aLevel + 1);
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
										j++;
									}
									while (j != num2);
								}
							}
						}
						i++;
					}
					while (i != num);
				}
			}
			return Result;
		}

		internal string FindRelationship(TPerson aTarget)
		{
			string Result = "";
			TObjectList path = new TObjectList(true);
			try
			{
				TGraph.TGraphLink link = aTarget.Node.LinkIn;
				if (link != null)
				{
					do
					{
						this.FixLink(path, link.Node1.ExtObj as TPerson, link.Node2.ExtObj as TPerson, (TGenEngine.TRelationKind)link.ExtData);
						link = link.Node1.LinkIn;
					}
					while (link != null);
				}
				string tmp = "";
				TGenEngine.TRelationKind prev_rel = TGenEngine.TRelationKind.rkNone;
				TGenEngine.TRelationKind fin_rel = TGenEngine.TRelationKind.rkNone;
				int great = 0;
				int i = path.Count - 1;
				if (i >= 0)
				{
					do
					{
						TAncestryChartBox.TRelLink L = path[i] as TAncestryChartBox.TRelLink;
						TGenEngine.TRelationKind cur_rel = L.xRel;
						if (this.FPathDebug)
						{
							if (BDSSystem.WStrCmp(tmp, "") != 0)
							{
								tmp += ", ";
							}
							if (L.xFrom.Rec != null)
							{
								tmp = string.Concat(new string[]
								{
									tmp, 
									L.xFrom.Rec.XRef, 
									">", 
									TGenEngine.RelationSigns[(int)cur_rel], 
									">"
								});
							}
							if (L.xTo.Rec != null)
							{
								tmp += L.xTo.Rec.XRef;
							}
						}
						if (prev_rel != TGenEngine.TRelationKind.rkUndefined)
						{
							int g = 0;
							int lev = 0;
							fin_rel = this.FEngine.FindKinship(prev_rel, cur_rel, ref g, ref lev);
							great += g;
							prev_rel = fin_rel;
						}
						i--;
					}
					while (i != -1);
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
				path.Free();
			}
			return Result;
		}

		internal void FixLink(TObjectList path, TPerson f, TPerson t, TGenEngine.TRelationKind rel)
		{
			TAncestryChartBox.TRelLink L = new TAncestryChartBox.TRelLink();
			L.xFrom = f;
			L.xTo = t;
			if (rel != TGenEngine.TRelationKind.rkParent)
			{
				if (rel != TGenEngine.TRelationKind.rkSpouse)
				{
					if (rel != TGenEngine.TRelationKind.rkChild)
					{
						L.xRel = rel;
					}
					else
					{
						TGEDCOMObject.TGEDCOMSex sex = L.xTo.Sex;
						if (sex != TGEDCOMObject.TGEDCOMSex.svMale)
						{
							if (sex == TGEDCOMObject.TGEDCOMSex.svFemale)
							{
								L.xRel = TGenEngine.TRelationKind.rkDaughter;
							}
						}
						else
						{
							L.xRel = TGenEngine.TRelationKind.rkSon;
						}
					}
				}
				else
				{
					TGEDCOMObject.TGEDCOMSex sex2 = L.xTo.Sex;
					if (sex2 != TGEDCOMObject.TGEDCOMSex.svMale)
					{
						if (sex2 == TGEDCOMObject.TGEDCOMSex.svFemale)
						{
							L.xRel = TGenEngine.TRelationKind.rkWife;
						}
					}
					else
					{
						L.xRel = TGenEngine.TRelationKind.rkHusband;
					}
				}
			}
			else
			{
				TGEDCOMObject.TGEDCOMSex sex3 = L.xTo.Sex;
				if (sex3 != TGEDCOMObject.TGEDCOMSex.svMale)
				{
					if (sex3 == TGEDCOMObject.TGEDCOMSex.svFemale)
					{
						L.xRel = TGenEngine.TRelationKind.rkMother;
					}
				}
				else
				{
					L.xRel = TGenEngine.TRelationKind.rkFather;
				}
			}
			path.Add(L);
		}

		internal string FixRelation(TPerson aTarget, TGenEngine.TRelationKind Rel, int Great)
		{
			string tmp = "";
			if (Great != 0)
			{
				if (Rel >= TGenEngine.TRelationKind.rkUncle && Rel < TGenEngine.TRelationKind.rkNephew)
				{
					tmp = TGenEngine.Numerals[Great + 1 - 1] + TGenEngine.NumKinship[(int)aTarget.Sex] + " ";
					if (Rel == TGenEngine.TRelationKind.rkUncle)
					{
						Rel = TGenEngine.TRelationKind.rkGrandfather;
					}
					if (Rel == TGenEngine.TRelationKind.rkAunt)
					{
						Rel = TGenEngine.TRelationKind.rkGrandmother;
					}
				}
				else
				{
					if (Rel != TGenEngine.TRelationKind.rkUndefined)
					{
						tmp = this.GetGreat(Great);
					}
				}
			}
			else
			{
				tmp = "";
			}
			return tmp + GKL.LSList[(int)TGenEngine.RelationKinds[(int)Rel] - 1];
		}

		internal string GetGreat(int n)
		{
			string Result = "";
			int arg_09_0 = 1;
			int num = n;
			int i = arg_09_0;
			int arg_12_0 = num;
			int arg_12_1 = i;
			num = num - i + 1;
			if (arg_12_0 >= arg_12_1)
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

		internal void InitEdges(ref int[] edges)
		{
			for (int i = 0; i <= 255; i++) edges[i] = 0;
		}

		internal void Line(Graphics aCanvas, int X1, int Y1, int X2, int Y2)
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
					TObjectHelper.Free(xpen);
				}
			}
		}

		internal void Predef()
		{
			float sc = (float)((double)this.FScale / 100.0);
			int fsz = (int)checked((long)Math.Round(unchecked((double)this.FOptions.DefFont_Size * (double)sc)));
			string f_name;

			if (fsz <= 7) {
				f_name = "Small Fonts";
			} else {
				f_name = this.FOptions.DefFont_Name;
			}

			this.FDrawFont = new Font(f_name, ((float)((double)fsz)), FontStyle.Regular, GraphicsUnit.Point);
			this.FSpouseDistance = (int)checked((long)Math.Round(unchecked(10.0 * (double)sc)));
			this.FBranchDistance = (int)checked((long)Math.Round(unchecked(40.0 * (double)sc)));
			this.FLevelDistance = (int)checked((long)Math.Round(unchecked(46.0 * (double)sc)));
			this.FMargin = (int)checked((long)Math.Round(unchecked(40.0 * (double)sc)));
		}

		internal void RecalcAncestorsChart()
		{
			int[] edges = new int[256];
			this.InitEdges(ref edges);
			TList prev = new TList();
			try
			{
				this.RecalcAnc(prev, ref edges, this.FRoot, TPoint.Create(this.FMargin, this.FMargin));
			}
			finally
			{
				prev.Free();
			}
		}

		internal void RecalcDescendantsChart(bool aPreDef)
		{
			int[] edges = new int[256];
			this.InitEdges(ref edges);
			this.RecalcDesc(ref edges, this.FRoot, TPoint.Create(this.FMargin, this.FMargin), aPreDef);
		}

		internal void RecalcChart()
		{
			if (this.FOptions.Kinship)
			{
				this.FGraph.FindPathTree(this.FKinRoot.Node);
				int num = this.FPersons.Count - 1;
				int i = 0;
				if (num >= i)
				{
					num++;
					do
					{
						TPerson p = this.FPersons[i];
						p.Kinship = this.FindRelationship(p);
						i++;
					}
					while (i != num);
				}
			}
			this.FHMax = 0;
			this.FWMax = 0;
			TAncestryChartBox.TChartKind fKind = this.FKind;

			switch (fKind) {
				case TAncestryChartBox.TChartKind.ckAncestors:
					this.RecalcAncestorsChart();
					break;
				case TAncestryChartBox.TChartKind.ckDescendants:
					this.RecalcDescendantsChart(true);
					break;
				case TAncestryChartBox.TChartKind.ckBoth:
					this.RecalcAncestorsChart();
					this.RecalcDescendantsChart(false);
					break;
			}

			this.FHMax = this.FHMax + this.FMargin - 1;
			this.FWMax = this.FWMax + this.FMargin - 1;
			this.FImageHeight = this.FHMax;
			this.FImageWidth = this.FWMax;
		}

		internal void ShiftAnc(ref int[] edges, TPerson aPerson, int aOffset)
		{
			TPerson pp = aPerson;
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

		internal void RecalcAnc(TList prev, ref int[] edges, TPerson aPerson, TPoint aPt)
		{
			if (aPerson != null)
			{
				aPerson.Pt = aPt;
				int gen = aPerson.Generation;
				int offset;
				if (edges[gen] > 0) {
					offset = this.FBranchDistance;
				} else {
					offset = this.FMargin;
				}
				if (aPerson.Rect.Left <= edges[gen] + offset) {
					this.ShiftAnc(ref edges, aPerson, edges[gen] + offset - aPerson.Rect.Left);
				}
				edges[gen] = aPerson.Rect.Right;
				prev.Add(aPerson);
				if (aPerson.Rect.Top < 0)
				{
					offset = 0 - aPerson.Rect.Top + this.Margin;
					int num = prev.Count - 1;
					int i = 0;
					if (num >= i)
					{
						num++;
						do
						{
							TPerson pp = prev[i] as TPerson;
							pp.PtY += offset;
							i++;
						}
						while (i != num);
					}
				}
				if (aPerson.Father != null && aPerson.Mother != null)
				{
					TPoint xpt = TPoint.Create(aPerson.PtX - (this.FSpouseDistance + aPerson.Father.Width / 2), aPerson.PtY - this.FLevelDistance - aPerson.Height);
					this.RecalcAnc(prev, ref edges, aPerson.Father, xpt);
					xpt = TPoint.Create(aPerson.PtX + (this.FSpouseDistance + aPerson.Mother.Width / 2), aPerson.PtY - this.FLevelDistance - aPerson.Height);
					this.RecalcAnc(prev, ref edges, aPerson.Mother, xpt);
					aPerson.PtX = (aPerson.Father.PtX + aPerson.Mother.PtX) / 2;
					edges[aPerson.Generation] = aPerson.Rect.Right;
				}
				else
				{
					TPoint xpt = TPoint.Create(aPerson.PtX, aPerson.PtY - this.FLevelDistance - aPerson.Height);
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

		internal void ShiftDesc(TPerson aPerson, int aOffset, bool aSingle)
		{
			if (aPerson != null)
			{
				if (object.Equals(aPerson, this.FRoot))
				{
					aSingle = false;
				}
				aPerson.PtX += aOffset;
				if (aPerson.BaseSpouse != null && (aPerson.BaseSpouse.Sex == TGEDCOMObject.TGEDCOMSex.svFemale || aPerson.BaseSpouse.SpousesCount == 1))
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
						if (aPerson.FFlags.InSet(TPerson.TPersonFlag.pfDescByFather))
						{
							this.ShiftDesc(aPerson.Father, aOffset, aSingle);
						}
						else
						{
							if (aPerson.FFlags.InSet(TPerson.TPersonFlag.pfDescByMother))
							{
								this.ShiftDesc(aPerson.Mother, aOffset, aSingle);
							}
						}
					}
				}
			}
		}

		internal void RecalcDescChilds(ref int[] edges, TPerson aPerson)
		{
			//edges = (int[])edges.Clone();
			if (aPerson.ChildsCount != 0)
			{
				bool fix_pair = aPerson.BaseSpouse != null && aPerson.BaseSpouse.SpousesCount == 1;
				int cent_x = 0;
				if (fix_pair)
				{
					TGEDCOMObject.TGEDCOMSex sex = aPerson.Sex;
					if (sex != TGEDCOMObject.TGEDCOMSex.svMale)
					{
						if (sex == TGEDCOMObject.TGEDCOMSex.svFemale)
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
				int i = 0;
				if (num >= i)
				{
					num++;
					do
					{
						childs_width += aPerson.GetChild(i).Width;
						i++;
					}
					while (i != num);
				}
				int cur_x = cent_x - childs_width / 2;
				int num2 = aPerson.ChildsCount - 1;
				i = 0;
				if (num2 >= i)
				{
					num2++;
					do
					{
						TPerson child = aPerson.GetChild(i);
						this.RecalcDesc(ref edges, child, TPoint.Create(cur_x + child.Width / 2, cur_y), true);
						cur_x = child.Rect.Right + this.FBranchDistance;
						i++;
					}
					while (i != num2);
				}
				cur_x = aPerson.GetChild(0).PtX;
				if (aPerson.ChildsCount > 1)
				{
					cur_x += (aPerson.GetChild(aPerson.ChildsCount - 1).PtX - cur_x) / 2;
				}
				if (fix_pair)
				{
					TGEDCOMObject.TGEDCOMSex sex2 = aPerson.Sex;
					if (sex2 != TGEDCOMObject.TGEDCOMSex.svMale)
					{
						if (sex2 == TGEDCOMObject.TGEDCOMSex.svFemale)
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

		internal void RecalcDesc(ref int[] edges, TPerson aPerson, TPoint aPt, bool aPreDef)
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
					offset = this.FMargin;
				}
				if (aPerson.Rect.Left <= edges[gen] + offset)
				{
					this.ShiftDesc(aPerson, edges[gen] + offset - aPerson.Rect.Left, true);
				}
				if (aPerson.Sex == TGEDCOMObject.TGEDCOMSex.svMale)
				{
					this.RecalcDescChilds(ref edges, aPerson);
					edges[gen] = aPerson.Rect.Right;
				}
				if (aPerson.SpousesCount > 0)
				{
					TPerson prev = aPerson;
					int num = aPerson.SpousesCount - 1;
					int i = 0;
					if (num >= i)
					{
						num++;
						do
						{
							TPerson sp = aPerson.GetSpouse(i);
							TGEDCOMObject.TGEDCOMSex sex = aPerson.Sex;
							TPoint sp_pt = new TPoint();
							if (sex != TGEDCOMObject.TGEDCOMSex.svMale)
							{
								if (sex == TGEDCOMObject.TGEDCOMSex.svFemale)
								{
									sp_pt = TPoint.Create(prev.Rect.Left - (this.FBranchDistance + sp.Width / 2), aPerson.PtY);
								}
							}
							else
							{
								sp_pt = TPoint.Create(prev.Rect.Right + (this.FBranchDistance + sp.Width / 2), aPerson.PtY);
							}
							this.RecalcDesc(ref edges, sp, sp_pt, true);
							if (sp.Sex != TGEDCOMObject.TGEDCOMSex.svMale)
							{
								prev = sp;
							}
							i++;
						}
						while (i != num);
					}
				}
				if (aPerson.Sex == TGEDCOMObject.TGEDCOMSex.svFemale)
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

		internal void DrawAncestors(Graphics aCanvas, TPerson aPerson)
		{
			this.Draw(aCanvas, aPerson.Father, TAncestryChartBox.TChartKind.ckAncestors);
			this.Draw(aCanvas, aPerson.Mother, TAncestryChartBox.TChartKind.ckAncestors);
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

		internal void DrawDescendants(Graphics aCanvas, TPerson aPerson)
		{
			int num = aPerson.ChildsCount - 1;
			int i = 0;
			if (num >= i)
			{
				num++;
				do
				{
					this.Draw(aCanvas, aPerson.GetChild(i), TAncestryChartBox.TChartKind.ckDescendants);
					i++;
				}
				while (i != num);
			}
			int spb_ofs = (aPerson.Height - 10) / (aPerson.SpousesCount + 1);
			int spb_beg = aPerson.PtY + (aPerson.Height - spb_ofs * (aPerson.SpousesCount - 1)) / 2;
			TGEDCOMObject.TGEDCOMSex sex = aPerson.Sex;
			if (sex != TGEDCOMObject.TGEDCOMSex.svMale)
			{
				if (sex == TGEDCOMObject.TGEDCOMSex.svFemale)
				{
					int num2 = aPerson.SpousesCount - 1;
					i = 0;
					if (num2 >= i)
					{
						num2++;
						do
						{
							int spb_v = spb_beg + spb_ofs * i;
							this.Line(aCanvas, aPerson.GetSpouse(i).Rect.Right + 1, spb_v, aPerson.Rect.Left, spb_v);
							i++;
						}
						while (i != num2);
					}
				}
			}
			else
			{
				int num3 = aPerson.SpousesCount - 1;
				i = 0;
				if (num3 >= i)
				{
					num3++;
					do
					{
						int spb_v = spb_beg + spb_ofs * i;
						this.Line(aCanvas, aPerson.Rect.Right + 1, spb_v, aPerson.GetSpouse(i).Rect.Left, spb_v);
						i++;
					}
					while (i != num3);
				}
			}
			int num4 = aPerson.SpousesCount - 1;
			i = 0;
			if (num4 >= i)
			{
				num4++;
				do
				{
					this.Draw(aCanvas, aPerson.GetSpouse(i), TAncestryChartBox.TChartKind.ckDescendants);
					i++;
				}
				while (i != num4);
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
				TGEDCOMObject.TGEDCOMSex sex2 = aPerson.Sex;
				if (sex2 != TGEDCOMObject.TGEDCOMSex.svMale)
				{
					if (sex2 == TGEDCOMObject.TGEDCOMSex.svFemale)
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
					TPoint child_pt = aPerson.GetChild(0).Pt;
					this.Line(aCanvas, child_pt.X, cr_y, child_pt.X, child_pt.Y);
				}
				else
				{
					int bpx = aPerson.GetChild(0).PtX;
					int epx = aPerson.GetChild(aPerson.ChildsCount - 1).PtX;
					this.Line(aCanvas, bpx, cr_y, epx, cr_y);
					int num5 = aPerson.ChildsCount - 1;
					i = 0;
					if (num5 >= i)
					{
						num5++;
						do
						{
							TPoint child_pt = aPerson.GetChild(i).Pt;
							this.Line(aCanvas, child_pt.X, cr_y, child_pt.X, child_pt.Y);
							i++;
						}
						while (i != num5);
					}
				}
			}
		}

		public override void InternalDraw(Graphics aCanvas, bool Default)
		{
			base.InternalDraw(aCanvas, Default);

			this.Draw(aCanvas, this.FRoot, this.FKind);
		}

		protected void Draw(Graphics aCanvas, TPerson aPerson, TChartKind aDirKind)
		{
			if (aPerson != null)
			{
				TAncestryChartBox.TChartKind fKind = this.FKind;
				if (fKind != TAncestryChartBox.TChartKind.ckAncestors)
				{
					if (fKind != TAncestryChartBox.TChartKind.ckDescendants)
					{
						if (fKind == TAncestryChartBox.TChartKind.ckBoth)
						{
							if (object.Equals(aPerson, this.FRoot) || aDirKind == TAncestryChartBox.TChartKind.ckAncestors)
							{
								this.DrawAncestors(aCanvas, aPerson);
							}
							if (object.Equals(aPerson, this.FRoot) || aDirKind == TAncestryChartBox.TChartKind.ckDescendants)
							{
								this.DrawDescendants(aCanvas, aPerson);
							}
						}
					}
					else
					{
						this.DrawDescendants(aCanvas, aPerson);
					}
				}
				else
				{
					this.DrawAncestors(aCanvas, aPerson);
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
			this.FKind = aKind;
			this.FSelected = null;
			this.FPersons.Clear();
			this.Predef();
			this.FGraph.Clear();
			this.DoFilter(aPerson);
			this.FRoot = null;
			TAncestryChartBox.TChartKind fKind = this.FKind;

			switch (fKind) {
				case TChartKind.ckAncestors:
					this.FRoot = this.DoAncestorsStep(null, aPerson, 1);
					break;
					
				case TChartKind.ckDescendants:
					this.FRoot = this.DoDescendantsStep(null, aPerson, 1);
					break;

				case TChartKind.ckBoth:
					this.FRoot = this.DoAncestorsStep(null, aPerson, 1);
					this.DoDescendantsStep(null, aPerson, 1);
					break;
			}

			this.FKinRoot = this.FRoot;
			this.RecalcChart();
		}

		public TAncestryChartBox()
		{
			this.InitSigns();
			this.FPersons = new TPersonList(true);
			this.FFilter = new TChartFilter();
			this.FSpouseDistance = 10;
			this.FBranchDistance = 40;
			this.FLevelDistance = 46;
			this.FMargin = 40;
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
				this.FPersons.Free();
				this.DoneSigns();
			}
			base.Dispose(Disposing);
		}

		public void DoFilter(TGEDCOMIndividualRecord aRoot)
		{
			if (this.FFilter.BranchCut != TChartFilter.TBranchCut.bcNone)
			{
				TGenEngine.InitExtCounts(this.FTree, 0);
				TAncestryChartBox._DoFilter_DoDescendantsFilter(this, aRoot);
				aRoot.ExtData = true;
			}
		}

		public TGenEngine.TChartPersonSigns GetPersonSign(TGEDCOMIndividualRecord iRec)
		{
			TGenEngine.TChartPersonSigns Result = (TGenEngine.TChartPersonSigns)0;
			int num = iRec.GetUserReferencesCount() - 1;
			int i = 0;
			if (num >= i)
			{
				num++;
				do
				{
					string rs = iRec.GetUserReference(i).StringValue;
					TGenEngine.TChartPersonSign cps = TGenEngine.TChartPersonSign.urRI_StGeorgeCross;
					do
					{
						if ((rs == TGenEngine.UserRefs[(int)cps].Name) && cps < (TGenEngine.TChartPersonSign)8)
						{
							Result = (TGenEngine.TChartPersonSigns)((int)Result | 1 << (int)cps);
						}
						cps++;
					}
					while (cps != (TGenEngine.TChartPersonSign)5);
					i++;
				}
				while (i != num);
			}
			return Result;
		}

		public void RebuildKinships()
		{
			if (this.FOptions.Kinship)
			{
				TPerson p = this.FSelected;
				if (p != null)
				{
					this.FKinRoot = p;
					this.RecalcChart();
					base.ScrollRange();
				}
			}
		}

		public void SaveSnapshot([In] string aFileName)
		{
			string ext = Path.GetExtension(aFileName).ToLower();

			if ((ext == ".bmp" || ext == ".jpg") && this.FImageWidth >= 65535)
			{
				TGKSys.ShowError(GKL.LSList[380]);
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
					TObjectHelper.Free(pic);
				}
			}
		}

		internal void SetSelected([In] TPerson Value)
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
			int i = 0;
			if (num >= i)
			{
				num++;
				TPerson p;
				while (true)
				{
					p = this.FPersons[i];
					if (p.Rect.Contains(aX, aY))
					{
						break;
					}
					i++;
					if (i == num)
					{
						goto IL_5C;
					}
				}
				this.SetSelected(p);
				return;
			}
			IL_5C:
			this.SetSelected(null);
		}

		public void SelectByRec(TGEDCOMIndividualRecord iRec)
		{
			int num = this.FPersons.Count - 1;
			int i = 0;
			if (num >= i)
			{
				num++;
				TPerson p;
				while (true)
				{
					p = this.FPersons[i];
					if (object.Equals(p.Rec, iRec))
					{
						break;
					}
					i++;
					if (i == num)
					{
						goto IL_44;
					}
				}
				this.SetSelected(p);
				return;
			}
			IL_44:
			this.SetSelected(null);
		}

		private static bool _DoFilter_DoDescendantsFilter([In] TAncestryChartBox Self, TGEDCOMIndividualRecord aPerson)
		{
			bool Result = false;
			if (aPerson != null)
			{
				TChartFilter.TBranchCut branchCut = Self.FFilter.BranchCut;
				if (branchCut != TChartFilter.TBranchCut.bcYears)
				{
					if (branchCut == TChartFilter.TBranchCut.bcPersons)
					{
						Result = (BDSSystem.Pos(aPerson.XRef + ";", Self.FFilter.BranchPersons) > 0);
					}
				}
				else
				{
					int year = TGenEngine.GetIndependentYear(aPerson, "BIRT");
					Result = (year >= Self.FFilter.BranchYear);
				}

				int num = aPerson.SpouseToFamilyLinksCount - 1;
				int i = 0;
				if (num >= i)
				{
					num++;
					do
					{
						TGEDCOMFamilyRecord family = aPerson.GetSpouseToFamilyLink(i).Family;

						int num2 = family.ChildrenCount - 1;
						int j = 0;
						if (num2 >= j)
						{
							num2++;
							do
							{
								TGEDCOMIndividualRecord child = family.GetChildren(j).Value as TGEDCOMIndividualRecord;
								bool res_child = TAncestryChartBox._DoFilter_DoDescendantsFilter(Self, child);
								Result |= res_child;
								j++;
							}
							while (j != num2);
						}
						i++;
					}
					while (i != num);
				}
				aPerson.ExtData = Result;
			}
			return Result;
		}
	}
}
