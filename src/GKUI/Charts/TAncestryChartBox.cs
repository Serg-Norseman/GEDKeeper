using System;
using System.Collections.Generic;
using System.Drawing;
using System.Drawing.Imaging;
using System.IO;
using System.Runtime.InteropServices;

using GedCom551;
using GKCore;
using GKCore.Sys;
using GKUI.Lists;

namespace GKUI.Charts
{
	public class TAncestryChartBox : TCustomChartBox
	{
		private class TRelLink
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

		private TChartKind FKind;
		private static readonly string[] SignsData;
		private int FBranchDistance;
		private int FDepthLimit;
		private int FHMax;
		private int FWMax;
		private new TChartFilter FFilter;
		private TGraph FGraph;
		private TPerson FKinRoot;
		private int FLevelDistance;
		private int FMargin;
		private bool FPathDebug;
		private TPersonList FPersons;
		private TPerson FRoot;
		private int FScale;
		private int FSpouseDistance;
		private TPerson FSelected;

		public Bitmap[] SignsPic = new Bitmap[4];

		private List<string> FPreparedFamilies = new List<string>();

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

		private TPerson AddDescPerson(TPerson aParent, TGEDCOMIndividualRecord iRec, TPerson.TPersonKind aKind, int aGeneration)
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

		private TPerson DoAncestorsStep(TPerson aChild, TGEDCOMIndividualRecord aPerson, int aGeneration, bool dup_flag)
		{
			TPerson Result = null;

			if (aPerson != null)
			{
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

		private TPerson DoDescendantsStep(TPerson aParent, TGEDCOMIndividualRecord aPerson, int aLevel)
		{
			TPerson Result = null;
			if (aPerson != null && (!this.FOptions.ChildlessExclude || aLevel <= 1 || aPerson.SpouseToFamilyLinks.Count != 0 || !this.IsChildless(aPerson)))
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

				TPerson res = this.AddDescPerson(aParent, aPerson, TPerson.TPersonKind.pkDefault, aLevel);
				Result = res;

				int num = aPerson.SpouseToFamilyLinks.Count - 1;
				int i = 0;
				if (num >= i)
				{
					num++;
					do
					{
						TGEDCOMFamilyRecord family = aPerson.SpouseToFamilyLinks[i].Family;
						if (TGenEngine.IsRecordAccess(family.Restriction, this.FShieldState))
						{
							TPerson res_parent = null;
							TGEDCOMSex sex = aPerson.Sex;
							TPerson ft = null;
							TPerson mt = null;
							TPerson.TPersonFlag desc_flag = TPerson.TPersonFlag.pfDescByFather;
							if (sex != TGEDCOMSex.svMale)
							{
								if (sex == TGEDCOMSex.svFemale)
								{
									TGEDCOMIndividualRecord sp = family.Husband.Value as TGEDCOMIndividualRecord;
									res_parent = this.AddDescPerson(null, sp, TPerson.TPersonKind.pkSpouse, aLevel);
									res_parent.Sex = TGEDCOMSex.svMale;
									ft = res_parent;
									mt = res;
									desc_flag = TPerson.TPersonFlag.pfDescByFather;
								}
							}
							else
							{
								TGEDCOMIndividualRecord sp = family.Wife.Value as TGEDCOMIndividualRecord;
								res_parent = this.AddDescPerson(null, sp, TPerson.TPersonKind.pkSpouse, aLevel);
								res_parent.Sex = TGEDCOMSex.svFemale;
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
								int num2 = family.Childrens.Count - 1;
								for (int j = 0; j <= num2; j++)
								{
									TGEDCOMIndividualRecord child_rec = family.Childrens[j].Value as TGEDCOMIndividualRecord;
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

		private string FindRelationship(TPerson aTarget)
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

				for (int i = path.Count - 1; i >= 0; i--)
				{
					TAncestryChartBox.TRelLink L = path[i] as TAncestryChartBox.TRelLink;
					TGenEngine.TRelationKind cur_rel = L.xRel;
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
					if (prev_rel != TGenEngine.TRelationKind.rkUndefined)
					{
						int g;
						int lev;
						fin_rel = this.FEngine.FindKinship(prev_rel, cur_rel, out g, out lev);
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
				path.Free();
			}
			return Result;
		}

		private void FixLink(TObjectList path, TPerson f, TPerson t, TGenEngine.TRelationKind rel)
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
						TGEDCOMSex sex = L.xTo.Sex;
						if (sex != TGEDCOMSex.svMale)
						{
							if (sex == TGEDCOMSex.svFemale)
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
					TGEDCOMSex sex2 = L.xTo.Sex;
					if (sex2 != TGEDCOMSex.svMale)
					{
						if (sex2 == TGEDCOMSex.svFemale)
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
				TGEDCOMSex sex3 = L.xTo.Sex;
				if (sex3 != TGEDCOMSex.svMale)
				{
					if (sex3 == TGEDCOMSex.svFemale)
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

		private string FixRelation(TPerson aTarget, TGenEngine.TRelationKind Rel, int Great)
		{
			string tmp = "";
			if (Great != 0)
			{
				if (Rel >= TGenEngine.TRelationKind.rkUncle && Rel < TGenEngine.TRelationKind.rkNephew)
				{
					tmp = TGenEngine.Numerals[Great] + TGenEngine.NumKinship[(int)aTarget.Sex] + " ";
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

		private string GetGreat(int n)
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
					TObjectHelper.Free(xpen);
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
			this.FMargin = (int)checked((long)Math.Round(40.0 * sc));
		}

		private void RecalcAncestorsChart()
		{
			int[] edges = new int[256];
			this.InitEdges(ref edges);
			TList prev = new TList();
			try
			{
				this.RecalcAnc(prev, ref edges, this.FRoot, new Point(this.FMargin, this.FMargin));
			}
			finally
			{
				prev.Free();
			}
		}

		private void RecalcDescendantsChart(bool aPreDef)
		{
			int[] edges = new int[256];
			this.InitEdges(ref edges);
			this.RecalcDesc(ref edges, this.FRoot, new Point(this.FMargin, this.FMargin), aPreDef);
		}

		private void RecalcChart()
		{
			if (this.FOptions.Kinship)
			{
				this.FGraph.FindPathTree(this.FKinRoot.Node);
				int num = this.FPersons.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					TPerson p = this.FPersons[i];
					p.Kinship = this.FindRelationship(p);
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

		private void ShiftAnc(ref int[] edges, TPerson aPerson, int aOffset)
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

		private void RecalcAnc(TList prev, ref int[] edges, TPerson aPerson, Point aPt)
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
					offset = 0 - aPerson.Rect.Top + this.FMargin;
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

		private void ShiftDesc(TPerson aPerson, int aOffset, bool aSingle)
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

		private void RecalcDescChilds(ref int[] edges, TPerson aPerson)
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
					TPerson child = aPerson.GetChild(i);
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

		private void RecalcDesc(ref int[] edges, TPerson aPerson, Point aPt, bool aPreDef)
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
				if (aPerson.Sex == TGEDCOMSex.svMale)
				{
					this.RecalcDescChilds(ref edges, aPerson);
					edges[gen] = aPerson.Rect.Right;
				}

				if (aPerson.SpousesCount > 0)
				{
					TPerson prev = aPerson;
					int num = aPerson.SpousesCount - 1;
					for (int i = 0; i <= num; i++)
					{
						TPerson sp = aPerson.GetSpouse(i);
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

		private void DrawAncestors(Graphics aCanvas, TPerson aPerson)
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

		private void DrawDescendants(Graphics aCanvas, TPerson aPerson)
		{
			int num = aPerson.ChildsCount - 1;
			for (int i = 0; i <= num; i++)
			{
				this.Draw(aCanvas, aPerson.GetChild(i), TAncestryChartBox.TChartKind.ckDescendants);
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
				this.Draw(aCanvas, aPerson.GetSpouse(i), TAncestryChartBox.TChartKind.ckDescendants);
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
		}

		protected void Draw(Graphics aCanvas, TPerson aPerson, TChartKind aDirKind)
		{
			if (aPerson != null)
			{
				switch (this.FKind) {
					case TAncestryChartBox.TChartKind.ckAncestors:
						this.DrawAncestors(aCanvas, aPerson);
						break;

					case TAncestryChartBox.TChartKind.ckDescendants:
						this.DrawDescendants(aCanvas, aPerson);
						break;

					case TAncestryChartBox.TChartKind.ckBoth:
						if (aPerson == this.FRoot || aDirKind == TAncestryChartBox.TChartKind.ckAncestors) this.DrawAncestors(aCanvas, aPerson);
						if (aPerson == this.FRoot || aDirKind == TAncestryChartBox.TChartKind.ckDescendants) this.DrawDescendants(aCanvas, aPerson);
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
				TAncestryChartBox.TChartKind fKind = this.FKind;

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

		public TEnumSet GetPersonSign(TGEDCOMIndividualRecord iRec)
		{
			TEnumSet result = new TEnumSet();
			int num = iRec.UserReferences.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				string rs = iRec.UserReferences[i].StringValue;
				TGenEngine.TChartPersonSign cps = TGenEngine.TChartPersonSign.urRI_StGeorgeCross;
				do
				{
					if (rs == TGenEngine.UserRefs[(int)cps].Name)
					{
						result.Include(cps);
					}
					cps++;
				}
				while (cps != (TGenEngine.TChartPersonSign)5);
			}
			return result;
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
				SysUtils.ShowError(GKL.LSList[380]);
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

		private void SetSelected([In] TPerson Value)
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
						Result = (SysUtils.Pos(aPerson.XRef + ";", Self.FFilter.BranchPersons) > 0);
					}
				}
				else
				{
					int year = TGenEngine.GetIndependentYear(aPerson, "BIRT");
					Result = (year >= Self.FFilter.BranchYear);
				}

				int num = aPerson.SpouseToFamilyLinks.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMFamilyRecord family = aPerson.SpouseToFamilyLinks[i].Family;

					int num2 = family.Childrens.Count - 1;
					for (int j = 0; j <= num2; j++)
					{
						TGEDCOMIndividualRecord child = family.Childrens[j].Value as TGEDCOMIndividualRecord;
						bool res_child = TAncestryChartBox._DoFilter_DoDescendantsFilter(Self, child);
						Result |= res_child;
					}
				}
				aPerson.ExtData = Result;
			}
			return Result;
		}
	}
}
