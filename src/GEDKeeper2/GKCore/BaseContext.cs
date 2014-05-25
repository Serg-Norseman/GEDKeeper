using System;

using ExtUtils;
using ExtUtils.Graph;
using GedCom551;
using GKCore.Interfaces;

namespace GKCore
{
	public class BaseContext : IBaseContext
	{
		#region Private fields
		
		private readonly TGEDCOMTree fTree;
		private readonly IBase fViewer;
		
		#endregion
		
		#region Public properties
		
		public TGEDCOMTree Tree
		{
			get { return this.fTree; }
		}
		
		#endregion
		
		#region Instance control
		
		public BaseContext(TGEDCOMTree tree, IBase viewer)
		{
			this.fTree = tree;
			this.fViewer = viewer;
		}
		
		#endregion

		#region Data search

		public TGEDCOMSourceRecord aux_FindSource(string sourceName)
		{
			TGEDCOMSourceRecord result = null;
			int num = this.fTree.RecordsCount - 1;
			for (int i = 0; i <= num; i++)
			{
				TGEDCOMRecord rec = this.fTree[i];

				if (rec is TGEDCOMSourceRecord && (rec as TGEDCOMSourceRecord).FiledByEntry == sourceName)
				{
					result = (rec as TGEDCOMSourceRecord);
					break;
				}
			}
			return result;
		}

		public void aux_GetSourcesList(StringList aSources)
		{
            if (aSources != null)
			{
				aSources.Clear();
				int num = this.fTree.RecordsCount - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMRecord rec = this.fTree[i];
					if (rec is TGEDCOMSourceRecord)
					{
						aSources.AddObject((rec as TGEDCOMSourceRecord).FiledByEntry, rec);
					}
				}
			}
		}
		
		#endregion

		#region Data Manipulation

		public TGEDCOMCustomEvent CreateEventEx(TGEDCOMRecord aRec, string evSign, string evDate, string evPlace)
		{
			if (aRec == null) return null;

			TGEDCOMCustomEvent result;

			if (aRec is TGEDCOMIndividualRecord)
			{
				TGEDCOMIndividualRecord ind_rec = aRec as TGEDCOMIndividualRecord;
				if (GKUtils.GetPersonEventKindBySign(evSign) == TPersonEventKind.ekEvent)
				{
					result = new TGEDCOMIndividualEvent(this.fTree, ind_rec, "", "");
				}
				else
				{
					result = new TGEDCOMIndividualAttribute(this.fTree, ind_rec, "", "");
				}
				ind_rec.AddIndividualEvent(result);
			}
			else if (aRec is TGEDCOMFamilyRecord)
			{
				TGEDCOMFamilyRecord fam_rec = aRec as TGEDCOMFamilyRecord;
				result = new TGEDCOMFamilyEvent(this.fTree, fam_rec, "", "");
				fam_rec.FamilyEvents.Add(result as TGEDCOMFamilyEvent);
			} 
			else
			{
				return null;
			}

			result.Name = evSign;

			if (evDate != "") {
				result.Detail.Date.ParseString(evDate);
			}

			if (evPlace != "") {
				result.Detail.Place.StringValue = evPlace;
			}

			return result;
		}

		public TGEDCOMIndividualRecord CreatePersonEx(string iName, string iPatronymic, string iSurname, TGEDCOMSex iSex, bool birthEvent)
		{
			TGEDCOMIndividualRecord iRec = this.fTree.aux_CreateIndividual(iName, iPatronymic, iSurname, iSex);
			if (birthEvent) this.CreateEventEx(iRec, "BIRT", "", "");
			return iRec;
		}

		#endregion
		
		#region Individual utils

		public bool IsChildless(TGEDCOMIndividualRecord iRec)
		{
			string exp = GKUtils.GetLifeExpectancy(iRec);
			return (exp != "" && exp != "?" && int.Parse(exp) < 15);
		}

		public int FindBirthYear(TGEDCOMIndividualRecord iRec)
		{
			if (iRec != null) {
				int year = GKUtils.GetIndependentYear(iRec, "BIRT");
				if (year > 0) {
					return year;
				}

				int num = iRec.SpouseToFamilyLinks.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMFamilyRecord family = iRec.SpouseToFamilyLinks[i].Family;

					int num2 = family.Childrens.Count - 1;
					for (int j = 0; j <= num2; j++)
					{
						TGEDCOMIndividualRecord child = family.Childrens[j].Value as TGEDCOMIndividualRecord;
						year = FindBirthYear(child);
						if (year > 0) {
							return (year - 20);
						}
					}
				}
			}

			return -1;
		}

		public int FindDeathYear(TGEDCOMIndividualRecord iRec)
		{
			if (iRec != null) {
				int year = GKUtils.GetIndependentYear(iRec, "DEAT");
				if (year > 0) {
					return year;
				}

				int max = 0;
				int num = iRec.SpouseToFamilyLinks.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMFamilyRecord family = iRec.SpouseToFamilyLinks[i].Family;

					int num2 = family.Childrens.Count - 1;
					for (int j = 0; j <= num2; j++)
					{
						TGEDCOMIndividualRecord child = family.Childrens[j].Value as TGEDCOMIndividualRecord;
						int y = FindBirthYear(child);
						if (y > 0) {
							if (max < y) max = y;
						}
					}
				}

				if (max > 0) {
					return max + 1;
				}
			}

			return -1;
		}

		#endregion
		
		#region Patriarchs Search

		private static int PatriarchsCompare(object item1, object item2)
		{
			return (item1 as TPatriarchObj).IBirthYear - (item2 as TPatriarchObj).IBirthYear;
		}

		public void GetPatriarchsList(ExtList patList, int gensMin, bool datesCheck)
		{
			TGEDCOMTree tree = this.fTree;
			IProgressController pctl = this.fViewer as IProgressController;
			
			pctl.ProgressInit(LangMan.LS(LSID.LSID_PatSearch), tree.RecordsCount);

			TreeStats.InitExtCounts(tree, -1);
			try
			{
				int num = tree.RecordsCount - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMRecord rec = tree[i];

					if (rec is TGEDCOMIndividualRecord) {
						TGEDCOMIndividualRecord i_rec = rec as TGEDCOMIndividualRecord;

						string nf, nn, np;
						i_rec.aux_GetNameParts(out nf, out nn, out np);

						int bYear = this.FindBirthYear(i_rec);
						int descGens = TreeStats.GetDescGenerations(i_rec);

						bool res = (i_rec.ChildToFamilyLinks.Count == 0);
						res = (res && i_rec.Sex == TGEDCOMSex.svMale);
						res = (res && /*nf != "" && nf != "?" &&*/ nn != "" && nn != "?");
						res = (res && descGens >= gensMin);

						if (datesCheck) {
							res = (res && bYear > 0);
						}

						if (res) {
							TPatriarchObj pObj = new TPatriarchObj();
							pObj.IRec = i_rec;
							pObj.IBirthYear = bYear;
							pObj.IDescendantsCount = TreeStats.GetDescendantsCount(i_rec) - 1;
							pObj.IDescGenerations = descGens;
							patList.Add(pObj);
						}
					}

					pctl.ProgressStep();
				}

				patList.Sort(PatriarchsCompare);
			}
			finally
			{
				pctl.ProgressDone();
			}
		}

		public void GetPatriarchsLinks(ExtList patList, int gensMin, bool datesCheck, bool loneSuppress)
		{
			GetPatriarchsList(patList, gensMin, datesCheck);

			IProgressController pctl = this.fViewer as IProgressController;

			pctl.ProgressInit(LangMan.LS(LSID.LSID_LinksSearch), patList.Count);
			try
			{
				int num2 = patList.Count - 1;
				for (int i = 0; i <= num2; i++)
				{
					TPatriarchObj patr = patList[i] as TPatriarchObj;

					for (int j = i + 1; j <= num2; j++)
					{
						TPatriarchObj patr2 = patList[j] as TPatriarchObj;

						TGEDCOMIndividualRecord cross;
						bool res = TreeTools.PL_SearchDesc(patr.IRec, patr2.IRec, out cross);

						if (res) {
							patr.HasLinks = true;
							patr2.HasLinks = true;

							if (cross.Sex == TGEDCOMSex.svFemale) {
								patr.ILinks.Add(patr2);
							} else {
								patr2.ILinks.Add(patr);
							}
						}
					}

					pctl.ProgressStep();
				}
			}
			finally
			{
				pctl.ProgressDone();
			}

			if (loneSuppress)
			{
				for (int i = patList.Count - 1; i >= 0; i--)
				{
					TPatriarchObj patr = patList[i] as TPatriarchObj;
					if (!patr.HasLinks) patList.Delete(i);
				}
				patList.Pack();
			}
		}

		private static void PL_WalkDescLinks(TGraph graph, PatriarchsGraphNode prevNode, TGEDCOMIndividualRecord ancestor)
		{
			for (int i = 0, count = ancestor.SpouseToFamilyLinks.Count; i < count; i++)
			{
				TGEDCOMFamilyRecord family = ancestor.SpouseToFamilyLinks[i].Family;
				PatriarchsGraphNode node = family.ExtData as PatriarchsGraphNode;

				if (node != null && node.Type != NodeType.ntDefault) {
					IVertex vtx = graph.FindVertex(node.FamilyXRef);
					if (vtx == null) {
						vtx = graph.AddVertex(node.FamilyXRef, node);
					}

					if (prevNode != null) {
						graph.AddDirectedEdge(prevNode.FamilyXRef, node.FamilyXRef, 1, null);
					}

					prevNode = node;
				}

				for (int k = 0, count2 = family.Childrens.Count; k < count2; k++)
				{
					TGEDCOMIndividualRecord child = family.Childrens[k].Value as TGEDCOMIndividualRecord;
					PL_WalkDescLinks(graph, prevNode, child);
				}
			}
		}

		public TGraph GetPatriarchsGraph(int gensMin, bool datesCheck, bool loneSuppress = true)
		{
			TGraph graph = new TGraph();

			using (ExtList patList = new ExtList(true))
			{
				TGEDCOMTree tree = this.fTree;
				IProgressController pctl = this.fViewer as IProgressController;

				this.GetPatriarchsList(patList, gensMin, datesCheck);

				// init
				TreeStats.InitExtData(tree);

				// prepare
				for (int i = 0, count = patList.Count; i < count; i++) {
					TPatriarchObj patNode = patList[i] as TPatriarchObj;
					TGEDCOMIndividualRecord iRec = patNode.IRec;

					for (int k = 0, count2 = iRec.SpouseToFamilyLinks.Count; k < count2; k++) {
						TGEDCOMFamilyRecord family = iRec.SpouseToFamilyLinks[k].Family;
						family.ExtData = new PatriarchsGraphNode(family.XRef, NodeType.ntPatriarch);
					}
				}

				pctl.ProgressInit(LangMan.LS(LSID.LSID_LinksSearch), patList.Count);
				try
				{
					int num2 = patList.Count - 1;
					for (int i = 0; i <= num2; i++)
					{
						TPatriarchObj patr = patList[i] as TPatriarchObj;

						for (int j = i + 1; j <= num2; j++)
						{
							TPatriarchObj patr2 = patList[j] as TPatriarchObj;

							TGEDCOMFamilyRecord cross = TreeTools.PL_SearchIntersection(patr.IRec, patr2.IRec);

							if (cross != null)
							{
								PatriarchsGraphNode node = cross.ExtData as PatriarchsGraphNode;

								if (node != null && node.Type == NodeType.ntPatriarch) {
									// dummy
								} else {
									cross.ExtData = new PatriarchsGraphNode(cross.XRef, NodeType.ntIntersection);
								}
							}
						}

						pctl.ProgressStep();
					}
				}
				finally
				{
					pctl.ProgressDone();
				}

				// create graph
				for (int i = 0, count = patList.Count; i < count; i++) {
					TPatriarchObj patNode = patList[i] as TPatriarchObj;
					PL_WalkDescLinks(graph, null, patNode.IRec);
				}

				// clear
				TreeStats.InitExtData(tree);

				/*if (gpl_params.aLoneSuppress) {
				for (int i = aList.Count - 1; i >= 0; i--) {
					TPatriarchObj patr = aList[i] as TPatriarchObj;
					if (patr.ILinks.Count == 0) aList.Delete(i);
				}
				aList.Pack();*/
			}

			return graph;
		}
		
		#endregion
	}
}
