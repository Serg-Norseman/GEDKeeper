
using GKCommon;
using GKCommon.GEDCOM;
using GKCommon.GEDCOM.Enums;
using GKCommon.Graph;
using GKCore.Interfaces;
using GKCore.Types;

namespace GKCore
{
	public class BaseContext : IBaseContext
	{
		#region Private fields
		
		private readonly GEDCOMTree fTree;
		private readonly IBase fViewer;
		
		#endregion
		
		#region Public properties
		
		public GEDCOMTree Tree
		{
			get { return this.fTree; }
		}
		
		#endregion
		
		#region Instance control
		
		public BaseContext(GEDCOMTree tree, IBase viewer)
		{
			this.fTree = tree;
			this.fViewer = viewer;
		}
		
		#endregion

		#region Data search

		public GEDCOMSourceRecord aux_FindSource(string sourceName)
		{
			GEDCOMSourceRecord result = null;
			int num = this.fTree.RecordsCount - 1;
			for (int i = 0; i <= num; i++)
			{
				GEDCOMRecord rec = this.fTree[i];

				if (rec is GEDCOMSourceRecord && (rec as GEDCOMSourceRecord).FiledByEntry == sourceName)
				{
					result = (rec as GEDCOMSourceRecord);
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
					GEDCOMRecord rec = this.fTree[i];
					if (rec is GEDCOMSourceRecord)
					{
						aSources.AddObject((rec as GEDCOMSourceRecord).FiledByEntry, rec);
					}
				}
			}
		}
		
		#endregion

		#region Data Manipulation

		public GEDCOMCustomEvent CreateEventEx(GEDCOMRecord aRec, string evSign, string evDate, string evPlace)
		{
			if (aRec == null) return null;

			GEDCOMCustomEvent result;

			if (aRec is GEDCOMIndividualRecord)
			{
				GEDCOMIndividualRecord ind_rec = aRec as GEDCOMIndividualRecord;
				if (GKUtils.GetPersonEventKindBySign(evSign) == PersonEventKind.ekEvent)
				{
					result = new GEDCOMIndividualEvent(this.fTree, ind_rec, "", "");
				}
				else
				{
					result = new GEDCOMIndividualAttribute(this.fTree, ind_rec, "", "");
				}
				ind_rec.AddIndividualEvent(result);
			}
			else if (aRec is GEDCOMFamilyRecord)
			{
				GEDCOMFamilyRecord fam_rec = aRec as GEDCOMFamilyRecord;
				result = new GEDCOMFamilyEvent(this.fTree, fam_rec, "", "");
				fam_rec.FamilyEvents.Add(result as GEDCOMFamilyEvent);
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

		public GEDCOMIndividualRecord CreatePersonEx(string iName, string iPatronymic, string iSurname, GEDCOMSex iSex, bool birthEvent)
		{
			GEDCOMIndividualRecord iRec = this.fTree.aux_CreateIndividual(iName, iPatronymic, iSurname, iSex);
			if (birthEvent) this.CreateEventEx(iRec, "BIRT", "", "");
			return iRec;
		}

		#endregion
		
		#region Individual utils

		public bool IsChildless(GEDCOMIndividualRecord iRec)
		{
			string exp = GKUtils.GetLifeExpectancy(iRec);
			return (exp != "" && exp != "?" && int.Parse(exp) < 15);
		}

		public int FindBirthYear(GEDCOMIndividualRecord iRec)
		{
			if (iRec != null) {
				int year = GKUtils.GetIndependentYear(iRec, "BIRT");
				if (year > 0) {
					return year;
				}

				int num = iRec.SpouseToFamilyLinks.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					GEDCOMFamilyRecord family = iRec.SpouseToFamilyLinks[i].Family;

					int num2 = family.Childrens.Count - 1;
					for (int j = 0; j <= num2; j++)
					{
						GEDCOMIndividualRecord child = family.Childrens[j].Value as GEDCOMIndividualRecord;
						year = FindBirthYear(child);
						if (year > 0) {
							return (year - 20);
						}
					}
				}
			}

			return -1;
		}

		public int FindDeathYear(GEDCOMIndividualRecord iRec)
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
					GEDCOMFamilyRecord family = iRec.SpouseToFamilyLinks[i].Family;

					int num2 = family.Childrens.Count - 1;
					for (int j = 0; j <= num2; j++)
					{
						GEDCOMIndividualRecord child = family.Childrens[j].Value as GEDCOMIndividualRecord;
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
			return (item1 as PatriarchObj).BirthYear - (item2 as PatriarchObj).BirthYear;
		}

		public void GetPatriarchsList(ExtList<PatriarchObj> patList, int gensMin, bool datesCheck)
		{
			GEDCOMTree tree = this.fTree;
			IProgressController pctl = this.fViewer as IProgressController;
			
			pctl.ProgressInit(LangMan.LS(LSID.LSID_PatSearch), tree.RecordsCount);

			TreeStats.InitExtCounts(tree, -1);
			try
			{
				int num = tree.RecordsCount - 1;
				for (int i = 0; i <= num; i++)
				{
					GEDCOMRecord rec = tree[i];

					if (rec is GEDCOMIndividualRecord) {
						GEDCOMIndividualRecord i_rec = rec as GEDCOMIndividualRecord;

						string nf, nn, np;
						i_rec.GetNameParts(out nf, out nn, out np);

						int bYear = this.FindBirthYear(i_rec);
						int descGens = TreeStats.GetDescGenerations(i_rec);

						bool res = (i_rec.ChildToFamilyLinks.Count == 0);
						res = (res && i_rec.Sex == GEDCOMSex.svMale);
						res = (res && /*nf != "" && nf != "?" &&*/ nn != "" && nn != "?");
						res = (res && descGens >= gensMin);

						if (datesCheck) {
							res = (res && bYear > 0);
						}

						if (res) {
							PatriarchObj pObj = new PatriarchObj();
							pObj.IRec = i_rec;
							pObj.BirthYear = bYear;
							pObj.DescendantsCount = TreeStats.GetDescendantsCount(i_rec) - 1;
							pObj.DescGenerations = descGens;
							patList.Add(pObj);
						}
					}

					pctl.ProgressStep();
				}

				patList.QuickSort(PatriarchsCompare);
			}
			finally
			{
				pctl.ProgressDone();
			}
		}

		public void GetPatriarchsLinks(ExtList<PatriarchObj> patList, int gensMin, bool datesCheck, bool loneSuppress)
		{
			GetPatriarchsList(patList, gensMin, datesCheck);

			IProgressController pctl = this.fViewer as IProgressController;

			pctl.ProgressInit(LangMan.LS(LSID.LSID_LinksSearch), patList.Count);
			try
			{
				int num2 = patList.Count - 1;
				for (int i = 0; i <= num2; i++)
				{
					PatriarchObj patr = patList[i] as PatriarchObj;

					for (int j = i + 1; j <= num2; j++)
					{
						PatriarchObj patr2 = patList[j] as PatriarchObj;

						GEDCOMIndividualRecord cross;
						bool res = TreeTools.PL_SearchDesc(patr.IRec, patr2.IRec, out cross);

						if (res) {
							patr.HasLinks = true;
							patr2.HasLinks = true;

							if (cross.Sex == GEDCOMSex.svFemale) {
								patr.Links.Add(patr2);
							} else {
								patr2.Links.Add(patr);
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
					PatriarchObj patr = patList[i] as PatriarchObj;
					if (!patr.HasLinks) patList.Delete(i);
				}
				patList.Pack();
			}
		}

		private static void PL_WalkDescLinks(TGraph graph, PatriarchsGraphNode prevNode, GEDCOMIndividualRecord ancestor)
		{
			for (int i = 0, count = ancestor.SpouseToFamilyLinks.Count; i < count; i++)
			{
				GEDCOMFamilyRecord family = ancestor.SpouseToFamilyLinks[i].Family;
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
					GEDCOMIndividualRecord child = family.Childrens[k].Value as GEDCOMIndividualRecord;
					PL_WalkDescLinks(graph, prevNode, child);
				}
			}
		}

		public TGraph GetPatriarchsGraph(int gensMin, bool datesCheck, bool loneSuppress = true)
		{
			TGraph graph = new TGraph();

			using (ExtList<PatriarchObj> patList = new ExtList<PatriarchObj>(true))
			{
				GEDCOMTree tree = this.fTree;
				IProgressController pctl = this.fViewer as IProgressController;

				this.GetPatriarchsList(patList, gensMin, datesCheck);

				// init
				TreeStats.InitExtData(tree);

				// prepare
				for (int i = 0, count = patList.Count; i < count; i++) {
					PatriarchObj patNode = patList[i] as PatriarchObj;
					GEDCOMIndividualRecord iRec = patNode.IRec;

					for (int k = 0, count2 = iRec.SpouseToFamilyLinks.Count; k < count2; k++) {
						GEDCOMFamilyRecord family = iRec.SpouseToFamilyLinks[k].Family;
						family.ExtData = new PatriarchsGraphNode(family.XRef, NodeType.ntPatriarch);
					}
				}

				pctl.ProgressInit(LangMan.LS(LSID.LSID_LinksSearch), patList.Count);
				try
				{
					int num2 = patList.Count - 1;
					for (int i = 0; i <= num2; i++)
					{
						PatriarchObj patr = patList[i] as PatriarchObj;

						for (int j = i + 1; j <= num2; j++)
						{
							PatriarchObj patr2 = patList[j] as PatriarchObj;

							GEDCOMFamilyRecord cross = TreeTools.PL_SearchIntersection(patr.IRec, patr2.IRec);

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
					PatriarchObj patNode = patList[i] as PatriarchObj;
					PL_WalkDescLinks(graph, null, patNode.IRec);
				}

				// clear
				TreeStats.InitExtData(tree);

				/*if (gpl_params.aLoneSuppress) {
				for (int i = aList.Count - 1; i >= 0; i--) {
					PatriarchObj patr = aList[i] as PatriarchObj;
					if (patr.ILinks.Count == 0) aList.Delete(i);
				}
				aList.Pack();*/
			}

			return graph;
		}
		
		#endregion
	}
}
