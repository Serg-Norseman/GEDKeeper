using System;
using System.Collections;
using System.Collections.Generic;
using System.Windows.Forms;

using GKCommon;
using GKCommon.GEDCOM;
using GKCommon.GEDCOM.Enums;
using GKCommon.Graph;
using GKCore.Interfaces;
using GKCore.Types;
using GKUI;

/// <summary>
/// Localization: dirty
/// </summary>

namespace GKCore
{
	public sealed class TPlaceObj : BaseObject
	{
		public string Name;
		public readonly ExtList<GEDCOMCustomEvent> Facts;

		public TPlaceObj()
		{
			this.Facts = new ExtList<GEDCOMCustomEvent>();
		}

		protected override void Dispose(bool disposing)
		{
			if (disposing)
			{
				this.Facts.Dispose();
			}
		}
	}

	public static class TreeTools
	{
		static TreeTools()
		{
		}

		#region Patriarchs Search

		public static bool PL_SearchAnc(GEDCOMIndividualRecord descendant, GEDCOMIndividualRecord searchRec, bool onlyMaleLine)
		{
			if (descendant == null) return false;

			bool res = (descendant == searchRec);

			if (!res && descendant.ChildToFamilyLinks.Count > 0)
			{
				GEDCOMFamilyRecord family = descendant.ChildToFamilyLinks[0].Family;

				GEDCOMIndividualRecord ancestor = family.Husband.Value as GEDCOMIndividualRecord;
				if (ancestor != null) {
					res = PL_SearchAnc(ancestor, searchRec, onlyMaleLine);
					if (res) return true;
				}

				/*if (!onlyMaleLine) {
					ancestor = family.Wife.Value as GEDCOMIndividualRecord;
					if (ancestor != null) {
						res = PL_SearchAnc2(ancestor, searchRec, onlyMaleLine);
						if (res) return true;
					}
				}*/
			}

			return res;
		}

		public static bool PL_SearchDesc(GEDCOMIndividualRecord ancestorRec, GEDCOMIndividualRecord searchRec, out GEDCOMIndividualRecord cross)
		{
			bool res = false;
			cross = null;

			int num = ancestorRec.SpouseToFamilyLinks.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				GEDCOMFamilyRecord family = ancestorRec.SpouseToFamilyLinks[i].Family;
				GEDCOMIndividualRecord spouse = family.aux_GetSpouse(ancestorRec);

				if (spouse != null)
				{
					res = TreeTools.PL_SearchAnc(spouse, searchRec, (ancestorRec.Sex == GEDCOMSex.svFemale));
					if (res) {
						cross = ancestorRec;
						return res;
					}
				}

				if (ancestorRec.Sex == GEDCOMSex.svMale) {
					int num2 = family.Childrens.Count - 1;
					for (int j = 0; j <= num2; j++)
					{
						GEDCOMIndividualRecord child = family.Childrens[j].Value as GEDCOMIndividualRecord;

						res = TreeTools.PL_SearchDesc(child, searchRec, out cross);
						if (res) return true;
					}
				}
			}

			return false;
		}

		public static GEDCOMFamilyRecord PL_SearchIntersection(GEDCOMIndividualRecord ancestor, GEDCOMIndividualRecord searchRec)
		{
			int num = ancestor.SpouseToFamilyLinks.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				GEDCOMFamilyRecord family = ancestor.SpouseToFamilyLinks[i].Family;
				GEDCOMIndividualRecord spouse = family.aux_GetSpouse(ancestor);

				if (spouse != null)
				{
					bool res = TreeTools.PL_SearchAnc(spouse, searchRec, (ancestor.Sex == GEDCOMSex.svFemale));
					if (res) return family;
				}

				if (ancestor.Sex == GEDCOMSex.svMale)
				{
					int num2 = family.Childrens.Count - 1;
					for (int j = 0; j <= num2; j++)
					{
						GEDCOMIndividualRecord child = family.Childrens[j].Value as GEDCOMIndividualRecord;

						GEDCOMFamilyRecord res = PL_SearchIntersection(child, searchRec);
						if (res != null) return res;
					}
				}
			}

			return null;
		}

		public static void GenPatriarchsGraphviz(IBase aBase, string outpath, int minGens, bool loneSuppress = true)
		{
            string[] options = { "ratio=auto" };
            GraphvizWriter GVW = new GraphvizWriter("Family Tree", options);

        	using (ExtList<PatriarchObj> patList = new ExtList<PatriarchObj>(false)) {
				aBase.Context.GetPatriarchsLinks(patList, minGens, false, loneSuppress);

				int num = patList.Count - 1;
				for (int i = 0; i <= num; i++) {
					PatriarchObj p_obj = patList[i] as PatriarchObj;

					if ((!loneSuppress) || (loneSuppress && p_obj.HasLinks)) {
						string color = (p_obj.IRec.Sex == GEDCOMSex.svFemale) ? "pink" : "blue";
						GVW.ListNode(p_obj.IRec.XRef, p_obj.IRec.aux_GetNameStr(true, false), "filled", color, "box");
					}
				}

				for (int i = 0; i <= num; i++) {
					PatriarchObj pat1 = patList[i] as PatriarchObj;

					for (int k = 0; k < pat1.Links.Count; k++) {
						PatriarchObj pat2 = pat1.Links[k];
						GVW.ConnNode(pat1.IRec.XRef, pat2.IRec.XRef);
					}
				}
			}

            GVW.SaveFile(outpath);
		}

		#endregion

		#region Tree Check

		private static void ReformNote(GEDCOMTree tree, GEDCOMNotes note)
		{
			StringList strData = new StringList();
			try
			{
				strData.Text = note.Notes.Text;
				GEDCOMNoteRecord noteRec = tree.aux_CreateNoteEx(null, strData);
				note.Clear();
				note.Value = noteRec;
			}
			finally
			{
                strData.Dispose();
			}
		}

		private static void ReformMultimediaLink(GEDCOMTree tree, GEDCOMMultimediaLink mmLink)
		{
			try
			{
				string title = mmLink.Title;
				GEDCOMMultimediaRecord mmRec = new GEDCOMMultimediaRecord(tree, tree, "", "");
				mmRec.InitNew();
				tree.AddRecord(mmRec);

				int num = mmLink.FileReferences.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					GEDCOMFileReference srcFileRef = mmLink.FileReferences[i];
					GEDCOMFileReferenceWithTitle tgtFileRef = new GEDCOMFileReferenceWithTitle(tree, mmRec, "", "");

					tgtFileRef.LinkFile(srcFileRef.StringValue);

					if (srcFileRef.MultimediaFormat != GEDCOMMultimediaFormat.mfNone)
					{
						tgtFileRef.MultimediaFormat = srcFileRef.MultimediaFormat;
					}
					if (srcFileRef.MediaType != GEDCOMMediaType.mtNone)
					{
						tgtFileRef.MediaType = srcFileRef.MediaType;
					}

					mmRec.FileReferences.Add(tgtFileRef);
				}

				mmLink.Clear();
				mmLink.Value = mmRec;
				mmLink.Title = title;
			}
			finally
			{
			}
		}

		private static void ReformSourceCitation(GEDCOMTree tree, GEDCOMSourceCitation sourCit)
		{
		}

		private static void CheckRecord_PrepareTag(GEDCOMTree tree, GEDCOMFormat format, GEDCOMTagWithLists tag)
		{
			int num = tag.MultimediaLinks.Count - 1;
			for (int i = 0; i <= num; i++) {
				GEDCOMMultimediaLink mmLink = tag.MultimediaLinks[i];
				if (!mmLink.IsPointer) ReformMultimediaLink(tree, mmLink);
			}

			num = tag.Notes.Count - 1;
			for (int i = 0; i <= num; i++) {
				GEDCOMNotes note = tag.Notes[i];
				if (!note.IsPointer) ReformNote(tree, note);
			}

			num = tag.SourceCitations.Count - 1;
			for (int i = 0; i <= num; i++) {
				GEDCOMSourceCitation sourCit = tag.SourceCitations[i];
				if (!sourCit.IsPointer) ReformSourceCitation(tree, sourCit);
			}
		}

		private static void CheckRecord_RepairTag(GEDCOMTree tree, GEDCOMFormat format, GEDCOMTagWithLists tag)
		{
			int num = tag.MultimediaLinks.Count - 1;
			for (int i = num; i >= 0; i--) {
				GEDCOMMultimediaLink mmLink = tag.MultimediaLinks[i];
				if (mmLink.IsPointer && mmLink.Value == null) tag.MultimediaLinks.Delete(i);
			}

			num = tag.Notes.Count - 1;
			for (int i = num; i >= 0; i--) {
				GEDCOMNotes note = tag.Notes[i];
				if (note.IsPointer && note.Value == null) tag.Notes.Delete(i);
			}

			num = tag.SourceCitations.Count - 1;
			for (int i = num; i >= 0; i--) {
				GEDCOMSourceCitation sourCit = tag.SourceCitations[i];
				if (sourCit.IsPointer && sourCit.Value == null) tag.SourceCitations.Delete(i);
			}
		}

		private static void CheckRecord_PreparePtr(GEDCOMTree tree, GEDCOMFormat format, GEDCOMPointerWithNotes ptr)
		{
			/*GEDCOMRecord val = ptr.Value;
			if (!string.IsNullOrEmpty(ptr.XRef) && val == null) {
				ptr.Value = null;
			}*/

			int num = ptr.Notes.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				GEDCOMNotes note = ptr.Notes[i];
				if (!note.IsPointer) ReformNote(tree, note);
			}
		}

		private static void CheckRecord_EventPlace(GEDCOMCustomEvent aEvent)
		{
			GEDCOMPlace place = aEvent.Detail.Place;
			if (place.Location.XRef != "" && place.Location.Value == null)
			{
				place.Location.XRef = "";
			}
			if (place.StringValue != "")
			{
				GEDCOMLocationRecord loc = place.Location.Value as GEDCOMLocationRecord;
				if (loc != null && place.StringValue != loc.LocationName)
				{
					place.StringValue = loc.LocationName;
				}
			}
		}

		private static void AddUserRef(GEDCOMTree tree, GEDCOMRecord record, string reference)
		{
            GEDCOMUserReference uRef = new GEDCOMUserReference(tree, record, "", "");
			uRef.StringValue = reference;
            record.UserReferences.Add(uRef);
		}

		private static void CheckRecord_AttrCompatible(GEDCOMTree tree, GEDCOMFormat format, GEDCOMIndividualRecord iRec, GEDCOMCustomEvent aEvent)
		{
			if (aEvent.Name == "_MILI")
			{
				string cause = aEvent.Detail.Classification.ToLower();

				if (cause.IndexOf("б/д") >= 0)
				{
					if (cause.IndexOf("+") >= 0)
					{
						AddUserRef(tree, iRec, GKData.UserRefs[3]);
					}
					else
					{
						AddUserRef(tree, iRec, GKData.UserRefs[2]);
					}

					aEvent.Detail.Classification = "";
				}
				else
				{
					if (cause.IndexOf("т/т") >= 0)
					{
						AddUserRef(tree, iRec, GKData.UserRefs[4]);
						aEvent.Detail.Classification = "";
					}
				}
			}
		}

		private static void CheckRecord_URefCompatible(GEDCOMIndividualRecord iRec, GEDCOMUserReference userRef)
		{
		}

		private static void CheckRecord_Individual(GEDCOMTree tree, GEDCOMFormat format, GEDCOMIndividualRecord iRec, ValuesCollection valuesCollection)
		{
			int i;
			if (format == GEDCOMFormat.gf_Native)
			{
				int num = iRec.IndividualEvents.Count - 1;
				for (i = 0; i <= num; i++)
				{
					GEDCOMCustomEvent evt = iRec.IndividualEvents[i];
					CheckRecord_EventPlace(evt);
					CheckRecord_AttrCompatible(tree, format, iRec, evt);
					CheckRecord_RepairTag(tree, format, evt.Detail);
					
					if (valuesCollection != null) {
						string evName = evt.Name;
						string evVal = evt.StringValue;
						valuesCollection.Add(evName, evVal, true);
					}
				}

				int num2 = iRec.UserReferences.Count - 1;
				for (i = 0; i <= num2; i++)
				{
					CheckRecord_URefCompatible(iRec, iRec.UserReferences[i]);
				}
			}
			else
			{
				int num3 = iRec.IndividualEvents.Count - 1;
				for (i = 0; i <= num3; i++)
				{
					CheckRecord_PrepareTag(tree, format, iRec.IndividualEvents[i].Detail);
				}

				int num4 = iRec.ChildToFamilyLinks.Count - 1;
				for (i = 0; i <= num4; i++)
				{
					CheckRecord_PreparePtr(tree, format, iRec.ChildToFamilyLinks[i]);
				}

				int num5 = iRec.SpouseToFamilyLinks.Count - 1;
				for (i = 0; i <= num5; i++)
				{
					CheckRecord_PreparePtr(tree, format, iRec.SpouseToFamilyLinks[i]);
				}

				int num6 = iRec.Associations.Count - 1;
				for (i = 0; i <= num6; i++)
				{
					CheckRecord_PreparePtr(tree, format, iRec.Associations[i]);
				}
			}

			for (i = iRec.ChildToFamilyLinks.Count - 1; i >= 0; i--)
			{
				if (iRec.ChildToFamilyLinks[i].Family == null)
					iRec.ChildToFamilyLinks.Delete(i);
			}

			for (i = iRec.SpouseToFamilyLinks.Count - 1; i >= 0; i--)
			{
				if (iRec.SpouseToFamilyLinks[i].Family == null)
					iRec.SpouseToFamilyLinks.Delete(i);
			}
			
			TfmGEDKeeper.Instance.NamesTable.ImportNames(iRec);
		}

		private static void CheckRecord_Family(GEDCOMTree tree, GEDCOMFormat format, GEDCOMFamilyRecord fam, ValuesCollection valuesCollection)
		{
			int i;
			if (format == GEDCOMFormat.gf_Native)
			{
				int num = fam.FamilyEvents.Count - 1;
				for (i = 0; i <= num; i++)
				{
					TreeTools.CheckRecord_EventPlace(fam.FamilyEvents[i]);
				}
			}
			else
			{
				int num2 = fam.FamilyEvents.Count - 1;
				for (i = 0; i <= num2; i++)
				{
					TreeTools.CheckRecord_PrepareTag(tree, format, fam.FamilyEvents[i].Detail);
				}
			}

			for (i = fam.Childrens.Count - 1; i >= 0; i--)
			{
				if (fam.Childrens[i].Value == null)
					fam.Childrens.Delete(i);
			}

			GEDCOMRecord val = fam.Husband.Value;
			if (!string.IsNullOrEmpty(fam.Husband.XRef) && val == null) {
				fam.Husband.Value = null;
			}
			
			val = fam.Wife.Value;
			if (!string.IsNullOrEmpty(fam.Wife.XRef) && val == null) {
				fam.Wife.Value = null;
			}
			
			fam.aux_SortChilds();
		}

		private static void CheckRecord_Group(GEDCOMGroupRecord group)
		{
			for (int i = group.Members.Count - 1; i >= 0; i--)
			{
				GEDCOMPointer ptr = group.Members[i];
				GEDCOMIndividualRecord irec = ptr.Value as GEDCOMIndividualRecord;
				if (irec == null)
				{
					group.Members.Delete(i);
				}
				else
				{
					if (irec.IndexOfGroup(group) < 0)
					{
						group.Members.Delete(i);
					}
				}
			}
		}

		private static void CheckRecord_Source(GEDCOMSourceRecord src)
		{
			for (int i = src.RepositoryCitations.Count - 1; i >= 0; i--) {
				GEDCOMRecord val = src.RepositoryCitations[i].Value;
				if (val == null) {
					src.RepositoryCitations.Delete(i);
				}
			}
		}

		public static void CheckRecord(GEDCOMTree tree, GEDCOMRecord rec, GEDCOMFormat format, ValuesCollection valuesCollection)
		{
			if (string.IsNullOrEmpty(rec.UID))
			{
				rec.NewUID();
			}

			if (format != GEDCOMFormat.gf_Native)
			{
				int num = rec.MultimediaLinks.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					GEDCOMMultimediaLink mmLink = rec.MultimediaLinks[i];
					if (!mmLink.IsPointer) TreeTools.ReformMultimediaLink(tree, mmLink);
				}

				num = rec.Notes.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					GEDCOMNotes note = rec.Notes[i];
					if (!note.IsPointer) TreeTools.ReformNote(tree, note);
				}

				num = rec.SourceCitations.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					GEDCOMSourceCitation sourCit = rec.SourceCitations[i];
					if (!sourCit.IsPointer) TreeTools.ReformSourceCitation(tree, sourCit);
				}
			}

			switch (rec.RecordType) {
				case GEDCOMRecordType.rtIndividual:
					CheckRecord_Individual(tree, format, rec as GEDCOMIndividualRecord, valuesCollection);
					break;

				case GEDCOMRecordType.rtFamily:
					CheckRecord_Family(tree, format, rec as GEDCOMFamilyRecord, valuesCollection);
					break;

				case GEDCOMRecordType.rtGroup:
					CheckRecord_Group(rec as GEDCOMGroupRecord);
					break;

				case GEDCOMRecordType.rtSource:
					CheckRecord_Source(rec as GEDCOMSourceRecord);
					break;				
			}
		}

		public static void CheckHeader(GEDCOMTree tree, GEDCOMFormat format)
		{
			if (format == GEDCOMFormat.gf_Native)
			{
				GEDCOMHeader header = tree.Header;
				GEDCOMTag tag;

				tag = header.FindTag("_ADVANCED", 0);
				if (tag != null) header.DeleteTag("_ADVANCED");

				tag = header.FindTag("_EXT_NAME", 0);
				if (tag != null) header.DeleteTag("_EXT_NAME");
			}
		}

		private static void CorrectIds(GEDCOMTree tree, IProgressController pc)
		{
			pc.ProgressInit(LangMan.LS(LSID.LSID_IDsCorrect), tree.RecordsCount);
			XRefReplacer repMap = new XRefReplacer();
			try
			{
				int num = tree.RecordsCount - 1;
				for (int i = 0; i <= num; i++)
				{
					GEDCOMRecord rec = tree[i];
					if (rec.GetId() < 0)
					{
						string newXRef = tree.XRefIndex_NewXRef(rec);
						repMap.AddXRef(rec, rec.XRef, newXRef);
						rec.XRef = newXRef;
					}
					pc.ProgressStep();
				}

				tree.Header.ReplaceXRefs(repMap);
				pc.ProgressInit(LangMan.LS(LSID.LSID_IDsCorrect), repMap.Count);

				int num2 = repMap.Count - 1;
				for (int i = 0; i <= num2; i++)
				{
					GEDCOMRecord rec = repMap[i].Rec;
					rec.ReplaceXRefs(repMap);
					pc.ProgressStep();
				}
			}
			finally
			{
                repMap.Dispose();
				pc.ProgressDone();
			}
		}

		public static bool CheckGEDCOMFormat(GEDCOMTree tree, ValuesCollection valuesCollection, IProgressController pc)
		{
			bool result = false;

			try
			{
				pc.ProgressInit(LangMan.LS(LSID.LSID_FormatCheck), tree.RecordsCount);
				try
				{
					GEDCOMFormat format = GKUtils.GetGEDCOMFormat(tree);
					bool idCheck = true;

					TreeTools.CheckHeader(tree, format);

					int num = tree.RecordsCount - 1;
					for (int i = 0; i <= num; i++)
					{
						GEDCOMRecord rec = tree[i];
						TreeTools.CheckRecord(tree, rec, format, valuesCollection);

						if (format != GEDCOMFormat.gf_Native && idCheck && rec.GetId() < 0)
						{
							idCheck = false;
						}

						pc.ProgressStep();
					}

					if (!idCheck && GKUtils.ShowQuestion(LangMan.LS(LSID.LSID_IDsCorrectNeed)) == DialogResult.Yes)
					{
						TreeTools.CorrectIds(tree, pc);
					}

					result = true;
				}
				finally
				{
					pc.ProgressDone();
				}
			}
			catch (Exception ex)
			{
				SysUtils.LogWrite("TreeTools.CheckGEDCOMFormat(): " + ex.Message);
				GKUtils.ShowError(LangMan.LS(LSID.LSID_CheckGedComFailed));
			}

			return result;
		}


		#endregion

		#region Tree Walk

		public enum TTreeWalkMode
		{
			twmAll,
			twmFamily,
			twmAncestors,
			twmDescendants,
			twmNone
		}

		public static void TreeWalk(GEDCOMIndividualRecord iRec, TTreeWalkMode aMode, ExtList<GEDCOMRecord> aList)
		{
			if (iRec != null && aList.IndexOf(iRec) < 0)
			{
				aList.Add(iRec);

				if (aMode != TTreeWalkMode.twmNone)
				{
					if ((aMode == TTreeWalkMode.twmAll || aMode == TTreeWalkMode.twmAncestors) && iRec.ChildToFamilyLinks.Count > 0)
					{
						GEDCOMFamilyRecord family = iRec.ChildToFamilyLinks[0].Family;

						GEDCOMIndividualRecord rel_person;

						rel_person = family.Husband.Value as GEDCOMIndividualRecord;
						TreeTools.TreeWalk(rel_person, aMode, aList);

						rel_person = (family.Wife.Value as GEDCOMIndividualRecord);
						TreeTools.TreeWalk(rel_person, aMode, aList);
					}

					if (aMode < TTreeWalkMode.twmAncestors || aMode == TTreeWalkMode.twmDescendants)
					{
						int num = iRec.SpouseToFamilyLinks.Count - 1;
						for (int i = 0; i <= num; i++)
						{
							GEDCOMFamilyRecord family = iRec.SpouseToFamilyLinks[i].Family;

							GEDCOMPointer sp = ((iRec.Sex == GEDCOMSex.svMale) ? family.Wife : family.Husband);

							TTreeWalkMode int_mode = ((aMode == TTreeWalkMode.twmAll) ? TTreeWalkMode.twmAll : TTreeWalkMode.twmNone);

							GEDCOMIndividualRecord rel_person = sp.Value as GEDCOMIndividualRecord;

							TreeTools.TreeWalk(rel_person, int_mode, aList);

							switch (aMode) {
								case TTreeWalkMode.twmAll:
									int_mode = TTreeWalkMode.twmAll;
									break;

								case TTreeWalkMode.twmFamily:
									int_mode = TTreeWalkMode.twmNone;
									break;

								case TTreeWalkMode.twmDescendants:
									int_mode = TTreeWalkMode.twmDescendants;
									break;
							}

							int num2 = family.Childrens.Count - 1;
							for (int j = 0; j <= num2; j++)
							{
								rel_person = (family.Childrens[j].Value as GEDCOMIndividualRecord);
								TreeTools.TreeWalk(rel_person, int_mode, aList);
							}
						}
					}
				}
			}
		}

		#endregion

		#region Tree Merge

		public static void TreeMerge(GEDCOMTree mainTree, string fileName, TextBox logBox)
		{
			if (logBox != null)
			{
				logBox.Clear();
				logBox.AppendText(string.Format(LangMan.LS(LSID.LSID_MainBaseSize), mainTree.RecordsCount.ToString()) + "\r\n");
			}

			GEDCOMTree extTree = new GEDCOMTree();

			XRefReplacer repMap = new XRefReplacer();
			try
			{
				extTree.LoadFromFile(fileName);
				extTree.Header.Clear();
				while (extTree.RecordsCount > 0)
				{
					GEDCOMRecord rec = extTree.Extract(0);
					string newXRef = mainTree.XRefIndex_NewXRef(rec);
					repMap.AddXRef(rec, rec.XRef, newXRef);
					rec.XRef = newXRef;
					rec.ResetOwner(mainTree);
					mainTree.AddRecord(rec);
				}

				int num = repMap.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					GEDCOMRecord rec = repMap[i].Rec;
					rec.ReplaceXRefs(repMap);
				}

				if (logBox != null)
				{
					logBox.AppendText(string.Format(LangMan.LS(LSID.LSID_MainBaseSize), mainTree.RecordsCount.ToString()) + "\r\n");
				}
			}
			finally
			{
                repMap.Dispose();
				extTree.Dispose();
			}
		}

		#endregion

		#region Base Checks

		public enum TCheckDiag
		{
			cdPersonLonglived,
			cdPersonSexless,
			cdLiveYearsInvalid,
			cdStrangeSpouse,
			cdStrangeParent,
			cdEmptyFamily
		}

		public enum TCheckSolve
		{
			csSkip,
			csSetIsDead,
			csDefineSex,
			csRemove
		}

		public sealed class TCheckObj
		{
			public string Comment;
			public TCheckDiag Diag;
			public GEDCOMRecord Rec;
			public TCheckSolve Solve;

			public TCheckObj(GEDCOMRecord rec, TCheckDiag diag, TCheckSolve solve)
			{
				this.Rec = rec;
				this.Diag = diag;
				this.Solve = solve;
			}

			public string GetRecordName()
			{
				string result = "[" + this.Rec.XRef + "] ";

				switch (this.Rec.RecordType)
				{
					case GEDCOMRecordType.rtIndividual:
						result = result + (this.Rec as GEDCOMIndividualRecord).aux_GetNameStr(true, false);
						break;

					case GEDCOMRecordType.rtFamily:
						result = result + GKUtils.aux_GetFamilyStr(this.Rec as GEDCOMFamilyRecord);
						break;
				}

				return result;
			}

			public void Free()
			{
				SysUtils.Free(this);
			}
		}

		private static void CheckIndividualRecord(GEDCOMIndividualRecord iRec, ExtList<TCheckObj> aChecksList)
		{
			int iAge;
			if (iRec.GetIndividualEvent("DEAT") == null)
			{
				string age = GKUtils.GetAge(iRec, -1);
				if (age != "" && age != "?")
				{
					iAge = int.Parse(age);
                    if (iAge >= GKConsts.ProvedLifeLength)
					{
						TCheckObj checkObj = new TCheckObj(iRec, TCheckDiag.cdPersonLonglived, TCheckSolve.csSetIsDead);
						checkObj.Comment = string.Format(LangMan.LS(LSID.LSID_PersonLonglived), age);
						aChecksList.Add(checkObj);
					}
				}
			}

			GEDCOMSex sex = iRec.Sex;
			if (sex < GEDCOMSex.svMale || sex >= GEDCOMSex.svUndetermined)
			{
				TCheckObj checkObj = new TCheckObj(iRec, TCheckDiag.cdPersonSexless, TCheckSolve.csDefineSex);
				checkObj.Comment = LangMan.LS(LSID.LSID_PersonSexless);
				aChecksList.Add(checkObj);
			}

			bool yBC1, yBC2;
			int y_birth = GKUtils.GetIndependentYear(iRec, "BIRT", out yBC1);
			int y_death = GKUtils.GetIndependentYear(iRec, "DEAT", out yBC2);
			int delta = (y_death - y_birth);
			if (y_birth > -1 && y_death > -1 && delta < 0 && !yBC2)
			{
				TCheckObj checkObj = new TCheckObj(iRec, TCheckDiag.cdLiveYearsInvalid, TCheckSolve.csSkip);
				checkObj.Comment = LangMan.LS(LSID.LSID_LiveYearsInvalid);
				aChecksList.Add(checkObj);
			}

			iAge = TreeStats.GetMarriageAge(iRec);
			if (iAge > 0 && (iAge <= 13 || iAge >= 50))
			{
				TCheckObj checkObj = new TCheckObj(iRec, TCheckDiag.cdStrangeSpouse, TCheckSolve.csSkip);
				checkObj.Comment = string.Format(LangMan.LS(LSID.LSID_StrangeSpouse), iAge.ToString());
				aChecksList.Add(checkObj);
			}

			GEDCOMIndividualRecord iDummy;
			iAge = TreeStats.GetFirstbornAge(iRec, out iDummy);
			if (iAge > 0 && (iAge <= 13 || iAge >= 50))
			{
				TCheckObj checkObj = new TCheckObj(iRec, TCheckDiag.cdStrangeParent, TCheckSolve.csSkip);
				checkObj.Comment = string.Format(LangMan.LS(LSID.LSID_StrangeParent), iAge.ToString());
				aChecksList.Add(checkObj);
			}
		}

		private static void CheckFamilyRecord(GEDCOMFamilyRecord fRec, ExtList<TCheckObj> aChecksList)
		{
			bool empty = (fRec.Notes.Count == 0 && fRec.SourceCitations.Count == 0 && fRec.MultimediaLinks.Count == 0 && fRec.UserReferences.Count == 0);
			empty = empty && (fRec.FamilyEvents.Count == 0 && fRec.Childrens.Count == 0 && fRec.SpouseSealings.Count == 0);
			empty = empty && (fRec.Husband.Value == null && fRec.Wife.Value == null);

			if (empty)
			{
				TCheckObj checkObj = new TCheckObj(fRec, TCheckDiag.cdEmptyFamily, TCheckSolve.csRemove);
				checkObj.Comment = LangMan.LS(LSID.LSID_EmptyFamily);
				aChecksList.Add(checkObj);
			}
		}

		public static void CheckBase(IBase aBase, ExtList<TCheckObj> aChecksList)
		{
			try
			{
				GEDCOMTree tree = aBase.Tree;

				aBase.ProgressInit(LangMan.LS(LSID.LSID_ToolOp_7), tree.RecordsCount);
				aChecksList.Clear();

				int num = tree.RecordsCount - 1;
				for (int i = 0; i <= num; i++)
				{
					aBase.ProgressStep();

					GEDCOMRecord rec = tree[i];

					switch (rec.RecordType) {
						case GEDCOMRecordType.rtIndividual:
							CheckIndividualRecord(rec as GEDCOMIndividualRecord, aChecksList);
							break;

						case GEDCOMRecordType.rtFamily:
							CheckFamilyRecord(rec as GEDCOMFamilyRecord, aChecksList);
							break;
					}
				}
			}
			finally
			{
				aBase.ProgressDone();
			}
		}

		public static void RepairProblem(IBase aBase, TCheckObj checkObj)
		{
		    if (aBase == null || checkObj == null) return;

			GEDCOMTree tree = aBase.Tree;

			switch (checkObj.Diag)
			{
				case TreeTools.TCheckDiag.cdPersonLonglived:
					{
						GEDCOMIndividualRecord iRec = checkObj.Rec as GEDCOMIndividualRecord;
						aBase.Context.CreateEventEx(iRec, "DEAT", "", "");
						//this.Base.ChangeRecord(iRec);
						break;
					}

				case TreeTools.TCheckDiag.cdPersonSexless:
					{
						GEDCOMIndividualRecord iRec = checkObj.Rec as GEDCOMIndividualRecord;
						aBase.CheckPersonSex(iRec);
						//this.Base.ChangeRecord(iRec);
						break;
					}

				case TreeTools.TCheckDiag.cdEmptyFamily:
					{
						tree.DeleteRecord(checkObj.Rec);
						break;
					}
			}
		}

		#endregion

		#region Tree Split

		private static void _CheckRelations_AddRel(ExtList<GEDCOMRecord> splitList, GEDCOMRecord aRec)
		{
			if (splitList.IndexOf(aRec) < 0)
			{
				splitList.Add(aRec);
			}
		}

		private static void _CheckRelations_CheckRecord(ExtList<GEDCOMRecord> splitList, GEDCOMRecord rec)
		{
			int num = rec.MultimediaLinks.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				_CheckRelations_AddRel(splitList, rec.MultimediaLinks[i].Value);
			}

			int num2 = rec.Notes.Count - 1;
			for (int i = 0; i <= num2; i++)
			{
				_CheckRelations_AddRel(splitList, rec.Notes[i].Value);
			}

			int num3 = rec.SourceCitations.Count - 1;
			for (int i = 0; i <= num3; i++)
			{
				_CheckRelations_AddRel(splitList, rec.SourceCitations[i].Value);
			}
		}

		private static void _CheckRelations_CheckTag(ExtList<GEDCOMRecord> splitList, GEDCOMTagWithLists tag)
		{
			int num = tag.MultimediaLinks.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				_CheckRelations_AddRel(splitList, tag.MultimediaLinks[i].Value);
			}

			int num2 = tag.Notes.Count - 1;
			for (int i = 0; i <= num2; i++)
			{
				_CheckRelations_AddRel(splitList, tag.Notes[i].Value);
			}

			int num3 = tag.SourceCitations.Count - 1;
			for (int i = 0; i <= num3; i++)
			{
				_CheckRelations_AddRel(splitList, tag.SourceCitations[i].Value);
			}
		}

		private static void _CheckRelations_CheckIndividual(ExtList<GEDCOMRecord> splitList, GEDCOMIndividualRecord iRec)
		{
			_CheckRelations_CheckRecord(splitList, iRec);

			int num = iRec.ChildToFamilyLinks.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				_CheckRelations_AddRel(splitList, iRec.ChildToFamilyLinks[i].Family);
			}

			int num2 = iRec.SpouseToFamilyLinks.Count - 1;
			for (int i = 0; i <= num2; i++)
			{
				_CheckRelations_AddRel(splitList, iRec.SpouseToFamilyLinks[i].Family);
			}

			int num3 = iRec.IndividualEvents.Count - 1;
			for (int i = 0; i <= num3; i++)
			{
				_CheckRelations_CheckTag(splitList, iRec.IndividualEvents[i].Detail);
			}

			int num4 = iRec.IndividualOrdinances.Count - 1;
			for (int i = 0; i <= num4; i++)
			{
				_CheckRelations_CheckTag(splitList, iRec.IndividualOrdinances[i]);
			}

			int num5 = iRec.Submittors.Count - 1;
			for (int i = 0; i <= num5; i++)
			{
				_CheckRelations_AddRel(splitList, iRec.Submittors[i].Value);
			}

			int num6 = iRec.Associations.Count - 1;
			for (int i = 0; i <= num6; i++)
			{
				_CheckRelations_AddRel(splitList, iRec.Associations[i].Value);
			}

			int num7 = iRec.Aliasses.Count - 1;
			for (int i = 0; i <= num7; i++)
			{
				_CheckRelations_AddRel(splitList, iRec.Aliasses[i].Value);
			}

			int num8 = iRec.AncestorsInterest.Count - 1;
			for (int i = 0; i <= num8; i++)
			{
				_CheckRelations_AddRel(splitList, iRec.AncestorsInterest[i].Value);
			}

			int num9 = iRec.DescendantsInterest.Count - 1;
			for (int i = 0; i <= num9; i++)
			{
				_CheckRelations_AddRel(splitList, iRec.DescendantsInterest[i].Value);
			}

			int num10 = iRec.Groups.Count - 1;
			for (int i = 0; i <= num10; i++)
			{
				_CheckRelations_AddRel(splitList, iRec.Groups[i].Value);
			}
		}

		private static void _CheckRelations_CheckFamily(ExtList<GEDCOMRecord> splitList, GEDCOMFamilyRecord fRec)
		{
			_CheckRelations_CheckRecord(splitList, fRec);

			int num = fRec.FamilyEvents.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				_CheckRelations_CheckTag(splitList, fRec.FamilyEvents[i].Detail);
			}
			_CheckRelations_AddRel(splitList, fRec.Submitter.Value);

			int num2 = fRec.SpouseSealings.Count - 1;
			for (int i = 0; i <= num2; i++)
			{
				_CheckRelations_CheckTag(splitList, fRec.SpouseSealings[i]);
			}
		}

		private static void _CheckRelations_CheckSource(ExtList<GEDCOMRecord> splitList, GEDCOMSourceRecord sRec)
		{
			_CheckRelations_CheckRecord(splitList, sRec);

			int num = sRec.RepositoryCitations.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				_CheckRelations_AddRel(splitList, sRec.RepositoryCitations[i].Value);
			}
		}

		public static void CheckRelations(ExtList<GEDCOMRecord> splitList)
		{
			int num = splitList.Count;
			for (int i = 0; i < num; i++)
			{
				GEDCOMRecord rec = splitList[i] as GEDCOMRecord;
				switch (rec.RecordType)
				{
					case GEDCOMRecordType.rtIndividual:
					{
						_CheckRelations_CheckIndividual(splitList, rec as GEDCOMIndividualRecord);
						break;
					}
					case GEDCOMRecordType.rtFamily:
					{
						_CheckRelations_CheckFamily(splitList, rec as GEDCOMFamilyRecord);
						break;
					}
					case GEDCOMRecordType.rtNote:
					{
						_CheckRelations_CheckRecord(splitList, rec);
						break;
					}
					case GEDCOMRecordType.rtMultimedia:
					{
						_CheckRelations_CheckRecord(splitList, rec);
						break;
					}
					case GEDCOMRecordType.rtSource:
					{
						_CheckRelations_CheckSource(splitList, rec as GEDCOMSourceRecord);
						break;
					}
					case GEDCOMRecordType.rtRepository:
					{
						_CheckRelations_CheckRecord(splitList, rec);
						break;
					}
					case GEDCOMRecordType.rtSubmitter:
					{
						_CheckRelations_CheckRecord(splitList, rec);
						break;
					}
				}
			}
		}

		#endregion

		#region Tree Compare

		public class IndividualRecordComparer: IComparer<ULIndividual>
		{
			public int Compare(ULIndividual x, ULIndividual y)
			{
				return string.Compare(x.Family, y.Family, false);
			}
		}

		public struct ULIndividual
		{
			public string Family;
			public GEDCOMIndividualRecord iRec;
		}

		public static List<ULIndividual> GetUnlinkedNamesakes(GEDCOMTree tree, IProgressController pc)
		{
			List<ULIndividual> result = new List<ULIndividual>();

			Hashtable families = new Hashtable();

			pc.ProgressInit("Этап 1", tree.RecordsCount);

			// составить таблицу фамилий и персон, относящихся к этим фамилиям
			for (int i = 0; i < tree.RecordsCount; i++)
			{
				GEDCOMRecord rec = tree[i];

				if (rec is GEDCOMIndividualRecord)
				{
					GEDCOMIndividualRecord iRec = rec as GEDCOMIndividualRecord;

					string[] fams = NamesTable.GetSurnames(iRec);
					for (int k = 0; k < fams.Length; k++)
					{
						string f = fams[k];
						if (f.Length > 1)
						{
							List<GEDCOMIndividualRecord> ps = (List<GEDCOMIndividualRecord>)families[f];
							if (ps == null) {
								ps = new List<GEDCOMIndividualRecord>();
								families[f] = ps;
							}
							ps.Add(iRec);
						}
					}
				}

				pc.ProgressStep();
			}

			pc.ProgressInit("Этап 2", families.Count);
			
			// найти всех персон одной фамилии, не связанных узами родства
			foreach (DictionaryEntry entry in families)
			{
				string fam = (string)entry.Key;
				List<GEDCOMIndividualRecord> ps = (List<GEDCOMIndividualRecord>)entry.Value;

				int i = 0;
				while (i < ps.Count)
				{
					GEDCOMIndividualRecord iRec = ps[i];

					using (ExtList<GEDCOMRecord> lst = new ExtList<GEDCOMRecord>())
					{
						TreeTools.TreeWalk(iRec, TreeTools.TTreeWalkMode.twmAll, lst);
						for (int k = 0; k < lst.Count; k++)
						{
							GEDCOMIndividualRecord item = lst[k] as GEDCOMIndividualRecord;
							int idx = ps.IndexOf(item);
							if (item != iRec && idx >= 0 && idx > i) ps.RemoveAt(idx);
						}
					}

					i++;
				}

				if (ps.Count > 1) {
					for (i = 0; i < ps.Count; i++) {
						ULIndividual indiv;
						indiv.Family = fam;
						indiv.iRec = ps[i];
						result.Add(indiv);
					}
				}

				pc.ProgressStep();
			}

			result.Sort(new IndividualRecordComparer());
			
			pc.ProgressDone();

			return result;
		}

		public delegate void DuplicateFoundFunc(GEDCOMIndividualRecord indivA, GEDCOMIndividualRecord indivB);

		public static void FindDuplicates(GEDCOMTree tree_A, GEDCOMTree tree_B, float matchThreshold, 
		                                  DuplicateFoundFunc foundFunc, IProgressController pc)
		{		
			MatchParams mParams;
			//mParams.IndistinctMatching = true;
			mParams.NamesIndistinctThreshold = 90.0f / 100.0f;
			mParams.DatesCheck = true;
			mParams.YearsInaccuracy = 3;
			mParams.RusNames = true;

			pc.ProgressInit("Поиск дубликатов", tree_A.RecordsCount);
			try
			{
				for (int i = 0; i <= tree_A.RecordsCount - 1; i++) {
					GEDCOMRecord recA = tree_A[i];
					if (recA is GEDCOMIndividualRecord) {
						for (int k = 0; k <= tree_B.RecordsCount - 1; k++) {
							GEDCOMRecord recB = tree_B[k];
							if (recB is GEDCOMIndividualRecord) {
								GEDCOMIndividualRecord indivA = recA as GEDCOMIndividualRecord;
								GEDCOMIndividualRecord indivB = recB as GEDCOMIndividualRecord;

								if (indivA != indivB && indivA.IsMatch(indivB, mParams) >= matchThreshold)
								{
									foundFunc(indivA, indivB);
								}
							}
						}
					}

					pc.ProgressStep();
					Application.DoEvents();
				}
			}
			finally
			{
				pc.ProgressDone();
			}
		}

		public static void TreeCompare(GEDCOMTree mainTree, string fileName, TextBox logBox)
		{
			GEDCOMTree tempTree = new GEDCOMTree();
			tempTree.LoadFromFile(fileName);

			StringList fams = new StringList();
			StringList names = new StringList();

			try
			{
				logBox.AppendText(LangMan.LS(LSID.LSID_SearchMatches) + "\r\n");

				int num = mainTree.RecordsCount - 1;
				for (int i = 0; i <= num; i++)
				{
					if (mainTree[i] is GEDCOMIndividualRecord)
					{
						GEDCOMIndividualRecord iRec = mainTree[i] as GEDCOMIndividualRecord;

						int idx = names.AddObject(iRec.aux_GetNameStr(true, false), new ExtList<GEDCOMIndividualRecord>());
						(names.GetObject(idx) as ExtList<GEDCOMIndividualRecord>).Add(iRec);

						string fam, nam, pat;
						iRec.GetNameParts(out fam, out nam, out pat);

						fams.AddObject(NamesTable.PrepareRusSurname(fam, iRec.Sex == GEDCOMSex.svFemale), null);
					}
				}

				int num2 = tempTree.RecordsCount - 1;
				for (int i = 0; i <= num2; i++)
				{
					if (tempTree[i] is GEDCOMIndividualRecord)
					{
						GEDCOMIndividualRecord iRec = tempTree[i] as GEDCOMIndividualRecord;

						string tm = iRec.aux_GetNameStr(true, false);
						int idx = names.IndexOf(tm);
						if (idx >= 0)
						{
							(names.GetObject(idx) as ExtList<GEDCOMIndividualRecord>).Add(iRec);
						}

						string fam, nam, pat;
						iRec.GetNameParts(out fam, out nam, out pat);

						tm = NamesTable.PrepareRusSurname(fam, iRec.Sex == GEDCOMSex.svFemale);
						idx = fams.IndexOf(tm);
						if (idx >= 0)
						{
							fams.SetObject(idx, 1);
						}
					}
				}

				for (int i = fams.Count - 1; i >= 0; i--)
				{
					if (fams.GetObject(i) == null || fams[i] == "?")
						fams.Delete(i);
				}

				for (int i = names.Count - 1; i >= 0; i--)
				{
					if ((names.GetObject(i) as ExtList<GEDCOMIndividualRecord>).Count == 1) {
						(names.GetObject(i) as ExtList<GEDCOMIndividualRecord>).Dispose();
						names.Delete(i);
					}
				}

				if (fams.Count != 0)
				{
					logBox.AppendText(LangMan.LS(LSID.LSID_SimilarSurnames) + "\r\n");

					int num3 = fams.Count - 1;
					for (int i = 0; i <= num3; i++)
					{
						logBox.AppendText("    " + fams[i] + "\r\n");
					}
				}

				if (names.Count != 0)
				{
					logBox.AppendText(LangMan.LS(LSID.LSID_SimilarNames) + "\r\n");

					int num4 = names.Count - 1;
					for (int i = 0; i <= num4; i++)
					{
						logBox.AppendText("    " + names[i] + "\r\n");
						ExtList<GEDCOMIndividualRecord> lst = names.GetObject(i) as ExtList<GEDCOMIndividualRecord>;

						int num5 = lst.Count - 1;
						for (int j = 0; j <= num5; j++)
						{
							GEDCOMIndividualRecord iRec = lst[j];
							logBox.AppendText("      * " + iRec.aux_GetNameStr(true, false) + " " + GKUtils.GetLifeStr(iRec) + "\r\n");
						}
					}
				}
			}
			finally
			{
				int num6 = names.Count - 1;
				for (int i = 0; i <= num6; i++)
				{
					SysUtils.Free(names.GetObject(i));
				}

                names.Dispose();
                fams.Dispose();

				tempTree.Dispose();
			}
		}

		#endregion

		#region Places Management

		public static void PlacesSearch_Clear(StringList placesList)
		{
			for (int i = placesList.Count - 1; i >= 0; i--) (placesList.GetObject(i) as TPlaceObj).Dispose();
			placesList.Clear();
		}

		private static void _CheckPlaces_PrepareEvent(StringList placesList, GEDCOMCustomEvent aEvent)
		{
			string place_str = aEvent.Detail.Place.StringValue;
			if (place_str != "")
			{
				GEDCOMLocationRecord loc = aEvent.Detail.Place.Location.Value as GEDCOMLocationRecord;
				if (loc != null)
				{
					place_str = "[*] " + place_str;
				}

				int idx = placesList.IndexOf(place_str);

				TPlaceObj place_obj;
				if (idx >= 0)
				{
					place_obj = (placesList.GetObject(idx) as TPlaceObj);
				}
				else
				{
					place_obj = new TPlaceObj();
					place_obj.Name = place_str;
					placesList.AddObject(place_str, place_obj);
				}
				place_obj.Facts.Add(aEvent);
			}
		}

		public static void PlacesSearch(GEDCOMTree tree, StringList placesList, IProgressController pc)
		{
			PlacesSearch_Clear(placesList);

			pc.ProgressInit(LangMan.LS(LSID.LSID_PlacesPrepare), tree.RecordsCount);

			int num = tree.RecordsCount - 1;
			for (int i = 0; i <= num; i++)
			{
				pc.ProgressStep();

				GEDCOMRecord record = tree[i];

				if (record is GEDCOMIndividualRecord)
				{
					GEDCOMIndividualRecord iRec = record as GEDCOMIndividualRecord;

					int num2 = iRec.IndividualEvents.Count - 1;
					for (int j = 0; j <= num2; j++)
					{
						_CheckPlaces_PrepareEvent(placesList, iRec.IndividualEvents[j]);
					}
				}
				else
				{
					if (record is GEDCOMFamilyRecord)
					{
						GEDCOMFamilyRecord fRec = record as GEDCOMFamilyRecord;

						int num3 = fRec.FamilyEvents.Count - 1;
						for (int j = 0; j <= num3; j++)
						{
							_CheckPlaces_PrepareEvent(placesList, fRec.FamilyEvents[j]);
						}
					}
				}
			}

			pc.ProgressDone();
		}

		#endregion

	}
}
