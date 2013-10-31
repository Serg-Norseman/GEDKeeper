using System;
using System.Collections;
using System.Collections.Generic;
using System.Windows.Forms;

using Ext.Utils;
using GedCom551;
using GKUI;

/// <summary>
/// Localization: dirty
/// </summary>

namespace GKCore
{
	public class TPatriarchObj : IDisposable
	{
		public TGEDCOMIndividualRecord IRec;
		public int IBirthYear;
		public int IDescendantsCount;
		public int IDescGenerations;
		public List<byte> ILinks = new List<byte>();
		private bool Disposed_;

		public void Dispose()
		{
			if (!this.Disposed_)
			{
				this.Disposed_ = true;
			}
		}
	}

	public class TPlaceObj : IDisposable
	{
		public string Name;
		public TList Facts;
		protected bool Disposed_;

		public TPlaceObj()
		{
			this.Facts = new TList();
		}

		public void Dispose()
		{
			if (!this.Disposed_)
			{
				this.Facts.Dispose();
				this.Disposed_ = true;
			}
		}
	}

	public static class TreeTools
	{
		public static LSID[] CheckSolveNames;

		static TreeTools()
		{
			CheckSolveNames = new LSID[4];
			CheckSolveNames[0] = LSID.LSID_RM_Skip;
			CheckSolveNames[1] = LSID.LSID_SetIsDead;
			CheckSolveNames[2] = LSID.LSID_DefineSex;
			CheckSolveNames[3] = LSID.LSID_DoDelete;
		}

		#region Patriarchs Search

		private static int PatriarchsCompare(object Item1, object Item2)
		{
			return (Item1 as TPatriarchObj).IBirthYear - (Item2 as TPatriarchObj).IBirthYear;
		}

		public static string GetPatriarchLinks(TList lst, TPatriarchObj pObj)
		{
			string Result = "";

			int num = pObj.ILinks.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				byte ix = pObj.ILinks[i];
				if (Result != "") Result += ", ";
				Result += (lst[ix] as TPatriarchObj).IRec.aux_GetNameStr(true, false);
			}
			return Result;
		}

		private static int _GetPatriarchsList_GetBirthYear(TGEDCOMIndividualRecord iRec)
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
						year = _GetPatriarchsList_GetBirthYear(child);
						if (year > 0) {
							return (year - 20);
						}
					}
				}
			}

			return -1;
		}

		private static bool _GetPatriarchsList_SearchAnc(TGEDCOMIndividualRecord descendantRec, TGEDCOMIndividualRecord searchRec, bool onlyMaleLine)
		{
			bool res = false;

			if (descendantRec != null)
			{
				res = (descendantRec == searchRec);

				if (!res && descendantRec.ChildToFamilyLinks.Count > 0)
				{
					TGEDCOMFamilyRecord family = descendantRec.ChildToFamilyLinks[0].Family;

					TGEDCOMIndividualRecord ancestor = family.Husband.Value as TGEDCOMIndividualRecord;
					if (ancestor != null)
					{
						res = TreeTools._GetPatriarchsList_SearchAnc(ancestor, searchRec, onlyMaleLine);
						if (res) return res;
					}

					/*if (!onlyMaleLine)
					{
						ancestor = (family.Wife.Value as TGEDCOMIndividualRecord);
						if (ancestor != null)
						{
							res = TreeTools._GetPatriarchsList_SearchAnc(ancestor, searchRec, false);
							if (res) return res;
						}
					}*/
				}
			}

			return res;
		}

		private static bool _GetPatriarchsList_SearchDesc(TGEDCOMIndividualRecord ancestorRec, TGEDCOMIndividualRecord searchRec, out TGEDCOMIndividualRecord cross)
		{
			bool res = false;
			cross = null;

			int num = ancestorRec.SpouseToFamilyLinks.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				TGEDCOMFamilyRecord family = ancestorRec.SpouseToFamilyLinks[i].Family;
				TGEDCOMIndividualRecord spouse = family.aux_GetSpouse(ancestorRec);

				if (spouse != null)
				{
					res = TreeTools._GetPatriarchsList_SearchAnc(spouse, searchRec, (ancestorRec.Sex == TGEDCOMSex.svFemale));
					if (res) {
						cross = ancestorRec;
						return res;
					}
				}

				if (ancestorRec.Sex == TGEDCOMSex.svMale) {
					int num2 = family.Childrens.Count - 1;
					for (int j = 0; j <= num2; j++)
					{
						TGEDCOMIndividualRecord child = family.Childrens[j].Value as TGEDCOMIndividualRecord;

						res = TreeTools._GetPatriarchsList_SearchDesc(child, searchRec, out cross);
						if (res) return res;
					}
				}
			}

			return false;
		}

		public static void GetPatriarchsList(TGEDCOMTree aTree, bool aProgress, bool aLinks, TList aList, int aMinGens, bool aDates = true)
		{
			if (aProgress) TfmProgress.ProgressInit(aTree.RecordsCount, LangMan.LSList[474]);

			TreeStats.InitExtCounts(aTree, -1);
			try
			{
				int num = aTree.RecordsCount - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMRecord rec = aTree[i];
					if (rec is TGEDCOMIndividualRecord)
					{
						TGEDCOMIndividualRecord i_rec = rec as TGEDCOMIndividualRecord;

						string nf, nn, np;
						i_rec.aux_GetNameParts(out nf, out nn, out np);

						int bYear = _GetPatriarchsList_GetBirthYear(i_rec);
						int descGens = TreeStats.GetDescGenerations(i_rec);
						bool res = i_rec.ChildToFamilyLinks.Count == 0;
						res = (res && i_rec.Sex == TGEDCOMSex.svMale);
						res = (res && /*nf != "" && nf != "?" &&*/ nn != "" && nn != "?");
						res = (res && descGens >= aMinGens);

						if (aDates) { res = (res && bYear > 0); }

						if (res)
						{
							TPatriarchObj pObj = new TPatriarchObj();
							pObj.IRec = i_rec;
							pObj.IBirthYear = bYear;
							pObj.IDescendantsCount = TreeStats.GetDescendantsCount(i_rec) - 1;
							pObj.IDescGenerations = descGens;
							aList.Add(pObj);
						}
					}

					if (aProgress) TfmProgress.ProgressStep();
				}

				aList.Sort(TreeTools.PatriarchsCompare);
			}
			finally
			{
				if (aProgress) TfmProgress.ProgressDone();
			}

			if (aLinks)
			{
				if (aProgress) TfmProgress.ProgressInit(aList.Count, LangMan.LSList[475]);
				try
				{
					int num2 = aList.Count - 1;
					for (int i = 0; i <= num2; i++)
					{
						TPatriarchObj patr = aList[i] as TPatriarchObj;

						for (int j = i + 1; j <= num2; j++)
						{
							TPatriarchObj patr2 = aList[j] as TPatriarchObj;

							TGEDCOMIndividualRecord cross;
							bool res = TreeTools._GetPatriarchsList_SearchDesc(patr.IRec, patr2.IRec, out cross);
							if (res)
							{
								if (cross.Sex == TGEDCOMSex.svFemale) {
									patr.ILinks.Add((byte)j);
								} else {
									patr2.ILinks.Add((byte)i);
								}
							}
						}

						if (aProgress) TfmProgress.ProgressStep();
					}
				}
				finally
				{
					if (aProgress) TfmProgress.ProgressDone();
				}
			}
		}

		#endregion

		#region Tree Check

		private static void ReformNote(TGEDCOMTree tree, TGEDCOMNotes note)
		{
			StringList strData = new StringList();
			try
			{
				strData.Text = note.Notes.Text;
				TGEDCOMNoteRecord noteRec = tree.aux_CreateNoteEx(null, strData);
				note.Clear();
				note.Value = noteRec;
			}
			finally
			{
				strData.Free();
			}
		}

		private static void ReformMultimediaLink(TGEDCOMTree tree, TGEDCOMMultimediaLink mmLink)
		{
			try
			{
				string title = mmLink.Title;
				TGEDCOMMultimediaRecord mmRec = new TGEDCOMMultimediaRecord(tree, tree, "", "");
				mmRec.InitNew();
				tree.AddRecord(mmRec);

				int num = mmLink.FileReferences.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMFileReference fr = mmLink.FileReferences[i];
					TGEDCOMFileReferenceWithTitle frt = new TGEDCOMFileReferenceWithTitle(tree, mmRec, "", "");
					if (fr.MultimediaFormat != TGEDCOMMultimediaFormat.mfNone)
					{
						frt.MultimediaFormat = fr.MultimediaFormat;
					}
					if (fr.MediaType != TGEDCOMMediaType.mtNone)
					{
						frt.MediaType = fr.MediaType;
					}
					frt.LinkFile(fr.StringValue, TGEDCOMMediaType.mtUnknown, TGEDCOMMultimediaFormat.mfUnknown);
					mmRec.FileReferences.Add(frt);
				}
				mmLink.Clear();
				mmLink.Value = mmRec;
				mmLink.Title = title;
			}
			finally
			{
			}
		}

		private static void ReformSourceCitation(TGEDCOMTree tree, TGEDCOMSourceCitation sourCit)
		{
		}

		private static void CheckRecord_PrepareTag(TGEDCOMTree tree, TGEDCOMFormat format, TGEDCOMTagWithLists tag)
		{
			int num = tag.MultimediaLinks.Count - 1;
			for (int i = 0; i <= num; i++) {
				TGEDCOMMultimediaLink mmLink = tag.MultimediaLinks[i];
				if (!mmLink.IsPointer) ReformMultimediaLink(tree, mmLink);
			}

			num = tag.Notes.Count - 1;
			for (int i = 0; i <= num; i++) {
				TGEDCOMNotes note = tag.Notes[i];
				if (!note.IsPointer) ReformNote(tree, note);
			}

			num = tag.SourceCitations.Count - 1;
			for (int i = 0; i <= num; i++) {
				TGEDCOMSourceCitation sourCit = tag.SourceCitations[i];
				if (!sourCit.IsPointer) ReformSourceCitation(tree, sourCit);
			}
		}

		private static void CheckRecord_RepairTag(TGEDCOMTree tree, TGEDCOMFormat format, TGEDCOMTagWithLists tag)
		{
			int num = tag.MultimediaLinks.Count - 1;
			for (int i = num; i >= 0; i--) {
				TGEDCOMMultimediaLink mmLink = tag.MultimediaLinks[i];
				if (mmLink.IsPointer && mmLink.Value == null) tag.MultimediaLinks.Delete(i);
			}

			num = tag.Notes.Count - 1;
			for (int i = num; i >= 0; i--) {
				TGEDCOMNotes note = tag.Notes[i];
				if (note.IsPointer && note.Value == null) tag.Notes.Delete(i);
			}

			num = tag.SourceCitations.Count - 1;
			for (int i = num; i >= 0; i--) {
				TGEDCOMSourceCitation sourCit = tag.SourceCitations[i];
				if (sourCit.IsPointer && sourCit.Value == null) tag.SourceCitations.Delete(i);
			}
		}

		private static void CheckRecord_PreparePtr(TGEDCOMTree tree, TGEDCOMFormat format, TGEDCOMPointerWithNotes ptr)
		{
			/*TGEDCOMRecord val = ptr.Value;
			if (!string.IsNullOrEmpty(ptr.XRef) && val == null) {
				ptr.Value = null;
			}*/

			int num = ptr.Notes.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				TGEDCOMNotes note = ptr.Notes[i];
				if (!note.IsPointer) ReformNote(tree, note);
			}
		}

		private static void CheckRecord_EventPlace(TGEDCOMCustomEvent aEvent)
		{
			TGEDCOMPlace place = aEvent.Detail.Place;
			if (place.Location.XRef != "" && place.Location.Value == null)
			{
				place.Location.XRef = "";
			}
			if (place.StringValue != "")
			{
				TGEDCOMLocationRecord loc = place.Location.Value as TGEDCOMLocationRecord;
				if (loc != null && place.StringValue != loc.LocationName)
				{
					place.StringValue = loc.LocationName;
				}
			}
		}

		private static void CheckRecord_AddUserRef(TGEDCOMTree tree, TGEDCOMFormat format, TGEDCOMIndividualRecord iRec, string reference)
		{
			TGEDCOMUserReference uRef = new TGEDCOMUserReference(tree, iRec, "", "");
			uRef.StringValue = reference;
			iRec.UserReferences.Add(uRef);
		}

		private static void CheckRecord_AttrCompatible(TGEDCOMTree tree, TGEDCOMFormat format, TGEDCOMIndividualRecord iRec, TGEDCOMCustomEvent aEvent)
		{
			if (aEvent.Name == "_MILI")
			{
				string cause = aEvent.Detail.Classification.ToLower();

				if (cause.IndexOf("б/д") >= 0)
				{
					if (cause.IndexOf("+") >= 0)
					{
						CheckRecord_AddUserRef(tree, format, iRec, GKData.UserRefs[3]);
					}
					else
					{
						CheckRecord_AddUserRef(tree, format, iRec, GKData.UserRefs[2]);
					}

					aEvent.Detail.Classification = "";
				}
				else
				{
					if (cause.IndexOf("т/т") >= 0)
					{
						CheckRecord_AddUserRef(tree, format, iRec, GKData.UserRefs[4]);
						aEvent.Detail.Classification = "";
					}
				}
			}
		}

		private static void CheckRecord_URefCompatible(TGEDCOMIndividualRecord iRec, TGEDCOMUserReference aUserRef)
		{
		}

		private static void CheckRecord_Individual(TGEDCOMTree tree, TGEDCOMFormat format, TGEDCOMIndividualRecord iRec, ValuesCollection valuesCollection)
		{
			int i;
			if (format == TGEDCOMFormat.gf_Native)
			{
				int num = iRec.IndividualEvents.Count - 1;
				for (i = 0; i <= num; i++)
				{
					TGEDCOMCustomEvent evt = iRec.IndividualEvents[i];
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

		private static void CheckRecord_Family(TGEDCOMTree tree, TGEDCOMFormat format, TGEDCOMFamilyRecord fam, ValuesCollection valuesCollection)
		{
			int i;
			if (format == TGEDCOMFormat.gf_Native)
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

			TGEDCOMRecord val = fam.Husband.Value;
			if (!string.IsNullOrEmpty(fam.Husband.XRef) && val == null) {
				fam.Husband.Value = null;
			}
			
			val = fam.Wife.Value;
			if (!string.IsNullOrEmpty(fam.Wife.XRef) && val == null) {
				fam.Wife.Value = null;
			}
			
			fam.aux_SortChilds();
		}

		private static void CheckRecord_Group(TGEDCOMGroupRecord group)
		{
			for (int i = group.Members.Count - 1; i >= 0; i--)
			{
				TGEDCOMPointer ptr = group.Members[i];
				TGEDCOMIndividualRecord irec = ptr.Value as TGEDCOMIndividualRecord;
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

		private static void CheckRecord_Source(TGEDCOMSourceRecord src)
		{
			for (int i = src.RepositoryCitations.Count - 1; i >= 0; i--) {
				TGEDCOMRecord val = src.RepositoryCitations[i].Value;
				if (val == null) {
					src.RepositoryCitations.Delete(i);
				}
			}
		}

		public static void CheckRecord(TGEDCOMTree tree, TGEDCOMRecord rec, TGEDCOMFormat format, ValuesCollection valuesCollection)
		{
			if (rec.UID == null || rec.UID == "")
			{
				rec.NewUID();
			}

			if (format != TGEDCOMFormat.gf_Native)
			{
				int num = rec.MultimediaLinks.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMMultimediaLink mmLink = rec.MultimediaLinks[i];
					if (!mmLink.IsPointer) TreeTools.ReformMultimediaLink(tree, mmLink);
				}

				num = rec.Notes.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMNotes note = rec.Notes[i];
					if (!note.IsPointer) TreeTools.ReformNote(tree, note);
				}

				num = rec.SourceCitations.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMSourceCitation sourCit = rec.SourceCitations[i];
					if (!sourCit.IsPointer) TreeTools.ReformSourceCitation(tree, sourCit);
				}
			}

			switch (rec.RecordType) {
				case TGEDCOMRecordType.rtIndividual:
					CheckRecord_Individual(tree, format, rec as TGEDCOMIndividualRecord, valuesCollection);
					break;

				case TGEDCOMRecordType.rtFamily:
					CheckRecord_Family(tree, format, rec as TGEDCOMFamilyRecord, valuesCollection);
					break;

				case TGEDCOMRecordType.rtGroup:
					CheckRecord_Group(rec as TGEDCOMGroupRecord);
					break;

				case TGEDCOMRecordType.rtSource:
					CheckRecord_Source(rec as TGEDCOMSourceRecord);
					break;				
			}
		}

		public static void CheckHeader(TGEDCOMTree tree, TGEDCOMFormat format)
		{
			if (format == TGEDCOMFormat.gf_Native)
			{
				TGEDCOMHeader header = tree.Header;
				TGEDCOMTag tag;

				tag = header.FindTag("_ADVANCED", 0);
				if (tag != null) header.DeleteTag("_ADVANCED");

				tag = header.FindTag("_EXT_NAME", 0);
				if (tag != null) header.DeleteTag("_EXT_NAME");
			}
		}

		private static void CorrectIds(TGEDCOMTree tree)
		{
			TfmProgress.ProgressInit(tree.RecordsCount, LangMan.LSList[469]);
			XRefReplacer repMap = new XRefReplacer();
			try
			{
				int num = tree.RecordsCount - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMRecord rec = tree[i];
					if (rec.aux_GetId() < 0)
					{
						string newXRef = tree.XRefIndex_NewXRef(rec);
						repMap.AddXRef(rec, rec.XRef, newXRef);
						rec.XRef = newXRef;
					}
					TfmProgress.ProgressStep();
				}

				tree.Header.ReplaceXRefs(repMap);
				TfmProgress.ProgressInit(repMap.Count, LangMan.LSList[469]);

				int num2 = repMap.Count - 1;
				for (int i = 0; i <= num2; i++)
				{
					TGEDCOMRecord rec = repMap[i].Rec;
					rec.ReplaceXRefs(repMap);
					TfmProgress.ProgressStep();
				}
			}
			finally
			{
				repMap.Free();
				TfmProgress.ProgressDone();
			}
		}

		public static bool CheckGEDCOMFormat(TGEDCOMTree tree, ValuesCollection valuesCollection, IProgressController pc)
		{
			bool result = false;

			try
			{
				pc.ProgressInit(tree.RecordsCount, LangMan.LSList[470]);
				try
				{
					TGEDCOMFormat format = GKUtils.GetGEDCOMFormat(tree);
					bool idCheck = true;

					TreeTools.CheckHeader(tree, format);

					int num = tree.RecordsCount - 1;
					for (int i = 0; i <= num; i++)
					{
						TGEDCOMRecord rec = tree[i];
						TreeTools.CheckRecord(tree, rec, format, valuesCollection);

						if (format != TGEDCOMFormat.gf_Native && idCheck && rec.aux_GetId() < 0)
						{
							idCheck = false;
						}

						pc.ProgressStep();
					}

					if (!idCheck && GKUtils.ShowQuestion(LangMan.LSList[471]) == DialogResult.Yes)
					{
						TreeTools.CorrectIds(tree);
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

		public enum TTreeWalkMode : byte
		{
			twmAll,
			twmFamily,
			twmAncestors,
			twmDescendants,
			twmNone
		}

		public static void TreeWalk(TGEDCOMIndividualRecord iRec, TTreeWalkMode aMode, TList aList)
		{
			if (iRec != null && aList.IndexOf(iRec) < 0)
			{
				aList.Add(iRec);

				if (aMode != TTreeWalkMode.twmNone)
				{
					if ((aMode == TTreeWalkMode.twmAll || aMode == TTreeWalkMode.twmAncestors) && iRec.ChildToFamilyLinks.Count > 0)
					{
						TGEDCOMFamilyRecord family = iRec.ChildToFamilyLinks[0].Family;

						TGEDCOMIndividualRecord rel_person;

						rel_person = family.Husband.Value as TGEDCOMIndividualRecord;
						TreeTools.TreeWalk(rel_person, aMode, aList);

						rel_person = (family.Wife.Value as TGEDCOMIndividualRecord);
						TreeTools.TreeWalk(rel_person, aMode, aList);
					}

					if (aMode < TTreeWalkMode.twmAncestors || aMode == TTreeWalkMode.twmDescendants)
					{
						int num = iRec.SpouseToFamilyLinks.Count - 1;
						for (int i = 0; i <= num; i++)
						{
							TGEDCOMFamilyRecord family = iRec.SpouseToFamilyLinks[i].Family;

							TGEDCOMPointer sp = ((iRec.Sex == TGEDCOMSex.svMale) ? family.Wife : family.Husband);

							TTreeWalkMode int_mode = ((aMode == TTreeWalkMode.twmAll) ? TTreeWalkMode.twmAll : TTreeWalkMode.twmNone);

							TGEDCOMIndividualRecord rel_person = sp.Value as TGEDCOMIndividualRecord;

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
								rel_person = (family.Childrens[j].Value as TGEDCOMIndividualRecord);
								TreeTools.TreeWalk(rel_person, int_mode, aList);
							}
						}
					}
				}
			}
		}

		#endregion

		#region Tree Merge

		public static void TreeMerge(TGEDCOMTree mainTree, string fileName, TextBox logBox)
		{
			if (logBox != null)
			{
				logBox.Clear();
				logBox.AppendText(string.Format(LangMan.LSList[472], mainTree.RecordsCount.ToString()) + "\r\n");
			}

			TGEDCOMTree extTree = new TGEDCOMTree();

			XRefReplacer repMap = new XRefReplacer();
			try
			{
				extTree.LoadFromFile(fileName);
				extTree.Header.Clear();
				while (extTree.RecordsCount > 0)
				{
					TGEDCOMRecord rec = extTree.Extract(0);
					string newXRef = mainTree.XRefIndex_NewXRef(rec);
					repMap.AddXRef(rec, rec.XRef, newXRef);
					rec.XRef = newXRef;
					rec.ResetOwner(mainTree);
					mainTree.AddRecord(rec);
				}

				int num = repMap.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMRecord rec = repMap[i].Rec;
					rec.ReplaceXRefs(repMap);
				}

				if (logBox != null)
				{
					logBox.AppendText(string.Format(LangMan.LSList[472], mainTree.RecordsCount.ToString()) + "\r\n");
				}
			}
			finally
			{
				repMap.Free();
				extTree.Dispose();
			}
		}

		#endregion

		#region Base Checks

		public enum TCheckDiag : byte
		{
			cdPersonLonglived,
			cdPersonSexless,
			cdLiveYearsInvalid,
			cdStrangeSpouse,
			cdStrangeParent,
			cdEmptyFamily
		}

		public enum TCheckSolve : byte
		{
			csSkip,
			csSetIsDead,
			csDefineSex,
			csRemove
		}

		public class TCheckObj
		{
			public string Comment;
			public TCheckDiag Diag;
			public TGEDCOMRecord Rec;
			public TCheckSolve Solve;

			public TCheckObj(TGEDCOMRecord rec, TCheckDiag diag, TCheckSolve solve)
			{
				this.Rec = rec;
				this.Diag = diag;
				this.Solve = solve;
			}

			public string GetRecordName()
			{
				string Result = "[" + this.Rec.XRef + "] ";

				switch (this.Rec.RecordType)
				{
					case TGEDCOMRecordType.rtIndividual:
						Result = Result + (this.Rec as TGEDCOMIndividualRecord).aux_GetNameStr(true, false);
						break;

					case TGEDCOMRecordType.rtFamily:
						Result = Result + GKUtils.aux_GetFamilyStr(this.Rec as TGEDCOMFamilyRecord);
						break;
				}

				return Result;
			}

			public void Free()
			{
				SysUtils.Free(this);
			}
		}

		private static void CheckIndividualRecord(TGEDCOMIndividualRecord iRec, TList aChecksList)
		{
			int iAge;
			if (GKUtils.GetIndividualEvent(iRec, "DEAT") == null)
			{
				string age = GKUtils.GetAge(iRec, -1);
				if (age != "" && age != "?")
				{
					iAge = int.Parse(age);
					if (iAge >= 130)
					{
						TCheckObj checkObj = new TCheckObj(iRec, TCheckDiag.cdPersonLonglived, TCheckSolve.csSetIsDead);
						checkObj.Comment = string.Format(LangMan.LSList[582], age);
						aChecksList.Add(checkObj);
					}
				}
			}

			TGEDCOMSex sex = iRec.Sex;
			if (sex < TGEDCOMSex.svMale || sex >= TGEDCOMSex.svUndetermined)
			{
				TCheckObj checkObj = new TCheckObj(iRec, TCheckDiag.cdPersonSexless, TCheckSolve.csDefineSex);
				checkObj.Comment = LangMan.LSList[583];
				aChecksList.Add(checkObj);
			}

			bool yBC1, yBC2;
			int y_birth = GKUtils.GetIndependentYear(iRec, "BIRT", out yBC1);
			int y_death = GKUtils.GetIndependentYear(iRec, "DEAT", out yBC2);
			int delta = (y_death - y_birth);
			if (y_birth > -1 && y_death > -1 && delta < 0 && !yBC2)
			{
				TCheckObj checkObj = new TCheckObj(iRec, TCheckDiag.cdLiveYearsInvalid, TCheckSolve.csSkip);
				checkObj.Comment = LangMan.LSList[584];
				aChecksList.Add(checkObj);
			}

			iAge = TreeStats.GetMarriageAge(iRec);
			if (iAge > 0 && (iAge <= 13 || iAge >= 50))
			{
				TCheckObj checkObj = new TCheckObj(iRec, TCheckDiag.cdStrangeSpouse, TCheckSolve.csSkip);
				checkObj.Comment = string.Format(LangMan.LSList[585], iAge.ToString());
				aChecksList.Add(checkObj);
			}

			TGEDCOMIndividualRecord iDummy;
			iAge = TreeStats.GetFirstbornAge(iRec, out iDummy);
			if (iAge > 0 && (iAge <= 13 || iAge >= 50))
			{
				TCheckObj checkObj = new TCheckObj(iRec, TCheckDiag.cdStrangeParent, TCheckSolve.csSkip);
				checkObj.Comment = string.Format(LangMan.LSList[586], iAge.ToString());
				aChecksList.Add(checkObj);
			}
		}

		private static void CheckFamilyRecord(TGEDCOMFamilyRecord fRec, TList aChecksList)
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

		public static void CheckBase(TGEDCOMTree aTree, TList aChecksList)
		{
			try
			{
				TfmProgress.ProgressInit(aTree.RecordsCount, LangMan.LSList[517]);
				aChecksList.Clear();

				int num = aTree.RecordsCount - 1;
				for (int i = 0; i <= num; i++)
				{
					TfmProgress.ProgressStep();
					TGEDCOMRecord rec = aTree[i];

					switch (rec.RecordType) {
						case TGEDCOMRecordType.rtIndividual:
							CheckIndividualRecord(rec as TGEDCOMIndividualRecord, aChecksList);
							break;

						case TGEDCOMRecordType.rtFamily:
							CheckFamilyRecord(rec as TGEDCOMFamilyRecord, aChecksList);
							break;
					}
				}
			}
			finally
			{
				TfmProgress.ProgressDone();
			}
		}

		public static void RepairProblem(TGEDCOMTree aTree, TCheckObj checkObj)
		{
			switch (checkObj.Diag) {
				case TreeTools.TCheckDiag.cdPersonLonglived:
					{
						TGEDCOMIndividualRecord iRec = checkObj.Rec as TGEDCOMIndividualRecord;
						GKUtils.CreateEventEx(aTree, iRec, "DEAT", "", "");
						//this.Base.ChangeRecord(iRec);
						break;
					}

				case TreeTools.TCheckDiag.cdPersonSexless:
					{
						TGEDCOMIndividualRecord iRec = checkObj.Rec as TGEDCOMIndividualRecord;
						TfmSexCheck.CheckPersonSex(iRec, GKUI.TfmGEDKeeper.Instance.NamesTable);
						//this.Base.ChangeRecord(iRec);
						break;
					}

				case TreeTools.TCheckDiag.cdEmptyFamily:
					{
						aTree.DeleteRecord(checkObj.Rec);
						break;
					}
			}
		}

		#endregion

		#region Tree Split

		private static void _CheckRelations_AddRel(TList splitList, TGEDCOMRecord aRec)
		{
			if (splitList.IndexOf(aRec) < 0)
			{
				splitList.Add(aRec);
			}
		}

		private static void _CheckRelations_CheckRecord(TList splitList, TGEDCOMRecord rec)
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

		private static void _CheckRelations_CheckTag(TList splitList, TGEDCOMTagWithLists tag)
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

		private static void _CheckRelations_CheckIndividual(TList splitList, TGEDCOMIndividualRecord iRec)
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

		private static void _CheckRelations_CheckFamily(TList splitList, TGEDCOMFamilyRecord fRec)
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

		private static void _CheckRelations_CheckSource(TList splitList, TGEDCOMSourceRecord sRec)
		{
			_CheckRelations_CheckRecord(splitList, sRec);

			int num = sRec.RepositoryCitations.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				_CheckRelations_AddRel(splitList, sRec.RepositoryCitations[i].Value);
			}
		}

		public static void CheckRelations(TList splitList)
		{
			int num = splitList.Count;
			for (int i = 0; i < num; i++)
			{
				TGEDCOMRecord rec = splitList[i] as TGEDCOMRecord;
				switch (rec.RecordType)
				{
					case TGEDCOMRecordType.rtIndividual:
					{
						_CheckRelations_CheckIndividual(splitList, rec as TGEDCOMIndividualRecord);
						break;
					}
					case TGEDCOMRecordType.rtFamily:
					{
						_CheckRelations_CheckFamily(splitList, rec as TGEDCOMFamilyRecord);
						break;
					}
					case TGEDCOMRecordType.rtNote:
					{
						_CheckRelations_CheckRecord(splitList, rec);
						break;
					}
					case TGEDCOMRecordType.rtMultimedia:
					{
						_CheckRelations_CheckRecord(splitList, rec);
						break;
					}
					case TGEDCOMRecordType.rtSource:
					{
						_CheckRelations_CheckSource(splitList, rec as TGEDCOMSourceRecord);
						break;
					}
					case TGEDCOMRecordType.rtRepository:
					{
						_CheckRelations_CheckRecord(splitList, rec);
						break;
					}
					case TGEDCOMRecordType.rtSubmitter:
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
			public TGEDCOMIndividualRecord iRec;
		}

		public static List<ULIndividual> GetUnlinkedNamesakes(TGEDCOMTree tree, IProgressController pc)
		{
			List<ULIndividual> result = new List<ULIndividual>();

			Hashtable families = new Hashtable();

			pc.ProgressInit(tree.RecordsCount, "Этап 1");

			// составить таблицу фамилий и персон, относящихся к этим фамилиям
			for (int i = 0; i < tree.RecordsCount; i++)
			{
				TGEDCOMRecord rec = tree[i];

				if (rec is TGEDCOMIndividualRecord)
				{
					TGEDCOMIndividualRecord iRec = rec as TGEDCOMIndividualRecord;

					string[] fams = NamesTable.GetSurnames(iRec);
					for (int k = 0; k < fams.Length; k++)
					{
						string f = fams[k];
						if (f.Length > 1)
						{
							List<TGEDCOMIndividualRecord> ps = (List<TGEDCOMIndividualRecord>)families[f];
							if (ps == null) {
								ps = new List<TGEDCOMIndividualRecord>();
								families[f] = ps;
							}
							ps.Add(iRec);
						}
					}
				}

				pc.ProgressStep();
			}

			pc.ProgressInit(families.Count, "Этап 2");
			
			// найти всех персон одной фамилии, не связанных узами родства
			foreach (DictionaryEntry entry in families)
			{
				string fam = (string)entry.Key;
				List<TGEDCOMIndividualRecord> ps = (List<TGEDCOMIndividualRecord>)entry.Value;

				int i = 0;
				while (i < ps.Count)
				{
					TGEDCOMIndividualRecord iRec = ps[i];

					using (TList lst = new TList())
					{
						TreeTools.TreeWalk(iRec, TreeTools.TTreeWalkMode.twmAll, lst);
						for (int k = 0; k < lst.Count; k++)
						{
							TGEDCOMIndividualRecord item = lst[k] as TGEDCOMIndividualRecord;
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

		public delegate void DuplicateFoundFunc(TGEDCOMIndividualRecord indivA, TGEDCOMIndividualRecord indivB);

		public static void FindDuplicates(TGEDCOMTree tree_A, TGEDCOMTree tree_B, float matchThreshold, 
		                                  DuplicateFoundFunc foundFunc, IProgressController pc)
		{		
			TGEDCOMRecord.MatchParams mParams;
			mParams.IndistinctNameMatching = true;
			mParams.IndistinctThreshold = 90 / 100.0;
			mParams.CheckBirthYear = true;
			mParams.YearInaccuracy = 3;
			mParams.RusNames = true;

			pc.ProgressInit(tree_A.RecordsCount, "Поиск дубликатов");
			try
			{
				for (int i = 0; i <= tree_A.RecordsCount - 1; i++) {
					TGEDCOMRecord recA = tree_A[i];
					if (recA is TGEDCOMIndividualRecord) {
						for (int k = 0; k <= tree_B.RecordsCount - 1; k++) {
							TGEDCOMRecord recB = tree_B[k];
							if (recB is TGEDCOMIndividualRecord) {
								TGEDCOMIndividualRecord indivA = recA as TGEDCOMIndividualRecord;
								TGEDCOMIndividualRecord indivB = recB as TGEDCOMIndividualRecord;

								if (indivA != indivB && indivA.aux_IsMatch(indivB, matchThreshold, mParams)) {
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

		public static void TreeCompare(TGEDCOMTree mainTree, string fileName, TextBox logBox)
		{
			TGEDCOMTree tempTree = new TGEDCOMTree();
			tempTree.LoadFromFile(fileName);

			StringList fams = new StringList();
			StringList names = new StringList();

			try
			{
				logBox.AppendText(LangMan.LSList[520] + "\r\n");

				int num = mainTree.RecordsCount - 1;
				for (int i = 0; i <= num; i++)
				{
					if (mainTree[i] is TGEDCOMIndividualRecord)
					{
						TGEDCOMIndividualRecord iRec = mainTree[i] as TGEDCOMIndividualRecord;

						int idx = names.AddObject(iRec.aux_GetNameStr(true, false), new TList());
						(names.GetObject(idx) as TList).Add(iRec);

						string fam, nam, pat;
						iRec.aux_GetNameParts(out fam, out nam, out pat);

						fams.AddObject(NamesTable.PrepareRusSurname(fam, iRec.Sex == TGEDCOMSex.svFemale), null);
					}
				}

				int num2 = tempTree.RecordsCount - 1;
				for (int i = 0; i <= num2; i++)
				{
					if (tempTree[i] is TGEDCOMIndividualRecord)
					{
						TGEDCOMIndividualRecord iRec = tempTree[i] as TGEDCOMIndividualRecord;

						string tm = iRec.aux_GetNameStr(true, false);
						int idx = names.IndexOf(tm);
						if (idx >= 0)
						{
							(names.GetObject(idx) as TList).Add(iRec);
						}

						string fam, nam, pat;
						iRec.aux_GetNameParts(out fam, out nam, out pat);

						tm = NamesTable.PrepareRusSurname(fam, iRec.Sex == TGEDCOMSex.svFemale);
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
					if ((names.GetObject(i) as TList).Count == 1)
					{
						(names.GetObject(i) as TList).Dispose();
						names.Delete(i);
					}
				}

				if (fams.Count != 0)
				{
					logBox.AppendText(LangMan.LSList[576] + "\r\n");

					int num3 = fams.Count - 1;
					for (int i = 0; i <= num3; i++)
					{
						logBox.AppendText("    " + fams[i] + "\r\n");
					}
				}

				if (names.Count != 0)
				{
					logBox.AppendText(LangMan.LSList[577] + "\r\n");

					int num4 = names.Count - 1;
					for (int i = 0; i <= num4; i++)
					{
						logBox.AppendText("    " + names[i] + "\r\n");
						TList lst = names.GetObject(i) as TList;

						int num5 = lst.Count - 1;
						for (int j = 0; j <= num5; j++)
						{
							TGEDCOMIndividualRecord iRec = lst[j] as TGEDCOMIndividualRecord;
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

				names.Free();
				fams.Free();

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

		private static void _CheckPlaces_PrepareEvent(StringList placesList, TGEDCOMCustomEvent aEvent)
		{
			string place_str = aEvent.Detail.Place.StringValue;
			if (place_str != "")
			{
				TGEDCOMLocationRecord loc = aEvent.Detail.Place.Location.Value as TGEDCOMLocationRecord;
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

		public static void PlacesSearch(TGEDCOMTree tree, StringList placesList, IProgressController pc)
		{
			PlacesSearch_Clear(placesList);

			pc.ProgressInit(tree.RecordsCount, LangMan.LSList[590]);

			int num = tree.RecordsCount - 1;
			for (int i = 0; i <= num; i++)
			{
				pc.ProgressStep();

				TGEDCOMRecord record = tree[i];

				if (record is TGEDCOMIndividualRecord)
				{
					TGEDCOMIndividualRecord iRec = record as TGEDCOMIndividualRecord;

					int num2 = iRec.IndividualEvents.Count - 1;
					for (int j = 0; j <= num2; j++)
					{
						_CheckPlaces_PrepareEvent(placesList, iRec.IndividualEvents[j]);
					}
				}
				else
				{
					if (record is TGEDCOMFamilyRecord)
					{
						TGEDCOMFamilyRecord fRec = record as TGEDCOMFamilyRecord;

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
