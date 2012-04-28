using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using System.Windows.Forms;

using Ext.Utils;
using GedCom551;
using GKUI;

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

	public class TreeTools
	{
		public TreeTools()
		{
		}

		#region Patriarchs Search

		private static int PatriarchsCompare(object Item1, object Item2)
		{
			return (Item1 as TPatriarchObj).IBirthYear - (Item2 as TPatriarchObj).IBirthYear;
		}

		public string GetPatriarchLinks(TList lst, TPatriarchObj pObj)
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
				int year = TGenEngine.GetIndependentYear(iRec, "BIRT");
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

		public static void GetPatriarchsList(TGEDCOMTree aTree, bool aProgress, bool aLinks, ref TList aList, int aMinGens, bool aDates = true)
		{
			if (aProgress) TfmProgress.ProgressInit(aTree.RecordsCount, LangMan.LSList[474]);

			TGenEngine.InitExtCounts(aTree, -1);
			try
			{
				int num = aTree.RecordsCount - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMRecord rec = aTree[i];
					if (rec is TGEDCOMIndividualRecord)
					{
						TGEDCOMIndividualRecord i_rec = (TGEDCOMIndividualRecord)rec;

						string nf, nn, np;
						i_rec.aux_GetNameParts(out nf, out nn, out np);

						int bYear = _GetPatriarchsList_GetBirthYear(i_rec);
						int descGens = TGenEngine.GetDescGenerations(i_rec);
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
							pObj.IDescendantsCount = TGenEngine.GetDescendantsCount(i_rec) - 1;
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

		private static void ReformNote(TGEDCOMTree aTree, TGEDCOMNotes note)
		{
			StringList strData = new StringList();
			try
			{
				strData.Text = note.Notes.Text;
				TGEDCOMNoteRecord noteRec = TGenEngine.CreateNoteEx(aTree, strData, null);
				note.Clear();
				note.Value = noteRec;
			}
			finally
			{
				strData.Free();
			}
		}

		private static void ReformMultimediaLink(TGEDCOMTree aTree, TGEDCOMMultimediaLink mmLink)
		{
			try
			{
				string title = mmLink.Title;
				TGEDCOMMultimediaRecord mmRec = new TGEDCOMMultimediaRecord(aTree, aTree, "", "");
				mmRec.InitNew();
				aTree.AddRecord(mmRec);

				int num = mmLink.FileReferences.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMFileReference fr = mmLink.FileReferences[i];
					TGEDCOMFileReferenceWithTitle frt = new TGEDCOMFileReferenceWithTitle(aTree, mmRec, "", "");
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

		private static void ReformSourceCitation(TGEDCOMTree aTree, TGEDCOMSourceCitation sourCit)
		{
		}

		private static void CheckRecord_PrepareTag([In] TGEDCOMTree aTree, TGEDCOMFormat aFormat, TGEDCOMTagWithLists tag)
		{
			int num = tag.MultimediaLinks.Count - 1;
			for (int i = 0; i <= num; i++) {
				TGEDCOMMultimediaLink mmLink = tag.MultimediaLinks[i];
				if (!mmLink.IsPointer) ReformMultimediaLink(aTree, mmLink);
			}

			num = tag.Notes.Count - 1;
			for (int i = 0; i <= num; i++) {
				TGEDCOMNotes note = tag.Notes[i];
				if (!note.IsPointer) ReformNote(aTree, note);
			}

			num = tag.SourceCitations.Count - 1;
			for (int i = 0; i <= num; i++) {
				TGEDCOMSourceCitation sourCit = tag.SourceCitations[i];
				if (!sourCit.IsPointer) ReformSourceCitation(aTree, sourCit);
			}
		}

		private static void CheckRecord_RepairTag([In] TGEDCOMTree aTree, TGEDCOMFormat aFormat, TGEDCOMTagWithLists tag)
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

		private static void CheckRecord_PreparePtr([In] TGEDCOMTree aTree, TGEDCOMFormat aFormat, TGEDCOMPointerWithNotes ptr)
		{
			int num = ptr.Notes.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				TGEDCOMNotes note = ptr.Notes[i];
				if (!note.IsPointer) ReformNote(aTree, note);
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

		private static void CheckRecord_AddUserRef([In] TGEDCOMTree aTree, TGEDCOMFormat aFormat, TGEDCOMIndividualRecord iRec, string reference)
		{
			TGEDCOMUserReference uRef = new TGEDCOMUserReference(aTree, iRec, "", "");
			uRef.StringValue = reference;
			iRec.UserReferences.Add(uRef);
		}

		private static void CheckRecord_AttrCompatible([In] TGEDCOMTree aTree, TGEDCOMFormat aFormat, TGEDCOMIndividualRecord iRec, TGEDCOMCustomEvent aEvent)
		{
			if (aEvent.Name == "_MILI")
			{
				string cause = aEvent.Detail.Classification.ToLower();

				if (cause.IndexOf("б/д") >= 0)
				{
					if (cause.IndexOf("+") >= 0)
					{
						CheckRecord_AddUserRef(aTree, aFormat, iRec, TGenEngine.UserRefs[3]);
					}
					else
					{
						CheckRecord_AddUserRef(aTree, aFormat, iRec, TGenEngine.UserRefs[2]);
					}

					aEvent.Detail.Classification = "";
				}
				else
				{
					if (cause.IndexOf("т/т") >= 0)
					{
						CheckRecord_AddUserRef(aTree, aFormat, iRec, TGenEngine.UserRefs[4]);
						aEvent.Detail.Classification = "";
					}
				}
			}
		}

		private static void CheckRecord_URefCompatible(TGEDCOMIndividualRecord iRec, TGEDCOMUserReference aUserRef)
		{
		}

		private static void CheckRecord_Individual([In] TGEDCOMTree aTree, TGEDCOMFormat aFormat, TGEDCOMIndividualRecord iRec)
		{
			int i;
			if (aFormat == TGEDCOMFormat.gf_Native)
			{
				int num = iRec.IndividualEvents.Count - 1;
				for (i = 0; i <= num; i++)
				{
					TGEDCOMCustomEvent evt = iRec.IndividualEvents[i];
					CheckRecord_EventPlace(evt);
					CheckRecord_AttrCompatible(aTree, aFormat, iRec, evt);
					CheckRecord_RepairTag(aTree, aFormat, evt.Detail);
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
					CheckRecord_PrepareTag(aTree, aFormat, iRec.IndividualEvents[i].Detail);
				}

				int num4 = iRec.ChildToFamilyLinks.Count - 1;
				for (i = 0; i <= num4; i++)
				{
					CheckRecord_PreparePtr(aTree, aFormat, iRec.ChildToFamilyLinks[i]);
				}

				int num5 = iRec.SpouseToFamilyLinks.Count - 1;
				for (i = 0; i <= num5; i++)
				{
					CheckRecord_PreparePtr(aTree, aFormat, iRec.SpouseToFamilyLinks[i]);
				}

				int num6 = iRec.Associations.Count - 1;
				for (i = 0; i <= num6; i++)
				{
					CheckRecord_PreparePtr(aTree, aFormat, iRec.Associations[i]);
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

		private static void CheckRecord_Family([In] TGEDCOMTree aTree, TGEDCOMFormat aFormat, TGEDCOMFamilyRecord fam)
		{
			int i;
			if (aFormat == TGEDCOMFormat.gf_Native)
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
					TreeTools.CheckRecord_PrepareTag(aTree, aFormat, fam.FamilyEvents[i].Detail);
				}
			}

			for (i = fam.Childrens.Count - 1; i >= 0; i--)
			{
				if (fam.Childrens[i].Value == null)
					fam.Childrens.Delete(i);
			}

			fam.SortChilds();
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

		public static void CheckRecord(TGEDCOMTree aTree, TGEDCOMRecord aRec, TGEDCOMFormat aFormat)
		{
			if (aRec.UID == null || aRec.UID == "")
			{
				aRec.NewUID();
			}

			if (aFormat != TGEDCOMFormat.gf_Native)
			{
				int num = aRec.MultimediaLinks.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMMultimediaLink mmLink = aRec.MultimediaLinks[i];
					if (!mmLink.IsPointer) TreeTools.ReformMultimediaLink(aTree, mmLink);
				}

				num = aRec.Notes.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMNotes note = aRec.Notes[i];
					if (!note.IsPointer) TreeTools.ReformNote(aTree, note);
				}

				num = aRec.SourceCitations.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMSourceCitation sourCit = aRec.SourceCitations[i];
					if (!sourCit.IsPointer) TreeTools.ReformSourceCitation(aTree, sourCit);
				}
			}

			switch (aRec.RecordType) {
				case TGEDCOMRecordType.rtIndividual:
					CheckRecord_Individual(aTree, aFormat, (TGEDCOMIndividualRecord)aRec);
					break;
				case TGEDCOMRecordType.rtFamily:
					CheckRecord_Family(aTree, aFormat, (TGEDCOMFamilyRecord)aRec);
					break;
				case TGEDCOMRecordType.rtGroup:
					CheckRecord_Group((TGEDCOMGroupRecord)aRec);
					break;
			}
		}

		public static void CheckHeader(TGEDCOMTree aTree, TGEDCOMFormat format)
		{
			if (format == TGEDCOMFormat.gf_Native)
			{
				TGEDCOMHeader header = aTree.Header;
				TGEDCOMTag tag;

				tag = header.FindTag("_ADVANCED", 0);
				if (tag != null) header.DeleteTag("_ADVANCED");

				tag = header.FindTag("_EXT_NAME", 0);
				if (tag != null) header.DeleteTag("_EXT_NAME");
			}
		}

		private static void CorrectIds(TGEDCOMTree aTree)
		{
			TfmProgress.ProgressInit(aTree.RecordsCount, LangMan.LSList[469]);
			TXRefReplaceMap repMap = new TXRefReplaceMap();
			try
			{
				int num = aTree.RecordsCount - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMRecord rec = aTree[i];
					if (TGenEngine.GetId(rec) < 0)
					{
						string newXRef = aTree.XRefIndex_NewXRef(rec);
						repMap.AddXRef(rec, rec.XRef, newXRef);
						rec.XRef = newXRef;
					}
					TfmProgress.ProgressStep();
				}

				aTree.Header.ReplaceXRefs(repMap);
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

		public static bool CheckGEDCOMFormat(TGEDCOMTree aTree)
		{
			bool result = false;

			try
			{
				TfmProgress.ProgressInit(aTree.RecordsCount, LangMan.LSList[470]);
				try
				{
					TGEDCOMFormat format = TGenEngine.GetGEDCOMFormat(aTree);
					bool idCheck = true;

					TreeTools.CheckHeader(aTree, format);

					int num = aTree.RecordsCount - 1;
					for (int i = 0; i <= num; i++)
					{
						TGEDCOMRecord rec = aTree[i];
						TreeTools.CheckRecord(aTree, rec, format);

						if (format != TGEDCOMFormat.gf_Native && idCheck && TGenEngine.GetId(rec) < 0)
						{
							idCheck = false;
						}

						TfmProgress.Progress = i;
					}

					if (!idCheck && TGenEngine.ShowQuestion(LangMan.LSList[471]) == DialogResult.Yes)
					{
						TreeTools.CorrectIds(aTree);
					}

					result = true;
				}
				finally
				{
					TfmProgress.ProgressDone();
				}
			}
			catch (Exception ex)
			{
				SysUtils.LogWrite("TreeTools.CheckGEDCOMFormat(): " + ex.Message);
				TGenEngine.ShowError(LangMan.LS(LSID.LSID_CheckGedComFailed));
			}

			return result;
		}


		#endregion

		#region Tree Utils

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

		public static void TreeMerge(TGEDCOMTree aMainTree, string aFileName, TextBox aLog)
		{
			if (aLog != null)
			{
				aLog.Clear();
				aLog.AppendText(string.Format(LangMan.LSList[472], aMainTree.RecordsCount.ToString()) + "\r\n");
			}

			TGEDCOMTree extTree = new TGEDCOMTree();

			TXRefReplaceMap repMap = new TXRefReplaceMap();
			try
			{
				extTree.LoadFromFile(aFileName);
				extTree.Header.Clear();
				while (extTree.RecordsCount > 0)
				{
					TGEDCOMRecord rec = extTree.Extract(0);
					string newXRef = aMainTree.XRefIndex_NewXRef(rec);
					repMap.AddXRef(rec, rec.XRef, newXRef);
					rec.XRef = newXRef;
					rec.ResetOwner(aMainTree);
					aMainTree.AddRecord(rec);
				}

				int num = repMap.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMRecord rec = repMap[i].Rec;
					rec.ReplaceXRefs(repMap);
				}

				if (aLog != null)
				{
					aLog.AppendText(string.Format(LangMan.LSList[472], aMainTree.RecordsCount.ToString()) + "\r\n");
				}
			}
			finally
			{
				repMap.Free();
				extTree.Dispose();
			}
		}

		public enum TSyncState : byte
		{
			ssUndefined,
			ssHasMaster,
			ssNoMaster
		}

		public class TSyncRec
		{
			public TGEDCOMRecord MasterRecord;
			public TGEDCOMRecord UpdateRecord;
			public TSyncState State;
			public string UpdateOldXRef;
			public string UpdateNewXRef;

			public void Free()
			{
				SysUtils.Free(this);
			}
		}

		public static void TreeSync(TGEDCOMTree aMainTree, string aFileName, TextBox aLog)
		{
			aLog.Clear();
			TGEDCOMTree extTree = new TGEDCOMTree();
			TXRefReplaceMap repMap = new TXRefReplaceMap();
			TList sync_list = new TList(true);
			try
			{
				extTree.LoadFromFile(aFileName);
				extTree.Header.Clear();

				TreeTools.CheckGEDCOMFormat(extTree);

				int num = extTree.RecordsCount - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMRecord rec = extTree[i];
					sync_list.Add(new TSyncRec
					{
						MasterRecord = null, 
						UpdateRecord = rec, 
						State = TSyncState.ssUndefined, 
						UpdateOldXRef = "", 
						UpdateNewXRef = ""
					});
				}

				int num2 = sync_list.Count - 1;
				for (int i = 0; i <= num2; i++)
				{
					TSyncRec sync_rec = sync_list[i] as TSyncRec;
					TGEDCOMRecord rec = aMainTree.FindUID(sync_rec.UpdateRecord.UID);
					if (rec != null)
					{
						sync_rec.MasterRecord = rec;
						sync_rec.State = TSyncState.ssHasMaster;
					}
					else
					{
						sync_rec.State = TSyncState.ssNoMaster;
						rec = extTree.Extract(extTree.IndexOfRecord(sync_rec.UpdateRecord));
						string newXRef = aMainTree.XRefIndex_NewXRef(rec);
						repMap.AddXRef(rec, rec.XRef, newXRef);
						rec.XRef = newXRef;
						rec.ResetOwner(aMainTree);
						aMainTree.AddRecord(rec);
					}
				}

				int num3 = repMap.Count - 1;
				for (int i = 0; i <= num3; i++)
				{
					TGEDCOMRecord rec = repMap[i].Rec;
					rec.ReplaceXRefs(repMap);
				}

				int num4 = extTree.RecordsCount - 1;
				for (int i = 0; i <= num4; i++)
				{
					TGEDCOMRecord rec = extTree[i];
					rec.ReplaceXRefs(repMap);
				}

				int num5 = sync_list.Count - 1;
				for (int i = 0; i <= num5; i++)
				{
					TSyncRec sync_rec = sync_list[i] as TSyncRec;
					if (sync_rec.State == TSyncState.ssHasMaster)
					{
						TGEDCOMRecord rec = extTree.Extract(extTree.IndexOfRecord(sync_rec.UpdateRecord));
						rec.XRef = aMainTree.XRefIndex_NewXRef(rec);
						rec.ResetOwner(aMainTree);
						aMainTree.AddRecord(rec);
						string backUID = sync_rec.MasterRecord.UID;
						sync_rec.UpdateRecord.MoveTo(sync_rec.MasterRecord, true);
						sync_rec.MasterRecord.UID = backUID;
						aMainTree.DeleteRecord(rec);
					}
				}

				aLog.AppendText(LangMan.LSList[473] + "\r\n");
			}
			finally
			{
				sync_list.Dispose();
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

			public string RecName
			{
				get	{ return this.GetRecName(); }
			}

			private string GetRecName()
			{
				string Result = "[" + this.Rec.XRef + "] ";
				switch (this.Rec.RecordType) {
					case TGEDCOMRecordType.rtIndividual:
						Result = Result + (this.Rec as TGEDCOMIndividualRecord).aux_GetNameStr(true, false);
						break;
					case TGEDCOMRecordType.rtFamily:
						Result = Result + TGenEngine.aux_GetFamilyStr(this.Rec as TGEDCOMFamilyRecord);
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
			if (TGenEngine.GetIndividualEvent(iRec, "DEAT") == null)
			{
				string age = TGenEngine.GetAge(iRec, -1);
				if (age != "" && age != "?")
				{
					iAge = int.Parse(age);
					if (iAge >= 130)
					{
						TCheckObj checkObj = new TCheckObj();
						checkObj.Rec = iRec;
						checkObj.Diag = TCheckDiag.cdPersonLonglived;
						checkObj.Solve = TCheckSolve.csSetIsDead;
						checkObj.Comment = string.Format(LangMan.LSList[582], age);
						aChecksList.Add(checkObj);
					}
				}
			}

			TGEDCOMSex sex = iRec.Sex;
			if (sex < TGEDCOMSex.svMale || sex >= TGEDCOMSex.svUndetermined)
			{
				TCheckObj checkObj = new TCheckObj();
				checkObj.Rec = iRec;
				checkObj.Diag = TCheckDiag.cdPersonSexless;
				checkObj.Solve = TCheckSolve.csDefineSex;
				checkObj.Comment = LangMan.LSList[583];
				aChecksList.Add(checkObj);
			}

			bool yBC1, yBC2;
			int y_birth = TGenEngine.GetIndependentYear(iRec, "BIRT", out yBC1);
			int y_death = TGenEngine.GetIndependentYear(iRec, "DEAT", out yBC2);
			int delta = (y_death - y_birth);
			if (y_birth > -1 && y_death > -1 && delta < 0 && !yBC2)
			{
				TCheckObj checkObj = new TCheckObj();
				checkObj.Rec = iRec;
				checkObj.Diag = TCheckDiag.cdLiveYearsInvalid;
				checkObj.Solve = TCheckSolve.csSkip;
				checkObj.Comment = LangMan.LSList[584];
				aChecksList.Add(checkObj);
			}

			iAge = TGenEngine.GetMarriageAge(iRec);
			if (iAge > 0 && (iAge <= 13 || iAge >= 50))
			{
				TCheckObj checkObj = new TCheckObj();
				checkObj.Rec = iRec;
				checkObj.Diag = TCheckDiag.cdStrangeSpouse;
				checkObj.Solve = TCheckSolve.csSkip;
				checkObj.Comment = string.Format(LangMan.LSList[585], iAge.ToString());
				aChecksList.Add(checkObj);
			}

			TGEDCOMIndividualRecord iDummy;
			iAge = TGenEngine.GetFirstbornAge(iRec, out iDummy);
			if (iAge > 0 && (iAge <= 13 || iAge >= 50))
			{
				TCheckObj checkObj = new TCheckObj();
				checkObj.Rec = iRec;
				checkObj.Diag = TCheckDiag.cdStrangeParent;
				checkObj.Solve = TCheckSolve.csSkip;
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
				TCheckObj checkObj = new TCheckObj();
				checkObj.Rec = fRec;
				checkObj.Diag = TCheckDiag.cdEmptyFamily;
				checkObj.Solve = TCheckSolve.csRemove;
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
						TGenEngine.CreateEventEx(aTree, iRec, "DEAT", "", "");
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

	}
}
