
using System;
using Ext.Utils;
using GedCom551;
using GKCore;

namespace GKUI
{

	public static class UIUtils
	{
		static UIUtils()
		{
		}

		public static void ShowAddressSummary(TGEDCOMAddress address, StringList summary)
		{
			if (!address.IsEmpty() && summary != null)
			{
				summary.Add("    " + LangMan.LSList[82] + ":");

				string ts = "";
				if (address.AddressCountry != "")
				{
					ts = ts + address.AddressCountry + ", ";
				}
				if (address.AddressState != "")
				{
					ts = ts + address.AddressState + ", ";
				}
				if (address.AddressCity != "")
				{
					ts += address.AddressCity;
				}
				if (ts != "")
				{
					summary.Add("    " + ts);
				}

				ts = "";
				if (address.AddressPostalCode != "")
				{
					ts = ts + address.AddressPostalCode + ", ";
				}
				if (address.Address.Text.Trim() != "")
				{
					ts += address.Address.Text.Trim();
				}
				if (ts != "")
				{
					summary.Add("    " + ts);
				}

				int num = address.PhoneNumbers.Count - 1;
				for (int i = 0; i <= num; i++) {
					summary.Add("    " + address.PhoneNumbers[i].StringValue);
				}

				int num2 = address.EmailAddresses.Count - 1;
				for (int i = 0; i <= num2; i++) {
					summary.Add("    " + address.EmailAddresses[i].StringValue);
				}

				int num3 = address.WebPages.Count - 1;
				for (int i = 0; i <= num3; i++) {
					summary.Add("    " + address.WebPages[i].StringValue);
				}
			}
		}

		public static void ShowDetailCause(TGEDCOMEventDetail eventDetail, StringList summary)
		{
			string cause = GKUtils.GetEventCause(eventDetail);
			if (summary != null && cause != "")
			{
				summary.Add("    " + cause);
			}
		}

		public static void ShowDetailInfo(TGEDCOMEventDetail eventDetail, StringList summary)
		{
			if (summary != null && eventDetail.SourceCitations.Count != 0)
			{
				summary.Add("    " + LangMan.LSList[56] + " (" + eventDetail.SourceCitations.Count.ToString() + "):");

				int num = eventDetail.SourceCitations.Count - 1;
				for (int idx = 0; idx <= num; idx++)
				{
					TGEDCOMSourceCitation cit = eventDetail.SourceCitations[idx];
					TGEDCOMSourceRecord sourceRec = cit.Value as TGEDCOMSourceRecord;
					if (sourceRec != null)
					{
						string nm = "\"" + sourceRec.FiledByEntry + "\"";
						if (cit.Page != "")
						{
							nm = nm + ", " + cit.Page;
						}
						summary.Add("      " + GKUtils.HyperLink(sourceRec.XRef, nm, 0));
					}
				}
			}
		}

		public static void ShowEvent(TGEDCOMRecord aSubject, StringList aToList, TGEDCOMRecord aRec, TGEDCOMCustomEvent evt)
		{
			if (aSubject is TGEDCOMNoteRecord)
			{
				int num = evt.Detail.Notes.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					if (evt.Detail.Notes[i].Value == aSubject)
					{
						UIUtils.ShowLink(aSubject, aToList, aRec, evt, null);
					}
				}
			}
			else
			{
				if (aSubject is TGEDCOMMultimediaRecord)
				{
					int num2 = evt.Detail.MultimediaLinks.Count - 1;
					for (int i = 0; i <= num2; i++)
					{
						if (evt.Detail.MultimediaLinks[i].Value == aSubject)
						{
							UIUtils.ShowLink(aSubject, aToList, aRec, evt, null);
						}
					}
				}
				else
				{
					if (aSubject is TGEDCOMSourceRecord)
					{
						int num3 = evt.Detail.SourceCitations.Count - 1;
						for (int i = 0; i <= num3; i++)
						{
							if (evt.Detail.SourceCitations[i].Value == aSubject)
							{
								UIUtils.ShowLink(aSubject, aToList, aRec, evt, evt.Detail.SourceCitations[i]);
							}
						}
					}
				}
			}
		}

		public static void ShowLink(TGEDCOMRecord aSubject, StringList aToList, TGEDCOMRecord aRec, TGEDCOMTag aTag, TGEDCOMPointer aExt)
		{
			string prefix;
			if (aSubject is TGEDCOMSourceRecord && aExt != null) {
                TGEDCOMSourceCitation cit = (aExt as TGEDCOMSourceCitation);
				if (cit.Page != "") {
					prefix = cit.Page + ": ";
				} else {
					prefix = "";
				}
			} else {
				prefix = "";
			}

			string suffix;
			if (aTag != null && aTag is TGEDCOMCustomEvent) {
				suffix = ", " + GKUtils.GetEventName(aTag as TGEDCOMCustomEvent).ToLower();
			} else {
				suffix = "";
			}
			aToList.Add("    " + prefix + GKUtils.GenRecordLink(aRec, true) + suffix);
		}

		public static void ShowPersonExtInfo(TGEDCOMTree tree, TGEDCOMIndividualRecord iRec, StringList summary)
		{
        	summary.Add("");
        	for (int i = 0, count = tree.RecordsCount; i < count; i++) {
        		TGEDCOMRecord rec = tree[i];
        		if (rec.RecordType == TGEDCOMRecordType.rtIndividual) {
        			TGEDCOMIndividualRecord ir = rec as TGEDCOMIndividualRecord;
        			
        			TGEDCOMAssociation asso;
        			bool first = true;
        			for (int k = 0, cnt = ir.Associations.Count; k < cnt; k++) {
        				asso = ir.Associations[k];
        				if (asso.Individual == iRec) {
        					if (first) {
        						summary.Add(LangMan.LS(LSID.LSID_Associations) + ":");
        						first = false;
        					}
        					summary.Add("    " + GKUtils.HyperLink(ir.XRef, ir.aux_GetNameStr(true, false), 0));
        				}
        			}
        		}
        	}
		}

		public static void ShowPersonNamesakes(TGEDCOMTree tree, TGEDCOMIndividualRecord iRec, StringList summary)
        {
            try
            {
                StringList namesakes = new StringList();
                try
                {
                    string st = iRec.aux_GetNameStr(true, false);

                    int num3 = tree.RecordsCount - 1;
                    for (int i = 0; i <= num3; i++)
                    {
                        TGEDCOMRecord rec = tree[i];
                        if (rec is TGEDCOMIndividualRecord && !object.Equals(rec, iRec))
                        {
                            TGEDCOMIndividualRecord rel_person = rec as TGEDCOMIndividualRecord;
                            string unk = rel_person.aux_GetNameStr(true, false);
                            if (st == unk)
                            {
                                namesakes.AddObject(unk + GKUtils.GetLifeStr(rel_person), rel_person);
                            }
                        }
                    }

                    if (namesakes.Count > 0)
                    {
                        summary.Add("");
                        summary.Add(LangMan.LSList[238] + ":");

                        int num4 = namesakes.Count - 1;
                        for (int i = 0; i <= num4; i++)
                        {
                            TGEDCOMIndividualRecord rel_person = namesakes.GetObject(i) as TGEDCOMIndividualRecord;
                            summary.Add("    " + GKUtils.HyperLink(rel_person.XRef, namesakes[i], 0));
                        }
                    }
                }
                finally
                {
                    namesakes.Free();
                }
            }
            catch (Exception E)
            {
                SysUtils.LogWrite("UIUtils.ShowPersonNamesakes(): " + E.Message);
            }
        }

		public static void ShowSubjectLinks(TGEDCOMRecord aInRecord, TGEDCOMRecord subject, StringList aToList)
		{
			try
			{
				int num;

				if (subject is TGEDCOMNoteRecord) {
					num = aInRecord.Notes.Count - 1;
					for (int i = 0; i <= num; i++) {
						if (object.Equals(aInRecord.Notes[i].Value, subject)) {
							UIUtils.ShowLink(subject, aToList, aInRecord, null, null);
						}
					}
				} else if (subject is TGEDCOMMultimediaRecord) {
					num = aInRecord.MultimediaLinks.Count - 1;
					for (int i = 0; i <= num; i++) {
						if (object.Equals(aInRecord.MultimediaLinks[i].Value, subject)) {
							UIUtils.ShowLink(subject, aToList, aInRecord, null, null);
						}
					}
				} else if (subject is TGEDCOMSourceRecord) {
					num = aInRecord.SourceCitations.Count - 1;
					for (int i = 0; i <= num; i++) {
						if (object.Equals(aInRecord.SourceCitations[i].Value, subject)) {
							UIUtils.ShowLink(subject, aToList, aInRecord, null, aInRecord.SourceCitations[i]);
						}
					}
				}

				if (aInRecord is TGEDCOMIndividualRecord) {
					TGEDCOMIndividualRecord i_rec = aInRecord as TGEDCOMIndividualRecord;
					num = i_rec.IndividualEvents.Count - 1;
					for (int i = 0; i <= num; i++) {
						UIUtils.ShowEvent(subject, aToList, i_rec, i_rec.IndividualEvents[i]);
					}
				} else if (aInRecord is TGEDCOMFamilyRecord) {
					TGEDCOMFamilyRecord f_rec = aInRecord as TGEDCOMFamilyRecord;
					num = f_rec.FamilyEvents.Count - 1;
					for (int i = 0; i <= num; i++) {
						UIUtils.ShowEvent(subject, aToList, f_rec, f_rec.FamilyEvents[i]);
					}
				}
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("UIUtils.ShowSubjectLinks(): " + E.Message);
			}
		}

	}

}
