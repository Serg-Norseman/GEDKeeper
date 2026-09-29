/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
using System.Text.RegularExpressions;
using BSLib;
using GDModel;
using GDModel.Providers.GEDCOM;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Locales;
using GKCore.Options;

namespace GKCore
{
    public static class GKInfoPanel
    {
        public static string HyperLink(string xref, string text)
        {
            string result;

            if (!string.IsNullOrEmpty(xref) && string.IsNullOrEmpty(text)) {
                text = "???";
            }

            if (!string.IsNullOrEmpty(xref) && !string.IsNullOrEmpty(text)) {
                result = string.Concat("[url=", xref, "]", text, "[/url]");
            } else {
                result = "";
            }

            return result;
        }

        /// <summary>
        /// Wraps all hyperlinks in the text with substrings A and B.
        /// </summary>
        public static string MakeLinks(string text)
        {
            if (string.IsNullOrEmpty(text))
                return text;

            //const string pattern = @"(https?://[^""'\s]+)";
            const string pattern = @"(?<!\]|\=|\])(https?://[^""'\s]+)(?!\[|\])";

            return Regex.Replace(text, pattern, match => $"[url={match.Value}]{match.Value}[/url]");
        }

        public static string GenRecordLink(GDMTree tree, GDMRecord record, bool signed)
        {
            string result = "";

            if (record != null) {
                result = HyperLink(record.XRef, GKUtils.GetRecordName(tree, record, signed));
            }

            return result;
        }

        public static Tuple<string, string> GenRecordLinkTuple(GDMTree tree, GDMRecord record, bool signed, string prefix = "", string suffix = "")
        {
            if (record != null) {
                string recName = GKUtils.GetRecordName(tree, record, signed);
                string recLink = prefix + HyperLink(record.XRef, recName) + suffix;
                // prefix always sortable
                return new Tuple<string, string>(prefix + recName, recLink);
            } else {
                return new Tuple<string, string>(string.Empty, string.Empty);
            }
        }

        public static string GetCorresponderStr(GDMTree tree, GDMCommunicationRecord commRec, bool aLink)
        {
            if (tree == null)
                throw new ArgumentNullException(nameof(tree));

            if (commRec == null)
                throw new ArgumentNullException(nameof(commRec));

            string result = "";
            var corr = tree.GetPtrValue(commRec.Corresponder);

            if (corr != null) {
                string nm = GKUtils.GetNameString(corr, false);
                if (aLink) {
                    nm = HyperLink(corr.XRef, nm);
                }
                result = "[ " + LangMan.LS(GKData.CommunicationDirs[(int)commRec.CommDirection]) + " ] " + nm;
            }
            return result;
        }

        public static string GetAddressStr(GDMTree tree, GDMAddress addr)
        {
            return $"{LangMan.LS(LSID.Address)}: {addr.Lines.Text}";
        }

        public static void ShowAddressSummary(GDMAddress address, StringList summary)
        {
            if (address != null && !address.IsEmpty() && summary != null) {
                summary.Add("    " + LangMan.LS(LSID.Address) + ":");

                string ts = "";
                if (address.AddressCountry != "") {
                    ts = ts + address.AddressCountry + ", ";
                }
                if (address.AddressState != "") {
                    ts = ts + address.AddressState + ", ";
                }
                if (address.AddressCity != "") {
                    ts += address.AddressCity;
                }
                if (ts != "") {
                    summary.Add("    " + ts);
                }

                ts = "";
                if (address.AddressPostalCode != "") {
                    ts = ts + address.AddressPostalCode + ", ";
                }
                if (address.Lines.Text.Trim() != "") {
                    ts += address.Lines.Text.Trim();
                }
                if (ts != "") {
                    summary.Add("    " + ts);
                }

                for (int i = 0, num = address.PhoneNumbers.Count; i < num; i++) {
                    summary.Add("    " + address.PhoneNumbers[i].StringValue);
                }

                for (int i = 0, num = address.EmailAddresses.Count; i < num; i++) {
                    summary.Add("    " + address.EmailAddresses[i].StringValue);
                }

                for (int i = 0, num = address.WebPages.Count; i < num; i++) {
                    summary.Add("    " + MakeLinks(address.WebPages[i].StringValue));
                }
            }
        }

        public static void ShowDetailCause(GDMCustomEvent evt, StringList summary)
        {
            string cause = GKUtils.GetEventCause(evt);
            if (summary != null && !string.IsNullOrEmpty(cause)) {
                summary.Add("    " + cause);
            }
        }

        private static void ShowEvent(GDMTree tree, GDMRecord subject, LinksList linksList, GDMRecord record, GDMCustomEvent evt)
        {
            switch (subject.RecordType) {
                case GDMRecordType.rtNote:
                    if (evt.HasNotes) {
                        for (int i = 0, num = evt.Notes.Count; i < num; i++) {
                            if (evt.Notes[i].XRef == subject.XRef) {
                                ShowLink(tree, subject, linksList, record, evt, null);
                            }
                        }
                    }
                    break;

                case GDMRecordType.rtMultimedia:
                    if (evt.HasMultimediaLinks) {
                        for (int i = 0, num = evt.MultimediaLinks.Count; i < num; i++) {
                            if (evt.MultimediaLinks[i].XRef == subject.XRef) {
                                ShowLink(tree, subject, linksList, record, evt, null);
                            }
                        }
                    }
                    break;

                case GDMRecordType.rtSource:
                    if (evt.HasSourceCitations) {
                        for (int i = 0, num = evt.SourceCitations.Count; i < num; i++) {
                            var sourCit = evt.SourceCitations[i];
                            if (sourCit.XRef == subject.XRef) {
                                ShowLink(tree, subject, linksList, record, evt, sourCit);
                            }
                        }
                    }
                    break;
            }
        }

        private static void ShowLink(GDMTree tree, GDMRecord aSubject, LinksList linksList, GDMRecord record, GDMTag aTag, GDMPointer aExt)
        {
            string prefix = "    ";
            if (aSubject is GDMSourceRecord && aExt is GDMSourceCitation cit) {
                if (!string.IsNullOrEmpty(cit.Page)) {
                    prefix += cit.Page + ": ";
                }
            }

            string suffix;
            if (aTag is GDMCustomEvent evt) {
                suffix = ", " + GKUtils.GetEventNameLd(evt);
            } else {
                suffix = "";
            }

            linksList.Add(GenRecordLinkTuple(tree, record, true, prefix, suffix));
        }

        public static int FindLinkStr(StringList list, string link)
        {
            if (list != null) {
                for (int i = 0, num = list.Count; i < num; i++) {
                    if (list[i].Contains(link)) {
                        return i;
                    }
                }
            }
            return -1;
        }

        public static void ExpandExtInfo(BaseContext context, IHyperView sender, string linkName)
        {
            string xref = linkName.Remove(0, GKData.INFO_HREF_EXPAND_ASSO.Length);
            var iRec = context.Tree.FindXRef<GDMIndividualRecord>(xref);
            if (iRec != null) {
                int lineIdx = FindLinkStr(sender.Lines, linkName);
                var strList = new StringList();
                ShowPersonExtInfo(context.Tree, iRec, strList, false);
                sender.Lines.AddStrings(strList);
                sender.Lines.Delete(lineIdx);
            }
        }

        public static void ShowPersonExtInfo(GDMTree tree, GDMIndividualRecord iRec, StringList summary, bool checkOpt = true)
        {
            if (tree == null || iRec == null || summary == null) return;

            if (checkOpt && !GlobalOptions.Instance.ShowIndiAssociations) return;

            bool first = true;
            summary.Add("");
            int num = tree.RecordsCount;
            for (int i = 0; i < num; i++) {
                GDMRecord rec = tree[i];
                if (rec.RecordType != GDMRecordType.rtIndividual) continue;

                GDMIndividualRecord ir = (GDMIndividualRecord)rec;
                if (!ir.HasAssociations) continue;

                for (int k = 0, cnt = ir.Associations.Count; k < cnt; k++) {
                    GDMAssociation asso = ir.Associations[k];

                    if (asso.XRef == iRec.XRef) {
                        if (first) {
                            summary.Add(LangMan.LS(LSID.Associations) + ":");
                            first = false;
                        }
                        summary.Add("    " + asso.Relation + ", " + HyperLink(ir.XRef, GKUtils.GetNameString(ir, true, false)));
                    }
                }
            }
        }

        private static void ShowPersonNamesakes(GDMTree tree, GDMIndividualRecord iRec, StringList summary)
        {
            if (!GlobalOptions.Instance.ShowIndiNamesakes) return;

            try {
                var namesakes = new List<string>();
                string st = GKUtils.GetNameString(iRec, false);

                int num3 = tree.RecordsCount;
                for (int i = 0; i < num3; i++) {
                    GDMRecord rec = tree[i];
                    if (rec.RecordType != GDMRecordType.rtIndividual || rec == iRec) continue;

                    GDMIndividualRecord relPerson = (GDMIndividualRecord)rec;
                    string unk = GKUtils.GetNameString(relPerson, false);
                    if (st == unk) {
                        namesakes.Add(HyperLink(relPerson.XRef, unk + GKUtils.GetLifeStr(relPerson)));
                    }
                }

                if (namesakes.Count > 0) {
                    summary.Add("");
                    summary.Add(LangMan.LS(LSID.Namesakes) + ":");

                    int num4 = namesakes.Count;
                    for (int i = 0; i < num4; i++) {
                        summary.Add("    " + namesakes[i]);
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.ShowPersonNamesakes()", ex);
            }
        }

        private static void ShowSubjectLinks(GDMTree tree, GDMRecord subject, StringList summary, bool sort = false)
        {
            try {
                var subjectType = subject.RecordType;
                var linksList = new LinksList();

                for (int k = 0, num2 = tree.RecordsCount; k < num2; k++) {
                    GDMRecord record = tree[k];

                    switch (subjectType) {
                        case GDMRecordType.rtNote:
                            if (record.HasNotes) {
                                for (int i = 0, num = record.Notes.Count; i < num; i++) {
                                    if (record.Notes[i].XRef == subject.XRef) {
                                        ShowLink(tree, subject, linksList, record, null, null);
                                    }
                                }
                            }
                            break;

                        case GDMRecordType.rtMultimedia:
                            if (record.HasMultimediaLinks) {
                                for (int i = 0, num = record.MultimediaLinks.Count; i < num; i++) {
                                    if (record.MultimediaLinks[i].XRef == subject.XRef) {
                                        ShowLink(tree, subject, linksList, record, null, null);
                                    }
                                }
                            }
                            break;

                        case GDMRecordType.rtSource:
                            if (record.HasSourceCitations) {
                                for (int i = 0, num = record.SourceCitations.Count; i < num; i++) {
                                    var sourCit = record.SourceCitations[i];
                                    if (sourCit.XRef == subject.XRef) {
                                        ShowLink(tree, subject, linksList, record, null, sourCit);
                                    }
                                }
                            }
                            break;
                    }

                    if (record is GDMRecordWithEvents evsRec && evsRec.HasEvents) {
                        for (int i = 0, num = evsRec.Events.Count; i < num; i++) {
                            ShowEvent(tree, subject, linksList, evsRec, evsRec.Events[i]);
                        }
                    }
                }

                if (linksList.Count > 0) {
                    summary.Add("");
                    summary.Add(LangMan.LS(LSID.Links) + ":");

                    if (sort) {
                        linksList.Sort();
                    }

                    for (int j = 0, num3 = linksList.Count; j < num3; j++) {
                        summary.Add(linksList[j].Item2);
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.ShowSubjectLinks()", ex);
            }
        }

        public static void SearchRecordLinks(List<IGDMObject> linksList, GDMRecord inRecord, GDMRecord searchRec)
        {
            try {
                int num;
                switch (searchRec.RecordType) {
                    case GDMRecordType.rtNote:
                        num = inRecord.Notes.Count;
                        for (int i = 0; i < num; i++) {
                            var notes = inRecord.Notes[i];
                            if (notes.XRef == searchRec.XRef) {
                                linksList.Add(notes);
                            }
                        }
                        break;

                    case GDMRecordType.rtMultimedia:
                        num = inRecord.MultimediaLinks.Count;
                        for (int i = 0; i < num; i++) {
                            var mmLink = inRecord.MultimediaLinks[i];
                            if (mmLink.XRef == searchRec.XRef) {
                                linksList.Add(mmLink);
                            }
                        }
                        break;

                    case GDMRecordType.rtSource:
                        num = inRecord.SourceCitations.Count;
                        for (int i = 0; i < num; i++) {
                            var sourCit = inRecord.SourceCitations[i];
                            if (sourCit.XRef == searchRec.XRef) {
                                linksList.Add(sourCit);
                            }
                        }
                        break;
                }

                /*var recordWithEvents = aInRecord as GEDCOMRecordWithEvents;
                if (recordWithEvents != null) {
                    GEDCOMRecordWithEvents evsRec = recordWithEvents;

                    num = evsRec.Events.Count;
                    for (int i = 0; i < num; i++) {
                        ShowEvent(subject, linksList, evsRec, evsRec.Events[i]);
                    }
                }*/
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.SearchRecordLinks()", ex);
            }
        }

        public static void SearchRecordLinks(List<IGDMObject> linksList, GDMTree tree, GDMRecord searchRec)
        {
            int num = tree.RecordsCount;
            for (int i = 0; i < num; i++) {
                SearchRecordLinks(linksList, tree[i], searchRec);
            }
        }

        private static void RecListMediaRefresh(GDMTree tree, IGDMStructWithMultimediaLinks structWML, StringList summary, string indent = "")
        {
            if (structWML == null || summary == null) return;

            try {
                if (structWML.HasMultimediaLinks) {
                    summary.Add("");
                    summary.Add(indent + LangMan.LS(LSID.RPMultimedia) + " (" + structWML.MultimediaLinks.Count.ToString() + "):");

                    int num = structWML.MultimediaLinks.Count;
                    for (int i = 0; i < num; i++) {
                        GDMMultimediaLink mmLink = structWML.MultimediaLinks[i];
                        GDMMultimediaRecord mmRec = tree.GetPtrValue<GDMMultimediaRecord>(mmLink);
                        if (mmRec == null || mmRec.FileReferences.Count == 0) continue;

                        string st = mmRec.FileReferences[0].Title;
                        summary.Add(indent + "  " + HyperLink(mmRec.XRef, st) + " (" +
                                    HyperLink(GKData.INFO_HREF_VIEW + mmRec.XRef, LangMan.LS(LSID.MediaView)) + ")");
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.RecListMediaRefresh()", ex);
            }
        }

        private static void RecListNotesRefresh(GDMTree tree, IGDMStructWithNotes structWN, StringList summary, string indent = "")
        {
            if (structWN == null || summary == null) return;

            try {
                if (structWN.HasNotes) {
                    summary.Add("");
                    summary.Add(indent + LangMan.LS(LSID.RPNotes) + " (" + structWN.Notes.Count.ToString() + "):");

                    int num = structWN.Notes.Count;
                    for (int i = 0; i < num; i++) {
                        if (i > 0) {
                            summary.Add("");
                        }

                        GDMLines noteLines = tree.GetNoteLines(structWN.Notes[i]);
                        summary.AddILines(noteLines, "    " + indent);
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.RecListNotesRefresh()", ex);
            }
        }

        private static void RecListSourcesRefresh(GDMTree tree, IGDMStructWithSourceCitations structWSC, StringList summary, string indent = "")
        {
            if (structWSC == null || summary == null) return;

            try {
                if (structWSC.HasSourceCitations) {
                    if (structWSC is IGDMRecord) {
                        summary.Add("");
                    }

                    summary.Add(indent + LangMan.LS(LSID.RPSources) + " (" + structWSC.SourceCitations.Count.ToString() + "):");

                    for (int i = 0, num = structWSC.SourceCitations.Count; i < num; i++) {
                        GDMSourceCitation sourCit = structWSC.SourceCitations[i];
                        ShowSourceCitationSummary(tree, sourCit, summary, indent);
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.RecListSourcesRefresh()", ex);
            }
        }

        public static void ShowSourceCitationSummary(GDMTree tree, GDMSourceCitation sourCit, StringList summary, string indent)
        {
            GDMSourceRecord sourceRec = tree.GetPtrValue<GDMSourceRecord>(sourCit);
            if (sourceRec == null) return;

            string nm = "\"" + sourceRec.ShortTitle + "\"";
            if (!string.IsNullOrEmpty(sourCit.Page)) {
                nm = nm + ", " + sourCit.Page;
            }
            summary.Add(indent + "  " + HyperLink(sourceRec.XRef, nm));

            var text = sourCit.Data.Text;
            if (!text.IsEmpty()) {
                summary.Add(indent + "    " + text.Lines.Text);
            }
        }

        public static string GetSourceCitationStr(GDMTree tree, GDMSourceCitation sourCit)
        {
            string result = string.Empty;

            GDMSourceRecord sourceRec = tree.GetPtrValue<GDMSourceRecord>(sourCit);
            if (sourceRec == null) return result;

            string nm = "\"" + sourceRec.ShortTitle + "\"";
            if (!string.IsNullOrEmpty(sourCit.Page)) {
                nm = nm + ", " + sourCit.Page;
            }
            result = nm;

            return result;
        }

        public static string GetUserReferenceStr(GDMTree tree, GDMUserReference userRef)
        {
            return string.Concat(userRef.ReferenceType, ", ", userRef.StringValue);
        }

        public static string GetAssociationStr(GDMTree tree, GDMAssociation ast)
        {
            var relIndi = tree.GetPtrValue(ast);
            string nm = (relIndi == null) ? string.Empty : GKUtils.GetNameString(relIndi, false);
            string xref = (relIndi == null) ? string.Empty : relIndi.XRef;
            return string.Concat(ast.Relation, " ", HyperLink(xref, nm));
        }

        private static void RecListAssociationsRefresh(BaseContext baseContext, GDMIndividualRecord record, StringList summary)
        {
            if (record == null || summary == null) return;

            try {
                if (record.HasAssociations) {
                    summary.Add("");
                    summary.Add(LangMan.LS(LSID.Associations) + ":");

                    int num = record.Associations.Count;
                    for (int i = 0; i < num; i++) {
                        GDMAssociation ast = record.Associations[i];
                        summary.Add("    " + GetAssociationStr(baseContext.Tree, ast));
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.RecListAssociationsRefresh()", ex);
            }
        }

        private static void RecListIndividualEventsRefresh(BaseContext baseContext, GDMIndividualRecord record, StringList summary)
        {
            if (record == null || summary == null) return;

            try {
                if (record.HasEvents) {
                    summary.Add("");
                    summary.Add(LangMan.LS(LSID.Events) + ":");

                    for (int i = 0, num = record.Events.Count; i < num; i++) {
                        summary.Add("");
                        ShowEventSummary(baseContext.Tree, record.Events[i], summary, true);
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.RecListIndividualEventsRefresh()", ex);
            }
        }

        public static string GetFactValueStr(GDMCustomEvent evt)
        {
            string result = evt.StringValue;
            if (result.StartsWith(GKData.INFO_HTTP_PREFIX)) {
                result = HyperLink(result, result);
            }
            return result;
        }

        private static void RecListFamilyEventsRefresh(BaseContext baseContext, GDMFamilyRecord record, StringList summary)
        {
            if (record == null || summary == null) return;

            try {
                if (record.HasEvents) {
                    summary.Add("");
                    summary.Add(LangMan.LS(LSID.Events) + ":");

                    for (int i = 0, num = record.Events.Count; i < num; i++) {
                        summary.Add("");
                        GDMFamilyEvent evt = (GDMFamilyEvent)record.Events[i];
                        ShowEventSummary(baseContext.Tree, evt, summary, false);
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.RecListFamilyEventsRefresh()", ex);
            }
        }

        public static void ShowEventSummary(GDMTree tree, GDMCustomEvent evt, StringList summary, bool individual)
        {
            string st = GKUtils.GetEventName(evt);

            string sv;
            if (individual) {
                sv = GetFactValueStr(evt);
                if (!string.IsNullOrEmpty(sv)) {
                    sv += ", ";
                }
            } else {
                sv = string.Empty;
            }

            summary.Add("  " + st + ": " + sv + GKUtils.GetEventDesc(tree, evt));

            ShowDetailCause(evt, summary);
            if (evt.HasAddress) {
                ShowAddressSummary(evt.Address, summary);
            }

            RecListSourcesRefresh(tree, evt, summary, "    ");
            RecListNotesRefresh(tree, evt, summary, "    ");
            RecListMediaRefresh(tree, evt, summary, "    ");
        }

        private static void RecListGroupsRefresh(BaseContext baseContext, GDMIndividualRecord record, StringList summary)
        {
            if (record == null || summary == null) return;

            try {
                if (record.HasGroups) {
                    summary.Add("");
                    summary.Add(LangMan.LS(LSID.RPGroups) + ":");

                    int num = record.Groups.Count;
                    for (int i = 0; i < num; i++) {
                        GDMPointer ptr = record.Groups[i];
                        GDMGroupRecord grp = baseContext.Tree.GetPtrValue<GDMGroupRecord>(ptr);
                        if (grp == null) continue;

                        summary.Add("    " + HyperLink(grp.XRef, grp.GroupName));
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.RecListGroupsRefresh()", ex);
            }
        }

        public static void ShowFamilyInfo(BaseContext baseContext, GDMFamilyRecord familyRec, StringList summary)
        {
            if (summary == null) return;

            try {
                summary.BeginUpdate();
                try {
                    summary.Clear();
                    if (familyRec != null) {
                        summary.Add("");

                        GDMIndividualRecord spRec = baseContext.Tree.GetPtrValue(familyRec.Husband);
                        string st = ((spRec == null) ? LangMan.LS(LSID.UnkMale) : HyperLink(spRec.XRef, GKUtils.GetNameString(spRec, false)));
                        summary.Add(LangMan.LS(LSID.Husband) + ": " + st + GKUtils.GetLifeStr(spRec));

                        spRec = baseContext.Tree.GetPtrValue(familyRec.Wife);
                        st = ((spRec == null) ? LangMan.LS(LSID.UnkFemale) : HyperLink(spRec.XRef, GKUtils.GetNameString(spRec, false)));
                        summary.Add(LangMan.LS(LSID.Wife) + ": " + st + GKUtils.GetLifeStr(spRec));

                        summary.Add("");
                        if (familyRec.Children.Count != 0) {
                            summary.Add(LangMan.LS(LSID.Childs) + ":");
                        }

                        int num = familyRec.Children.Count;
                        for (int i = 0; i < num; i++) {
                            var child = baseContext.Tree.GetPtrValue(familyRec.Children[i]);
                            summary.Add("    " + HyperLink(child.XRef, GKUtils.GetNameString(child, false)) + GKUtils.GetLifeStr(child));
                        }
                        summary.Add("");

                        RecListFamilyEventsRefresh(baseContext, familyRec, summary);
                        RecListNotesRefresh(baseContext.Tree, familyRec, summary);
                        RecListMediaRefresh(baseContext.Tree, familyRec, summary);
                        RecListSourcesRefresh(baseContext.Tree, familyRec, summary);
                    }
                } finally {
                    summary.EndUpdate();
                }
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.ShowFamilyInfo()", ex);
            }
        }

        public static void ShowGroupInfo(BaseContext baseContext, GDMGroupRecord groupRec, StringList summary)
        {
            if (summary == null) return;

            try {
                StringList mbrList = new StringList();
                summary.BeginUpdate();
                try {
                    summary.Clear();
                    if (groupRec != null) {
                        summary.Add("");
                        summary.Add("[u][b][size=+1]" + groupRec.GroupName + "[/size][/b][/u]");
                        summary.Add("");
                        summary.Add(LangMan.LS(LSID.Members) + " (" + groupRec.Members.Count.ToString() + "):");

                        int num = groupRec.Members.Count;
                        for (int i = 0; i < num; i++) {
                            GDMPointer ptr = groupRec.Members[i];
                            var member = baseContext.Tree.GetPtrValue<GDMIndividualRecord>(ptr);

                            mbrList.AddObject(GKUtils.GetNameString(member, false), member);
                        }
                        mbrList.Sort();

                        int num2 = mbrList.Count;
                        for (int i = 0; i < num2; i++) {
                            GDMIndividualRecord member = (GDMIndividualRecord)mbrList.GetObject(i);

                            summary.Add("    " + HyperLink(member.XRef, mbrList[i]));
                        }

                        RecListNotesRefresh(baseContext.Tree, groupRec, summary);
                        RecListMediaRefresh(baseContext.Tree, groupRec, summary);
                    }
                } finally {
                    summary.EndUpdate();
                    mbrList.Dispose();
                }
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.ShowGroupInfo()", ex);
            }
        }

        public static void ShowMultimediaInfo(BaseContext baseContext, GDMMultimediaRecord mediaRec, StringList summary)
        {
            if (summary == null) return;

            try {
                summary.BeginUpdate();
                try {
                    summary.Clear();
                    if (mediaRec != null) {
                        for (int i = 0, num = mediaRec.FileReferences.Count; i < num; i++) {
                            GDMFileReferenceWithTitle fileRef = mediaRec.FileReferences[i];

                            string mediaTitle = (fileRef == null) ? LangMan.LS(LSID.Unknown) : fileRef.Title;

                            summary.Add("");
                            summary.Add("[u][b][size=+1]" + mediaTitle + "[/size][/b][/u]");
                            summary.Add("");
                            if (fileRef != null) {
                                summary.Add("( " + HyperLink($"{GKData.INFO_HREF_VIEW}{mediaRec.XRef}_{i}", LangMan.LS(LSID.View)) + " )");
                            }
                        }

                        ShowSubjectLinks(baseContext.Tree, mediaRec, summary);

                        RecListNotesRefresh(baseContext.Tree, mediaRec, summary);
                        RecListSourcesRefresh(baseContext.Tree, mediaRec, summary);
                    }
                } finally {
                    summary.EndUpdate();
                }
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.ShowMultimediaInfo()", ex);
            }
        }

        private static void AddILines(this StringList summary, GDMLines lines, string indent = "")
        {
            if (lines == null) return;

            for (int k = 0, num2 = lines.Count; k < num2; k++) {
                summary.Add(indent + MakeLinks(lines[k]));
            }
        }

        public static void ShowNoteInfo(BaseContext baseContext, GDMNoteRecord noteRec, StringList summary)
        {
            if (summary == null) return;

            try {
                summary.BeginUpdate();
                try {
                    summary.Clear();
                    if (noteRec != null) {
                        summary.Add("");
                        summary.AddILines(noteRec.Lines, "");

                        ShowSubjectLinks(baseContext.Tree, noteRec, summary);

                        RecListSourcesRefresh(baseContext.Tree, noteRec, summary);
                    }
                } finally {
                    summary.EndUpdate();
                }
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.ShowNoteInfo()", ex);
            }
        }

        public static void ShowParentsInfo(GDMTree tree, GDMIndividualRecord iRec, StringList summary)
        {
            if (summary == null) return;

            try {
                for (int p = 0; p < iRec.ChildToFamilyLinks.Count; p++) {
                    var ctfLink = iRec.ChildToFamilyLinks[p];
                    var famRec = tree.GetPtrValue(ctfLink);

                    GDMIndividualRecord father, mother;
                    tree.GetSpouses(famRec, out father, out mother);

                    if (father != null || mother != null) {
                        var plType = ctfLink.PedigreeLinkageType;
                        string linkType =
                            (plType == GDMPedigreeLinkageType.plNone || plType == GDMPedigreeLinkageType.plBirth) ?
                            string.Empty : string.Format(" ({0})", LangMan.LS(GKData.ParentTypes[(int)plType]));

                        summary.Add("");
                        summary.Add(LangMan.LS(LSID.Parents) + linkType + ":");

                        string st;

                        st = (father == null) ? LangMan.LS(LSID.UnkMale) : HyperLink(father.XRef, GKUtils.GetNameString(father, false));
                        summary.Add("  " + LangMan.LS(LSID.Father) + ": " + st + GKUtils.GetLifeStr(father));

                        st = (mother == null) ? LangMan.LS(LSID.UnkFemale) : HyperLink(mother.XRef, GKUtils.GetNameString(mother, false));
                        summary.Add("  " + LangMan.LS(LSID.Mother) + ": " + st + GKUtils.GetLifeStr(mother));
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.ShowParentsInfo()", ex);
            }
        }

        public static void ShowSpousesInfo(BaseContext baseContext, GDMTree tree, GDMIndividualRecord iRec, StringList summary)
        {
            if (summary == null) return;

            try {
                int num = iRec.SpouseToFamilyLinks.Count;
                for (int i = 0; i < num; i++) {
                    GDMFamilyRecord family = tree.GetPtrValue(iRec.SpouseToFamilyLinks[i]);
                    if (family == null) continue;
                    if (!baseContext.IsRecordAccess(family.Restriction)) continue;

                    string st;
                    GDMIndividualRecord spRec;
                    string unk;
                    if (iRec.Sex == GDMSex.svMale) {
                        spRec = tree.GetPtrValue(family.Wife);
                        st = LangMan.LS(LSID.Wife) + ": ";
                        unk = LangMan.LS(LSID.UnkFemale);
                    } else {
                        spRec = tree.GetPtrValue(family.Husband);
                        st = LangMan.LS(LSID.Husband) + ": ";
                        unk = LangMan.LS(LSID.UnkMale);
                    }
                    string marr = GKUtils.GetMarriageDateStr(family, GlobalOptions.Instance.DefDateFormat);
                    if (marr != "") {
                        marr = LangMan.LS(LSID.LMarriage) + " " + marr;
                    } else {
                        marr = LangMan.LS(LSID.LFamily);
                    }

                    summary.Add("");
                    if (spRec != null) {
                        st = st + HyperLink(spRec.XRef, GKUtils.GetNameString(spRec, false)) + " (" + HyperLink(family.XRef, marr) + ")";
                    } else {
                        st = st + unk + " (" + HyperLink(family.XRef, marr) + ")";
                    }
                    summary.Add(st);

                    int chNum = family.Children.Count;
                    if (chNum != 0) {
                        summary.Add("");
                        summary.Add(LangMan.LS(LSID.Childs) + ":");

                        for (int k = 0; k < chNum; k++) {
                            GDMIndividualRecord child = tree.GetPtrValue(family.Children[k]);
                            if (child == null) continue;

                            summary.Add("    " + HyperLink(child.XRef, GKUtils.GetNameString(child, false)) + GKUtils.GetLifeStr(child));
                        }
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.ShowSpousesInfo()", ex);
            }
        }

        public static void ShowPersonInfo(BaseContext baseContext, GDMIndividualRecord iRec, StringList summary, RecordContentType contentType)
        {
            if (summary == null) return;

            try {
                summary.BeginUpdate();
                summary.Clear();
                try {
                    if (iRec != null) {
                        GDMTree tree = baseContext.Tree;

                        summary.Add("");

                        bool firstSurname = GlobalOptions.Instance.SurnameFirstInOrder;
                        for (int i = 0; i < iRec.PersonalNames.Count; i++) {
                            var persName = iRec.PersonalNames[i];
                            summary.Add("[u][b][size=+1]" + GKUtils.GetNameString(iRec, persName, firstSurname, true) + "[/size][/u][/b]");
                        }

                        summary.Add(LangMan.LS(LSID.Sex) + ": " + GKUtils.SexStr(iRec.Sex));

                        ShowParentsInfo(tree, iRec, summary);
                        ShowSpousesInfo(baseContext, tree, iRec, summary);

                        RecListIndividualEventsRefresh(baseContext, iRec, summary);
                        RecListNotesRefresh(baseContext.Tree, iRec, summary);
                        RecListMediaRefresh(baseContext.Tree, iRec, summary);
                        RecListSourcesRefresh(baseContext.Tree, iRec, summary);
                        RecListAssociationsRefresh(baseContext, iRec, summary);
                        RecListGroupsRefresh(baseContext, iRec, summary);

                        ShowPersonNamesakes(tree, iRec, summary);
                        ShowPersonExtInfo(tree, iRec, summary);

                        ShowRFN(iRec, summary);

                        if (contentType == RecordContentType.Full) {
                            summary.Add("");
                            summary.Add(HyperLink(GKData.INFO_HREF_EXPAND_ASSO + iRec.XRef, "[ + ] " + LangMan.LS(LSID.Associations)));
                        }
                    }
                } finally {
                    summary.EndUpdate();
                }
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.ShowPersonInfo()", ex);
            }
        }

        private static void ShowRFN(GDMIndividualRecord iRec, StringList summary)
        {
            var rfnTag = iRec.FindTag("RFN", 0);
            if (rfnTag == null) return;

            var rfnVal = rfnTag.StringValue;
            if (string.IsNullOrEmpty(rfnVal)) return;

            var parts = rfnVal.Split(new char[] { ':' }, StringSplitOptions.RemoveEmptyEntries);
            if (parts.Length < 2) return;

            var resourceId = parts[0];
            var recordId = parts[1];

            var res = AppHost.ExtResources.FindURL(resourceId);
            if (res == null) return;

            var fullURL = res.URL + recordId;
            summary.Add("");
            summary.Add(HyperLink(fullURL, string.Format("{0}: {1}", res.Name, fullURL)));
        }

        private static void AddQValue(this StringList summary, string name, string value)
        {
            if (value == null) return;

            value = value.Trim();
            if (string.IsNullOrEmpty(value)) return;

            summary.AddMultiline(name + ": \"" + MakeLinks(value) + "\"");
        }

        public static void ShowSourceInfo(BaseContext baseContext, GDMSourceRecord sourceRec, StringList summary, RecordContentType contentType)
        {
            if (summary == null) return;

            try {
                summary.BeginUpdate();
                try {
                    summary.Clear();
                    if (sourceRec != null) {
                        summary.Add("");
                        summary.Add("[u][b][size=+1]" + sourceRec.ShortTitle + "[/size][/b][/u]");
                        summary.Add("");
                        summary.AddQValue(LangMan.LS(LSID.Author), sourceRec.Originator.Lines.Text);
                        summary.AddQValue(LangMan.LS(LSID.Title), sourceRec.Title.Lines.Text);
                        summary.AddQValue(LangMan.LS(LSID.Publication), sourceRec.Publication.Lines.Text);
                        summary.AddQValue(LangMan.LS(LSID.Text), sourceRec.Text.Lines.Text);

                        if (sourceRec.RepositoryCitations.Count > 0) {
                            summary.Add("");
                            summary.Add(LangMan.LS(LSID.RPRepositories) + ":");

                            int num = sourceRec.RepositoryCitations.Count;
                            for (int i = 0; i < num; i++) {
                                GDMRepositoryRecord rep = baseContext.Tree.GetPtrValue<GDMRepositoryRecord>(sourceRec.RepositoryCitations[i]);

                                summary.Add("    " + HyperLink(rep.XRef, rep.RepositoryName));
                            }
                        }

                        ShowSubjectLinks(baseContext.Tree, sourceRec, summary, true);

                        RecListNotesRefresh(baseContext.Tree, sourceRec, summary);
                        RecListMediaRefresh(baseContext.Tree, sourceRec, summary);

                        summary.Add("");
                        summary.Add("");
                        if (contentType == RecordContentType.Full) {
                            summary.Add("[ " + HyperLink(GKData.INFO_HREF_FILTER_INDI + sourceRec.XRef, LangMan.LS(LSID.MIFilter)) + " ]");
                            summary.Add("");
                        }
                    }
                } finally {
                    summary.EndUpdate();
                }
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.ShowSourceInfo()", ex);
            }
        }

        public static void ShowRepositoryInfo(BaseContext baseContext, GDMRepositoryRecord repositoryRec, StringList summary)
        {
            if (summary == null) return;

            try {
                summary.BeginUpdate();
                try {
                    summary.Clear();
                    if (repositoryRec != null) {
                        summary.Add("");
                        summary.Add("[u][b][size=+1]" + MakeLinks(repositoryRec.RepositoryName.Trim()) + "[/size][/b][/u]");
                        summary.Add("");

                        if (repositoryRec.HasAddress)
                            ShowAddressSummary(repositoryRec.Address, summary);

                        summary.Add("");
                        summary.Add(LangMan.LS(LSID.RPSources) + ":");

                        var sortedSources = new LinksList();
                        GDMTree tree = baseContext.Tree;
                        int num = tree.RecordsCount;
                        for (int i = 0; i < num; i++) {
                            GDMRecord rec = tree[i];

                            if (rec.RecordType == GDMRecordType.rtSource) {
                                GDMSourceRecord srcRec = (GDMSourceRecord)rec;

                                int num2 = srcRec.RepositoryCitations.Count;
                                for (int j = 0; j < num2; j++) {
                                    if (srcRec.RepositoryCitations[j].XRef == repositoryRec.XRef) {
                                        sortedSources.Add(GenRecordLinkTuple(baseContext.Tree, srcRec, false));
                                    }
                                }
                            }
                        }
                        sortedSources.Sort();
                        foreach (var tpl in sortedSources) {
                            summary.Add("    " + tpl.Item2);
                        }

                        RecListNotesRefresh(baseContext.Tree, repositoryRec, summary);
                    }
                } finally {
                    summary.EndUpdate();
                }
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.ShowRepositoryInfo()", ex);
            }
        }

        public static string GetPriorityStr(GDMResearchPriority priority)
        {
            return LangMan.LS(GKData.PriorityNames[(int)priority]);
        }

        public static void ShowResearchInfo(BaseContext baseContext, GDMResearchRecord researchRec, StringList summary)
        {
            if (summary == null) return;

            try {
                summary.BeginUpdate();
                try {
                    summary.Clear();
                    if (researchRec != null) {
                        summary.Add("");
                        summary.Add(LangMan.LS(LSID.Title) + ": [u][b][size=+1]\"" + researchRec.ResearchName.Trim() + "\"[/size][/b][/u]");
                        summary.Add("");
                        summary.Add(LangMan.LS(LSID.Priority) + ": " + LangMan.LS(GKData.PriorityNames[(int)researchRec.Priority]));
                        summary.Add(LangMan.LS(LSID.Status) + ": " + LangMan.LS(GKData.StatusNames[(int)researchRec.Status]) + " (" + researchRec.Percent.ToString() + "%)");
                        summary.Add(LangMan.LS(LSID.StartDate) + ": " + researchRec.StartDate.GetDisplayString(GlobalOptions.Instance.DefDateFormat));
                        summary.Add(LangMan.LS(LSID.StopDate) + ": " + researchRec.StopDate.GetDisplayString(GlobalOptions.Instance.DefDateFormat));

                        if (researchRec.Tasks.Count > 0) {
                            summary.Add("");
                            summary.Add(LangMan.LS(LSID.RPTasks) + ":");

                            int num = researchRec.Tasks.Count;
                            for (int i = 0; i < num; i++) {
                                var taskRec = baseContext.Tree.GetPtrValue<GDMTaskRecord>(researchRec.Tasks[i]);
                                summary.Add("    " + GenRecordLink(baseContext.Tree, taskRec, false));
                            }
                        }

                        if (researchRec.Communications.Count > 0) {
                            summary.Add("");
                            summary.Add(LangMan.LS(LSID.RPCommunications) + ":");

                            int num2 = researchRec.Communications.Count;
                            for (int i = 0; i < num2; i++) {
                                var corrRec = baseContext.Tree.GetPtrValue<GDMCommunicationRecord>(researchRec.Communications[i]);
                                summary.Add("    " + GenRecordLink(baseContext.Tree, corrRec, false));
                            }
                        }

                        if (researchRec.Groups.Count != 0) {
                            summary.Add("");
                            summary.Add(LangMan.LS(LSID.RPGroups) + ":");

                            int num3 = researchRec.Groups.Count;
                            for (int i = 0; i < num3; i++) {
                                var grp = baseContext.Tree.GetPtrValue<GDMGroupRecord>(researchRec.Groups[i]);
                                summary.Add("    " + HyperLink(grp.XRef, grp.GroupName));
                            }
                        }

                        RecListNotesRefresh(baseContext.Tree, researchRec, summary);
                    }
                } finally {
                    summary.EndUpdate();
                }
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.ShowResearchInfo()", ex);
            }
        }

        public static void ShowTaskInfo(BaseContext baseContext, GDMTaskRecord taskRec, StringList summary)
        {
            if (summary == null) return;

            try {
                summary.BeginUpdate();
                try {
                    summary.Clear();
                    if (taskRec != null) {
                        summary.Add("");
                        summary.Add(LangMan.LS(LSID.Goal) + ": [u][b][size=+1]" + GKUtils.GetTaskGoalStr(baseContext.Tree, taskRec) + "[/size][/b][/u]");
                        summary.Add("");
                        summary.Add(LangMan.LS(LSID.Priority) + ": " + LangMan.LS(GKData.PriorityNames[(int)taskRec.Priority]));
                        summary.Add(LangMan.LS(LSID.StartDate) + ": " + taskRec.StartDate.GetDisplayString(GlobalOptions.Instance.DefDateFormat));
                        summary.Add(LangMan.LS(LSID.StopDate) + ": " + taskRec.StopDate.GetDisplayString(GlobalOptions.Instance.DefDateFormat));

                        RecListNotesRefresh(baseContext.Tree, taskRec, summary);
                    }
                } finally {
                    summary.EndUpdate();
                }
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.ShowTaskInfo()", ex);
            }
        }

        public static void ShowCommunicationInfo(BaseContext baseContext, GDMCommunicationRecord commRec, StringList summary)
        {
            if (summary == null) return;

            try {
                summary.BeginUpdate();
                try {
                    summary.Clear();
                    if (commRec != null) {
                        GDMTree tree = baseContext.Tree;

                        summary.Add("");
                        summary.Add(LangMan.LS(LSID.Theme) + ": [u][b][size=+1]\"" + commRec.CommName.Trim() + "\"[/size][/b][/u]");
                        summary.Add("");
                        summary.Add(LangMan.LS(LSID.Corresponder) + ": " + GetCorresponderStr(tree, commRec, true));
                        summary.Add(LangMan.LS(LSID.Type) + ": " + LangMan.LS(GKData.CommunicationNames[(int)commRec.CommunicationType]));
                        summary.Add(LangMan.LS(LSID.Date) + ": " + commRec.Date.GetDisplayString(GlobalOptions.Instance.DefDateFormat));

                        RecListNotesRefresh(baseContext.Tree, commRec, summary);
                        RecListMediaRefresh(baseContext.Tree, commRec, summary);
                    }
                } finally {
                    summary.EndUpdate();
                }
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.ShowCommunicationInfo()", ex);
            }
        }

        public static string GetMapStr(GDMTree tree, GDMMap map)
        {
            return string.Concat(GEDCOMUtils.CoordToStr(map.Lati), "; ", GEDCOMUtils.CoordToStr(map.Long));
        }

        public static void ShowLocationInfo(BaseContext baseContext, GDMLocationRecord locRec, StringList summary)
        {
            if (summary == null) return;

            try {
                summary.BeginUpdate();
                summary.Clear();

                StringList linkList = null;
                try {
                    if (locRec == null) return;

                    summary.Add("");

                    GlobalOptions glob = GlobalOptions.Instance;
                    for (int i = 0; i < locRec.Names.Count; i++) {
                        var locName = locRec.Names[i];
                        summary.Add("[u][b][size=+1]" + locName.StringValue + "[/size][/b][/u]");

                        string st = locName.Abbreviation;
                        if (!string.IsNullOrEmpty(st)) {
                            summary.Add("    " + st);
                        }

                        st = locName.Date.GetDisplayString(glob.DefDateFormat, glob.ShowDatesSign, glob.ShowDatesCalendar);
                        if (!string.IsNullOrEmpty(st)) {
                            summary.Add("    " + st);
                        }

                        summary.Add("");
                    }

                    GDMTree tree = baseContext.Tree;

                    if (locRec.TopLevels.Count > 0) {
                        summary.Add("");
                        summary.Add(LangMan.LS(LSID.TopLevelLinks) + ":");

                        for (int i = 0; i < locRec.TopLevels.Count; i++) {
                            var topLev = locRec.TopLevels[i];
                            var topLoc = tree.GetPtrValue<GDMLocationRecord>(topLev);

                            string st = HyperLink(topLev.XRef, topLoc.GetNameByDate(topLev.Date.Value));
                            if (!string.IsNullOrEmpty(st)) {
                                summary.Add("    " + st);
                            }

                            st = topLev.Date.GetDisplayString(glob.DefDateFormat, glob.ShowDatesSign, glob.ShowDatesCalendar);
                            if (!string.IsNullOrEmpty(st)) {
                                summary.Add("    " + st);
                            }

                            summary.Add("");
                        }
                    }

                    summary.Add(LangMan.LS(LSID.Latitude) + ": " + locRec.Map.Lati);
                    summary.Add(LangMan.LS(LSID.Longitude) + ": " + locRec.Map.Long);

                    var fullNames = locRec.GetFullNames(ATDEnumeration.fStL, glob.EL_AbbreviatedNames);
                    if (fullNames.Count > 0) {
                        //linkList.Sort();

                        summary.Add("");
                        summary.Add(LangMan.LS(LSID.History) + ":");

                        int num = fullNames.Count;
                        for (int i = 0; i < num; i++) {
                            var xName = fullNames[i];
                            summary.Add("    " + string.Format("{0}: {1}", xName.Date.GetDisplayString(glob.DefDateFormat, glob.ShowDatesSign, glob.ShowDatesCalendar, false), xName.StringValue));
                        }
                    }

                    linkList = GKUtils.GetLocationLinks(tree, locRec, false);
                    if (linkList.Count > 0) {
                        linkList.Sort();

                        summary.Add("");
                        summary.Add(LangMan.LS(LSID.Links) + ":");

                        int num = linkList.Count;
                        for (int i = 0; i < num; i++) {
                            GDMRecord rec = linkList.GetObject(i) as GDMRecord;
                            summary.Add("    " + HyperLink(rec.XRef, linkList[i]));
                        }
                    }

                    RecListNotesRefresh(baseContext.Tree, locRec, summary);
                    RecListMediaRefresh(baseContext.Tree, locRec, summary);

                    var subLinks = GKUtils.GetLocationSubordinatesList(tree, locRec);
                    if (subLinks.Count > 0) {
                        subLinks.Sort();

                        summary.Add("");
                        summary.Add(LangMan.LS(LSID.SubordinateLocationsLinks) + ":");

                        for (int i = 0, num = subLinks.Count; i < num; i++) {
                            GDMRecord rec = subLinks.GetObject(i) as GDMRecord;
                            summary.Add("    " + HyperLink(rec.XRef, subLinks[i]));
                        }
                    }

                    summary.Add("");
                    summary.Add(HyperLink(GKData.INFO_HREF_LOC_SUB + locRec.XRef, $"[ {LangMan.LS(LSID.MapOfPlaces)} ] "));

                    summary.Add("");
                    summary.Add(HyperLink(GKData.INFO_HREF_LOC_INDI + locRec.XRef, $"[ {LangMan.LS(LSID.MapOfPersons)} ] "));
                } finally {
                    if (linkList != null) linkList.Dispose();
                    summary.EndUpdate();
                }
            } catch (Exception ex) {
                Logger.WriteError("GKUtils.ShowLocationInfo()", ex);
            }
        }

        public static void GetRecordContent(BaseContext baseContext, GDMRecord record, StringList ctx, RecordContentType contentType)
        {
            if (record == null || ctx == null) return;

            switch (record.RecordType) {
                case GDMRecordType.rtIndividual:
                    ShowPersonInfo(baseContext, record as GDMIndividualRecord, ctx, contentType);
                    break;

                case GDMRecordType.rtFamily:
                    ShowFamilyInfo(baseContext, record as GDMFamilyRecord, ctx);
                    break;

                case GDMRecordType.rtNote:
                    ShowNoteInfo(baseContext, record as GDMNoteRecord, ctx);
                    break;

                case GDMRecordType.rtMultimedia:
                    ShowMultimediaInfo(baseContext, record as GDMMultimediaRecord, ctx);
                    break;

                case GDMRecordType.rtSource:
                    ShowSourceInfo(baseContext, record as GDMSourceRecord, ctx, contentType);
                    break;

                case GDMRecordType.rtRepository:
                    ShowRepositoryInfo(baseContext, record as GDMRepositoryRecord, ctx);
                    break;

                case GDMRecordType.rtGroup:
                    ShowGroupInfo(baseContext, record as GDMGroupRecord, ctx);
                    break;

                case GDMRecordType.rtResearch:
                    ShowResearchInfo(baseContext, record as GDMResearchRecord, ctx);
                    break;

                case GDMRecordType.rtTask:
                    ShowTaskInfo(baseContext, record as GDMTaskRecord, ctx);
                    break;

                case GDMRecordType.rtCommunication:
                    ShowCommunicationInfo(baseContext, record as GDMCommunicationRecord, ctx);
                    break;

                case GDMRecordType.rtLocation:
                    ShowLocationInfo(baseContext, record as GDMLocationRecord, ctx);
                    break;
            }
        }
    }
}
