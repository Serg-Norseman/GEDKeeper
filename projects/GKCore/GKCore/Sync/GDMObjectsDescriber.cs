/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using BSLib;
using GDModel;
using GDModel.Providers.GEDCOM;
using GKCore.Locales;

namespace GKCore.Sync
{
    public class GDMObjectsDescriber
    {
        public static string GetBriefDescription(GDMTree tree, GDMRecord record, GDMTag tag)
        {
            string result = string.Empty;
            if (tag == null) return result;

            if (tag is GDMPersonalName persName) {
                result = GetPersonalNameStr(tree, record, persName);
            } else if (tag is GDMChildToFamilyLink ctfLink) {
                result = GetPtrDescription(LangMan.LS(LSID.Parents), tree, ctfLink);
            } else if (tag is GDMSpouseToFamilyLink stfLink) {
                result = GetPtrDescription(LangMan.LS(LSID.Family), tree, stfLink);
            } else if (tag is GDMChildLink childLink) {
                result = GetPtrDescription(LangMan.LS(LSID.Child), tree, childLink);
            } else if (tag is GDMCustomEvent evt) {
                result = GKUtils.GetEventStr(evt);
            } else if (tag is GDMNotes notes) {
                result = GetNotesPtrStr(tree, notes);
            } else if (tag is GDMMultimediaLink mediaLink) {
                result = GetPtrDescription(LangMan.LS(LSID.RPMultimedia), tree, mediaLink);
            } else if (tag is GDMSourceCitation sourLink) {
                result = $"{LangMan.LS(LSID.Source)}: {GKInfoPanel.GetSourceCitationStr(tree, sourLink)}";
            } else if (tag is GDMUserReference userRef) {
                result = $"{LangMan.LS(LSID.UserRef)}: {GKInfoPanel.GetUserReferenceStr(tree, userRef)}";
            } else if (tag is GDMAddress addr) {
                result = GKInfoPanel.GetAddressStr(tree, addr);
            } else if (tag is GDMRepositoryCitation repoCit) {
                result = GetPtrDescription(LangMan.LS(LSID.Repository), tree, repoCit);
            } else if (tag is GDMFileReferenceWithTitle fileRef) {
                result = GetFileReferenceStr(tree, fileRef);
            } else if (tag is GDMAssociation asso) {
                result = $"{LangMan.LS(LSID.Association)}: {GKInfoPanel.GetAssociationStr(tree, asso)}";
            } else if (tag is GDMLocationName locName) {
                result = GetLocationNameStr(tree, locName);
            } else if (tag is GDMLocationLink locLink) {
                result = GetLocationLinkStr(tree, locLink);
            } else if (tag is GDMMemberLink memberLink) {
                result = GetPtrDescription(LangMan.LS(LSID.Member), tree, memberLink);
            } else if (tag is GDMGroupLink groupLink) {
                result = GetPtrDescription(LangMan.LS(LSID.Group), tree, groupLink);
            } else if (tag is GDMSourceCallNumber callNum) {
                result = GetSourceCallNumberStr(tree, callNum);
            } else if (tag is GDMSourceData sourData) {
                result = GetSourceDataStr(tree, sourData);
            } else if (tag is GDMMap map) {
                result = $"{LangMan.LS(LSID.Coordinates)}: {GKInfoPanel.GetMapStr(tree, map)}";
            } else if (tag is GDMDNATest dnaTest) {
                result = GetDNATestStr(tree, dnaTest);
            } else if (tag is GDMValueTag valTag) {
                result = GetTagDescription(valTag);
            } else if (tag is GDMPointer ptr) {
                result = $"Unk ptr"; // All derived classes are in the list above!
            } else {
                result = $"Unk tag";
            }

            return result;
        }

        public static void GetFullDescription(GDMTree tree, GDMRecord record, GDMTag tag, StringList summary)
        {
            summary.Clear();
            if (tag == null) {
                summary.Add(" --- ");
                return;
            }

            string result = string.Empty;

            if (tag is GDMPersonalName persName) {
                ShowPersonalNameSummary(tree, record, persName, summary);
            } else if (tag is GDMChildToFamilyLink ctfLink) {
                ShowChildToFamilyLinkSummary(tree, ctfLink, summary);
            } else if (tag is GDMCustomEvent evt) {
                bool individual = record is GDMIndividualRecord;
                GKInfoPanel.ShowEventSummary(tree, evt, summary, individual);
            } else if (tag is GDMNotes notes) {
                ShowNotesSummary(tree, notes, summary);
            } else if (tag is GDMMultimediaLink mediaLink) {
                ShowMultimediaLinkSummary(tree, mediaLink, summary);
            } else if (tag is GDMSourceCitation sourLink) {
                GKInfoPanel.ShowSourceCitationSummary(tree, sourLink, summary, "");
            } else if (tag is GDMAddress addr) {
                GKInfoPanel.ShowAddressSummary(addr, summary);
            } else if (tag is GDMRepositoryCitation repoCit) {
                ShowRepositoryCitationSummary(tree, repoCit, summary);
            } else if (tag is GDMFileReferenceWithTitle fileRef) {
                ShowFileReferenceSummary(tree, fileRef, summary);
            } else if (tag is GDMLocationName locName) {
                ShowLocationNameSummary(tree, locName, summary);
            } else if (tag is GDMLocationLink locLink) {
                ShowLocationLinkSummary(tree, locLink, summary);
            } else if (tag is GDMSourceCallNumber callNum) {
                ShowSourceCallNumberSummary(tree, callNum, summary);
            } else if (tag is GDMSourceData sourData) {
                ShowSourceDataSummary(tree, sourData, summary);
            } else if (tag is GDMDNATest dnaTest) {
                ShowDNATestSummary(tree, dnaTest, summary);
            } else {
                result = GetBriefDescription(tree, record, tag);
            }

            if (!string.IsNullOrEmpty(result))
                summary.Add(result);
        }

        private static string GetPtrDescription(string ptrName, GDMTree tree, GDMPointer ptr)
        {
            var record = tree.GetPtrValue<GDMRecord>(ptr);
            return string.Format("{0}: [{1}] {2}", ptrName, ptr.XRef, GKUtils.GetRecordName(tree, record, false));
        }

        private static string GetTagDescription(GDMValueTag valueTag)
        {
            string tagName;

            var tagType = (GEDCOMTagType)valueTag.Id;
            switch (tagType) {
                case GEDCOMTagType.NOTE:
                    tagName = LangMan.LS(LSID.Note);
                    break;
                case GEDCOMTagType.NAME:
                    tagName = LangMan.LS(LSID.GeneralName);
                    break;
                case GEDCOMTagType.TYPE:
                    tagName = LangMan.LS(LSID.Type);
                    break;
                case GEDCOMTagType.SEX:
                    tagName = LangMan.LS(LSID.Sex);
                    break;
                case GEDCOMTagType.DATE:
                    tagName = LangMan.LS(LSID.Date);
                    break;
                case GEDCOMTagType.RESN:
                    tagName = LangMan.LS(LSID.Restriction);
                    break;
                case GEDCOMTagType._STAT:
                    tagName = LangMan.LS(LSID.Status);
                    break;
                case GEDCOMTagType._GOAL:
                    tagName = LangMan.LS(LSID.Goal);
                    break;

                case GEDCOMTagType.HUSB:
                    tagName = LangMan.LS(LSID.Husband);
                    break;
                case GEDCOMTagType.WIFE:
                    tagName = LangMan.LS(LSID.Wife);
                    break;

                case GEDCOMTagType.ABBR:
                    tagName = LangMan.LS(LSID.ShortTitle);
                    break;
                case GEDCOMTagType.TITL:
                    tagName = LangMan.LS(LSID.Title);
                    break;
                case GEDCOMTagType.AUTH:
                    tagName = LangMan.LS(LSID.Author);
                    break;
                case GEDCOMTagType.PUBL:
                    tagName = LangMan.LS(LSID.Publication);
                    break;
                case GEDCOMTagType.TEXT:
                    tagName = LangMan.LS(LSID.Text);
                    break;

                default:
                    tagName = tagType.ToString();
                    break;
            }

            return string.Format("{0}: {1}", tagName, valueTag.StringValue);
        }


        private static string GetNotesPtrStr(GDMTree tree, GDMNotes notes)
        {
            return $"Notes Link: {notes.XRef}";
        }

        private static void ShowNotesSummary(GDMTree tree, GDMNotes notes, StringList summary)
        {
            summary.Add($"Notes Link: {notes.XRef}");
        }


        private static string GetFileReferenceStr(GDMTree tree, GDMFileReferenceWithTitle fileRef)
        {
            return $"{LangMan.LS(LSID.File)}: {fileRef.Title}";
        }

        private static void ShowFileReferenceSummary(GDMTree tree, GDMFileReferenceWithTitle fileRef, StringList summary)
        {
            summary.Add($"File Reference: {fileRef.StringValue}");
            //return $"File Reference: {fileRef.StringValue}";
        }


        private static string GetLocationNameStr(GDMTree tree, GDMLocationName locName)
        {
            return $"Location Name: {locName.StringValue}";
        }

        private static void ShowLocationNameSummary(GDMTree tree, GDMLocationName locName, StringList summary)
        {
            summary.Add($"Location Name: {locName.StringValue}");
        }


        private static string GetLocationLinkStr(GDMTree tree, GDMLocationLink locLink)
        {
            return $"Location Link: {locLink.XRef}";
        }

        private static void ShowLocationLinkSummary(GDMTree tree, GDMLocationLink locLink, StringList summary)
        {
            summary.Add($"Location Link: {locLink.XRef}");
        }


        private static string GetSourceCallNumberStr(GDMTree tree, GDMSourceCallNumber callNum)
        {
            return $"Call Number: {callNum.StringValue}";
        }

        private static void ShowSourceCallNumberSummary(GDMTree tree, GDMSourceCallNumber callNum, StringList summary)
        {
            summary.Add($"Call Number: {callNum.StringValue}");
        }


        private static string GetSourceDataStr(GDMTree tree, GDMSourceData sourData)
        {
            return $"Source Data: {sourData.StringValue}";
        }

        private static void ShowSourceDataSummary(GDMTree tree, GDMSourceData sourData, StringList summary)
        {
            summary.Add($"Source Data: {sourData.StringValue}");
        }


        private static string GetDNATestStr(GDMTree tree, GDMDNATest dnaTest)
        {
            return $"{LangMan.LS(LSID.DNATest)}: {dnaTest.TestName}";
        }

        private static void ShowDNATestSummary(GDMTree tree, GDMDNATest dnaTest, StringList summary)
        {
            summary.Add($"{LangMan.LS(LSID.DNATest)}: {dnaTest.TestName}");
        }


        private static string GetPersonalNameStr(GDMTree tree, GDMRecord record, GDMPersonalName persName)
        {
            var indiRec = record as GDMIndividualRecord;
            return $"Personal Name: {GKUtils.GetNameString(indiRec, persName, false, false)}";
        }

        private static void ShowPersonalNameSummary(GDMTree tree, GDMRecord record, GDMPersonalName persName, StringList summary)
        {
            var indiRec = record as GDMIndividualRecord;
            summary.Add($"Personal Name: {GKUtils.GetNameString(indiRec, persName, false, false)}");
        }


        private static void ShowChildToFamilyLinkSummary(GDMTree tree, GDMChildToFamilyLink ctfLink, StringList summary)
        {
            summary.Add($"ChildToFamily Link: {ctfLink.XRef}");
        }

        private static void ShowMultimediaLinkSummary(GDMTree tree, GDMMultimediaLink mediaLink, StringList summary)
        {
            summary.Add($"Media Link: {mediaLink.XRef}");
        }

        private static void ShowRepositoryCitationSummary(GDMTree tree, GDMRepositoryCitation repoCit, StringList summary)
        {
            summary.Add($"Repository Citation: {repoCit.StringValue}");
        }
    }
}
