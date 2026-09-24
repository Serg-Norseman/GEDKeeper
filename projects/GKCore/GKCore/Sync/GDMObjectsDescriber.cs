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
        // TODO: Implement a better and more efficient solution,
        // but only after all the data is being extracted.
        public static string GetBriefDescription(GDMTree tree, GDMTag tag)
        {
            string result = string.Empty;
            if (tag == null) return result;

            if (tag is GDMValueTag valTag) {
                result = GetTagDescription(valTag);
            } else if (tag is GDMPersonalName persName) {
                result = $"Personal Name: {persName.StringValue}";
            } else if (tag is GDMChildToFamilyLink ctfLink) {
                result = $"ChildToFamily Link: {ctfLink.XRef}";
            } else if (tag is GDMSpouseToFamilyLink stfLink) {
                result = GetPtrDescription(LangMan.LS(LSID.Spouse), tree, stfLink);
            } else if (tag is GDMChildLink childLink) {
                result = GetPtrDescription("Child", tree, childLink); // FIXME: single form
            } else if (tag is GDMCustomEvent evt) {
                result = $"Event: {GKUtils.GetEventName(evt)}";
            } else if (tag is GDMNotes notes) {
                result = $"Notes Link: {notes.XRef}";
            } else if (tag is GDMMultimediaLink mediaLink) {
                result = $"Media Link: {mediaLink.XRef}";
            } else if (tag is GDMSourceCitation sourLink) {
                result = $"Source Link: {sourLink.XRef}, Page: {sourLink.Page}, CertaintyAssessment: {sourLink.CertaintyAssessment}";
            } else if (tag is GDMUserReference userRef) {
                result = $"UserRef: {GKUtils.GetUserReferenceStr(tree, userRef)}"; // FIXME: single form
            } else if (tag is GDMAddress addr) {
                result = $"Address: {addr.StringValue}";
            } else if (tag is GDMRepositoryCitation repoCit) {
                result = $"Repository Citation: {repoCit.StringValue}";
            } else if (tag is GDMFileReferenceWithTitle fileRef) {
                result = $"File Reference: {fileRef.StringValue}";
            } else if (tag is GDMAssociation asso) {
                result = $"{LangMan.LS(LSID.Association)}: {GKUtils.GetAssociationStr(tree, asso)}";
            } else if (tag is GDMLocationName locName) {
                result = $"Location Name: {locName.StringValue}";
            } else if (tag is GDMLocationLink locLink) {
                result = $"Location Link: {locLink.XRef}";
            } else if (tag is GDMMemberLink memberLink) {
                result = GetPtrDescription("Member", tree, memberLink); // FIXME: single form
            } else if (tag is GDMGroupLink groupLink) {
                result = GetPtrDescription(LangMan.LS(LSID.Group), tree, groupLink);
            } else if (tag is GDMSourceCallNumber callNum) {
                result = $"Call Number: {callNum.StringValue}";
            } else if (tag is GDMSourceData sourData) {
                result = $"Source Data: {sourData.StringValue}";
            } else if (tag is GDMMap map) {
                result = $"{LangMan.LS(LSID.Coordinates)}: {GKUtils.GetMapStr(tree, map)}";
            } else if (tag is GDMDNATest dnaTest) {
                result = $"DNA Test: {dnaTest.StringValue}";
            } else if (tag is GDMPointer ptr) {
                result = $"Unk ptr"; // All derived classes are in the list above!
            } else {
                result = $"Unk tag";
            }

            return result;
        }

        public static void GetFullDescription(GDMTree tree, GDMTag tag, StringList summary)
        {
            summary.Clear();
            if (tag == null) {
                summary.Add(" --- ");
                return;
            }

            string result = string.Empty;

            if (tag is GDMPersonalName persName) {
                result = $"Personal Name: {persName.StringValue}";
            } else if (tag is GDMChildToFamilyLink ctfLink) {
                result = $"ChildToFamily Link: {ctfLink.XRef}";
            } else if (tag is GDMSpouseToFamilyLink stfLink) {
                result = GetPtrDescription(LangMan.LS(LSID.Spouse), tree, stfLink);
            } else if (tag is GDMCustomEvent evt) {
                result = $"Event: {GKUtils.GetEventName(evt)}";
            } else if (tag is GDMNotes notes) {
                result = $"Notes Link: {notes.XRef}";
            } else if (tag is GDMMultimediaLink mediaLink) {
                result = $"Media Link: {mediaLink.XRef}";
            } else if (tag is GDMSourceCitation sourLink) {
                result = $"Source Link: {sourLink.XRef}, Page: {sourLink.Page}, CertaintyAssessment: {sourLink.CertaintyAssessment}";
            } else if (tag is GDMAddress addr) {
                GKUtils.ShowAddressSummary(addr, summary);
            } else if (tag is GDMRepositoryCitation repoCit) {
                result = $"Repository Citation: {repoCit.StringValue}";
            } else if (tag is GDMFileReferenceWithTitle fileRef) {
                result = $"File Reference: {fileRef.StringValue}";
            } else if (tag is GDMLocationName locName) {
                result = $"Location Name: {locName.StringValue}";
            } else if (tag is GDMLocationLink locLink) {
                result = $"Location Link: {locLink.XRef}";
            } else if (tag is GDMSourceCallNumber callNum) {
                result = $"Call Number: {callNum.StringValue}";
            } else if (tag is GDMSourceData sourData) {
                result = $"Source Data: {sourData.StringValue}";
            } else if (tag is GDMDNATest dnaTest) {
                result = $"DNA Test: {dnaTest.StringValue}";
            } else {
                result = GetBriefDescription(tree, tag);
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
                case GEDCOMTagType.RIN:
                    tagName = "AutomatedRecordID";
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
    }
}
