/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2019 by Sergey V. Zhdanovskih.
 *
 *  This file is part of "GEDKeeper".
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

using System;
using BSLib;
using GDModel;
using GDModel.Providers.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKCore.Types;

namespace GDModel.Providers.GEDCOM
{
    /// <summary>
    /// Class to check the GEDCOM format.
    /// </summary>
    public static class GEDCOMChecker
    {
        private static void TransformNote(GDMTree tree, GDMNotes note)
        {
            GDMNoteRecord noteRec = tree.CreateNote();
            noteRec.Lines.Assign(note.Lines);

            note.Clear();
            note.Value = noteRec;
        }

        private static void TransformMultimediaLink(GDMTree tree, GDMMultimediaLink mmLink)
        {
            string title = mmLink.Title;
            GDMMultimediaRecord mmRec = tree.CreateMultimedia();

            int num = mmLink.FileReferences.Count;
            for (int i = 0; i < num; i++) {
                GDMFileReference srcFileRef = mmLink.FileReferences[i];
                GDMFileReferenceWithTitle tgtFileRef = new GDMFileReferenceWithTitle(mmRec);

                tgtFileRef.LinkFile(srcFileRef.StringValue);

                if (srcFileRef.MultimediaFormat != GDMMultimediaFormat.mfNone) {
                    tgtFileRef.MultimediaFormat = srcFileRef.MultimediaFormat;
                }
                if (srcFileRef.MediaType != GDMMediaType.mtUnknown) {
                    tgtFileRef.MediaType = srcFileRef.MediaType;
                }
                tgtFileRef.Title = title;

                mmRec.FileReferences.Add(tgtFileRef);
            }

            mmLink.Clear();
            mmLink.Value = mmRec;
        }

        private static void TransformSourceCitation(GDMTree tree, GDMSourceCitation sourCit)
        {
            GDMSourceRecord sourRec = tree.CreateSource();

            StringList description = sourCit.Description;
            string page = sourCit.Page;
            int certaintyAssessment = sourCit.CertaintyAssessment;

            sourRec.Text.Lines.Assign(description);

            sourCit.Clear();
            sourCit.Value = sourRec;
            sourCit.Page = page;
            sourCit.CertaintyAssessment = certaintyAssessment;
        }

        private static void CheckTagWithLists(GDMTree tree, GEDCOMFormat format, GDMTagWithLists tag)
        {
            int num = tag.MultimediaLinks.Count;
            for (int i = 0; i < num; i++) {
                GDMMultimediaLink mmLink = tag.MultimediaLinks[i];
                if (!mmLink.IsPointer) TransformMultimediaLink(tree, mmLink);
            }

            num = tag.Notes.Count;
            for (int i = 0; i < num; i++) {
                GDMNotes note = tag.Notes[i];
                if (!note.IsPointer) TransformNote(tree, note);
            }

            num = tag.SourceCitations.Count;
            for (int i = 0; i < num; i++) {
                GDMSourceCitation sourCit = tag.SourceCitations[i];
                if (!sourCit.IsPointer) TransformSourceCitation(tree, sourCit);
            }
        }

        // TODO: refactor
        private static void CheckRecord_RepairTag(GDMTree tree, GEDCOMFormat format, GDMTagWithLists tag)
        {
            for (int i = tag.MultimediaLinks.Count - 1; i >= 0; i--) {
                GDMMultimediaLink mmLink = tag.MultimediaLinks[i];
                if (mmLink.IsPointer && mmLink.Value == null) tag.MultimediaLinks.DeleteAt(i);
            }

            for (int i = tag.Notes.Count - 1; i >= 0; i--) {
                GDMNotes note = tag.Notes[i];
                if (note.IsPointer && note.Value == null) tag.Notes.DeleteAt(i);
            }

            for (int i = tag.SourceCitations.Count - 1; i >= 0; i--) {
                GDMSourceCitation sourCit = tag.SourceCitations[i];
                if (sourCit.IsPointer && sourCit.Value == null) tag.SourceCitations.DeleteAt(i);
            }
        }

        private static void CheckPointerWithNotes(GDMTree tree, GEDCOMFormat format, GDMPointerWithNotes ptr)
        {
            // TODO: checkit!
            GDMRecord val = ptr.Value;
            if (!string.IsNullOrEmpty(ptr.XRef) && val == null) {
                ptr.Value = null;
            }

            int num = ptr.Notes.Count;
            for (int i = 0; i < num; i++) {
                GDMNotes note = ptr.Notes[i];
                if (!note.IsPointer) TransformNote(tree, note);
            }
        }

        private static void CheckIndividualEvent(GDMCustomEvent evt, GEDCOMFormat format)
        {
            // Fix for Family Tree Maker 2008 which exports occupation as generic EVEN events
            if (format == GEDCOMFormat.gf_FamilyTreeMaker) {
                string subtype = evt.Classification.ToLower();
                if (evt.Id == (int)GEDCOMTagType.EVEN && subtype == "occupation") {
                    evt.SetName(GEDCOMTagType.OCCU);
                    evt.Classification = string.Empty;
                }
            }
        }

        private static void CheckEventPlace(GDMCustomEvent aEvent)
        {
            GDMPlace place = aEvent.FindTag(GEDCOMTagName.PLAC, 0) as GDMPlace;
            if (place == null) return;

            GDMPointer placeLocation = place.FindTag(GEDCOMTagName._LOC, 0) as GDMPointer;
            if (placeLocation == null) return;

            if (placeLocation.XRef != "" && placeLocation.Value == null) {
                placeLocation.XRef = "";
            }

            if (place.StringValue != "") {
                GDMLocationRecord loc = placeLocation.Value as GDMLocationRecord;
                if (loc != null && place.StringValue != loc.LocationName) {
                    place.StringValue = loc.LocationName;
                }
            }
        }

        private static void CheckAttrCompatible(GDMTree tree, GEDCOMFormat format, GDMIndividualRecord iRec, GDMCustomEvent aEvent)
        {
        }

        private static void CheckURefCompatible(GDMIndividualRecord iRec, GDMUserReference userRef)
        {
        }

        private static void CheckPersonalName(GDMIndividualRecord iRec, GDMPersonalName persName, IBaseContext baseContext)
        {
            baseContext.CollectNameLangs(persName);
        }

        private static void CheckIndividualRecord(IBaseContext baseContext, GDMTree tree, GEDCOMFormat format,
                                                  GDMIndividualRecord iRec)
        {
            if (format == GEDCOMFormat.gf_Native) {
                for (int i = 0, num = iRec.Events.Count; i < num; i++) {
                    GDMCustomEvent evt = iRec.Events[i];

                    CheckEventPlace(evt);
                    CheckAttrCompatible(tree, format, iRec, evt);
                    CheckRecord_RepairTag(tree, format, evt);

                    baseContext.CollectEventValues(evt);
                }

                for (int i = 0, num = iRec.UserReferences.Count; i < num; i++) {
                    CheckURefCompatible(iRec, iRec.UserReferences[i]);
                }

                for (int i = 0, num = iRec.PersonalNames.Count; i < num; i++) {
                    CheckPersonalName(iRec, iRec.PersonalNames[i], baseContext);
                }
            } else {
                for (int i = 0, num = iRec.Events.Count; i < num; i++) {
                    GDMCustomEvent evt = iRec.Events[i];

                    CheckIndividualEvent(evt, format);
                    CheckTagWithLists(tree, format, evt);
                }

                for (int i = 0, num = iRec.ChildToFamilyLinks.Count; i < num; i++) {
                    CheckPointerWithNotes(tree, format, iRec.ChildToFamilyLinks[i]);
                }

                for (int i = 0, num = iRec.SpouseToFamilyLinks.Count; i < num; i++) {
                    CheckPointerWithNotes(tree, format, iRec.SpouseToFamilyLinks[i]);
                }

                for (int i = 0, num = iRec.Associations.Count; i < num; i++) {
                    CheckPointerWithNotes(tree, format, iRec.Associations[i]);
                }
            }

            for (int i = iRec.ChildToFamilyLinks.Count - 1; i >= 0; i--) {
                if (iRec.ChildToFamilyLinks[i].Family == null)
                    iRec.ChildToFamilyLinks.DeleteAt(i);
            }

            for (int i = iRec.SpouseToFamilyLinks.Count - 1; i >= 0; i--) {
                if (iRec.SpouseToFamilyLinks[i].Family == null)
                    iRec.SpouseToFamilyLinks.DeleteAt(i);
            }

            baseContext.ImportNames(iRec);
        }

        private static void CheckFamilyRecord(IBaseContext baseContext, GDMTree tree, GEDCOMFormat format,
                                              GDMFamilyRecord fam)
        {
            if (format == GEDCOMFormat.gf_Native) {
                int num = fam.Events.Count;
                for (int i = 0; i < num; i++) {
                    CheckEventPlace(fam.Events[i]);
                }
            } else {
                int num2 = fam.Events.Count;
                for (int i = 0; i < num2; i++) {
                    CheckTagWithLists(tree, format, fam.Events[i]);
                }
            }

            for (int i = fam.Children.Count - 1; i >= 0; i--) {
                if (fam.Children[i].Value == null)
                    fam.Children.DeleteAt(i);
            }

            GDMRecord val = fam.Husband.Value;
            if (!string.IsNullOrEmpty(fam.Husband.XRef) && val == null) {
                fam.Husband.Value = null;
            }

            val = fam.Wife.Value;
            if (!string.IsNullOrEmpty(fam.Wife.XRef) && val == null) {
                fam.Wife.Value = null;
            }
        }

        private static void CheckGroupRecord(GDMGroupRecord group)
        {
            for (int i = group.Members.Count - 1; i >= 0; i--) {
                GDMIndividualRecord mbr = group.Members[i].Individual;
                if (mbr == null) {
                    group.Members.DeleteAt(i);
                } else {
                    if (mbr.IndexOfGroup(group) < 0) {
                        group.Members.DeleteAt(i);
                    }
                }
            }
        }

        private static void CheckSourceRecord(GDMSourceRecord src)
        {
            for (int i = src.RepositoryCitations.Count - 1; i >= 0; i--) {
                GDMRecord val = src.RepositoryCitations[i].Value;
                if (val == null) {
                    src.RepositoryCitations.DeleteAt(i);
                }
            }
        }

        private static void CheckMultimediaRecord(GDMMultimediaRecord mmRec, GEDCOMFormat format, int fileVer)
        {
            for (int i = 0; i < mmRec.FileReferences.Count; i++) {
                GDMFileReferenceWithTitle fileRef = mmRec.FileReferences[i];

                GDMMultimediaFormat mmFormat = fileRef.MultimediaFormat;
                if (mmFormat == GDMMultimediaFormat.mfUnknown || mmFormat == GDMMultimediaFormat.mfNone) {
                    // tag 'FORM' can be corrupted or GEDCOMCore in past not recognize format attempt recovery
                    fileRef.MultimediaFormat = GDMFileReference.RecognizeFormat(fileRef.StringValue);
                }

                if (format == GEDCOMFormat.gf_Native && fileVer == 39) {
                    // the transition to normalized names after GKv39
                    // only for not direct references AND not relative references (platform specific paths)

                    var mediaStore = GKUtils.GetStoreType(fileRef);
                    if (mediaStore.StoreType != MediaStoreType.mstReference
                        && mediaStore.StoreType != MediaStoreType.mstRelativeReference) {
                        fileRef.StringValue = FileHelper.NormalizeFilename(fileRef.StringValue);
                    }
                }
            }
        }

        private static void CheckRecord(IBaseContext baseContext, GDMTree tree, GDMRecord rec,
                                        GEDCOMFormat format, int fileVer)
        {
            if (format != GEDCOMFormat.gf_Native) {
                int num = rec.MultimediaLinks.Count;
                for (int i = 0; i < num; i++) {
                    GDMMultimediaLink mmLink = rec.MultimediaLinks[i];
                    if (!mmLink.IsPointer) TransformMultimediaLink(tree, mmLink);
                }

                num = rec.Notes.Count;
                for (int i = 0; i < num; i++) {
                    GDMNotes note = rec.Notes[i];
                    if (!note.IsPointer) TransformNote(tree, note);
                }

                num = rec.SourceCitations.Count;
                for (int i = 0; i < num; i++) {
                    GDMSourceCitation sourCit = rec.SourceCitations[i];
                    if (!sourCit.IsPointer) TransformSourceCitation(tree, sourCit);
                }
            }

            switch (rec.RecordType) {
                case GDMRecordType.rtIndividual:
                    CheckIndividualRecord(baseContext, tree, format, rec as GDMIndividualRecord);
                    break;

                case GDMRecordType.rtFamily:
                    CheckFamilyRecord(baseContext, tree, format, rec as GDMFamilyRecord);
                    break;

                case GDMRecordType.rtGroup:
                    CheckGroupRecord(rec as GDMGroupRecord);
                    break;

                case GDMRecordType.rtSource:
                    CheckSourceRecord(rec as GDMSourceRecord);
                    break;

                case GDMRecordType.rtMultimedia:
                    CheckMultimediaRecord(rec as GDMMultimediaRecord, format, fileVer);
                    break;
            }
        }

        private static bool CheckRecordXRef(GDMRecord record)
        {
            string stdSign = GEDCOMUtils.GetSignByRecord(record);
            string xrefNum = record.GetXRefNum();
            string recXRef = record.XRef;

            return ((recXRef == stdSign + xrefNum) && record.GetId() >= 0);
        }

        private static void ConvertIdentifiers(GDMTree tree, IProgressController pc)
        {
            pc.ProgressInit(LangMan.LS(LSID.LSID_IDsCorrect), tree.RecordsCount * 2);
            GDMXRefReplacer repMap = new GDMXRefReplacer();
            try {
                int recsCount = tree.RecordsCount;
                for (int i = 0; i < recsCount; i++) {
                    GDMRecord rec = tree[i];
                    if (!CheckRecordXRef(rec)) {
                        string newXRef = tree.XRefIndex_NewXRef(rec);
                        repMap.AddXRef(rec, rec.XRef, newXRef);
                        rec.XRef = newXRef;
                    }
                    pc.ProgressStep();
                }

                tree.Header.ReplaceXRefs(repMap);
                for (int i = 0; i < recsCount; i++) {
                    GDMRecord rec = tree[i];
                    rec.ReplaceXRefs(repMap);
                    pc.ProgressStep();
                }
            } finally {
                repMap.Dispose();
                pc.ProgressDone();
            }
        }

        public static bool CheckGEDCOMFormat(GDMTree tree, IBaseContext baseContext, IProgressController pc)
        {
            if (tree == null)
                throw new ArgumentNullException("tree");

            if (baseContext == null)
                throw new ArgumentNullException("baseContext");

            if (pc == null)
                throw new ArgumentNullException("pc");

            bool result = false;

            try {
                GEDCOMFormat format = GEDCOMProvider.GetGEDCOMFormat(tree);
                int fileVer;
                // remove a deprecated features
                if (format == GEDCOMFormat.gf_Native) {
                    GDMHeader header = tree.Header;
                    GDMTag tag;

                    tag = header.FindTag("_ADVANCED", 0);
                    if (tag != null) header.DeleteTag("_ADVANCED");

                    tag = header.FindTag("_EXT_NAME", 0);
                    if (tag != null) header.DeleteTag("_EXT_NAME");

                    fileVer = ConvertHelper.ParseInt(header.Source.Version, GKData.APP_FORMAT_DEFVER);
                } else {
                    fileVer = -1;
                }

                pc.ProgressInit(LangMan.LS(LSID.LSID_FormatCheck), 100);
                try {
                    bool xrefValid = true;
                    bool isExtraneous = (format != GEDCOMFormat.gf_Native);

                    int progress = 0;
                    int num = tree.RecordsCount;
                    for (int i = 0; i < num; i++) {
                        GDMRecord rec = tree[i];
                        CheckRecord(baseContext, tree, rec, format, fileVer);

                        if (isExtraneous && xrefValid && !CheckRecordXRef(rec)) {
                            xrefValid = false;
                        }

                        int newProgress = (int)Math.Min(100, ((i + 1) * 100.0f) / num);
                        if (progress != newProgress) {
                            progress = newProgress;
                            pc.ProgressStep(progress);
                        }
                    }

                    // obsolete: AppHost.StdDialogs.ShowQuestionYN(LangMan.LS(LSID.LSID_IDsCorrectNeed))
                    if (!xrefValid) {
                        ConvertIdentifiers(tree, pc);
                    }

                    result = true;
                } finally {
                    pc.ProgressDone();
                }
            } catch (Exception ex) {
                Logger.LogWrite("GEDCOMChecker.CheckGEDCOMFormat(): " + ex.Message);
                AppHost.StdDialogs.ShowError(LangMan.LS(LSID.LSID_CheckGedComFailed));
            }

            return result;
        }

        public static void ClearGEDCOMFormat(GDMTree tree, IBaseContext baseContext, IProgressController pc)
        {
            // TODO
            // INDI: remove AFN, RFN
            // INDI,FAM: remove SUBM
        }
    }
}
