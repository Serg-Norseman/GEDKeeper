/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2021 by Sergey V. Zhdanovskih.
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
            note.XRef = noteRec.XRef;
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
            mmLink.XRef = mmRec.XRef;
        }

        private static void TransformSourceCitation(GDMTree tree, GEDCOMFormat format, GDMSourceCitation sourCit)
        {
            GDMSourceRecord sourRec = tree.CreateSource();

            sourRec.Title.Lines.Assign(sourCit.Description);
            sourRec.Text.Lines.Assign(sourCit.Text.Lines);

            // transfers notes and multimedia from the citation to the source record
            sourRec.AssignList(sourCit.Notes, sourRec.Notes);
            sourRec.AssignList(sourCit.MultimediaLinks, sourRec.MultimediaLinks);

            sourCit.Description.Clear();
            sourCit.Text.Clear();
            sourCit.Notes.Clear();
            sourCit.MultimediaLinks.Clear();
            sourCit.XRef = sourRec.XRef;

            CheckTagWithNotes(tree, format, sourRec);
            CheckTagWithMultimediaLinks(tree, format, sourRec);
        }

        private static void CheckTagWithNotes(GDMTree tree, GEDCOMFormat format, IGDMStructWithNotes tag)
        {
            for (int i = tag.Notes.Count - 1; i >= 0; i--) {
                GDMNotes note = tag.Notes[i];
                if (!note.IsPointer) {
                    TransformNote(tree, note);
                } else {
                    var noteRec = tree.GetPtrValue<GDMNoteRecord>(note);
                    if (noteRec == null) tag.Notes.DeleteAt(i);
                }
            }
        }

        private static void CheckTagWithSourceCitations(GDMTree tree, GEDCOMFormat format, IGDMStructWithSourceCitations tag)
        {
            for (int i = tag.SourceCitations.Count - 1; i >= 0; i--) {
                GDMSourceCitation sourCit = tag.SourceCitations[i];
                if (!sourCit.IsPointer) {
                    TransformSourceCitation(tree, format, sourCit);
                } else {
                    var sourRec = tree.GetPtrValue<GDMSourceRecord>(sourCit);
                    if (sourRec == null) tag.SourceCitations.DeleteAt(i);
                }
            }
        }

        private static void CheckTagWithMultimediaLinks(GDMTree tree, GEDCOMFormat format, IGDMStructWithMultimediaLinks tag)
        {
            for (int i = tag.MultimediaLinks.Count - 1; i >= 0; i--) {
                GDMMultimediaLink mmLink = tag.MultimediaLinks[i];
                if (!mmLink.IsPointer) {
                    TransformMultimediaLink(tree, mmLink);
                } else {
                    var mmRec = tree.GetPtrValue<GDMMultimediaRecord>(mmLink);
                    if (mmRec == null) tag.MultimediaLinks.DeleteAt(i);
                }
            }
        }

        private static void CheckPointerWithNotes(GDMTree tree, GEDCOMFormat format, GDMPointerWithNotes ptr)
        {
            GDMRecord val = tree.GetPtrValue<GDMRecord>(ptr);
            if (!string.IsNullOrEmpty(ptr.XRef) && val == null) {
                ptr.XRef = string.Empty;
            }

            CheckTagWithNotes(tree, format, ptr);
        }

        private static void CheckStructWL(GDMTree tree, GEDCOMFormat format, IGDMStructWithLists swl)
        {
            CheckTagWithNotes(tree, format, swl);
            CheckTagWithMultimediaLinks(tree, format, swl);
            CheckTagWithSourceCitations(tree, format, swl);
        }

        private static void CheckEventPlace(GDMTree tree, GEDCOMFormat format, GDMPlace place)
        {
            GDMPointer placeLocation = place.Location;
            GDMLocationRecord locRec = tree.GetPtrValue<GDMLocationRecord>(placeLocation);

            if (placeLocation.XRef != "" && locRec == null) {
                placeLocation.XRef = "";
            }

            if (place.StringValue != "") {
                if (locRec != null && place.StringValue != locRec.LocationName) {
                    place.StringValue = locRec.LocationName;
                }
            }

            CheckTagWithNotes(tree, format, place);
        }

        private static void CheckEvent(GDMTree tree, GEDCOMFormat format, GDMCustomEvent evt)
        {
            CheckStructWL(tree, format, evt);

            // Fix for Family Tree Maker 2008 which exports occupation as generic EVEN events
            if (format == GEDCOMFormat.gf_FamilyTreeMaker) {
                string subtype = evt.Classification.ToLower();
                if (evt.Id == (int)GEDCOMTagType.EVEN && subtype == "occupation") {
                    evt.SetName(GEDCOMTagType.OCCU);
                    evt.Classification = string.Empty;
                }
            }

            CheckEventPlace(tree, format, evt.Place);
        }

        private static void CheckUserRef(GDMIndividualRecord iRec, GDMUserReference userRef)
        {
        }

        private static void CheckPersonalName(IBaseContext baseContext, GDMTree tree, GEDCOMFormat format, GDMPersonalName persName)
        {
            CheckTagWithNotes(tree, format, persName);
            CheckTagWithSourceCitations(tree, format, persName);

            baseContext.CollectNameLangs(persName);
        }

        private static void CheckIndividualRecord(IBaseContext baseContext, GDMTree tree, GEDCOMFormat format,
                                                  GDMIndividualRecord iRec)
        {
            for (int i = 0, num = iRec.Events.Count; i < num; i++) {
                GDMCustomEvent evt = iRec.Events[i];

                CheckEvent(tree, format, evt);

                baseContext.CollectEventValues(evt);
            }

            for (int i = 0, num = iRec.UserReferences.Count; i < num; i++) {
                CheckUserRef(iRec, iRec.UserReferences[i]);
            }

            for (int i = 0, num = iRec.PersonalNames.Count; i < num; i++) {
                CheckPersonalName(baseContext, tree, format, iRec.PersonalNames[i]);
            }

            for (int i = iRec.ChildToFamilyLinks.Count - 1; i >= 0; i--) {
                var cfl = iRec.ChildToFamilyLinks[i];
                if (cfl.Family == null) {
                    iRec.ChildToFamilyLinks.DeleteAt(i);
                } else {
                    CheckPointerWithNotes(tree, format, cfl);
                }
            }

            for (int i = iRec.SpouseToFamilyLinks.Count - 1; i >= 0; i--) {
                var sfl = iRec.SpouseToFamilyLinks[i];
                if (sfl.Family == null) {
                    iRec.SpouseToFamilyLinks.DeleteAt(i);
                } else {
                    CheckPointerWithNotes(tree, format, sfl);
                }
            }

            for (int i = 0, num = iRec.Associations.Count; i < num; i++) {
                var asso = iRec.Associations[i];
                CheckPointerWithNotes(tree, format, asso);
                CheckTagWithSourceCitations(tree, format, asso);
            }

            baseContext.ImportNames(iRec);
        }

        private static void CheckFamilyRecord(IBaseContext baseContext, GDMTree tree, GEDCOMFormat format,
                                              GDMFamilyRecord fam)
        {
            for (int i = 0, num = fam.Events.Count; i < num; i++) {
                GDMCustomEvent evt = fam.Events[i];

                CheckEvent(tree, format, evt);
            }

            for (int i = fam.Children.Count - 1; i >= 0; i--) {
                var childRec = baseContext.Tree.GetPtrValue<GDMIndividualRecord>(fam.Children[i]);
                if (childRec == null)
                    fam.Children.DeleteAt(i);
            }

            GDMRecord val = baseContext.Tree.GetPtrValue<GDMIndividualRecord>(fam.Husband);
            if (!string.IsNullOrEmpty(fam.Husband.XRef) && val == null) {
                fam.Husband.XRef = string.Empty;
            }

            val = baseContext.Tree.GetPtrValue<GDMIndividualRecord>(fam.Wife);
            if (!string.IsNullOrEmpty(fam.Wife.XRef) && val == null) {
                fam.Wife.XRef = string.Empty;
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

        private static void CheckSourceRecord(IBaseContext baseContext, GDMSourceRecord src)
        {
            for (int i = src.RepositoryCitations.Count - 1; i >= 0; i--) {
                GDMRecord val = baseContext.Tree.GetPtrValue<GDMRecord>(src.RepositoryCitations[i]);
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
            CheckStructWL(tree, format, rec);

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
                    CheckSourceRecord(baseContext, rec as GDMSourceRecord);
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
                        string oldXRef = rec.XRef;
                        string newXRef = tree.NewXRef(rec, true);
                        repMap.AddXRef(rec, oldXRef, newXRef);
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

                    tree.TrimExcess();

                    result = true;
                } finally {
                    pc.ProgressDone();
                }
            } catch (Exception ex) {
                Logger.WriteError("GEDCOMChecker.CheckGEDCOMFormat()", ex);
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
