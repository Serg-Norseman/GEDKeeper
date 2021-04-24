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
    public class GEDCOMChecker
    {
        private IBaseContext fBaseContext;
        private GEDCOMFormat fFormat;
        private IProgressController fProgress;
        private GDMTree fTree;

        private GEDCOMChecker(IBaseContext baseContext, IProgressController progress)
        {
            fBaseContext = baseContext;
            fTree = fBaseContext.Tree;
            fFormat = GEDCOMProvider.GetGEDCOMFormat(fTree);
            fProgress = progress;
        }

        private void TransformNote(GDMNotes note)
        {
            GDMNoteRecord noteRec = fTree.CreateNote();
            noteRec.Lines.Assign(note.Lines);

            note.Clear();
            note.XRef = noteRec.XRef;
        }

        private void TransformMultimediaLink(GDMMultimediaLink mmLink)
        {
            string title = mmLink.Title;
            GDMMultimediaRecord mmRec = fTree.CreateMultimedia();

            int num = mmLink.FileReferences.Count;
            for (int i = 0; i < num; i++) {
                GDMFileReference srcFileRef = mmLink.FileReferences[i];
                GDMFileReferenceWithTitle tgtFileRef = new GDMFileReferenceWithTitle();

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

        private void TransformSourceCitation(GDMSourceCitation sourCit)
        {
            GDMSourceRecord sourRec = fTree.CreateSource();

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

            CheckTagWithNotes(sourRec);
            CheckTagWithMultimediaLinks(sourRec);
        }

        private void CheckTagWithNotes(IGDMStructWithNotes tag)
        {
            for (int i = tag.Notes.Count - 1; i >= 0; i--) {
                GDMNotes note = tag.Notes[i];
                if (!note.IsPointer) {
                    TransformNote(note);
                } else {
                    var noteRec = fTree.GetPtrValue<GDMNoteRecord>(note);
                    if (noteRec == null) tag.Notes.DeleteAt(i);
                }
            }
        }

        private void CheckTagWithSourceCitations(IGDMStructWithSourceCitations tag)
        {
            for (int i = tag.SourceCitations.Count - 1; i >= 0; i--) {
                GDMSourceCitation sourCit = tag.SourceCitations[i];
                if (!sourCit.IsPointer) {
                    TransformSourceCitation(sourCit);
                } else {
                    var sourRec = fTree.GetPtrValue<GDMSourceRecord>(sourCit);
                    if (sourRec == null) tag.SourceCitations.DeleteAt(i);
                }
            }
        }

        private void CheckTagWithMultimediaLinks(IGDMStructWithMultimediaLinks tag)
        {
            for (int i = tag.MultimediaLinks.Count - 1; i >= 0; i--) {
                GDMMultimediaLink mmLink = tag.MultimediaLinks[i];
                if (!mmLink.IsPointer) {
                    TransformMultimediaLink(mmLink);
                } else {
                    var mmRec = fTree.GetPtrValue<GDMMultimediaRecord>(mmLink);
                    if (mmRec == null) tag.MultimediaLinks.DeleteAt(i);
                }
            }
        }

        private void CheckPointerWithNotes(GDMPointerWithNotes ptr)
        {
            GDMRecord val = fTree.GetPtrValue<GDMRecord>(ptr);
            if (!string.IsNullOrEmpty(ptr.XRef) && val == null) {
                ptr.XRef = string.Empty;
            }

            CheckTagWithNotes(ptr);
        }

        private void CheckStructWL(IGDMStructWithLists swl)
        {
            CheckTagWithNotes(swl);
            CheckTagWithMultimediaLinks(swl);
            CheckTagWithSourceCitations(swl);
        }

        private void CheckEventPlace(GDMPlace place)
        {
            GDMPointer placeLocation = place.Location;
            GDMLocationRecord locRec = fTree.GetPtrValue<GDMLocationRecord>(placeLocation);

            if (placeLocation.XRef != "" && locRec == null) {
                placeLocation.XRef = "";
            }

            if (place.StringValue != "") {
                if (locRec != null && place.StringValue != locRec.LocationName) {
                    place.StringValue = locRec.LocationName;
                }
            }

            CheckTagWithNotes(place);
        }

        private void CheckEvent(GDMCustomEvent evt)
        {
            CheckStructWL(evt);

            // Fix for Family Tree Maker 2008 which exports occupation as generic EVEN events
            if (fFormat == GEDCOMFormat.gf_FamilyTreeMaker) {
                string subtype = evt.Classification.ToLower();
                if (evt.Id == (int)GEDCOMTagType.EVEN && subtype == "occupation") {
                    evt.SetName(GEDCOMTagType.OCCU);
                    evt.Classification = string.Empty;
                }
            }

            CheckEventPlace(evt.Place);
        }

        private void CheckUserRef(GDMIndividualRecord iRec, GDMUserReference userRef)
        {
        }

        private void CheckPersonalName(GDMPersonalName persName)
        {
            CheckTagWithNotes(persName);
            CheckTagWithSourceCitations(persName);

            fBaseContext.CollectNameLangs(persName);
        }

        private void CheckIndividualRecord(GDMIndividualRecord iRec)
        {
            for (int i = 0, num = iRec.Events.Count; i < num; i++) {
                GDMCustomEvent evt = iRec.Events[i];

                CheckEvent(evt);

                fBaseContext.CollectEventValues(evt);
            }

            for (int i = 0, num = iRec.UserReferences.Count; i < num; i++) {
                CheckUserRef(iRec, iRec.UserReferences[i]);
            }

            for (int i = 0, num = iRec.PersonalNames.Count; i < num; i++) {
                CheckPersonalName(iRec.PersonalNames[i]);
            }

            for (int i = iRec.ChildToFamilyLinks.Count - 1; i >= 0; i--) {
                var cfl = iRec.ChildToFamilyLinks[i];
                if (fTree.GetPtrValue(cfl) == null) {
                    iRec.ChildToFamilyLinks.DeleteAt(i);
                } else {
                    CheckPointerWithNotes(cfl);
                }
            }

            for (int i = iRec.SpouseToFamilyLinks.Count - 1; i >= 0; i--) {
                var sfl = iRec.SpouseToFamilyLinks[i];
                if (fTree.GetPtrValue(sfl) == null) {
                    iRec.SpouseToFamilyLinks.DeleteAt(i);
                } else {
                    CheckPointerWithNotes(sfl);
                }
            }

            for (int i = 0, num = iRec.Associations.Count; i < num; i++) {
                var asso = iRec.Associations[i];
                CheckPointerWithNotes(asso);
                CheckTagWithSourceCitations(asso);
            }

            fBaseContext.ImportNames(iRec);
        }

        private void CheckChildLink(GDMFamilyRecord fam, int index)
        {
            GDMIndividualLink childLink = fam.Children[index];
            var childRec = fTree.GetPtrValue<GDMIndividualRecord>(childLink);
            if (childRec == null) {
                fam.Children.DeleteAt(index);
                return;
            }

            if (fFormat == GEDCOMFormat.gf_AGES) {
                var frelTag = FindSubTagValue(childLink, "_FREL");
                var mrelTag = FindSubTagValue(childLink, "_MREL");
                if (frelTag == "ADOPTED" && mrelTag == "ADOPTED") {
                    GDMChildToFamilyLink ctfLink = childRec.FindChildToFamilyLink(fam);
                    ctfLink.PedigreeLinkageType = GDMPedigreeLinkageType.plAdopted;

                    childLink.DeleteTag("_FREL");
                    childLink.DeleteTag("_MREL");
                }
            }
        }

        private static string FindSubTagValue(GDMTag tag, string subTagName)
        {
            var subTag = tag.FindTag(subTagName, 0);
            return (subTag == null) ? string.Empty : subTag.StringValue;
        }

        private void CheckFamilyRecord(GDMFamilyRecord fam)
        {
            for (int i = 0, num = fam.Events.Count; i < num; i++) {
                GDMCustomEvent evt = fam.Events[i];
                CheckEvent(evt);
            }

            for (int i = fam.Children.Count - 1; i >= 0; i--) {
                CheckChildLink(fam, i);
            }

            GDMRecord val = fTree.GetPtrValue<GDMIndividualRecord>(fam.Husband);
            if (!string.IsNullOrEmpty(fam.Husband.XRef) && val == null) {
                fam.Husband.XRef = string.Empty;
            }

            val = fTree.GetPtrValue<GDMIndividualRecord>(fam.Wife);
            if (!string.IsNullOrEmpty(fam.Wife.XRef) && val == null) {
                fam.Wife.XRef = string.Empty;
            }
        }

        private void CheckGroupRecord(GDMGroupRecord group)
        {
            for (int i = group.Members.Count - 1; i >= 0; i--) {
                GDMIndividualRecord mbr = fTree.GetPtrValue(group.Members[i]);
                if (mbr == null) {
                    group.Members.DeleteAt(i);
                } else {
                    if (mbr.IndexOfGroup(group) < 0) {
                        group.Members.DeleteAt(i);
                    }
                }
            }
        }

        private void CheckSourceRecord(GDMSourceRecord src)
        {
            for (int i = src.RepositoryCitations.Count - 1; i >= 0; i--) {
                GDMRecord val = fTree.GetPtrValue<GDMRecord>(src.RepositoryCitations[i]);
                if (val == null) {
                    src.RepositoryCitations.DeleteAt(i);
                }
            }
        }

        private void CheckMultimediaRecord(GDMMultimediaRecord mmRec, int fileVer)
        {
            for (int i = 0; i < mmRec.FileReferences.Count; i++) {
                GDMFileReferenceWithTitle fileRef = mmRec.FileReferences[i];

                GDMMultimediaFormat mmFormat = fileRef.MultimediaFormat;
                if (mmFormat == GDMMultimediaFormat.mfUnknown || mmFormat == GDMMultimediaFormat.mfNone) {
                    // tag 'FORM' can be corrupted or GEDCOMCore in past not recognize format attempt recovery
                    fileRef.MultimediaFormat = GDMFileReference.RecognizeFormat(fileRef.StringValue);
                }

                if (fFormat == GEDCOMFormat.gf_Native && fileVer == 39) {
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

        private void CheckRecord(GDMRecord rec, int fileVer)
        {
            CheckStructWL(rec);

            // TODO
            // INDI: remove AFN, RFN - discuss???
            // INDI,FAM: remove SUBM - discuss???

            switch (rec.RecordType) {
                case GDMRecordType.rtIndividual:
                    CheckIndividualRecord(rec as GDMIndividualRecord);
                    break;

                case GDMRecordType.rtFamily:
                    CheckFamilyRecord(rec as GDMFamilyRecord);
                    break;

                case GDMRecordType.rtGroup:
                    CheckGroupRecord(rec as GDMGroupRecord);
                    break;

                case GDMRecordType.rtSource:
                    CheckSourceRecord(rec as GDMSourceRecord);
                    break;

                case GDMRecordType.rtMultimedia:
                    CheckMultimediaRecord(rec as GDMMultimediaRecord, fileVer);
                    break;
            }
        }

        private bool CheckRecordXRef(GDMRecord record)
        {
            string stdSign = GEDCOMUtils.GetSignByRecord(record);
            string xrefNum = record.GetXRefNum();
            string recXRef = record.XRef;

            return ((recXRef == stdSign + xrefNum) && record.GetId() >= 0);
        }

        private void ConvertIdentifiers()
        {
            fProgress.ProgressInit(LangMan.LS(LSID.LSID_IDsCorrect), fTree.RecordsCount * 2);
            GDMXRefReplacer repMap = new GDMXRefReplacer();
            try {
                int recsCount = fTree.RecordsCount;
                for (int i = 0; i < recsCount; i++) {
                    GDMRecord rec = fTree[i];
                    if (!CheckRecordXRef(rec)) {
                        string oldXRef = rec.XRef;
                        string newXRef = fTree.NewXRef(rec, true);
                        repMap.AddXRef(rec, oldXRef, newXRef);
                    }
                    fProgress.ProgressStep();
                }

                fTree.Header.ReplaceXRefs(repMap);
                for (int i = 0; i < recsCount; i++) {
                    GDMRecord rec = fTree[i];
                    rec.ReplaceXRefs(repMap);
                    fProgress.ProgressStep();
                }
            } finally {
                repMap.Dispose();
                fProgress.ProgressDone();
            }
        }

        private bool CheckFormat()
        {
            bool result = false;

            try {
                int fileVer;
                // remove a deprecated features
                if (fFormat == GEDCOMFormat.gf_Native) {
                    GDMHeader header = fTree.Header;
                    GDMTag tag;

                    tag = header.FindTag("_ADVANCED", 0);
                    if (tag != null) header.DeleteTag("_ADVANCED");

                    tag = header.FindTag("_EXT_NAME", 0);
                    if (tag != null) header.DeleteTag("_EXT_NAME");

                    fileVer = ConvertHelper.ParseInt(header.Source.Version, GKData.APP_FORMAT_DEFVER);
                } else {
                    fileVer = -1;
                }

                fProgress.ProgressInit(LangMan.LS(LSID.LSID_FormatCheck), 100);
                try {
                    bool xrefValid = true;
                    bool isExtraneous = (fFormat != GEDCOMFormat.gf_Native);

                    int progress = 0;
                    int num = fTree.RecordsCount;
                    for (int i = 0; i < num; i++) {
                        GDMRecord rec = fTree[i];
                        CheckRecord(rec, fileVer);

                        if (isExtraneous && xrefValid && !CheckRecordXRef(rec)) {
                            xrefValid = false;
                        }

                        int newProgress = (int)Math.Min(100, ((i + 1) * 100.0f) / num);
                        if (progress != newProgress) {
                            progress = newProgress;
                            fProgress.ProgressStep(progress);
                        }
                    }

                    // obsolete: AppHost.StdDialogs.ShowQuestionYN(LangMan.LS(LSID.LSID_IDsCorrectNeed))
                    if (!xrefValid) {
                        ConvertIdentifiers();
                    }

                    fTree.TrimExcess();

                    result = true;
                } finally {
                    fProgress.ProgressDone();
                }
            } catch (Exception ex) {
                Logger.WriteError("GEDCOMChecker.CheckFormat()", ex);
                AppHost.StdDialogs.ShowError(LangMan.LS(LSID.LSID_CheckGedComFailed));
            }

            return result;
        }

        public static bool CheckGEDCOMFormat(IBaseContext baseContext, IProgressController pc)
        {
            if (baseContext == null)
                throw new ArgumentNullException("baseContext");

            if (pc == null)
                throw new ArgumentNullException("pc");

            var instance = new GEDCOMChecker(baseContext, pc);
            return instance.CheckFormat();
        }
    }
}
