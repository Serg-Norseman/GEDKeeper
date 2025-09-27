/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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
using System.Collections.Generic;
using System.IO;
using BSLib;
using GKCore;
using GKCore.Design;
using GKCore.Locales;
using GKCore.Media;

namespace GDModel.Providers.GEDCOM
{
    /// <summary>
    /// Class to check the GEDCOM format and data model.
    /// The logical integrity of data in the database is checked in class <see cref="GKCore.Tools.TreeInspector"/>.
    /// </summary>
    public class GEDCOMChecker
    {
        private const string EmptyRecordContent = "---";

        private readonly BaseContext fBaseContext;
        private readonly int fFileVer;
        private readonly GEDCOMFormat fFormat;
        private readonly string fMediaContainerName;
        private readonly IProgressController fProgress;
        private readonly GDMTree fTree;

        private GEDCOMChecker(BaseContext baseContext, IProgressController progress)
        {
            fBaseContext = baseContext;
            fTree = fBaseContext.Tree;
            fProgress = progress;

            fMediaContainerName = Path.GetFileNameWithoutExtension(fBaseContext.FileName);

            fFormat = GEDCOMProvider.GetGEDCOMFormat(fTree, out _);
            fFileVer = (fFormat == GEDCOMFormat.Native) ? ConvertHelper.ParseInt(fTree.Header.Source.Version, GKData.APP_FORMAT_DEFVER) : -1;
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

                if (srcFileRef.GetMultimediaFormat() != GDMMultimediaFormat.mfNone) {
                    tgtFileRef.MultimediaFormat = srcFileRef.MultimediaFormat;
                }
                if (srcFileRef.MediaType != GDMMediaType.mtUnknown) {
                    tgtFileRef.MediaType = srcFileRef.MediaType;
                }
                tgtFileRef.Title = title;

                mmRec.FileReferences.Add(tgtFileRef);
            }

            var isPrimary = mmLink.IsPrimary;
            var isPrimaryCutout = mmLink.IsPrimaryCutout;
            GDMCutoutPosition cutoutPosition = new GDMCutoutPosition();
            cutoutPosition.Assign(mmLink.CutoutPosition);

            mmLink.Clear();
            mmLink.XRef = mmRec.XRef;

            mmLink.IsPrimary = isPrimary;
            mmLink.IsPrimaryCutout = isPrimaryCutout;
            mmLink.CutoutPosition.Assign(cutoutPosition);
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
            if (!tag.HasNotes) return;

            for (int i = tag.Notes.Count - 1; i >= 0; i--) {
                GDMNotes note = tag.Notes[i];
                if (!note.IsPointer) {
                    TransformNote(note);
                } else {
                    var noteRec = fTree.GetPtrValue<GDMNoteRecord>(note);
                    if (noteRec == null) tag.Notes.RemoveAt(i);
                }
            }
        }

        private void CheckTagWithSourceCitations(IGDMStructWithSourceCitations tag)
        {
            if (!tag.HasSourceCitations) return;

            for (int i = tag.SourceCitations.Count - 1; i >= 0; i--) {
                GDMSourceCitation sourCit = tag.SourceCitations[i];
                if (!sourCit.IsPointer) {
                    TransformSourceCitation(sourCit);
                } else {
                    var sourRec = fTree.GetPtrValue<GDMSourceRecord>(sourCit);
                    if (sourRec == null) tag.SourceCitations.RemoveAt(i);
                }
            }
        }

        private void CheckTagWithMultimediaLinks(IGDMStructWithMultimediaLinks tag)
        {
            if (!tag.HasMultimediaLinks) return;

            for (int i = tag.MultimediaLinks.Count - 1; i >= 0; i--) {
                GDMMultimediaLink mmLink = tag.MultimediaLinks[i];
                if (!mmLink.IsPointer) {
                    TransformMultimediaLink(mmLink);
                } else {
                    var mmRec = fTree.GetPtrValue<GDMMultimediaRecord>(mmLink);
                    if (mmRec == null) tag.MultimediaLinks.RemoveAt(i);
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

        private void CheckEventPlace(GDMPlace place, GDMCustomEvent evt)
        {
            GDMPointer placeLocation = place.Location;
            GDMLocationRecord locRec = fTree.GetPtrValue<GDMLocationRecord>(placeLocation);

            // if the pointer is damaged
            if (placeLocation.XRef != "" && locRec == null) {
                placeLocation.XRef = "";
            }

            /*if (place.StringValue != "" && locRec != null) {
                place.StringValue = GKUtils.GetLocationNameExt(locRec, evt.Date.Value);
            }*/

            CheckTagWithNotes(place);
        }

        private void CheckEvent(GDMCustomEvent evt)
        {
            CheckTagWithNotes(evt);
            CheckTagWithMultimediaLinks(evt);
            CheckTagWithSourceCitations(evt);

            var tagType = (GEDCOMTagType)evt.Id;
            string evType = string.IsNullOrEmpty(evt.Classification) ? string.Empty : evt.Classification;

            switch (fFormat) {
                // Fix for Family Tree Maker 2008 which exports occupation as generic EVEN events
                case GEDCOMFormat.FamilyTreeMaker: {
                        if (tagType == GEDCOMTagType.EVEN && evType.ToLower() == "occupation") {
                            evt.SetName(GEDCOMTagType.OCCU);
                            evt.Classification = string.Empty;
                        }
                        break;
                    }

                // FIXME: move to base repair tool
                // StrValue can be address, or comment, or place!
                /*case GEDCOMFormat.gf_Native:
                    if (tagType == GEDCOMTagType.RESI && !string.IsNullOrEmpty(evt.StringValue)) {
                        if (string.IsNullOrEmpty(evt.Address.Lines.Text)) {
                            evt.Address.SetAddressText(evt.StringValue);
                        }
                        evt.StringValue = string.Empty;
                    }
                    break;*/
            }

            if (evt.HasAddress) {
                CheckAddress(evt.Address);
            }

            if (evt.HasPlace) {
                CheckEventPlace(evt.Place, evt);
            }

            string key = evt.GetTagName() + ":" + evType;
            fBaseContext.EventStats.Increment(key);
        }

        private void CheckUserRef(GDMRecord rec, GDMUserReference userRef)
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
            if (iRec.HasEvents) {
                for (int i = 0, num = iRec.Events.Count; i < num; i++) {
                    GDMCustomEvent evt = iRec.Events[i];

                    CheckEvent(evt);

                    fBaseContext.CollectEventValues(evt);
                }
            }

            for (int i = 0, num = iRec.PersonalNames.Count; i < num; i++) {
                CheckPersonalName(iRec.PersonalNames[i]);
            }

            for (int i = iRec.ChildToFamilyLinks.Count - 1; i >= 0; i--) {
                var cfl = iRec.ChildToFamilyLinks[i];
                if (fTree.GetPtrValue(cfl) == null) {
                    iRec.ChildToFamilyLinks.RemoveAt(i);
                } else {
                    CheckPointerWithNotes(cfl);
                }
            }

            for (int i = iRec.SpouseToFamilyLinks.Count - 1; i >= 0; i--) {
                var sfl = iRec.SpouseToFamilyLinks[i];
                if (fTree.GetPtrValue(sfl) == null) {
                    iRec.SpouseToFamilyLinks.RemoveAt(i);
                } else {
                    CheckPointerWithNotes(sfl);
                }
            }

            if (iRec.HasAssociations) {
                for (int i = 0, num = iRec.Associations.Count; i < num; i++) {
                    var asso = iRec.Associations[i];
                    CheckPointerWithNotes(asso);
                    CheckTagWithSourceCitations(asso);
                }
            }

            fBaseContext.ImportNames(iRec);

            if (fFormat == GEDCOMFormat.RootsMagic) {
                // _FSFTID -> fsft
                var fsftTag = FindSubTagValue(iRec, "_FSFTID");
                if (!string.IsNullOrEmpty(fsftTag)) {
                    iRec.AddTag(new GDMValueTag((int)GEDCOMTagType.RFN, "fsft:" + fsftTag));
                    iRec.DeleteTag("_FSFTID");
                }
            }

            // Empty records in files from other programs
            if (iRec.PersonalNames.Count == 0) {
                var name = new GDMPersonalName();
                // when saving protection from skipping
                name.Given = EmptyRecordContent;
                iRec.PersonalNames.Add(name);
            }
        }

        private void CheckChildLink(GDMFamilyRecord fam, int index)
        {
            GDMIndividualLink childLink = fam.Children[index];
            var childRec = fTree.GetPtrValue<GDMIndividualRecord>(childLink);
            if (childRec == null) {
                fam.Children.RemoveAt(index);
                return;
            }

            if (fFormat == GEDCOMFormat.AGES) {
                var frelTag = FindSubTagValue(childLink, "_FREL");
                var mrelTag = FindSubTagValue(childLink, "_MREL");
                if (frelTag == "ADOPTED" && mrelTag == "ADOPTED") {
                    GDMChildToFamilyLink ctfLink = childRec.FindChildToFamilyLink(fam);
                    if (ctfLink != null) {
                        ctfLink.PedigreeLinkageType = GDMPedigreeLinkageType.plAdopted;

                        childLink.DeleteTag("_FREL");
                        childLink.DeleteTag("_MREL");
                    }
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
            if (fam.HasEvents) {
                for (int i = 0, num = fam.Events.Count; i < num; i++) {
                    GDMCustomEvent evt = fam.Events[i];
                    CheckEvent(evt);
                }
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
                    group.Members.RemoveAt(i);
                } else {
                    if (mbr.IndexOfGroup(group) < 0) {
                        group.Members.RemoveAt(i);
                    }
                }
            }
        }

        private void CheckSourceRecord(GDMSourceRecord src)
        {
            for (int i = src.RepositoryCitations.Count - 1; i >= 0; i--) {
                GDMRecord val = fTree.GetPtrValue<GDMRecord>(src.RepositoryCitations[i]);
                if (val == null) {
                    src.RepositoryCitations.RemoveAt(i);
                }
            }
        }

        private void CheckMultimediaRecord(GDMMultimediaRecord mmRec)
        {
            for (int i = 0; i < mmRec.FileReferences.Count; i++) {
                GDMFileReferenceWithTitle fileRef = mmRec.FileReferences[i];

                GDMMultimediaFormat mmFormat = fileRef.GetMultimediaFormat();
                if (mmFormat == GDMMultimediaFormat.mfUnknown || mmFormat == GDMMultimediaFormat.mfNone) {
                    // tag 'FORM' can be corrupted or GEDCOMCore in past not recognize format attempt recovery
                    fileRef.MultimediaFormat = GDMFileReference.GetMultimediaExt(fileRef.StringValue);
                }

                CheckMediaStoreType(fileRef);
            }

            // Empty records in files from other programs
            if (mmRec.FileReferences.Count == 0) {
                var fileRef = new GDMFileReferenceWithTitle();
                // when saving protection from skipping
                fileRef.Title = EmptyRecordContent;
                mmRec.FileReferences.Add(fileRef);
            }
        }

        private void CheckMediaStoreType(GDMFileReferenceWithTitle fileRef)
        {
            string filePath;
            var storeType = MediaStore.GetStoreType(fileRef.StringValue, out filePath);

            // for external files, the default path type should be defined as absolute

            if (fFormat == GEDCOMFormat.Native) {
                if (fFileVer < 48) {
                    // the transition to normalized names for all paths
                    // https://en.wikipedia.org/wiki/File_URI_scheme

                    if (storeType != MediaStoreType.mstURL) {
                        filePath = FileHelper.NormalizeFilename(filePath);
                    }

                    switch (storeType) {
                        case MediaStoreType.mstReference_Old:
                            fileRef.StringValue = $"file:///{filePath}";
                            break;

                        case MediaStoreType.mstRelativeReference_Old:
                            filePath = MediaStore.NormalizeRelativePath(filePath);
                            fileRef.StringValue = $"file:{filePath}";
                            break;

                        case MediaStoreType.mstArchive_Old:
                            fileRef.StringValue = $"arcp:///{filePath}";
                            break;

                        case MediaStoreType.mstStorage_Old:
                            fileRef.StringValue = $"file:./{fMediaContainerName}/{filePath}";
                            break;
                    }
                } else if (fFileVer <= 39) {
                    // obsolete
                    // the transition to normalized names after GKv39
                    // only for not direct references AND not relative references (platform specific paths)

                    if (storeType != MediaStoreType.mstReference && storeType != MediaStoreType.mstRelativeReference) {
                        fileRef.StringValue = FileHelper.NormalizeFilename(fileRef.StringValue);
                    }
                }
            } else {

            }
        }

        private void CheckNoteRecord(GDMNoteRecord noteRec)
        {
            // Empty records in files from other programs
            if (noteRec.Lines.Count == 0) {
                // when saving protection from skipping
                noteRec.Lines.Text = EmptyRecordContent;
            }
        }

        // compatibility issue #635
        private static void CheckAddress(GDMAddress address)
        {
            var lines = address.Lines;

            if (lines.Count == 0 || (lines.Count == 1 && string.IsNullOrEmpty(lines[0]))) {
                if (!string.IsNullOrEmpty(address.AddressLine1)) {
                    lines.Add(address.AddressLine1);
                    address.AddressLine1 = string.Empty;
                }

                if (!string.IsNullOrEmpty(address.AddressLine2)) {
                    lines.Add(address.AddressLine2);
                    address.AddressLine2 = string.Empty;
                }

                if (!string.IsNullOrEmpty(address.AddressLine3)) {
                    lines.Add(address.AddressLine3);
                    address.AddressLine3 = string.Empty;
                }
            }
        }

        private void CheckRepositoryRecord(GDMRepositoryRecord repoRec)
        {
            if (repoRec.HasAddress) {
                CheckAddress(repoRec.Address);
            }
        }

        private void CheckRecord(GDMRecord rec)
        {
            CheckTagWithNotes(rec);
            CheckTagWithMultimediaLinks(rec);
            CheckTagWithSourceCitations(rec);

            if (rec.HasUserReferences) {
                for (int i = 0, num = rec.UserReferences.Count; i < num; i++) {
                    CheckUserRef(rec, rec.UserReferences[i]);
                }
            }

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
                    CheckMultimediaRecord(rec as GDMMultimediaRecord);
                    break;

                case GDMRecordType.rtNote:
                    CheckNoteRecord(rec as GDMNoteRecord);
                    break;

                case GDMRecordType.rtRepository:
                    CheckRepositoryRecord(rec as GDMRepositoryRecord);
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
            if (fProgress != null)
                fProgress.Begin(LangMan.LS(LSID.IDsCorrect), fTree.RecordsCount * 2);

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

                    if (fProgress != null)
                        fProgress.Increment();
                }

                fTree.Header.ReplaceXRefs(repMap);
                for (int i = 0; i < recsCount; i++) {
                    GDMRecord rec = fTree[i];
                    rec.ReplaceXRefs(repMap);

                    if (fProgress != null)
                        fProgress.Increment();
                }
            } finally {
                repMap.Dispose();

                if (fProgress != null)
                    fProgress.End();
            }
        }

        private bool CheckFormat()
        {
            bool result = false;

            try {
                // remove a deprecated features
                if (fFormat == GEDCOMFormat.Native) {
                    GDMHeader header = fTree.Header;
                    GDMTag tag;

                    tag = header.FindTag("_ADVANCED", 0);
                    if (tag != null) header.DeleteTag("_ADVANCED");

                    tag = header.FindTag("_EXT_NAME", 0);
                    if (tag != null) header.DeleteTag("_EXT_NAME");
                }

                if (fProgress != null)
                    fProgress.Begin(LangMan.LS(LSID.FormatCheck), 100);

                try {
                    bool xrefValid = true;
                    bool isExtraneous = (fFormat != GEDCOMFormat.Native);

                    int progress = 0;
                    int num = fTree.RecordsCount;
                    for (int i = 0; i < num; i++) {
                        GDMRecord rec = fTree[i];
                        CheckRecord(rec);

                        if (isExtraneous && xrefValid && !CheckRecordXRef(rec)) {
                            xrefValid = false;
                        }

                        if (fProgress != null) {
                            int newProgress = (int)Math.Min(100, ((i + 1) * 100.0f) / num);
                            if (progress != newProgress) {
                                progress = newProgress;
                                fProgress.StepTo(progress);
                            }
                        }
                    }

                    if (!xrefValid) {
                        ConvertIdentifiers();
                    }

                    fTree.TrimExcess();

                    result = true;
                } finally {
                    if (fProgress != null)
                        fProgress.End();
                }
            } catch (Exception ex) {
                Logger.WriteError("GEDCOMChecker.CheckFormat()", ex);
                AppHost.StdDialogs.ShowError(LangMan.LS(LSID.CheckGedComFailed));
            }

            return result;
        }

        public static bool CheckGEDCOMFormat(BaseContext baseContext, IProgressController pc)
        {
            if (baseContext == null)
                throw new ArgumentNullException("baseContext");

            var instance = new GEDCOMChecker(baseContext, pc);
            return instance.CheckFormat();
        }


        private struct LocPair
        {
            public GDMLocationRecord Location;
            public GDMCustomDate Date;

            public LocPair(GDMLocationRecord location, GDMCustomDate date)
            {
                Location = location;
                Date = date;
            }
        }


        public static void SyncTreeLocations(BaseContext baseContext, IProgressController pc)
        {
            if (baseContext == null)
                throw new ArgumentNullException("baseContext");

            if (pc == null)
                throw new ArgumentNullException("pc");

            try {
                var tree = baseContext.Tree;
                pc.Begin(LangMan.LS(LSID.FormatCheck), 100);

                try {
                    var cache = new Dictionary<LocPair, string>();

                    int progress = 0;
                    int num = tree.RecordsCount;
                    for (int i = 0; i < num; i++) {
                        GDMRecord rec = tree[i];
                        if (rec.RecordType != GDMRecordType.rtIndividual && rec.RecordType != GDMRecordType.rtFamily) continue;

                        var rwe = rec as GDMRecordWithEvents;
                        if (!rwe.HasEvents) continue;

                        for (int k = 0, num2 = rwe.Events.Count; k < num2; k++) {
                            GDMCustomEvent evt = rwe.Events[k];
                            if (!evt.HasPlace) continue;

                            GDMLocationRecord locRec = tree.GetPtrValue<GDMLocationRecord>(evt.Place.Location);
                            if (locRec != null) {
                                var pair = new LocPair(locRec, evt.Date.Value);
                                if (!cache.TryGetValue(pair, out string locName)) {
                                    locName = GKUtils.GetLocationNameExt(locRec, evt.Date.Value);
                                    cache.Add(pair, locName);
                                }
                                evt.Place.StringValue = locName;
                            }
                        }

                        int newProgress = (int)Math.Min(100, ((i + 1) * 100.0f) / num);
                        if (progress != newProgress) {
                            progress = newProgress;
                            pc.StepTo(progress);
                        }
                    }
                } finally {
                    pc.End();
                }
            } catch (Exception ex) {
                Logger.WriteError("GEDCOMChecker.SyncTreeLocations()", ex);
            }
        }
    }
}
