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
using System.Security.Cryptography;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using BSLib;
using GDModel;
using GDModel.Providers;
using GDModel.Providers.FamilyShow;
using GDModel.Providers.GEDCOM;
using GDModel.Providers.GedML;
using GDModel.Providers.GEDZIP;
using GKCore.Controllers;
using GKCore.Cultures;
using GKCore.Design;
using GKCore.Design.Graphics;
using GKCore.Design.Views;
using GKCore.Interfaces;
using GKCore.Names;
using GKCore.Operations;
using GKCore.Options;
using GKCore.Search;
using GKCore.Types;

namespace GKCore
{
    /// <summary>
    ///
    /// </summary>
    public class BaseContext : BaseObject, IBaseContext
    {
        #region Private fields

        private ICulture fCulture;
        private GDMLanguageID fDefaultLanguage;
        private string fFileName;
        private bool fModified;
        private ShieldState fShieldState;

        private readonly FreqCollection<string> fEventStats;
        private readonly FreqCollection<GDMLanguageID> fLangStats;
        private readonly List<GDMRecord> fLockedRecords;
        private readonly GDMTree fTree;
        private readonly ValuesCollection fValuesCollection;
        private readonly IBaseWindow fViewer;
        private readonly ChangeTracker fUndoman;

        #endregion

        #region Public properties

        public ICulture Culture
        {
            get
            {
                GDMLanguageID langID = fTree.Header.Language;
                if (fCulture == null || fCulture.Language != langID) {
                    fCulture = CulturesPool.DefineCulture(langID);
                }
                return fCulture;
            }
        }

        public GDMLanguageID DefaultLanguage
        {
            get { return fDefaultLanguage; }
            set { fDefaultLanguage = value; }
        }

        public FreqCollection<string> EventStats
        {
            get { return fEventStats; }
        }

        public string FileName
        {
            get { return fFileName; }
        }

        public FreqCollection<GDMLanguageID> LangStats
        {
            get { return fLangStats; }
        }

        public bool Modified
        {
            get
            {
                return fModified;
            }
            set
            {
                fModified = value;

                var eventHandler = ModifiedChanged;
                if (eventHandler != null) eventHandler(this, null);
            }
        }

        public GDMTree Tree
        {
            get { return fTree; }
        }

        public ChangeTracker Undoman
        {
            get { return fUndoman; }
        }

        public ValuesCollection ValuesCollection
        {
            get { return fValuesCollection; }
        }

        public IBaseWindow Viewer
        {
            get { return fViewer; }
        }

        public ShieldState ShieldState
        {
            get
            {
                return fShieldState;
            }
            set
            {
                if (fShieldState != value) {
                    fShieldState = value;

                    if (fViewer != null) {
                        fViewer.RefreshLists(false);
                    }
                }
            }
        }

        public event EventHandler ModifiedChanged;

        #endregion

        #region Instance control

        public BaseContext(IBaseWindow viewer)
        {
            fFileName = "";
            fTree = new GDMTree();
            fViewer = viewer;
            fUndoman = new ChangeTracker(this);
            fValuesCollection = new ValuesCollection();
            fLockedRecords = new List<GDMRecord>();

            fLangStats = new FreqCollection<GDMLanguageID>();
            fEventStats = new FreqCollection<string>();
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                fTree.Dispose();
            }
            base.Dispose(disposing);
        }

        #endregion

        #region Data Manipulation

        public void CollectEventValues(GDMCustomEvent evt)
        {
            if (evt == null) return;

            string evKey = evt.GetEventKey();
            string evVal = evt.StringValue;
            if (!string.IsNullOrEmpty(evKey) && !string.IsNullOrEmpty(evVal)) {
                fValuesCollection.Add(evKey, evVal);
            }

            if (!string.IsNullOrEmpty(evt.Cause)) {
                fValuesCollection.Add(GEDCOMTagName.CAUS, evt.Cause);
            }

            if (!string.IsNullOrEmpty(evt.Agency)) {
                fValuesCollection.Add(GEDCOMTagName.AGNC, evt.Agency);
            }
        }

        public void CollectNameLangs(GDMPersonalName persName)
        {
            if (persName == null) return;

            GDMLanguageID langId = persName.Language;
            if (langId != GDMLanguageID.Unknown) {
                fLangStats.Increment(langId);
            }
        }

        public void ImportNames(GDMIndividualRecord iRec)
        {
            AppHost.NamesTable.ImportNames(this, iRec);
        }

        public GDMCustomEvent CreateEventEx(GDMRecordWithEvents aRec, string evTag, GDMCustomDate evDate, string evPlace)
        {
            return CreateEventEx(aRec, evTag, evDate.StringValue, evPlace);
        }

        public GDMCustomEvent CreateEventEx(GDMRecordWithEvents aRec, string evTag, string evDate, string evPlace)
        {
            if (aRec == null) return null;

            GDMCustomEvent result;

            if (aRec is GDMIndividualRecord) {
                if (GKUtils.GetPredefinedEventKind(evTag) == EventKind.ekEvent) {
                    result = new GDMIndividualEvent();
                } else {
                    result = new GDMIndividualAttribute();
                }
            } else if (aRec is GDMFamilyRecord) {
                result = new GDMFamilyEvent();
            } else {
                return null;
            }

            aRec.AddEvent(result);

            result.SetName(evTag);

            if (evDate != "") {
                result.Date.ParseString(evDate);
            }

            if (evPlace != "") {
                result.Place.StringValue = evPlace;
            }

            return result;
        }

        public GDMIndividualRecord CreatePersonEx(string iName, string iPatronymic, string iSurname, GDMSex iSex, bool birthEvent)
        {
            GDMIndividualRecord iRec = fTree.CreateIndividual();
            iRec.Sex = iSex;

            GDMPersonalName pName = iRec.AddPersonalName(new GDMPersonalName());
            GKUtils.SetNameParts(pName, iSurname, iName, iPatronymic);

            if (birthEvent) CreateEventEx(iRec, GEDCOMTagName.BIRT, "", "");

            return iRec;
        }

        public async Task<bool> DeleteRecord(GDMRecord record)
        {
            bool result = false;
            if (record == null) return result;

            try {
                BeginUpdate();

                switch (record.RecordType) {
                    case GDMRecordType.rtIndividual:
                        result = fTree.DeleteIndividualRecord(record as GDMIndividualRecord);
                        break;

                    case GDMRecordType.rtFamily:
                        result = fTree.DeleteFamilyRecord(record as GDMFamilyRecord);
                        break;

                    case GDMRecordType.rtNote:
                        result = fTree.DeleteNoteRecord(record as GDMNoteRecord);
                        break;

                    case GDMRecordType.rtMultimedia:
                        result = await DeleteMediaRecord(record as GDMMultimediaRecord);
                        break;

                    case GDMRecordType.rtSource:
                        result = fTree.DeleteSourceRecord(record as GDMSourceRecord);
                        break;

                    case GDMRecordType.rtRepository:
                        result = fTree.DeleteRepositoryRecord(record as GDMRepositoryRecord);
                        break;

                    case GDMRecordType.rtGroup:
                        result = fTree.DeleteGroupRecord(record as GDMGroupRecord);
                        break;

                    case GDMRecordType.rtResearch:
                        result = fTree.DeleteResearchRecord(record as GDMResearchRecord);
                        break;

                    case GDMRecordType.rtTask:
                        result = fTree.DeleteTaskRecord(record as GDMTaskRecord);
                        break;

                    case GDMRecordType.rtCommunication:
                        result = fTree.DeleteCommunicationRecord(record as GDMCommunicationRecord);
                        break;

                    case GDMRecordType.rtLocation:
                        result = fTree.DeleteLocationRecord(record as GDMLocationRecord);
                        break;
                }
            } finally {
                EndUpdate();
            }

            if (result) {
                SetModified();

                if (fViewer != null) {
                    fViewer.NotifyRecord(record, RecordAction.raDelete);
                }
            }

            return result;
        }

        public async Task<bool> DeleteMediaRecord(GDMMultimediaRecord mRec)
        {
            if (mRec == null)
                throw new ArgumentNullException("mRec");

            if (mRec.FileReferences.Count > 0) {
                bool fileDeleted = await MediaDelete(mRec.FileReferences[0]);
                if (!fileDeleted) {
                    // message?
                    return false;
                }
            }

            return fTree.DeleteMediaRecord(mRec);
        }

        public IList<ISearchResult> FindAll(GDMRecordType recordType, string searchPattern)
        {
            var result = new List<ISearchResult>();

            Regex regex = GKUtils.InitMaskRegex(searchPattern);

            int num = fTree.RecordsCount;
            for (int i = 0; i < num; i++) {
                GDMRecord rec = fTree[i];
                if (rec.RecordType != recordType) continue;

                string recName = GKUtils.GetRecordName(fTree, rec, false);
                if (GKUtils.MatchesRegex(recName, regex)) {
                    result.Add(new SearchResult(rec));
                }
            }

            return result;
        }

        public bool IsRecordAccess(GDMRestriction restriction)
        {
            bool result = false;

            switch (fShieldState) {
                case ShieldState.Maximum:
                    result = (restriction != GDMRestriction.rnConfidential && restriction != GDMRestriction.rnPrivacy);
                    break;

                case ShieldState.Middle:
                    result = (restriction != GDMRestriction.rnPrivacy);
                    break;

                case ShieldState.None:
                    result = true;
                    break;
            }

            return result;
        }

        #endregion

        #region Data search

        public GDMIndividualRecord FindIndividual(string searchName, Dictionary<string, string> facts)
        {
            int num = fTree.RecordsCount;
            for (int i = 0; i < num; i++) {
                var rec = fTree[i] as GDMIndividualRecord;
                if (rec == null) continue;

                string indiName = GKUtils.GetNameString(rec, false);
                if (indiName == searchName) {
                    var res = true;
                    foreach (var pair in facts) {
                        if (pair.Key == "birth_year") {
                            int birthYear = rec.GetChronologicalYear(GEDCOMTagName.BIRT);
                            res = res && (birthYear.ToString() == pair.Value);
                        }

                        if (!res) break;
                    }

                    if (res) {
                        return rec;
                    }
                }
            }

            return null;
        }

        public GDMSourceRecord FindSource(string sourceName)
        {
            GDMSourceRecord result = null;

            int num = fTree.RecordsCount;
            for (int i = 0; i < num; i++) {
                var rec = fTree[i] as GDMSourceRecord;

                if (rec != null && rec.ShortTitle == sourceName) {
                    result = rec;
                    break;
                }
            }

            return result;
        }

        public void GetSourcesList(StringList sources)
        {
            if (sources == null) return;

            sources.Clear();

            int num = fTree.RecordsCount;
            for (int i = 0; i < num; i++) {
                var rec = fTree[i] as GDMSourceRecord;
                if (rec != null) {
                    sources.AddObject(rec.ShortTitle, rec);
                }
            }
        }

        public void GetRepositoriesList(StringList list)
        {
            if (list == null) return;

            list.Clear();

            int num = fTree.RecordsCount;
            for (int i = 0; i < num; i++) {
                var rec = fTree[i] as GDMRepositoryRecord;
                if (rec != null) {
                    list.AddObject(rec.RepositoryName, rec);
                }
            }
        }

        #endregion

        #region Individual utils

        public string[] GetIndividualSurnames(GDMIndividualRecord iRec)
        {
            return Culture.GetSurnames(iRec);
        }

        /// <summary>
        /// Attention: returns or creates only the first marriage!
        /// </summary>
        /// <param name="canCreate">can create if does not exist</param>
        /// <returns></returns>
        public GDMFamilyRecord GetMarriageFamily(GDMIndividualRecord iRec, bool canCreate = false)
        {
            GDMFamilyRecord result = fTree.GetMarriageFamily(iRec);

            if (result == null && canCreate) {
                result = fTree.CreateFamily();
                result.AddSpouse(iRec);
            }

            return result;
        }

        /// <summary>
        /// Attention: returns or creates only the first parents family!
        /// </summary>
        /// <param name="canCreate">can create if does not exist</param>
        /// <returns></returns>
        public GDMFamilyRecord GetParentsFamily(GDMIndividualRecord iRec, bool canCreate = false)
        {
            GDMFamilyRecord result = fTree.GetParentsFamily(iRec);

            if (result == null && canCreate) {
                result = fTree.CreateFamily();
                result.AddChild(iRec);
            }

            return result;
        }

        public bool IsChildless(GDMIndividualRecord iRec)
        {
            int exp = GKUtils.GetLifeExpectancy(iRec);
            return (exp != -1 && exp < 15);
        }

        public int FindBirthYear(GDMIndividualRecord iRec)
        {
            if (iRec != null) {
                int birthDate = iRec.GetChronologicalYear(GEDCOMTagName.BIRT);
                if (birthDate != 0) {
                    return birthDate;
                }

                int num = iRec.SpouseToFamilyLinks.Count;
                for (int i = 0; i < num; i++) {
                    GDMFamilyRecord family = fTree.GetPtrValue(iRec.SpouseToFamilyLinks[i]);

                    int num2 = family.Children.Count;
                    for (int j = 0; j < num2; j++) {
                        GDMIndividualRecord child = fTree.GetPtrValue(family.Children[j]);
                        birthDate = FindBirthYear(child);
                        if (birthDate != 0) {
                            return birthDate - 20;
                        }
                    }
                }
            }

            return 0;
        }

        public int FindDeathYear(GDMIndividualRecord iRec)
        {
            if (iRec != null) {
                int deathDate = iRec.GetChronologicalYear(GEDCOMTagName.DEAT);
                if (deathDate != 0) {
                    return deathDate;
                }

                int maxBirth = 0;
                int num = iRec.SpouseToFamilyLinks.Count;
                for (int i = 0; i < num; i++) {
                    GDMFamilyRecord family = fTree.GetPtrValue(iRec.SpouseToFamilyLinks[i]);

                    int num2 = family.Children.Count;
                    for (int j = 0; j < num2; j++) {
                        GDMIndividualRecord child = fTree.GetPtrValue(family.Children[j]);

                        int chbDate = FindBirthYear(child);
                        if (chbDate != 0 && maxBirth < chbDate) {
                            maxBirth = chbDate;
                        }
                    }
                }

                if (maxBirth != 0) {
                    return maxBirth + 1;
                }
            }

            return 0;
        }

        public void CollectTips(StringList tipsList)
        {
            if (tipsList == null)
                throw new ArgumentNullException("tipsList");

            try {
                bool onlyAlive = true;
                var dtNow = DateTime.Now;

                bool firstTip = true;
                var indiEnum = fTree.GetEnumerator<GDMIndividualRecord>();
                GDMIndividualRecord iRec;
                while (indiEnum.MoveNext(out iRec)) {
                    var lifeEvents = iRec.GetLifeEvents(true);

                    if (onlyAlive) {
                        if (lifeEvents.DeathEvent != null || lifeEvents.BurialEvent != null) continue;
                    }

                    int years;
                    bool anniversary;

                    if (lifeEvents.BirthEvent == null) continue;
                    var dt = lifeEvents.BirthEvent.Date.Value as GDMDate;
                    if (dt == null || !dt.IsValidDate()) continue;

                    int days = GKUtils.GetDaysFor(dt, dtNow, out years, out anniversary);
                    if (days < 0 || days >= 3) continue;

                    if (firstTip) {
                        tipsList.Add("#" + LangMan.LS(LSID.BirthDays));
                        firstTip = false;
                    }

                    string tip = GKUtils.GetBirthTipMessage(Culture, iRec, days, years, anniversary);
                    tipsList.Add(tip);
                }
            } catch (Exception ex) {
                Logger.WriteError("BaseContext.CollectTips()", ex);
            }
        }

        #endregion

        #region Name and sex functions

        public async Task<string> DefinePatronymic(IView owner, string name, GDMSex sex, bool confirm)
        {
            ICulture culture = this.Culture;
            if (!culture.HasPatronymic) return string.Empty;

            string result = "";

            INamesTable namesTable = AppHost.NamesTable;

            NameEntry n = namesTable.FindName(name);
            if (n == null) {
                if (!confirm) {
                    return result;
                }

                n = namesTable.AddName(name);
            }

            switch (sex) {
                case GDMSex.svMale:
                    result = n.M_Patronymic;
                    break;

                case GDMSex.svFemale:
                    result = n.F_Patronymic;
                    break;
            }

            if (string.IsNullOrEmpty(result)) {
                if (!confirm) {
                    return result;
                }

                await BaseController.ModifyName(owner, this, n);
            }

            switch (sex) {
                case GDMSex.svMale:
                    result = n.M_Patronymic;
                    break;

                case GDMSex.svFemale:
                    result = n.F_Patronymic;
                    break;
            }

            return result;
        }

        public async Task<GDMSex> DefineSex(IView owner, string iName, string iPatr)
        {
            INamesTable namesTable = AppHost.NamesTable;

            GDMSex result = namesTable.GetSexByName(iName);

            if (result == GDMSex.svUnknown) {
                result = await this.Culture.GetSex(iName, iPatr, false);

                using (var dlg = AppHost.ResolveDialog<ISexCheckDlg>()) {
                    dlg.IndividualName = iName + " " + iPatr;
                    dlg.Sex = result;
                    if (await AppHost.Instance.ShowModalAsync(dlg, owner, false)) {
                        result = dlg.Sex;
                    }
                }

                if (result != GDMSex.svUnknown) {
                    namesTable.SetNameSex(iName, result);
                }
            }

            return result;
        }

        public async Task CheckPersonSex(IView owner, GDMIndividualRecord iRec)
        {
            if (iRec == null)
                throw new ArgumentNullException("iRec");

            try {
                BeginUpdate();

                if (iRec.Sex != GDMSex.svMale && iRec.Sex != GDMSex.svFemale) {
                    var parts = GKUtils.GetNameParts(fTree, iRec);
                    iRec.Sex = await DefineSex(owner, parts.Name, parts.Patronymic);
                }
            } finally {
                EndUpdate();
            }
        }

        #endregion

        #region Private media support

        private static string GetTreePath(string treeName)
        {
            return Path.GetDirectoryName(treeName) + Path.DirectorySeparatorChar;
        }

        private string GetTreeRelativePath(string fileName)
        {
            string result = GKUtils.GetRelativePath(GetTreePath(fFileName), fileName);
            return result;
        }

        public string GetArcFileName()
        {
            string treeName = fFileName;
            string result = GetTreePath(treeName) + Path.GetFileNameWithoutExtension(treeName) + ".zip";
            return result;
        }

        public string GetStgFolder(bool create)
        {
            string treeName = fFileName;
            string result = GetTreePath(treeName) + Path.GetFileNameWithoutExtension(treeName) + Path.DirectorySeparatorChar;
            if (!Directory.Exists(result) && create) Directory.CreateDirectory(result);
            return result;
        }

        // TODO: Controlling the version of the GK GEDCOM file to determine the zip archive encoding!
        private Encoding GetZipEncoding()
        {
            int treeVer = 0;
            return (treeVer == 0) ? Encoding.GetEncoding("CP866") : Encoding.UTF8;
        }

        private void ArcFileLoad(string targetFn, Stream toStream)
        {
            targetFn = FileHelper.NormalizeFilename(targetFn);

            using (ZipStorer zip = ZipStorer.Open(GetArcFileName(), FileAccess.Read, GetZipEncoding())) {
                ZipStorer.ZipFileEntry entry = zip.FindFile(targetFn);
                if (entry != null) {
                    zip.ExtractStream(entry, toStream);
                }
            }
        }

        private void ArcFileSave(string fileName, string sfn)
        {
            string arcFn = GetArcFileName();
            ZipStorer zip = null;

            try {
                if (File.Exists(arcFn)) {
                    zip = ZipStorer.Open(arcFn, FileAccess.ReadWrite, GetZipEncoding());
                } else {
                    zip = ZipStorer.Create(arcFn, "");
                }
                zip.AddFile(ZipStorer.Compression.Deflate, fileName, sfn, null);
            } finally {
                if (zip != null) zip.Dispose();
            }
        }

        private void ArcFileDelete(string targetFn)
        {
            targetFn = FileHelper.NormalizeFilename(targetFn);

            using (ZipStorer zip = ZipStorer.Open(GetArcFileName(), FileAccess.Read, GetZipEncoding())) {
                ZipStorer.ZipFileEntry entry = zip.FindFile(targetFn);
                if (entry != null) {
                    var zfes = new List<ZipStorer.ZipFileEntry>();
                    zfes.Add(entry);
                    // TODO: optimize this method!
                    ZipStorer.RemoveEntries(zip, zfes);
                }
            }
        }

        private bool ArcFileExists(string targetFn)
        {
            targetFn = FileHelper.NormalizeFilename(targetFn);

            using (ZipStorer zip = ZipStorer.Open(GetArcFileName(), FileAccess.Read, GetZipEncoding())) {
                ZipStorer.ZipFileEntry entry = zip.FindFile(targetFn);
                return (entry != null);
            }
        }

        public void MoveMediaContainers(string oldFileName, string newFileName, bool createCopy = false)
        {
            // do nothing if file name is not changed
            if (string.IsNullOrEmpty(oldFileName) || string.Equals(oldFileName, newFileName)) return;

            bool hasArc = File.Exists(GetArcFileName());
            bool hasStg = Directory.Exists(GetStgFolder(false));

            string newPath = Path.GetDirectoryName(newFileName);
            string newName = Path.GetFileName(newFileName);

            // move the archive and the storage folder to a new location
            if (hasArc) {
                string newArc = newPath + Path.DirectorySeparatorChar + GKUtils.GetContainerName(newName, true);
                if (!createCopy) {
                    File.Move(GetArcFileName(), newArc);
                } else {
                    CopyFile(GetArcFileName(), newArc, true);
                }
            }

            if (hasStg) {
                string newStg = newPath + Path.DirectorySeparatorChar + GKUtils.GetContainerName(newName, false);
                if (!createCopy) {
                    Directory.Move(GetStgFolder(false), newStg);
                } else {
                    // TODO!
                }
            }
        }

        #endregion

        #region Public media support

        /// <summary>
        /// Check the status of the tree's file saving to define
        /// the path where will be located a storage or archive.
        /// </summary>
        /// <returns>The status of the existence of the file path.</returns>
        public bool CheckBasePath()
        {
            string path = string.IsNullOrEmpty(fFileName) ? string.Empty : Path.GetDirectoryName(fFileName);

            bool result = (!string.IsNullOrEmpty(path));
            if (!result) {
                AppHost.StdDialogs.ShowError(LangMan.LS(LSID.NewDBFileNeedToSave));
            }
            return result;
        }

        public MediaStore GetStoreType(GDMFileReference fileReference)
        {
            return GKUtils.GetStoreType(fileReference);
        }

        public bool MoveMediaFile(GDMMultimediaRecord mediaRec, MediaStoreType newStoreType)
        {
            bool result = false;

            if (mediaRec == null || mediaRec.FileReferences.Count < 1) return result;

            var fileRef = mediaRec.FileReferences[0];
            var oldStoreType = GKUtils.GetStoreType(fileRef).StoreType;

            if (oldStoreType == newStoreType) return result;

            switch (newStoreType) {
                case MediaStoreType.mstReference:
                    // maybe it will be implemented in the future (if make a folder selection)
                    break;

                case MediaStoreType.mstRelativeReference:
                    if (oldStoreType == MediaStoreType.mstReference) {
                        string targetFile = GetTreeRelativePath(fileRef.StringValue);
                        string refPath = GKData.GKStoreTypes[(int)newStoreType].Sign + targetFile;
                        fileRef.StringValue = FileHelper.NormalizeFilename(refPath);
                        result = true;
                    } else {
                        // maybe it will be implemented in the future (if make a folder selection)
                    }
                    break;

                case MediaStoreType.mstStorage:
                    if (oldStoreType != MediaStoreType.mstArchive) {
                        string currentFileName = MediaLoad(fileRef);
                        result = MediaSave(fileRef, currentFileName, newStoreType);
                    }
                    break;

                case MediaStoreType.mstArchive:
                    if (oldStoreType != MediaStoreType.mstStorage) {
                        string currentFileName = MediaLoad(fileRef);
                        result = MediaSave(fileRef, currentFileName, newStoreType);
                    }
                    break;

                case MediaStoreType.mstURL:
                    // impossible in this method
                    break;
            }

            return result;
        }

        public Stream MediaLoad(GDMFileReference fileReference, bool throwException)
        {
            Stream stream = null;
            if (fileReference == null) return null;

            var mediaStore = GetStoreType(fileReference);
            string targetFn = mediaStore.FileName;
            MediaStoreType gst = mediaStore.StoreType;

            switch (gst) {
                case MediaStoreType.mstStorage:
                    targetFn = GetStgFolder(false) + targetFn;
                    if (!File.Exists(targetFn)) {
                        if (throwException) {
                            throw new MediaFileNotFoundException(targetFn);
                        }

                        AppHost.StdDialogs.ShowError(LangMan.LS(LSID.MediaFileNotLoaded));
                    } else {
                        stream = new FileStream(targetFn, FileMode.Open, FileAccess.Read);
                    }
                    break;

                case MediaStoreType.mstArchive:
                    stream = new MemoryStream();
                    string arcFile = GetArcFileName();
                    if (!File.Exists(arcFile)) {
                        if (throwException) {
                            throw new MediaFileNotFoundException(arcFile);
                        }

                        AppHost.StdDialogs.ShowError(LangMan.LS(LSID.MediaFileNotLoaded));
                    } else {
                        ArcFileLoad(targetFn, stream);
                        stream.Seek(0, SeekOrigin.Begin);
                    }
                    break;

                case MediaStoreType.mstRelativeReference:
                case MediaStoreType.mstReference:
                    if (gst == MediaStoreType.mstRelativeReference) {
                        string treeName = fFileName;
                        targetFn = GetTreePath(treeName) + targetFn;
                    }
                    if (!File.Exists(targetFn)) {
                        if (throwException) {
                            throw new MediaFileNotFoundException(targetFn);
                        }
                        AppHost.StdDialogs.ShowError(LangMan.LS(LSID.FileNotFound, targetFn));
                    } else {
                        stream = new FileStream(targetFn, FileMode.Open, FileAccess.Read);
                    }
                    break;

                case MediaStoreType.mstURL:
                    stream = GKUtils.GetWebStream(targetFn);
                    break;
            }

            return stream;
        }

        public string MediaLoad(GDMFileReference fileReference)
        {
            string fileName = string.Empty;
            if (fileReference == null) return string.Empty;

            try {
                MediaStore mediaStore = GetStoreType(fileReference);

                if (mediaStore.StoreType != MediaStoreType.mstURL && !VerifyMediaFileWM(fileReference)) {
                    return string.Empty;
                }

                string targetFn = mediaStore.FileName;

                switch (mediaStore.StoreType) {
                    case MediaStoreType.mstStorage:
                        fileName = GetStgFolder(false) + targetFn;
                        break;

                    case MediaStoreType.mstArchive: {
                            fileName = GKUtils.GetTempDir() + Path.GetFileName(targetFn);
                            FileStream fs = new FileStream(fileName, FileMode.Create, FileAccess.Write);
                            try {
                                if (!File.Exists(GetArcFileName())) {
                                    AppHost.StdDialogs.ShowError(LangMan.LS(LSID.MediaFileNotLoaded));
                                } else {
                                    ArcFileLoad(targetFn, fs);
                                }
                            } finally {
                                fs.Close();
                                fs.Dispose();
                            }
                        }
                        break;

                    case MediaStoreType.mstRelativeReference:
                        string treeName = fFileName;
                        fileName = GetTreePath(treeName) + targetFn;
                        break;

                    case MediaStoreType.mstReference: {
                            fileName = targetFn;
                            if (!File.Exists(fileName)) {
                                string newPath = FileHelper.NormalizeFilename(fileName);
                                if (!string.IsNullOrEmpty(newPath)) {
                                    fileName = newPath;
                                }
                            }
                            break;
                        }

                    case MediaStoreType.mstURL: {
                            fileName = GKUtils.GetTempDir() + Path.GetFileName(targetFn);
                            FileStream fs = new FileStream(fileName, FileMode.Create, FileAccess.Write);
                            try {
                                var dataBytes = GKUtils.GetWebData(targetFn);
                                fs.Write(dataBytes, 0, dataBytes.Length);
                            } finally {
                                fs.Close();
                                fs.Dispose();
                            }
                        }
                        break;
                }
            } catch (Exception ex) {
                Logger.WriteError("BaseContext.MediaLoad_fn()", ex);
                fileName = "";
            }

            return fileName;
        }

        public bool MediaSave(GDMFileReference fileReference, string fileName, MediaStoreType storeType)
        {
            if (fileReference == null) return false;

            string storeFile = Path.GetFileName(fileName);
            string storePath = GKUtils.GetStoreFolder(GKUtils.GetMultimediaKind(GDMFileReference.RecognizeFormat(fileName)));

            string refPath = string.Empty;
            string targetFile = string.Empty;

            // set paths and links
            switch (storeType) {
                case MediaStoreType.mstReference:
                    refPath = fileName;
                    break;

                case MediaStoreType.mstRelativeReference:
                    targetFile = GetTreeRelativePath(fileName);
                    refPath = GKData.GKStoreTypes[(int)storeType].Sign + targetFile;
                    break;

                case MediaStoreType.mstArchive:
                    targetFile = storePath + storeFile;
                    refPath = GKData.GKStoreTypes[(int)storeType].Sign + targetFile;
                    break;

                case MediaStoreType.mstStorage:
                    targetFile = storePath + storeFile;
                    refPath = GKData.GKStoreTypes[(int)storeType].Sign + targetFile;
                    break;

                case MediaStoreType.mstURL:
                    refPath = fileName;
                    break;
            }

            if (storeType != MediaStoreType.mstURL) {
                refPath = FileHelper.NormalizeFilename(refPath);
            }

            // verify existence
            bool alreadyExists = MediaExists(refPath);
            if (alreadyExists) {
                AppHost.StdDialogs.ShowError(LangMan.LS(LSID.FileWithSameNameAlreadyExists));
                return false;
            }

            bool result = true;

            // save a copy to archive or storage
            switch (storeType) {
                case MediaStoreType.mstArchive:
                    ArcFileSave(fileName, targetFile);
                    break;

                case MediaStoreType.mstStorage:
                    string targetFn = string.Empty;
                    try {
                        string targetDir = GetStgFolder(true) + storePath;
                        if (!Directory.Exists(targetDir)) Directory.CreateDirectory(targetDir);

                        targetFn = targetDir + storeFile;
                        result = CopyFile(fileName, targetFn, !AppHost.TEST_MODE);
                    } catch (IOException ex) {
                        Logger.WriteError(string.Format("BaseContext.MediaSave({0}, {1})", fileName, targetFn), ex);
                        AppHost.StdDialogs.ShowError(LangMan.LS(LSID.FileWithSameNameAlreadyExists));
                        result = false;
                    }
                    break;
            }

            if (result) {
                fileReference.LinkFile(refPath);
            }

            return result;
        }

        public async Task<bool> MediaDelete(GDMFileReference fileReference)
        {
            if (fileReference == null) return false;

            try {
                MediaStore mediaStore = GetStoreType(fileReference);
                string fileName = mediaStore.FileName;

                MediaStoreStatus storeStatus = VerifyMediaFile(fileReference, out fileName);
                bool result = false;

                switch (storeStatus) {
                    case MediaStoreStatus.mssExists: {
                            if (mediaStore.StoreType == MediaStoreType.mstArchive || mediaStore.StoreType == MediaStoreType.mstStorage) {
                                if (!GlobalOptions.Instance.AllowDeleteMediaFileFromStgArc) {
                                    return true;
                                }
                            }

                            if (mediaStore.StoreType == MediaStoreType.mstReference || mediaStore.StoreType == MediaStoreType.mstRelativeReference) {
                                if (!GlobalOptions.Instance.AllowDeleteMediaFileFromRefs) {
                                    return true;
                                }
                            }

                            if (!GlobalOptions.Instance.DeleteMediaFileWithoutConfirm) {
                                string msg = string.Format(LangMan.LS(LSID.MediaFileDeleteQuery));
                                // TODO: may be Yes/No/Cancel?
                                var res = await AppHost.StdDialogs.ShowQuestion(msg);
                                if (!res) {
                                    return false;
                                }
                            }

                            if (mediaStore.StoreType == MediaStoreType.mstArchive) {
                                ArcFileDelete(fileName);
                            } else {
                                File.Delete(fileName);
                            }
                            result = true;
                        }
                        break;

                    case MediaStoreStatus.mssFileNotFound:
                        result = await AppHost.StdDialogs.ShowQuestion(LangMan.LS(LSID.ContinueQuestion, LangMan.LS(LSID.FileNotFound, fileName)));
                        break;

                    case MediaStoreStatus.mssStgNotFound:
                        result = await AppHost.StdDialogs.ShowQuestion(LangMan.LS(LSID.ContinueQuestion, LangMan.LS(LSID.StgNotFound)));
                        break;

                    case MediaStoreStatus.mssArcNotFound:
                        result = await AppHost.StdDialogs.ShowQuestion(LangMan.LS(LSID.ContinueQuestion, LangMan.LS(LSID.ArcNotFound)));
                        break;

                    case MediaStoreStatus.mssBadData:
                        // can be deleted
                        result = true;
                        break;
                }

                return result;
            } catch (Exception ex) {
                Logger.WriteError("BaseContext.MediaDelete()", ex);
                return false;
            }
        }

        public bool VerifyMediaFileWM(GDMFileReference fileReference)
        {
            string fileName;
            MediaStoreStatus storeStatus = VerifyMediaFile(fileReference, out fileName);
            if (storeStatus != MediaStoreStatus.mssExists) {
                switch (storeStatus) {
                    case MediaStoreStatus.mssFileNotFound:
                        AppHost.StdDialogs.ShowError(LangMan.LS(LSID.FileNotFound, fileName));
                        break;

                    case MediaStoreStatus.mssStgNotFound:
                        AppHost.StdDialogs.ShowError(LangMan.LS(LSID.StgNotFound));
                        break;

                    case MediaStoreStatus.mssArcNotFound:
                        AppHost.StdDialogs.ShowError(LangMan.LS(LSID.ArcNotFound));
                        break;

                    case MediaStoreStatus.mssBadData:
                        break;
                }
                return false;
            }
            return true;
        }

        public MediaStoreStatus VerifyMediaFile(GDMFileReference fileReference, out string fileName)
        {
            if (fileReference == null)
                throw new ArgumentNullException("fileReference");

            MediaStoreStatus result = MediaStoreStatus.mssBadData;

            try {
                MediaStore mediaStore = GetStoreType(fileReference);
                fileName = mediaStore.FileName;

                switch (mediaStore.StoreType) {
                    case MediaStoreType.mstStorage:
                        string stgPath = GetStgFolder(false);
                        if (!Directory.Exists(stgPath)) {
                            result = MediaStoreStatus.mssStgNotFound;
                        } else {
                            fileName = stgPath + fileName;
                            if (!File.Exists(fileName)) {
                                result = MediaStoreStatus.mssFileNotFound;
                            } else {
                                result = MediaStoreStatus.mssExists;
                            }
                        }
                        break;

                    case MediaStoreType.mstArchive:
                        if (!File.Exists(GetArcFileName())) {
                            result = MediaStoreStatus.mssArcNotFound;
                        } else {
                            if (!ArcFileExists(fileName)) {
                                result = MediaStoreStatus.mssFileNotFound;
                            } else {
                                result = MediaStoreStatus.mssExists;
                            }
                        }
                        break;

                    case MediaStoreType.mstReference:
                        if (!File.Exists(fileName)) {
                            string xFileName = FileHelper.NormalizeFilename(fileName);
                            if (string.IsNullOrEmpty(xFileName)) {
                                result = MediaStoreStatus.mssFileNotFound;
                            } else {
                                result = MediaStoreStatus.mssExists;
                                fileName = xFileName;
                            }
                        } else {
                            result = MediaStoreStatus.mssExists;
                        }
                        break;
                }
            } catch (Exception ex) {
                Logger.WriteError("BaseContext.VerifyMediaFile()", ex);
                fileName = string.Empty;
            }

            return result;
        }

        public bool MediaExists(string refPath)
        {
            if (string.IsNullOrEmpty(refPath)) return false;

            int num = fTree.RecordsCount;
            for (int i = 0; i < num; i++) {
                GDMMultimediaRecord rec = fTree[i] as GDMMultimediaRecord;
                if (rec != null && rec.FileReferences.Count > 0) {
                    if (string.Compare(rec.FileReferences[0].StringValue, refPath, true) == 0) {
                        return true;
                    }
                }
            }

            return false;
        }

        public bool CopyFile(string sourceFileName, string destFileName, bool showProgress = true)
        {
            bool result = false;

            if (showProgress) {
                try {
                    var source = new FileInfo(sourceFileName);
                    var target = new FileInfo(destFileName);

                    AppHost.Instance.ExecuteWork((controller) => {
                        try {
                            controller.Begin(LangMan.LS(LSID.CopyingFile), 100);
                            try {
                                GKUtils.CopyFile(source, target, controller);
                                result = true;
                            } catch (Exception ex) {
                                Logger.WriteError(string.Format("BaseContext.CopyFile.1.1({0}, {1})", sourceFileName, destFileName), ex);
                            }
                        } finally {
                            controller.End();
                        }
                    });
                } catch (Exception ex) {
                    Logger.WriteError(string.Format("BaseContext.CopyFile.1({0}, {1})", sourceFileName, destFileName), ex);
                }
            } else {
                try {
                    File.Copy(sourceFileName, destFileName, AppHost.TEST_MODE);
                    result = true;
                } catch (Exception ex) {
                    Logger.WriteError(string.Format("BaseContext.CopyFile.2({0}, {1})", sourceFileName, destFileName), ex);
                }
            }

            return result;
        }

        public IImage LoadMediaImage(GDMMultimediaRecord mmRec, int thumbWidth, int thumbHeight, ExtRect cutoutArea, bool throwException)
        {
            if (mmRec == null || mmRec.FileReferences.Count < 1) return null;

            IImage result = null;

            try {
                var gfxProvider = AppHost.GfxProvider;

                Stream inputStream;

                string cachedFile = GetCachedImageFilename(mmRec.UID);
                if (File.Exists(cachedFile)) {
                    // if cached, then already transformed
                    inputStream = new FileStream(cachedFile, FileMode.Open, FileAccess.Read, FileShare.Read);
                } else {
                    inputStream = MediaLoad(mmRec.FileReferences[0], throwException);
                    if (inputStream != null) {
                        var transformStream = gfxProvider.CheckOrientation(inputStream);
                        if (transformStream != inputStream) {
                            // image transformed
                            using (var cachedStream = new FileStream(cachedFile, FileMode.CreateNew, FileAccess.Write, FileShare.Write)) {
                                transformStream.CopyTo(cachedStream);
                                cachedStream.Flush();
                            }
                            inputStream.Dispose();
                            inputStream = transformStream;
                        }
                    }
                }

                if (inputStream != null && inputStream.Length != 0) {
                    result = gfxProvider.LoadImage(inputStream, thumbWidth, thumbHeight, cutoutArea, cachedFile);
                }
            } catch (MediaFileNotFoundException) {
                throw;
            } catch (Exception ex) {
                Logger.WriteError("BaseContext.LoadMediaImage()", ex);
                result = null;
            }

            return result;
        }

        internal static string GetCachedImageFilename(string imageUID)
        {
            return AppHost.GetCachePath() + imageUID + ".bmp";
        }

        public IImage GetPrimaryBitmap(GDMIndividualRecord iRec, int thumbWidth, int thumbHeight, bool throwException)
        {
            if (iRec == null) return null;

            IImage result = null;
            try {
                GDMMultimediaLink mmLink = iRec.GetPrimaryMultimediaLink();
                GDMMultimediaRecord mmRec = fTree.GetPtrValue<GDMMultimediaRecord>(mmLink);

                if (mmLink != null && mmRec != null) {
                    var cutoutArea = mmLink.IsPrimaryCutout ? mmLink.CutoutPosition.Value : ExtRect.CreateEmpty();
                    result = LoadMediaImage(mmRec, thumbWidth, thumbHeight, cutoutArea, throwException);
                }
            } catch (MediaFileNotFoundException) {
                throw;
            } catch (Exception ex) {
                Logger.WriteError("BaseContext.GetPrimaryBitmap()", ex);
                result = null;
            }
            return result;
        }

        public string GetPrimaryBitmapUID(GDMIndividualRecord iRec)
        {
            if (iRec == null) return null;

            string result;
            try {
                GDMMultimediaLink mmLink = iRec.GetPrimaryMultimediaLink();
                result = (mmLink == null) ? null : mmLink.GetUID(fTree);
            } catch (Exception ex) {
                Logger.WriteError("BaseContext.GetPrimaryBitmapUID()", ex);
                result = null;
            }
            return result;
        }

        #endregion

        #region Files

        public bool IsUnknown()
        {
            return string.IsNullOrEmpty(fFileName) || !File.Exists(fFileName);
        }

        public void SetFileName(string fileName)
        {
            fFileName = fileName;
        }

        public void Clear()
        {
            fTree.Clear();
            fUndoman.Clear();
        }

        public async Task<bool> FileLoad(string fileName, bool showProgress = true)
        {
            return await FileLoad(fileName, true, showProgress, true);
        }

        public async Task<bool> FileLoad(string fileName, bool loadSecure, bool showProgress, bool checkValidation)
        {
            bool result = false;

            try {
                FileProvider fileProvider;

                string pw = null;
                string ext = FileHelper.GetFileExtension(fileName);
                if (ext == ".ged") {
                    fileProvider = new GEDCOMProvider(fTree);
                } else if (ext == ".geds") {
                    fileProvider = new GEDCOMProvider(fTree);

                    if (loadSecure) {
                        pw = await AppHost.StdDialogs.GetPassword(LangMan.LS(LSID.Password));
                        if (string.IsNullOrEmpty(pw)) {
                            AppHost.StdDialogs.ShowError(LangMan.LS(LSID.PasswordIsNotSpecified));
                            return false;
                        }
                    } else {
                        return false;
                    }
                } else if (ext == ".xml") {
                    fileProvider = new GedMLProvider(fTree);
                } else if (ext == ".familyx") {
                    fileProvider = new FamilyXProvider(fTree);
                } else if (ext == ".gdz" || ext == ".zip") {
                    fileProvider = new GEDZIPProvider(fTree);
                } else {
                    // TODO: message?
                    return false;
                }

                if (!showProgress) {
                    FileLoad(fileProvider, fileName, pw, null);
                } else {
                    AppHost.Instance.ExecuteWork((controller) => {
                        controller.Begin(LangMan.LS(LSID.Loading), 100);
                        FileLoad(fileProvider, fileName, pw, controller);
                        controller.End();
                    });
                }

                AppHost.ForceGC();

                if (checkValidation) {
                    if (!showProgress) {
                        GEDCOMChecker.CheckGEDCOMFormat(this, null);
                    } else {
                        AppHost.Instance.ExecuteWork((controller) => {
                            GEDCOMChecker.CheckGEDCOMFormat(this, controller);
                        });
                    }

                    AppHost.ForceGC();
                }

                result = true;
            } catch (Exception ex) {
                Logger.WriteError("BaseContext.FileLoad()", ex);
                AppHost.StdDialogs.ShowError(LangMan.LS(LSID.LoadGedComFailed));
            }

            return result;
        }

        private void FileLoad(FileProvider fileProvider, string fileName, string password, IProgressController progress)
        {
            fTree.ProgressCallback = progress;

            if (string.IsNullOrEmpty(password)) {
                fileProvider.LoadFromFile(fileName, GlobalOptions.Instance.CharsetDetection);
            } else {
                LoadFromSecFile(fileName, password, GlobalOptions.Instance.CharsetDetection);
            }

            fTree.ProgressCallback = null;

            fFileName = fileName;
        }

        public async Task<bool> FileSave(string fileName)
        {
            bool result = false;

            try {
                string pw = null;
                string ext = FileHelper.GetFileExtension(fileName);
                if (ext == ".geds") {
                    pw = await AppHost.StdDialogs.GetPassword(LangMan.LS(LSID.Password));
                    if (string.IsNullOrEmpty(pw)) {
                        AppHost.StdDialogs.ShowError(LangMan.LS(LSID.PasswordIsNotSpecified));
                        return false;
                    }
                }

                FileSave(fileName, pw);
                result = true;
            } catch (UnauthorizedAccessException) {
                AppHost.StdDialogs.ShowError(string.Format(LangMan.LS(LSID.FileSaveError), new object[] { fileName, ": access denied" }));
            } catch (Exception ex) {
                AppHost.StdDialogs.ShowError(string.Format(LangMan.LS(LSID.FileSaveError), new object[] { fileName, "" }));
                Logger.WriteError("BaseContext.FileSave()", ex);
            }

            return result;
        }

        private void RemoveOldestBackups(string fileName, string bakPath)
        {
            string backupFiles = Path.GetFileName(fileName) + ".*";
            DirectoryInfo bakPathInfo = new DirectoryInfo(bakPath);
            FileInfo[] bakFiles = bakPathInfo.GetFiles(backupFiles);
            if (bakFiles.Length > GlobalOptions.Instance.FileBackupEachRevisionMaxCount) {
                List<Tuple<string, int>> tuples = new List<Tuple<string, int>>();
                foreach (var bak in bakFiles) {
                    try {
                        int bakVersion = Convert.ToInt32(bak.Extension.Substring(1));
                        tuples.Add(new Tuple<string, int>(bak.FullName, bakVersion));
                    } catch (Exception) {
                    }
                }
                tuples.Sort((a, b) => b.Item2.CompareTo(a.Item2));
                for (int i = GlobalOptions.Instance.FileBackupEachRevisionMaxCount; i < tuples.Count; i++) {
                    try {
                        File.Delete(tuples[i].Item1);
                    } catch (Exception) {
                    }
                }
            }
        }

        private static string CheckFileName(string fileName)
        {
            // Control of possible folder management problems in Windows
            // if there is a space before the dot in the file name
            // (the folder cannot be deleted, copied, renamed, moved).
            string purePath = Path.GetDirectoryName(fileName);
            string pureFileName = Path.GetFileNameWithoutExtension(fileName);
            string ext = Path.GetExtension(fileName);
            if (pureFileName.EndsWith(" ")) {
                pureFileName = pureFileName.TrimEnd();
                fileName = Path.Combine(purePath, Path.ChangeExtension(pureFileName, ext));
            }
            return fileName;
        }

        private void FileSave(string fileName, string password)
        {
            string oldFileName = fFileName;

            fileName = CheckFileName(fileName);

            switch (GlobalOptions.Instance.FileBackup) {
                case FileBackup.fbNone:
                    break;

                case FileBackup.fbOnlyPrev:
                    if (string.Equals(oldFileName, fileName) && File.Exists(oldFileName)) {
                        string bakFile = fileName + ".bak";
                        if (File.Exists(bakFile)) {
                            File.Delete(bakFile);
                        }

                        File.Move(oldFileName, bakFile);
                    }
                    break;

                case FileBackup.fbEachRevision:
                    if (File.Exists(fileName)) {
                        int rev = fTree.Header.File.Revision;
                        string bakPath = Path.GetDirectoryName(fileName) + Path.DirectorySeparatorChar + "__history" + Path.DirectorySeparatorChar;
                        string bakFile = Path.GetFileName(fileName) + "." + ConvertHelper.AdjustNumber(rev, 3);

                        if (!Directory.Exists(bakPath)) Directory.CreateDirectory(bakPath);
                        File.Move(fileName, bakPath + bakFile);

                        if (GlobalOptions.Instance.FileBackupEachRevisionMaxCount > 0) RemoveOldestBackups(fileName, bakPath);
                    }
                    break;
            }

            // check for archive and storage, move them if the file changes location
            MoveMediaContainers(oldFileName, fileName);

            if (string.IsNullOrEmpty(password)) {
                GKUtils.PrepareHeader(fTree, fileName, GlobalOptions.Instance.DefCharacterSet, false);

                var gedcomProvider = new GEDCOMProvider(fTree, GlobalOptions.Instance.KeepRichNames);
                gedcomProvider.SaveToFile(fileName, GlobalOptions.Instance.DefCharacterSet);
            } else {
                SaveToSecFile(fileName, GlobalOptions.Instance.DefCharacterSet, password);
            }

            fFileName = fileName;
        }

        public void CriticalSave()
        {
            try {
                string rfn = Path.ChangeExtension(fFileName, ".restore");

                GEDCOMCharacterSet charSet = GlobalOptions.Instance.DefCharacterSet;
                GKUtils.PrepareHeader(fTree, rfn, charSet, false);

                var gedcomProvider = new GEDCOMProvider(fTree, GlobalOptions.Instance.KeepRichNames);
                gedcomProvider.SaveToFile(rfn, charSet);
            } catch (Exception ex) {
                Logger.WriteError("BaseContext.CriticalSave()", ex);
            }
        }

        private const string GEDSEC_HEADER = "GEDSECAA";
        private const byte GS_MAJOR_VER = 1;
        private const byte GS_MINOR_VER = 2;

        private static SymmetricAlgorithm CreateCSP(byte majorVer, byte minorVer, string password)
        {
#if NETCORE
            const int BlockSize = 128;
#else
            const int BlockSize = 256;
#endif

            if (majorVer >= 1) {
                SymmetricAlgorithm csp = null;

                byte[] pwd = Encoding.Unicode.GetBytes(password);

                switch (minorVer) {
                    case 1:
                        {
                            byte[] salt = SCCrypt.CreateRandomSalt(7);
                            csp = new DESCryptoServiceProvider();
                            var pdb = new PasswordDeriveBytes(pwd, salt);
                            try {
                                csp.Key = pdb.CryptDeriveKey("DES", "SHA1", csp.KeySize, csp.IV);
                                SCCrypt.ClearBytes(salt);
                            } finally {
                                var pdbDisp = pdb as IDisposable;
                                if (pdbDisp != null) pdbDisp.Dispose();
                            }
                        }
                        break;

                    case 2:
                        {
                            var keyBytes = new byte[BlockSize / 8];
                            Array.Copy(pwd, keyBytes, Math.Min(keyBytes.Length, pwd.Length));
                            csp = new RijndaelManaged();
                            csp.KeySize = BlockSize;
                            csp.BlockSize = BlockSize;
                            csp.Key = keyBytes;
                            csp.IV = keyBytes;
                            csp.Padding = PaddingMode.PKCS7;
                            csp.Mode = CipherMode.CBC;
                        }
                        break;
                }

                SCCrypt.ClearBytes(pwd);

                return csp;
            }
            return null;
        }

        public void LoadFromSecStream(Stream stream, string password, bool charsetDetection = false)
        {
            byte[] gsHeader = new byte[8];
            stream.Read(gsHeader, 0, 8);
            byte gsMajVer = gsHeader[6];
            byte gsMinVer = gsHeader[7];
            gsHeader[6] = 65;
            gsHeader[7] = 65;
            string gsh = Encoding.ASCII.GetString(gsHeader);

            if (!string.Equals(gsh, GEDSEC_HEADER)) {
                throw new GKException(LangMan.LS(LSID.ItsNotGEDSECCompatibleFile));
            }

            if (gsMajVer < GS_MAJOR_VER || gsMinVer < GS_MINOR_VER) {
                // dummy for future
            }

            using (var cryptic = CreateCSP(gsMajVer, gsMinVer, password)) {
                using (CryptoStream crStream = new CryptoStream(stream, cryptic.CreateDecryptor(), CryptoStreamMode.Read)) {
                    var gedcomProvider = new GEDCOMProvider(fTree);
                    gedcomProvider.LoadFromStreamExt(stream, crStream, charsetDetection);
                }
            }
        }

        public void SaveToSecStream(Stream stream, GEDCOMCharacterSet charSet, string password)
        {
            byte[] gsHeader = Encoding.ASCII.GetBytes(GEDSEC_HEADER);
            gsHeader[6] = GS_MAJOR_VER;
            gsHeader[7] = GS_MINOR_VER;
            stream.Write(gsHeader, 0, 8);

            using (var cryptic = CreateCSP(GS_MAJOR_VER, GS_MINOR_VER, password)) {
                using (CryptoStream crStream = new CryptoStream(stream, cryptic.CreateEncryptor(), CryptoStreamMode.Write)) {
                    var gedcomProvider = new GEDCOMProvider(fTree, GlobalOptions.Instance.KeepRichNames);
                    gedcomProvider.SaveToStreamExt(crStream, charSet);
                    crStream.Flush();
                }
            }
        }

        public void LoadFromSecFile(string fileName, string password, bool charsetDetection = false)
        {
            using (var fileStream = new FileStream(fileName, FileMode.Open, FileAccess.Read)) {
                LoadFromSecStream(fileStream, password, charsetDetection);
            }
        }

        public void SaveToSecFile(string fileName, GEDCOMCharacterSet charSet, string password)
        {
            using (var fileStream = new FileStream(fileName, FileMode.Create, FileAccess.Write)) {
                GKUtils.PrepareHeader(fTree, fileName, charSet, false);

                SaveToSecStream(fileStream, charSet, password);
            }
        }

        #endregion

        #region Updating

        public bool IsUpdated()
        {
            return fTree.IsUpdated();
        }

        public void BeginUpdate()
        {
            fTree.BeginUpdate();
        }

        public void EndUpdate()
        {
            fTree.EndUpdate();
        }

        public void SwitchShieldState()
        {
            ShieldState ss = fShieldState;
            if (ss == ShieldState.None) {
                ss = ShieldState.Maximum;
            } else {
                ss = (ShieldState)((int)ss + 1);
            }

            ShieldState = ss;
        }

        #endregion

        #region Undo/Redo

        public void DoUndo()
        {
            fUndoman.Undo();

            if (fViewer != null) fViewer.RefreshLists(false);
            if (AppHost.Instance != null) AppHost.Instance.UpdateControls(false);
        }

        public void DoRedo()
        {
            fUndoman.Redo();

            if (fViewer != null) fViewer.RefreshLists(false);
            if (AppHost.Instance != null) AppHost.Instance.UpdateControls(false);
        }

        public void DoCommit()
        {
            fUndoman.Commit();
        }

        public void DoRollback()
        {
            fUndoman.Rollback();
        }

        #endregion

        #region Modify routines

        public void SetModified()
        {
            fTree.Header.TransmissionDateTime = DateTime.Now;
            Modified = true;
        }

        /// <summary>
        /// This method performs a basic locking of the records for their
        /// editors.
        ///
        /// The original idea was to call the methods Lock/Unlock records,
        /// in the edit dialogs of the records. However, it would be unsafe,
        /// because in the case of a failure of dialogue, the record would
        /// remain locked. Therefore, the locking and unlocking of records
        /// must take on a methods that controls the dialog.
        /// </summary>
        /// <param name="record"></param>
        public void LockRecord(GDMRecord record)
        {
            fLockedRecords.Add(record);
        }

        public void UnlockRecord(GDMRecord record)
        {
            fLockedRecords.Remove(record);
        }

        public bool IsAvailableRecord(GDMRecord record)
        {
            bool result = fLockedRecords.IndexOf(record) < 0;

            if (!result) {
                // message, for exclude of duplication
                AppHost.StdDialogs.ShowWarning(LangMan.LS(LSID.RecordIsLocked));
            }

            return result;
        }

        #endregion

        #region UI control functions

        public async Task<GDMFamilyRecord> SelectFamily(IView owner, GDMIndividualRecord target, TargetMode targetMode = TargetMode.tmFamilyChild)
        {
            GDMFamilyRecord result;

            try {
                using (var dlg = AppHost.ResolveDialog<IRecordSelectDialog>(fViewer, GDMRecordType.rtFamily)) {
                    dlg.SetTarget(targetMode, target, GDMSex.svUnknown);

                    if (await AppHost.Instance.ShowModalAsync(dlg, owner, false)) {
                        result = dlg.ResultRecord as GDMFamilyRecord;
                    } else {
                        result = null;
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("BaseContext.SelectFamily()", ex);
                result = null;
            }

            return result;
        }

        public async Task<GDMIndividualRecord> SelectPerson(IView owner, GDMIndividualRecord target, TargetMode targetMode, GDMSex needSex)
        {
            GDMIndividualRecord result;

            try {
                using (var dlg = AppHost.ResolveDialog<IRecordSelectDialog>(fViewer, GDMRecordType.rtIndividual)) {
                    dlg.SetTarget(targetMode, target, needSex);

                    if (await AppHost.Instance.ShowModalAsync(dlg, owner, false)) {
                        result = dlg.ResultRecord as GDMIndividualRecord;
                    } else {
                        result = null;
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("BaseContext.SelectPerson()", ex);
                result = null;
            }

            return result;
        }

        public async Task<GDMRecord> SelectRecord(IView owner, GDMRecordType mode, params object[] args)
        {
            GDMRecord result;

            try {
                using (var dlg = AppHost.ResolveDialog<IRecordSelectDialog>(fViewer, mode)) {
                    var flt = (args != null && args.Length > 0) ? (args[0] as string) : "*";
                    dlg.SetTarget(TargetMode.tmNone, null, GDMSex.svUnknown, flt);

                    if (await AppHost.Instance.ShowModalAsync(dlg, owner, false)) {
                        result = dlg.ResultRecord;
                    } else {
                        result = null;
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("BaseContext.SelectRecord()", ex);
                result = null;
            }

            return result;
        }

        #endregion

        #region Data modification functions

        private async Task<GDMFamilyRecord> GetFamilyBySpouse(GDMIndividualRecord newParent)
        {
            GDMFamilyRecord result = null;

            int num = fTree.RecordsCount;
            for (int i = 0; i < num; i++) {
                GDMRecord rec = fTree[i];

                if (rec.RecordType == GDMRecordType.rtFamily) {
                    GDMFamilyRecord fam = (GDMFamilyRecord)rec;
                    GDMIndividualRecord husb = fTree.GetPtrValue(fam.Husband);
                    GDMIndividualRecord wife = fTree.GetPtrValue(fam.Wife);
                    if (husb == newParent || wife == newParent) {
                        string msg = string.Format(LangMan.LS(LSID.ParentsQuery), GKUtils.GetFamilyString(fTree, fam));
                        if (await AppHost.StdDialogs.ShowQuestion(msg)) {
                            result = fam;
                            break;
                        }
                    }
                }
            }

            return result;
        }

        public async Task<GDMFamilyRecord> GetChildFamily(GDMIndividualRecord iChild, bool canCreate,
                                                 GDMIndividualRecord newParent)
        {
            GDMFamilyRecord result = null;

            if (iChild != null) {
                if (iChild.ChildToFamilyLinks.Count != 0) {
                    result = fTree.GetPtrValue(iChild.ChildToFamilyLinks[0]);
                } else {
                    if (canCreate) {
                        GDMFamilyRecord fam = await GetFamilyBySpouse(newParent);
                        if (fam == null) {
                            fam = fTree.CreateFamily();
                        }
                        fam.AddChild(iChild);
                        result = fam;
                    }
                }
            }

            return result;
        }

        public GDMFamilyRecord AddFamilyForSpouse(GDMIndividualRecord spouse)
        {
            if (spouse == null)
                throw new ArgumentNullException(@"spouse");

            GDMSex sex = spouse.Sex;
            if (sex < GDMSex.svMale || sex > GDMSex.svFemale) {
                AppHost.StdDialogs.ShowError(LangMan.LS(LSID.IsNotDefinedSex));
                return null;
            }

            GDMFamilyRecord family = fTree.CreateFamily();
            family.AddSpouse(spouse);
            return family;
        }

        public async Task<GDMIndividualRecord> AddChildForParent(IView owner, GDMIndividualRecord parent, GDMSex needSex)
        {
            GDMIndividualRecord resultChild = null;

            if (parent != null) {
                if (parent.SpouseToFamilyLinks.Count > 1) {
                    AppHost.StdDialogs.ShowError(LangMan.LS(LSID.ThisPersonHasSeveralFamilies));
                } else {
                    GDMFamilyRecord family = (parent.SpouseToFamilyLinks.Count == 0) ? null : fTree.GetPtrValue(parent.SpouseToFamilyLinks[0]);
                    GDMIndividualRecord father = (family == null) ? null : fTree.GetPtrValue(family.Husband);
                    if (father == null && parent.Sex == GDMSex.svMale) {
                        father = parent;
                    }

                    GDMIndividualRecord child = await SelectPerson(owner, father, TargetMode.tmParent, needSex);

                    if (child != null) {
                        if (family == null) {
                            family = AddFamilyForSpouse(parent);
                            if (family == null) {
                                return null;
                            }
                        }

                        if (family.HasMember(child)) {
                            AppHost.StdDialogs.ShowAlert(LangMan.LS(LSID.InvalidLink));
                            return null;
                        }

                        if (family.AddChild(child)) {
                            // this repetition necessary, because the call of CreatePersonDialog only works if person already has a father,
                            // what to call AddChild () is no; all this is necessary in order to in the namebook were correct patronymics.
                            ImportNames(child);

                            ProcessIndividual(child);

                            resultChild = child;
                        }
                    }
                }
            }

            return resultChild;
        }

        public async Task<GDMIndividualRecord> SelectSpouseFor(IView owner, GDMIndividualRecord iRec)
        {
            if (iRec == null)
                throw new ArgumentNullException(@"iRec");

            GDMSex needSex;
            switch (iRec.Sex) {
                case GDMSex.svMale:
                    needSex = GDMSex.svFemale;
                    break;

                case GDMSex.svFemale:
                    needSex = GDMSex.svMale;
                    break;

                default:
                    AppHost.StdDialogs.ShowError(LangMan.LS(LSID.IsNotDefinedSex));
                    return null;
            }

            return await SelectPerson(owner, iRec, TargetMode.tmSpouse, needSex);
        }

        public void ProcessFamily(GDMFamilyRecord famRec)
        {
            if (famRec == null) return;

            if (GlobalOptions.Instance.AutoSortChildren) {
                fTree.SortChildren(famRec);
            }
        }

        public void ProcessIndividual(GDMIndividualRecord indiRec)
        {
            if (indiRec == null) return;

            if (indiRec.ChildToFamilyLinks.Count > 0) {
                ProcessFamily(fTree.GetPtrValue(indiRec.ChildToFamilyLinks[0]));
            }

            if (GlobalOptions.Instance.AutoSortSpouses) {
                fTree.SortSpouses(indiRec);
            }
        }

        #endregion
    }
}
