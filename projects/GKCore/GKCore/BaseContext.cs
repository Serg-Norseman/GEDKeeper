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
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using BSLib;
using GDModel;
using GDModel.Providers;
using GDModel.Providers.FamilyShow;
using GDModel.Providers.GEDCOM;
using GDModel.Providers.GedML;
using GDModel.Providers.GEDZIP;
using GKCore.Backups;
using GKCore.Controllers;
using GKCore.Cultures;
using GKCore.Design;
using GKCore.Design.Graphics;
using GKCore.Design.Views;
using GKCore.Events;
using GKCore.Lists;
using GKCore.Locales;
using GKCore.Media;
using GKCore.Names;
using GKCore.Operations;
using GKCore.Options;
using GKCore.Search;
using GKCore.Utilities;

namespace GKCore
{
    public class FileProps
    {
        public int Checksum;
        public int LastBackupTime;
    }


    /// <summary>
    /// Status display the protected information in lists, pedigrees and graphs.
    /// </summary>
    public enum ShieldState
    {
        Maximum,
        Middle,
        None
    }


    /// <summary>
    /// Database context that combines GEDCOM file data with the environment,
    /// language, culture, processing and editing tools.
    /// </summary>
    public class BaseContext : BaseObject, IDisposable
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
            get {
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
            get {
                return fModified;
            }
            set {
                fModified = value;

                ModifiedChanged?.Invoke(this, null);
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
            get {
                return fShieldState;
            }
            set {
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

        #endregion

        #region Name and sex functions

        public async Task<string> DefinePatronymic(IView owner, string name, GDMSex sex, bool confirm)
        {
            ICulture culture = this.Culture;
            if (!culture.HasPatronymic) return string.Empty;

            string result = "";

            var namesTable = AppHost.NamesTable;

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
            var namesTable = AppHost.NamesTable;

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

        #region Media support

        public string GetTreePath()
        {
            return Path.GetDirectoryName(fFileName) + Path.DirectorySeparatorChar;
        }

        public string GetTreeRelativePath(string fileName)
        {
            string result = GKUtils.GetRelativePath(GetTreePath(), fileName);
            return result;
        }

        public string GetArcFileName()
        {
            if (IsGEDZIP()) {
                return fFileName;
            }

            string result = GetTreePath() + Path.GetFileNameWithoutExtension(fFileName) + ".zip";
            return result;
        }

        public string GetStgFolder()
        {
            string result = GetTreePath() + Path.GetFileNameWithoutExtension(fFileName) + Path.DirectorySeparatorChar;
            return result;
        }

        public void MoveMediaContainers(string oldFileName, string newFileName, bool createCopy = false)
        {
            // do nothing if file name is not changed
            if (string.IsNullOrEmpty(oldFileName) || string.Equals(oldFileName, newFileName)) return;

            bool hasArc = File.Exists(GetArcFileName());
            bool hasStg = Directory.Exists(GetStgFolder());

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
                    Directory.Move(GetStgFolder(), newStg);
                } else {
                    // TODO!
                }
            }
        }

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

        public bool CheckNewMedia(string fileName, MediaStoreType storeType)
        {
            if ((storeType == MediaStoreType.mstArchive || storeType == MediaStoreType.mstStorage) && !CheckBasePath()) {
                return false;
            }

            if (string.IsNullOrEmpty(fileName) || (storeType != MediaStoreType.mstURL && !File.Exists(fileName))) {
                AppHost.StdDialogs.ShowError(LangMan.LS(LSID.InvalidFileName));
                return false;
            }

            return true;
        }

        public bool MoveMediaFile(GDMMultimediaRecord mediaRec, MediaStoreType newStoreType)
        {
            bool result = false;

            if (mediaRec == null || mediaRec.FileReferences.Count < 1) return result;

            var fileRef = mediaRec.FileReferences[0];
            var oldStoreType = MediaStore.GetStoreType(fileRef.StringValue);

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

        public bool IsGEDZIP()
        {
            return FileHelper.GetFileExtension(fFileName) == ".gdz";
        }

        public Stream MediaLoad(GDMFileReference fileReference, bool throwException)
        {
            if (fileReference == null) return null;
            var mediaStore = MediaStore.GetMediaStore(this, fileReference.StringValue);
            return mediaStore.MediaLoad(throwException);
        }

        public string MediaLoad(GDMFileReference fileReference)
        {
            return (fileReference == null) ? string.Empty : MediaLoad(fileReference.StringValue);
        }

        public string MediaLoad(string fileReference)
        {
            if (string.IsNullOrEmpty(fileReference)) return string.Empty;
            var mediaStore = MediaStore.GetMediaStore(this, fileReference);
            return mediaStore.MediaLoad();
        }

        public bool MediaSave(GDMFileReference fileReference, string fileName, MediaStoreType storeType)
        {
            if (fileReference == null) return false;

            bool result = MediaSave(out string refPath, fileName, storeType);
            if (result) {
                fileReference.LinkFile(refPath);
            }
            return result;
        }

        public bool MediaSave(out string refPath, string fileName, MediaStoreType storeType)
        {
            var mediaStore = MediaStore.CreateMediaStore(this, storeType);
            return mediaStore.MediaSave(fileName, out refPath);
        }

        public async Task<bool> MediaDelete(GDMFileReference fileReference)
        {
            if (fileReference == null) return false;
            var mediaStore = MediaStore.GetMediaStore(this, fileReference.StringValue);
            return await mediaStore.MediaDelete();
        }

        public MediaStoreStatus VerifyMediaFile(string fileReference, out string displayFileName)
        {
            if (fileReference == null)
                throw new ArgumentNullException("fileReference");

            var mediaStore = MediaStore.GetMediaStore(this, fileReference);
            return mediaStore.VerifyMediaFile(out displayFileName);
        }

        public bool MediaExists(string refPath)
        {
            if (string.IsNullOrEmpty(refPath)) return false;

            for (int i = 0, num = fTree.RecordsCount; i < num; i++) {
                if (fTree[i] is GDMMultimediaRecord rec && rec.FileReferences.Count > 0) {
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

        /// <summary>
        /// Loading an image from a multimedia link with the features to get a thumbnail for the trees and cut out a part from the whole.
        /// </summary>
        /// <param name="mmRec"></param>
        /// <param name="thumbWidth">thumbnail width, if <= 0 - then the image width is unchanged</param>
        /// <param name="thumbHeight">thumbnail height, if <= 0 - then the image height is unchanged</param>
        /// <param name="cutoutArea"></param>
        /// <param name="throwException"></param>
        /// <returns></returns>
        public IImage LoadMediaImage(GDMMultimediaRecord mmRec, int thumbWidth, int thumbHeight, ExtRect cutoutArea, bool reduce, bool throwException)
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
                    result = gfxProvider.LoadImage(inputStream, thumbWidth, thumbHeight, cutoutArea, reduce);
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

        // Used in FamilyBookExporter, TreeChart and PersonEdit
        public IImage GetPrimaryBitmap(GDMIndividualRecord iRec, int thumbWidth, int thumbHeight, bool throwException)
        {
            if (iRec == null) return null;

            IImage result = null;
            try {
                GDMMultimediaLink mmLink = iRec.GetPrimaryMultimediaLink();
                GDMMultimediaRecord mmRec = fTree.GetPtrValue<GDMMultimediaRecord>(mmLink);

                if (mmLink != null && mmRec != null) {
                    var cutoutArea = mmLink.IsPrimaryCutout ? mmLink.CutoutPosition.Value : ExtRect.CreateEmpty();
                    result = LoadMediaImage(mmRec, thumbWidth, thumbHeight, cutoutArea, true, throwException);
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
            var isNew = string.IsNullOrEmpty(fFileName) || !File.Exists(fFileName);
            if (isNew) return true;

            var ext = FileHelper.GetFileExtension(fFileName);
            return ext != ".ged" && ext != ".geds";
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
                string ext = FileHelper.GetFileExtension(fileName);
                if (ext == ".ged") {
                    fileProvider = new GEDCOMProvider(fTree);
                } else if (ext == ".geds") {
                    string pw;
                    if (loadSecure) {
                        pw = await AppHost.StdDialogs.GetPassword(LangMan.LS(LSID.Password));
                        if (string.IsNullOrEmpty(pw)) {
                            AppHost.StdDialogs.ShowError(LangMan.LS(LSID.PasswordIsNotSpecified));
                            return false;
                        }
                    } else {
                        return false;
                    }

                    fileProvider = new SecGEDCOMProvider(fTree, pw);
                } else if (ext == ".xml") {
                    fileProvider = new GedMLProvider(fTree);
                } else if (ext == ".familyx") {
                    fileProvider = new FamilyXProvider(fTree);
                } else if (ext == ".gdz" || ext == ".zip") {
                    fileProvider = new GEDZIPProvider(fTree);
                } else {
                    AppHost.StdDialogs.ShowError(LangMan.LS(LSID.FormatUnsupported));
                    return false;
                }

                if (!showProgress) {
                    FileLoad(fileProvider, fileName, null);
                } else {
                    AppHost.Instance.ExecuteWork((controller) => {
                        controller.Begin(LangMan.LS(LSID.Loading), 100);
                        FileLoad(fileProvider, fileName, controller);
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

        private void FileLoad(FileProvider fileProvider, string fileName, IProgressController progress)
        {
            fTree.ProgressCallback = progress;

            fileProvider.LoadFromFile(fileName, GlobalOptions.Instance.CharsetDetection);

            fTree.ProgressCallback = null;

            fFileName = fileName;
        }

        public async Task<bool> FileSave(string fileName, bool strict = false)
        {
            try {
                var gedcomProvider = await CreateGedcomProvider(fileName, strict);
                if (gedcomProvider == null) {
                    return false;
                }

                FileSave(gedcomProvider, fileName, strict);
                return true;
            } catch (UnauthorizedAccessException) {
                AppHost.StdDialogs.ShowError(string.Format(LangMan.LS(LSID.FileSaveError), new object[] { fileName, ": access denied" }));
            } catch (Exception ex) {
                AppHost.StdDialogs.ShowError(string.Format(LangMan.LS(LSID.FileSaveError), new object[] { fileName, "" }));
                Logger.WriteError("BaseContext.FileSave()", ex);
            }

            return false;
        }

        private async Task<GEDCOMProvider> CreateGedcomProvider(string fileName, bool strict)
        {
            var ext = FileHelper.GetFileExtension(fileName);
            if (ext == ".ged") {
                return new GEDCOMProvider(fTree, GlobalOptions.Instance.KeepRichNames, strict);
            }

            if (ext == ".geds" && !strict) {
                var pw = await AppHost.StdDialogs.GetPassword(LangMan.LS(LSID.Password));
                if (string.IsNullOrEmpty(pw)) {
                    AppHost.StdDialogs.ShowError(LangMan.LS(LSID.PasswordIsNotSpecified));
                    return null;
                }

                return new SecGEDCOMProvider(fTree, pw, GlobalOptions.Instance.KeepRichNames, false);
            }

            /*if (ext == ".gdz") {
                return new GEDZIPProvider(fTree);
            }*/

            AppHost.StdDialogs.ShowError(LangMan.LS(LSID.FormatUnsupported));
            return null;
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

        private void FileSave(GEDCOMProvider gedcomProvider, string fileName, bool strict)
        {
            fileName = CheckFileName(fileName);

            var defaultCharacterSet = GlobalOptions.Instance.DefCharacterSet;

            if (!strict) {
                string oldFileName = fFileName;

                BackupTool.ProcessBackup(fTree, oldFileName, fileName);

                // check for archive and storage, move them if the file changes location
                MoveMediaContainers(oldFileName, fileName, IsGEDZIP());

                BackupTool.ExtendedBackup(this, fileName);

                fFileName = fileName;
            }

            GKUtils.PrepareHeader(fTree, fileName, defaultCharacterSet, false);
            gedcomProvider.SaveToFile(fileName, defaultCharacterSet);
        }

        public void CriticalSave()
        {
            try {
                string rfn = Path.ChangeExtension(fFileName, ".restore");

                GEDCOMCharacterSet charSet = GlobalOptions.Instance.DefCharacterSet;
                GKUtils.PrepareHeader(fTree, rfn, charSet, false);

                var gedcomProvider = new GEDCOMProvider(fTree, GlobalOptions.Instance.KeepRichNames, false);
                gedcomProvider.SaveToFile(rfn, charSet);
            } catch (Exception ex) {
                Logger.WriteError("BaseContext.CriticalSave()", ex);
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

            // Trying to deal with memory leaks in WinForms and WPF
            // when displaying dialogs on the screen.
            AppHost.ForceGC();
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

        /// <summary>
        /// Set the data modification flag and timestamp.
        /// </summary>
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
