/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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

#define FILECOPY_EX

using System;
using System.Collections.Generic;
using System.IO;
using System.Security.Cryptography;
using System.Text;
using System.Text.RegularExpressions;

using BSLib;
using GKCommon;
using GKCommon.GEDCOM;
using GKCore.Cultures;
using GKCore.Interfaces;
using GKCore.Operations;
using GKCore.Options;
using GKCore.Tools;
using GKCore.Types;
using GKCore.UIContracts;

namespace GKCore
{
    public class MediaFileNotFoundException : Exception
    {
        public MediaFileNotFoundException(string fileName)
            : base(string.Format("Media file {0} not found", fileName))
        {
        }
    }

    /// <summary>
    /// 
    /// </summary>
    public class BaseContext : BaseObject, IBaseContext
    {
        #region Private fields

        private string fFileName;
        private bool fModified;
        private ShieldState fShieldState;

        private readonly List<GEDCOMLanguageID> fLangsList;
        private readonly List<GEDCOMRecord> fLockedRecords;
        private readonly GEDCOMTree fTree;
        private readonly ValuesCollection fValuesCollection;
        private readonly IBaseWindow fViewer;
        private readonly ChangeTracker fUndoman;

        #endregion

        #region Public properties

        public ICulture Culture
        {
            get {
                GEDCOMLanguageID langID = fTree.Header.Language.Value;
                ICulture culture;

                switch (langID) {
                    case GEDCOMLanguageID.Russian:
                    case GEDCOMLanguageID.Ukrainian:
                        culture = new RussianCulture();
                        break;

                    case GEDCOMLanguageID.Polish:
                        culture = new PolishCulture();
                        break;

                    case GEDCOMLanguageID.German:
                        culture = new GermanCulture();
                        break;

                    case GEDCOMLanguageID.Swedish:
                        culture = new SwedishCulture();
                        break;

                    case GEDCOMLanguageID.Icelandic:
                        culture = new IcelandCulture();
                        break;

                    case GEDCOMLanguageID.Armenian:
                        culture = new ArmenianCulture();
                        break;

                    case GEDCOMLanguageID.Turkish:
                        culture = new TurkishCulture();
                        break;

                    case GEDCOMLanguageID.French:
                        culture = new FrenchCulture();
                        break;

                    case GEDCOMLanguageID.Italian:
                        culture = new ItalianCulture();
                        break;

                    case GEDCOMLanguageID.Cantonese:
                    case GEDCOMLanguageID.Mandrin:
                        culture = new ChineseCulture();
                        break;

                    case GEDCOMLanguageID.English:
                    default:
                        culture = new BritishCulture();
                        break;
                }

                return culture;
            }
        }

        public string FileName
        {
            get { return fFileName; }
        }

        public List<GEDCOMLanguageID> LangsList
        {
            get { return fLangsList; }
        }

        public bool Modified
        {
            get {
                return fModified;
            }
            set {
                fModified = value;

                var eventHandler = ModifiedChanged;
                if (eventHandler != null) eventHandler(this, null);
            }
        }

        public GEDCOMTree Tree
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
            fTree = new GEDCOMTree();
            fViewer = viewer;
            fUndoman = new ChangeTracker(fTree);
            fValuesCollection = new ValuesCollection();
            fLockedRecords = new List<GEDCOMRecord>();
            fLangsList = new List<GEDCOMLanguageID>();
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                fUndoman.Dispose();
                fTree.Dispose();
            }
            base.Dispose(disposing);
        }

        #endregion

        #region Data Manipulation

        public void CollectEventValues(GEDCOMCustomEvent evt)
        {
            if (evt == null) return;

            string evName = evt.Name;
            string evVal = evt.StringValue;
            if (!string.IsNullOrEmpty(evName) && !string.IsNullOrEmpty(evVal)) {
                fValuesCollection.Add(evName, evVal, true);
            }
        }

        public void CollectNameLangs(GEDCOMPersonalName persName)
        {
            if (persName == null) return;

            GEDCOMLanguageID lang = persName.Language.Value;
            if (lang != GEDCOMLanguageID.Unknown && !fLangsList.Contains(lang)) {
                fLangsList.Add(lang);
            }
        }

        public GEDCOMCustomEvent CreateEventEx(GEDCOMRecordWithEvents aRec, string evSign, GEDCOMCustomDate evDate, string evPlace)
        {
            return CreateEventEx(aRec, evSign, evDate.StringValue, evPlace);
        }

        public GEDCOMCustomEvent CreateEventEx(GEDCOMRecordWithEvents aRec, string evSign, string evDate, string evPlace)
        {
            if (aRec == null) return null;

            GEDCOMCustomEvent result;

            if (aRec is GEDCOMIndividualRecord) {
                if (GKUtils.GetPersonEventKindBySign(evSign) == PersonEventKind.ekEvent) {
                    result = new GEDCOMIndividualEvent(fTree, aRec, "", "");
                } else {
                    result = new GEDCOMIndividualAttribute(fTree, aRec, "", "");
                }
            } else if (aRec is GEDCOMFamilyRecord) {
                result = new GEDCOMFamilyEvent(fTree, aRec, "", "");
            } else {
                return null;
            }

            aRec.AddEvent(result);

            result.SetName(evSign);

            if (evDate != "") {
                result.Date.ParseString(evDate);
            }

            if (evPlace != "") {
                result.Place.StringValue = evPlace;
            }

            return result;
        }

        public GEDCOMIndividualRecord CreatePersonEx(string iName, string iPatronymic, string iSurname, GEDCOMSex iSex, bool birthEvent)
        {
            GEDCOMIndividualRecord iRec = fTree.CreateIndividual();
            iRec.Sex = iSex;

            GEDCOMPersonalName pName = iRec.AddPersonalName(new GEDCOMPersonalName(fTree, iRec, "", ""));
            GKUtils.SetNameParts(pName, iSurname, iName, iPatronymic);

            if (birthEvent) CreateEventEx(iRec, "BIRT", "", "");

            return iRec;
        }

        public bool DeleteRecord(GEDCOMRecord record)
        {
            bool result = false;
            if (record == null) return result;

            try {
                // `Viewer` (BaseWin) is notified about deletion of the record
                // before the fact of deletion
                if (fViewer != null) {
                    fViewer.NotifyRecord(record, RecordAction.raDelete);
                }

                BeginUpdate();

                switch (record.RecordType)
                {
                    case GEDCOMRecordType.rtIndividual:
                        result = fTree.DeleteIndividualRecord(record as GEDCOMIndividualRecord);
                        break;

                    case GEDCOMRecordType.rtFamily:
                        result = fTree.DeleteFamilyRecord(record as GEDCOMFamilyRecord);
                        break;

                    case GEDCOMRecordType.rtNote:
                        result = fTree.DeleteNoteRecord(record as GEDCOMNoteRecord);
                        break;

                    case GEDCOMRecordType.rtMultimedia:
                        result = fTree.DeleteMediaRecord(record as GEDCOMMultimediaRecord);
                        break;

                    case GEDCOMRecordType.rtSource:
                        result = fTree.DeleteSourceRecord(record as GEDCOMSourceRecord);
                        break;

                    case GEDCOMRecordType.rtRepository:
                        result = fTree.DeleteRepositoryRecord(record as GEDCOMRepositoryRecord);
                        break;

                    case GEDCOMRecordType.rtGroup:
                        result = fTree.DeleteGroupRecord(record as GEDCOMGroupRecord);
                        break;

                    case GEDCOMRecordType.rtResearch:
                        result = fTree.DeleteResearchRecord(record as GEDCOMResearchRecord);
                        break;

                    case GEDCOMRecordType.rtTask:
                        result = fTree.DeleteTaskRecord(record as GEDCOMTaskRecord);
                        break;

                    case GEDCOMRecordType.rtCommunication:
                        result = fTree.DeleteCommunicationRecord(record as GEDCOMCommunicationRecord);
                        break;

                    case GEDCOMRecordType.rtLocation:
                        result = fTree.DeleteLocationRecord(record as GEDCOMLocationRecord);
                        break;
                }
            } finally {
                EndUpdate();
            }

            if (result) {
                fTree.Header.TransmissionDateTime = DateTime.Now;
                Modified = true;
            }

            return result;
        }

        public IList<ISearchResult> FindAll(GEDCOMRecordType recordType, string searchPattern)
        {
            List<ISearchResult> result = new List<ISearchResult>();

            Regex regex = GKUtils.InitMaskRegex(searchPattern);

            int num = fTree.RecordsCount;
            for (int i = 0; i < num; i++) {
                GEDCOMRecord rec = fTree[i];
                if (rec.RecordType != recordType) continue;

                string recName = GKUtils.GetRecordName(rec, false);
                if (GKUtils.MatchesRegex(recName, regex)) {
                    result.Add(new SearchResult(rec));
                }
            }

            return result;
        }

        public bool IsRecordAccess(GEDCOMRestriction restriction)
        {
            bool result = false;

            switch (fShieldState) {
                case ShieldState.Maximum:
                    result = (restriction != GEDCOMRestriction.rnConfidential && restriction != GEDCOMRestriction.rnPrivacy);
                    break;

                case ShieldState.Middle:
                    result = (restriction != GEDCOMRestriction.rnPrivacy);
                    break;

                case ShieldState.None:
                    result = true;
                    break;
            }

            return result;
        }

        #endregion

        #region Data search

        public GEDCOMSourceRecord FindSource(string sourceName)
        {
            GEDCOMSourceRecord result = null;

            int num = fTree.RecordsCount;
            for (int i = 0; i < num; i++)
            {
                GEDCOMRecord rec = fTree[i];

                if (rec.RecordType == GEDCOMRecordType.rtSource && ((GEDCOMSourceRecord) rec).FiledByEntry == sourceName)
                {
                    result = (rec as GEDCOMSourceRecord);
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
            for (int i = 0; i < num; i++)
            {
                GEDCOMRecord rec = fTree[i];
                if (rec is GEDCOMSourceRecord)
                {
                    sources.AddObject((rec as GEDCOMSourceRecord).FiledByEntry, rec);
                }
            }
        }

        #endregion

        #region Individual utils

        public bool IsChildless(GEDCOMIndividualRecord iRec)
        {
            int exp = GKUtils.GetLifeExpectancy(iRec);
            return (exp != -1 && exp < 15);
        }

        public int FindBirthYear(GEDCOMIndividualRecord iRec)
        {
            if (iRec != null) {
                int birthDate = iRec.GetChronologicalYear("BIRT");
                if (birthDate != 0) {
                    return birthDate;
                }

                int num = iRec.SpouseToFamilyLinks.Count;
                for (int i = 0; i < num; i++)
                {
                    GEDCOMFamilyRecord family = iRec.SpouseToFamilyLinks[i].Family;

                    int num2 = family.Children.Count;
                    for (int j = 0; j < num2; j++)
                    {
                        GEDCOMIndividualRecord child = family.Children[j].Value as GEDCOMIndividualRecord;
                        birthDate = FindBirthYear(child);
                        if (birthDate != 0) {
                            return birthDate - 20;
                        }
                    }
                }
            }

            return 0;
        }

        public int FindDeathYear(GEDCOMIndividualRecord iRec)
        {
            if (iRec != null) {
                int deathDate = iRec.GetChronologicalYear("DEAT");
                if (deathDate != 0) {
                    return deathDate;
                }

                int maxBirth = 0;
                int num = iRec.SpouseToFamilyLinks.Count;
                for (int i = 0; i < num; i++)
                {
                    GEDCOMFamilyRecord family = iRec.SpouseToFamilyLinks[i].Family;

                    int num2 = family.Children.Count;
                    for (int j = 0; j < num2; j++)
                    {
                        GEDCOMIndividualRecord child = family.Children[j].Value as GEDCOMIndividualRecord;

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

            if (!GlobalOptions.Instance.ShowTips) return;

            try
            {
                try
                {
                    bool firstTip = true;
                    int num = fTree.RecordsCount;
                    for (int i = 0; i < num; i++)
                    {
                        GEDCOMRecord rec = fTree[i];
                        if (rec.RecordType != GEDCOMRecordType.rtIndividual) continue;

                        GEDCOMIndividualRecord iRec = (GEDCOMIndividualRecord) rec;

                        int days = GKUtils.GetDaysForBirth(iRec);
                        if (days >= 0 && days < 3) {
                            string tip;

                            if (firstTip) {
                                tipsList.Add("#" + LangMan.LS(LSID.LSID_BirthDays));
                                firstTip = false;
                            }

                            string nm = Culture.GetPossessiveName(iRec);

                            switch (days) {
                                case 0:
                                    tip = string.Format(LangMan.LS(LSID.LSID_BirthdayToday), nm);
                                    break;
                                case 1:
                                    tip = string.Format(LangMan.LS(LSID.LSID_BirthdayTomorrow), nm);
                                    break;
                                default:
                                    tip = string.Format(LangMan.LS(LSID.LSID_DaysRemained), nm, days);
                                    break;
                            }

                            tipsList.Add(tip);
                        }
                    }
                }
                finally
                {
                    // temp stub, remove try/finally here?
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("BaseContext.CollectTips(): " + ex.Message);
            }
        }

        #endregion

        #region Name and sex functions

        public string DefinePatronymic(string name, GEDCOMSex sex, bool confirm)
        {
            ICulture culture = this.Culture;
            if (!culture.HasPatronymic()) return string.Empty;

            string result = "";

            INamesTable namesTable = AppHost.NamesTable;

            NameEntry n = namesTable.FindName(name);
            if (n == null) {
                if (!confirm) {
                    return result;
                }

                n = namesTable.AddName(name);
            }

            switch (sex)
            {
                case GEDCOMSex.svMale:
                    result = n.M_Patronymic;
                    break;

                case GEDCOMSex.svFemale:
                    result = n.F_Patronymic;
                    break;
            }

            if (result == "") {
                if (!confirm) {
                    return result;
                }

                BaseController.ModifyName(this, ref n);
            }

            switch (sex)
            {
                case GEDCOMSex.svMale:
                    result = n.M_Patronymic;
                    break;

                case GEDCOMSex.svFemale:
                    result = n.F_Patronymic;
                    break;
            }

            return result;
        }

        public GEDCOMSex DefineSex(string iName, string iPatr)
        {
            INamesTable namesTable = AppHost.NamesTable;

            GEDCOMSex result = namesTable.GetSexByName(iName);

            if (result == GEDCOMSex.svNone)
            {
                using (var dlg = AppHost.Container.Resolve<ISexCheckDlg>())
                {
                    dlg.IndividualName = iName + " " + iPatr;
                    result = this.Culture.GetSex(iName, iPatr, false);

                    dlg.Sex = result;
                    if (AppHost.Instance.ShowModalX(dlg, false))
                    {
                        result = dlg.Sex;

                        if (result != GEDCOMSex.svNone)
                        {
                            namesTable.SetNameSex(iName, result);
                        }
                    }
                }
            }

            return result;
        }

        public void CheckPersonSex(GEDCOMIndividualRecord iRec)
        {
            if (iRec == null)
                throw new ArgumentNullException("iRec");

            try {
                BeginUpdate();

                if (iRec.Sex == GEDCOMSex.svNone || iRec.Sex == GEDCOMSex.svUndetermined)
                {
                    var parts = GKUtils.GetNameParts(iRec);
                    iRec.Sex = DefineSex(parts.Name, parts.Patronymic);
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

        private string GetArcFileName()
        {
            string treeName = fFileName;
            string result = GetTreePath(treeName) + Path.GetFileNameWithoutExtension(treeName) + ".zip";
            return result;
        }

        private string GetStgFolder(bool create)
        {
            string treeName = fFileName;
            string result = GetTreePath(treeName) + Path.GetFileNameWithoutExtension(treeName) + Path.DirectorySeparatorChar;
            if (!Directory.Exists(result) && create) Directory.CreateDirectory(result);
            return result;
        }

        // TODO: Controlling the version of the GK GEDCOM file to determine the zip archive encoding!
        private void ArcFileLoad(string targetFn, Stream toStream)
        {
            int treeVer = 0;
            Encoding zipCharset = (treeVer == 0) ? Encoding.GetEncoding("CP866") : Encoding.UTF8;

            targetFn = SysUtils.NormalizeFilename(targetFn);

            using (ZipStorer zip = ZipStorer.Open(GetArcFileName(), FileAccess.Read, zipCharset))
            {
                ZipStorer.ZipFileEntry entry = zip.FindFile(targetFn);
                if (entry != null) {
                    zip.ExtractStream(entry, toStream);
                }
            }
        }

        // TODO: Controlling the version of the GK GEDCOM file to determine the zip archive encoding!
        private void ArcFileSave(string fileName, string sfn)
        {
            int treeVer = 0;
            Encoding zipCharset = (treeVer == 0) ? Encoding.GetEncoding("CP866") : Encoding.UTF8;

            string arcFn = GetArcFileName();
            ZipStorer zip = null;

            try
            {
                if (File.Exists(arcFn)) {
                    zip = ZipStorer.Open(arcFn, FileAccess.ReadWrite, zipCharset);
                } else {
                    zip = ZipStorer.Create(arcFn, "");
                }
                zip.AddFile(ZipStorer.Compression.Deflate, fileName, sfn, null);
            }
            finally
            {
                if (zip != null) zip.Dispose();
            }
        }

        private void MoveMediaContainers(string oldFileName, string newFileName)
        {
            // do nothing if file name is not changed
            if (string.Equals(oldFileName, newFileName)) return;

            bool hasArc = File.Exists(GetArcFileName());
            bool hasStg = Directory.Exists(GetStgFolder(false));

            string newPath = Path.GetDirectoryName(newFileName);
            string newName = Path.GetFileName(newFileName);

            // move the archive and the storage folder to a new location
            if (hasArc) {
                string newArc = newPath + Path.DirectorySeparatorChar + GKUtils.GetContainerName(newName, true);
                File.Move(GetArcFileName(), newArc);
            }

            if (hasStg) {
                string newStg = newPath + Path.DirectorySeparatorChar + GKUtils.GetContainerName(newName, false);
                Directory.Move(GetStgFolder(false), newStg);
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
            string path = Path.GetDirectoryName(fFileName);

            bool result = (!string.IsNullOrEmpty(path));
            if (!result)
            {
                AppHost.StdDialogs.ShowError(LangMan.LS(LSID.LSID_NewDBFileNeedToSave));
            }
            return result;
        }

        public MediaStore GetStoreType(GEDCOMFileReference fileReference)
        {
            return GKUtils.GetStoreType(fileReference);
        }

        public Stream MediaLoad(GEDCOMFileReference fileReference, bool throwException)
        {
            Stream stream = null;
            if (fileReference == null) return null;

            var mediaStore = GetStoreType(fileReference);
            string targetFn = mediaStore.FileName;
            MediaStoreType gst = mediaStore.StoreType;

            switch (gst) {
                case MediaStoreType.mstStorage:
                    targetFn = GetStgFolder(false) + targetFn;
                    if (!File.Exists(targetFn))
                    {
                        if (throwException) {
                            throw new MediaFileNotFoundException(targetFn);
                        }

                        AppHost.StdDialogs.ShowError(LangMan.LS(LSID.LSID_ArcNotFound));
                    }
                    else {
                        stream = new FileStream(targetFn, FileMode.Open);
                    }
                    break;

                case MediaStoreType.mstArchive:
                    stream = new MemoryStream();
                    string arcFile = GetArcFileName();
                    if (!File.Exists(arcFile))
                    {
                        if (throwException) {
                            throw new MediaFileNotFoundException(arcFile);
                        }

                        AppHost.StdDialogs.ShowError(LangMan.LS(LSID.LSID_ArcNotFound));
                    }
                    else {
                        ArcFileLoad(targetFn, stream);
                        stream.Seek(0, SeekOrigin.Begin);
                    }
                    break;

                case MediaStoreType.mstReference:
                    stream = new FileStream(targetFn, FileMode.Open);
                    break;
            }

            return stream;
        }

        public string MediaLoad(GEDCOMFileReference fileReference)
        {
            string fileName = string.Empty;
            if (fileReference == null) return string.Empty;

            try
            {
                MediaStore mediaStore = GetStoreType(fileReference);
                string targetFn = mediaStore.FileName;

                switch (mediaStore.StoreType)
                {
                    case MediaStoreType.mstStorage:
                        fileName = GetStgFolder(false) + targetFn;
                        break;

                    case MediaStoreType.mstArchive:
                        fileName = GKUtils.GetTempDir() + Path.GetFileName(targetFn);
                        FileStream fs = new FileStream(fileName, FileMode.Create);
                        try
                        {
                            if (!File.Exists(GetArcFileName())) {
                                AppHost.StdDialogs.ShowError(LangMan.LS(LSID.LSID_ArcNotFound));
                            } else {
                                ArcFileLoad(targetFn, fs);
                            }
                        }
                        finally
                        {
                            fs.Close();
                            fs.Dispose();
                        }
                        break;

                    case MediaStoreType.mstReference:
                        {
                            fileName = targetFn;
                            if (!File.Exists(fileName)) {
                                string newPath = AppHost.PathReplacer.TryReplacePath(fileName);
                                if (!string.IsNullOrEmpty(newPath)) {
                                    fileName = newPath;
                                }
                            }
                            break;
                        }
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("BaseContext.MediaLoad_fn(): " + ex.Message);
                fileName = "";
            }

            return fileName;
        }

        // TODO: check existing of file
        public bool MediaSave(GEDCOMFileReference fileReference, string fileName, MediaStoreType storeType)
        {
            if (fileReference == null) return false;

            bool result = true;

            string storeFile = Path.GetFileName(fileName);
            string storePath = "";
            string refPath = "";

            storePath = GKUtils.GetStoreFolder(GKUtils.GetMultimediaKind(GEDCOMFileReference.RecognizeFormat(fileName)));

            switch (storeType)
            {
                case MediaStoreType.mstReference:
                    refPath = fileName;
                    break;

                case MediaStoreType.mstArchive:
                    refPath = GKData.GKStoreTypes[(int)storeType].Sign + storePath + storeFile;
                    ArcFileSave(fileName, storePath + storeFile);
                    break;

                case MediaStoreType.mstStorage:
                    refPath = GKData.GKStoreTypes[(int)storeType].Sign + storePath + storeFile;
                    try
                    {
                        string targetDir = GetStgFolder(true) + storePath;
                        if (!Directory.Exists(targetDir)) Directory.CreateDirectory(targetDir);

                        string targetFn = targetDir + storeFile;
                        CopyFile(fileName, targetFn);
                    }
                    catch (IOException)
                    {
                        AppHost.StdDialogs.ShowError(LangMan.LS(LSID.LSID_FileWithSameNameAlreadyExistsInStorage));
                        result = false;
                    }
                    break;
            }

            if (result) {
                refPath = SysUtils.NormalizeFilename(refPath);
                fileReference.LinkFile(refPath);
            }

            return result;
        }

        private void CopyFile(string sourceFileName, string destFileName)
        {
            #if FILECOPY_EX

            IProgressController progress = AppHost.Progress;
            try {
                progress.ProgressInit(LangMan.LS(LSID.LSID_CopyingFile), 100);

                var source = new FileInfo(sourceFileName);
                var target = new FileInfo(destFileName);
                GKUtils.CopyFile(source, target, progress);
            } finally {
                progress.ProgressDone();
            }

            #else

            File.Copy(sourceFileName, destFileName, false);

            #endif
        }

        public IImage LoadMediaImage(GEDCOMFileReference fileReference, bool throwException)
        {
            if (fileReference == null) return null;

            IImage result = null;
            try
            {
                Stream stm = MediaLoad(fileReference, throwException);
                if (stm != null) {
                    try {
                        if (stm.Length != 0) {
                            result = AppHost.GfxProvider.CreateImage(stm);
                        }
                    } finally {
                        stm.Dispose();
                    }
                }
            }
            catch (MediaFileNotFoundException)
            {
                throw;
            }
            catch (Exception ex)
            {
                Logger.LogWrite("BaseContext.LoadMediaImage(): " + ex.Message);
                result = null;
            }
            return result;
        }

        public IImage LoadMediaImage(GEDCOMFileReference fileReference, int thumbWidth, int thumbHeight, ExtRect cutoutArea, bool throwException)
        {
            if (fileReference == null) return null;

            IImage result = null;
            try
            {
                Stream stm = MediaLoad(fileReference, throwException);
                if (stm != null) {
                    try {
                        if (stm.Length != 0) {
                            result = AppHost.GfxProvider.CreateImage(stm, thumbWidth, thumbHeight, cutoutArea);
                        }
                    } finally {
                        stm.Dispose();
                    }
                }
            }
            catch (MediaFileNotFoundException)
            {
                throw;
            }
            catch (Exception ex)
            {
                Logger.LogWrite("BaseContext.LoadMediaImage(): " + ex.Message);
                result = null;
            }
            return result;
        }

        public IImage GetPrimaryBitmap(GEDCOMIndividualRecord iRec, int thumbWidth, int thumbHeight, bool throwException)
        {
            if (iRec == null) return null;

            IImage result = null;
            try
            {
                GEDCOMMultimediaLink mmLink = iRec.GetPrimaryMultimediaLink();
                if (mmLink != null && mmLink.Value != null)
                {
                    ExtRect cutoutArea;
                    if (mmLink.IsPrimaryCutout) {
                        cutoutArea = mmLink.CutoutPosition.Value;
                    } else {
                        cutoutArea = ExtRect.CreateEmpty();
                    }

                    GEDCOMMultimediaRecord mmRec = (GEDCOMMultimediaRecord)mmLink.Value;
                    result = LoadMediaImage(mmRec.FileReferences[0], thumbWidth, thumbHeight, cutoutArea, throwException);
                }
            }
            catch (MediaFileNotFoundException)
            {
                throw;
            }
            catch (Exception ex)
            {
                Logger.LogWrite("BaseContext.GetPrimaryBitmap(): " + ex.Message);
                result = null;
            }
            return result;
        }

        public string GetPrimaryBitmapUID(GEDCOMIndividualRecord iRec)
        {
            if (iRec == null) return null;

            string result = null;
            try
            {
                GEDCOMMultimediaLink mmLink = iRec.GetPrimaryMultimediaLink();
                result = (mmLink == null) ? null : mmLink.GetUID();
            }
            catch (Exception ex)
            {
                Logger.LogWrite("BaseContext.GetPrimaryBitmapUID(): " + ex.Message);
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

        private void LoadProgress(object sender, int progress)
        {
            AppHost.Progress.ProgressStep(progress);
        }

        public void Clear()
        {
            fTree.Clear();
            fUndoman.Clear();
        }

        public bool FileLoad(string fileName)
        {
            bool result = false;

            try
            {
                string pw = null;
                string ext = SysUtils.GetFileExtension(fileName);
                if (ext == ".geds" && !AppHost.StdDialogs.GetPassword(LangMan.LS(LSID.LSID_Password), ref pw)) {
                    AppHost.StdDialogs.ShowError(LangMan.LS(LSID.LSID_PasswordIsNotSpecified));
                    return false;
                }

                IProgressController progress = AppHost.Progress;
                progress.ProgressInit(LangMan.LS(LSID.LSID_Loading), 100);
                fTree.OnProgress += LoadProgress;
                try
                {
                    FileLoad(fileName, pw);
                    TreeTools.CheckGEDCOMFormat(fTree, this, progress);
                    result = true;
                }
                finally
                {
                    fTree.OnProgress -= LoadProgress;
                    progress.ProgressDone();
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("BaseContext.FileLoad(): " + ex.Message);
                AppHost.StdDialogs.ShowError(LangMan.LS(LSID.LSID_LoadGedComFailed));
            }

            return result;
        }

        private void FileLoad(string fileName, string password)
        {
            if (string.IsNullOrEmpty(password)) {
                var gedcomProvider = new GEDCOMProvider(fTree);
                gedcomProvider.LoadFromFile(fileName);
            } else {
                LoadFromSecFile(fileName, password);
            }

            fFileName = fileName;
        }

        public bool FileSave(string fileName)
        {
            bool result = false;

            try
            {
                string pw = null;
                string ext = SysUtils.GetFileExtension(fileName);
                if (ext == ".geds" && !AppHost.StdDialogs.GetPassword(LangMan.LS(LSID.LSID_Password), ref pw)) {
                    AppHost.StdDialogs.ShowError(LangMan.LS(LSID.LSID_PasswordIsNotSpecified));
                    return false;
                }

                FileSave(fileName, pw);
                result = true;
            }
            catch (UnauthorizedAccessException)
            {
                AppHost.StdDialogs.ShowError(string.Format(LangMan.LS(LSID.LSID_FileSaveError), new object[] { fileName, ": access denied" }));
            }
            catch (Exception ex)
            {
                AppHost.StdDialogs.ShowError(string.Format(LangMan.LS(LSID.LSID_FileSaveError), new object[] { fileName, "" }));
                Logger.LogWrite("BaseContext.FileSave(): " + ex.Message);
            }

            return result;
        }

        private void FileSave(string fileName, string password)
        {
            string oldFileName = fFileName;

            switch (GlobalOptions.Instance.FileBackup)
            {
                case FileBackup.fbNone:
                    break;

                case FileBackup.fbOnlyPrev:
                    if (string.Equals(oldFileName, fileName) && File.Exists(oldFileName))
                    {
                        string bakFile = Path.GetFileName(fileName) + ".bak";
                        if (File.Exists(bakFile)) {
                            File.Delete(bakFile);
                        }

                        File.Move(oldFileName, bakFile);
                    }
                    break;

                case FileBackup.fbEachRevision:
                    if (File.Exists(fileName))
                    {
                        int rev = fTree.Header.FileRevision;
                        string bakPath = Path.GetDirectoryName(fileName) + Path.DirectorySeparatorChar + "__history" + Path.DirectorySeparatorChar;
                        string bakFile = Path.GetFileName(fileName) + "." + ConvertHelper.AdjustNumber(rev, 3);

                        if (!Directory.Exists(bakPath)) Directory.CreateDirectory(bakPath);
                        File.Move(fileName, bakPath + bakFile);
                    }
                    break;
            }

            // check for archive and storage, move them if the file changes location
            MoveMediaContainers(oldFileName, fileName);

            if (string.IsNullOrEmpty(password)) {
                GKUtils.PrepareHeader(fTree, fileName, GlobalOptions.Instance.DefCharacterSet, false);

                var gedcomProvider = new GEDCOMProvider(fTree);
                gedcomProvider.SaveToFile(fileName, GlobalOptions.Instance.DefCharacterSet);
            } else {
                SaveToSecFile(fileName, GlobalOptions.Instance.DefCharacterSet, password);
            }

            fFileName = fileName;
        }

        public void CriticalSave()
        {
            try
            {
                string rfn = Path.ChangeExtension(fFileName, ".restore");
                // TODO: PrepareHeader or not?

                var gedcomProvider = new GEDCOMProvider(fTree);
                gedcomProvider.SaveToFile(rfn, GlobalOptions.Instance.DefCharacterSet);
            } catch (Exception ex) {
                Logger.LogWrite("BaseContext.CriticalSave(): " + ex.Message);
            }
        }

        private const string GEDSEC_HEADER = "GEDSECAA";
        private const byte GS_MAJOR_VER = 1;
        private const byte GS_MINOR_VER = 2;

        private static SymmetricAlgorithm CreateCryptoServiceProvider(byte majorVer, byte minorVer, PasswordDeriveBytes pdb)
        {
            if (majorVer >= 1) {
                SymmetricAlgorithm csp = null;

                switch (minorVer) {
                    case 1:
                        csp = new DESCryptoServiceProvider();
                        csp.Key = pdb.CryptDeriveKey("DES", "SHA1", csp.KeySize, csp.IV);
                        break;

                    case 2:
                        csp = new AesCryptoServiceProvider();
                        csp.Key = pdb.CryptDeriveKey("AES", "SHA1", csp.KeySize, csp.IV);
                        break;
                }

                return csp;
            }
            return null;
        }

        private void LoadFromSecFile(string fileName, string password)
        {
            using (FileStream fileStream = new FileStream(fileName, FileMode.Open, FileAccess.Read))
            {
                byte[] gsHeader = new byte[8];
                fileStream.Read(gsHeader, 0, 8);
                byte gsMajVer = gsHeader[6];
                byte gsMinVer = gsHeader[7];
                gsHeader[6] = 65;
                gsHeader[7] = 65;
                string gsh = Encoding.ASCII.GetString(gsHeader);

                if (!string.Equals(gsh, GEDSEC_HEADER)) {
                    throw new Exception(LangMan.LS(LSID.LSID_ItsNotGEDSECCompatibleFile));
                }

                if (gsMajVer < GS_MAJOR_VER || gsMinVer < GS_MINOR_VER)
                {
                    // dummy for future
                }

                byte[] pwd = Encoding.Unicode.GetBytes(password);
                byte[] salt = SCCrypt.CreateRandomSalt(7);

                PasswordDeriveBytes pdb = new PasswordDeriveBytes(pwd, salt);
                try {
                    using (var cryptic = CreateCryptoServiceProvider(gsMajVer, gsMinVer, pdb)) {
                        using (CryptoStream crStream = new CryptoStream(fileStream, cryptic.CreateDecryptor(), CryptoStreamMode.Read))
                        {
                            var gedcomProvider = new GEDCOMProvider(fTree);
                            gedcomProvider.LoadFromStreamExt(fileStream, crStream, fileName);
                        }

                        SCCrypt.ClearBytes(pwd);
                        SCCrypt.ClearBytes(salt);
                    }
                } finally {
                    var pdbDisp = pdb as IDisposable;
                    if (pdbDisp != null) pdbDisp.Dispose();
                }
            }
        }

        private void SaveToSecFile(string fileName, GEDCOMCharacterSet charSet, string password)
        {
            using (FileStream fileStream = new FileStream(fileName, FileMode.Create, FileAccess.Write))
            {
                byte[] gsHeader = Encoding.ASCII.GetBytes(GEDSEC_HEADER);
                gsHeader[6] = GS_MAJOR_VER;
                gsHeader[7] = GS_MINOR_VER;
                fileStream.Write(gsHeader, 0, 8);

                byte[] pwd = Encoding.Unicode.GetBytes(password);
                byte[] salt = SCCrypt.CreateRandomSalt(7);

                PasswordDeriveBytes pdb = new PasswordDeriveBytes(pwd, salt);
                try {
                    using (var cryptic = CreateCryptoServiceProvider(GS_MAJOR_VER, GS_MINOR_VER, pdb)) {
                        using (CryptoStream crStream = new CryptoStream(fileStream, cryptic.CreateEncryptor(), CryptoStreamMode.Write))
                        {
                            GKUtils.PrepareHeader(fTree, fileName, charSet, false);

                            var gedcomProvider = new GEDCOMProvider(fTree);
                            gedcomProvider.SaveToStreamExt(crStream, fileName, charSet);

                            crStream.Flush();
                        }

                        SCCrypt.ClearBytes(pwd);
                        SCCrypt.ClearBytes(salt);
                    }
                } finally {
                    var pdbDisp = pdb as IDisposable;
                    if (pdbDisp != null) pdbDisp.Dispose();
                }
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
        public void LockRecord(GEDCOMRecord record)
        {
            fLockedRecords.Add(record);
        }

        public void UnlockRecord(GEDCOMRecord record)
        {
            fLockedRecords.Remove(record);
        }

        public bool IsAvailableRecord(GEDCOMRecord record)
        {
            bool result = fLockedRecords.IndexOf(record) < 0;

            if (!result) {
                // message, for exclude of duplication
                AppHost.StdDialogs.ShowWarning(LangMan.LS(LSID.LSID_RecordIsLocked));
            }

            return result;
        }

        #endregion

        #region UI control functions

        public GEDCOMFamilyRecord SelectFamily(GEDCOMIndividualRecord target)
        {
            GEDCOMFamilyRecord result;

            try
            {
                using (var dlg = AppHost.Container.Resolve<IRecordSelectDialog>())
                {
                    dlg.InitDialog(fViewer);

                    dlg.Target = target;
                    dlg.NeedSex = GEDCOMSex.svNone;
                    dlg.TargetMode = TargetMode.tmFamilyChild;
                    dlg.RecType = GEDCOMRecordType.rtFamily;
                    if (AppHost.Instance.ShowModalX(dlg, false)) {
                        result = (dlg.ResultRecord as GEDCOMFamilyRecord);
                    } else {
                        result = null;
                    }
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("BaseContext.SelectFamily(): " + ex.Message);
                result = null;
            }

            return result;
        }

        public GEDCOMIndividualRecord SelectPerson(GEDCOMIndividualRecord target,
                                                   TargetMode targetMode, GEDCOMSex needSex)
        {
            GEDCOMIndividualRecord result;

            try
            {
                using (var dlg = AppHost.Container.Resolve<IRecordSelectDialog>())
                {
                    dlg.InitDialog(fViewer);

                    dlg.Target = target;
                    dlg.NeedSex = needSex;
                    dlg.TargetMode = targetMode;
                    dlg.RecType = GEDCOMRecordType.rtIndividual;

                    if (AppHost.Instance.ShowModalX(dlg, false)) {
                        result = (dlg.ResultRecord as GEDCOMIndividualRecord);
                    } else {
                        result = null;
                    }
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("BaseContext.SelectPerson(): " + ex.Message);
                result = null;
            }

            return result;
        }

        public GEDCOMRecord SelectRecord(GEDCOMRecordType mode, params object[] args)
        {
            GEDCOMRecord result;

            try
            {
                using (var dlg = AppHost.Container.Resolve<IRecordSelectDialog>())
                {
                    dlg.InitDialog(fViewer);

                    dlg.RecType = mode;

                    if (args != null && args.Length > 0) {
                        dlg.FastFilter = (args[0] as string);
                    }

                    if (AppHost.Instance.ShowModalX(dlg, false)) {
                        result = dlg.ResultRecord;
                    } else {
                        result = null;
                    }
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("BaseContext.SelectRecord(): " + ex.Message);
                result = null;
            }

            return result;
        }

        #endregion

        #region Data modification functions

        private GEDCOMFamilyRecord GetFamilyBySpouse(GEDCOMIndividualRecord newParent)
        {
            GEDCOMFamilyRecord result = null;

            int num = fTree.RecordsCount;
            for (int i = 0; i < num; i++)
            {
                GEDCOMRecord rec = fTree[i];

                if (rec.RecordType == GEDCOMRecordType.rtFamily)
                {
                    GEDCOMFamilyRecord fam = (GEDCOMFamilyRecord) rec;
                    GEDCOMIndividualRecord husb = fam.GetHusband();
                    GEDCOMIndividualRecord wife = fam.GetWife();
                    if (husb == newParent || wife == newParent)
                    {
                        string msg = string.Format(LangMan.LS(LSID.LSID_ParentsQuery), GKUtils.GetFamilyString(fam));
                        if (AppHost.StdDialogs.ShowQuestionYN(msg))
                        {
                            result = fam;
                            break;
                        }
                    }
                }
            }

            return result;
        }

        public GEDCOMFamilyRecord GetChildFamily(GEDCOMIndividualRecord iChild,
                                                 bool canCreate,
                                                 GEDCOMIndividualRecord newParent)
        {
            GEDCOMFamilyRecord result = null;

            if (iChild != null)
            {
                if (iChild.ChildToFamilyLinks.Count != 0)
                {
                    result = iChild.ChildToFamilyLinks[0].Family;
                }
                else
                {
                    if (canCreate)
                    {
                        GEDCOMFamilyRecord fam = GetFamilyBySpouse(newParent);
                        if (fam == null)
                        {
                            fam = fTree.CreateFamily();
                        }
                        fam.AddChild(iChild);
                        result = fam;
                    }
                }
            }

            return result;
        }

        public GEDCOMFamilyRecord AddFamilyForSpouse(GEDCOMIndividualRecord spouse)
        {
            if (spouse == null)
                throw new ArgumentNullException(@"spouse");

            GEDCOMSex sex = spouse.Sex;
            if (sex < GEDCOMSex.svMale || sex >= GEDCOMSex.svUndetermined)
            {
                AppHost.StdDialogs.ShowError(LangMan.LS(LSID.LSID_IsNotDefinedSex));
                return null;
            }

            GEDCOMFamilyRecord family = fTree.CreateFamily();
            family.AddSpouse(spouse);
            return family;
        }

        public GEDCOMIndividualRecord AddChildForParent(GEDCOMIndividualRecord parent, GEDCOMSex needSex)
        {
            GEDCOMIndividualRecord resultChild = null;

            if (parent != null)
            {
                if (parent.SpouseToFamilyLinks.Count > 1)
                {
                    AppHost.StdDialogs.ShowError(LangMan.LS(LSID.LSID_ThisPersonHasSeveralFamilies));
                }
                else
                {
                    GEDCOMFamilyRecord family;

                    if (parent.SpouseToFamilyLinks.Count == 0)
                    {
                        //GKUtils.ShowError(LangMan.LS(LSID.LSID_IsNotFamilies));

                        family = AddFamilyForSpouse(parent);
                        if (family == null) {
                            return null;
                        }
                    } else {
                        family = parent.SpouseToFamilyLinks[0].Family;
                    }

                    GEDCOMIndividualRecord child = SelectPerson(family.GetHusband(), TargetMode.tmParent, needSex);

                    if (child != null && family.AddChild(child))
                    {
                        // this repetition necessary, because the call of CreatePersonDialog only works if person already has a father,
                        // what to call AddChild () is no; all this is necessary in order to in the namebook were correct patronymics.
                        AppHost.NamesTable.ImportNames(child);

                        resultChild = child;
                    }
                }
            }

            return resultChild;
        }

        public GEDCOMIndividualRecord SelectSpouseFor(GEDCOMIndividualRecord iRec)
        {
            if (iRec == null)
                throw new ArgumentNullException(@"iRec");

            GEDCOMSex needSex;
            switch (iRec.Sex)
            {
                case GEDCOMSex.svMale:
                    needSex = GEDCOMSex.svFemale;
                    break;

                case GEDCOMSex.svFemale:
                    needSex = GEDCOMSex.svMale;
                    break;

                default:
                    AppHost.StdDialogs.ShowError(LangMan.LS(LSID.LSID_IsNotDefinedSex));
                    return null;
            }

            GEDCOMIndividualRecord target = null;
            TargetMode targetMode = TargetMode.tmNone;
            if (needSex == GEDCOMSex.svFemale) {
                target = iRec;
                targetMode = TargetMode.tmWife;
            }

            GEDCOMIndividualRecord result = SelectPerson(target, targetMode, needSex);
            return result;
        }

        #endregion
    }
}
