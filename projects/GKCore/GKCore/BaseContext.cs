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
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Drawing.Imaging;
using System.IO;
using System.Security.Cryptography;
using System.Text;
using System.Text.RegularExpressions;

using Externals;
using GKCommon;
using GKCommon.GEDCOM;
using GKCore.Cultures;
using GKCore.Interfaces;
using GKCore.Operations;
using GKCore.Options;
using GKCore.Tools;
using GKCore.Types;

namespace GKCore
{
    /// <summary>
    /// 
    /// </summary>
    public class BaseContext : BaseObject, IBaseContext
    {
        #region Private fields

        private string fFileName;
        private readonly IHost fHost;
        private readonly GEDCOMTree fTree;
        private readonly ValuesCollection fValuesCollection;
        private readonly IBaseWindow fViewer;
        private readonly ChangeTracker fUndoman;
        private readonly List<GEDCOMRecord> fLockedRecords;

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

        #endregion

        #region Instance control

        public BaseContext(IBaseWindow viewer)
        {
            fFileName = "";
            fTree = new GEDCOMTree();
            fViewer = viewer;
            fHost = (viewer == null) ? null : viewer.Host;
            fUndoman = new ChangeTracker(fTree);
            fValuesCollection = new ValuesCollection();
            fLockedRecords = new List<GEDCOMRecord>();
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                fUndoman.Dispose();

                fTree.Dispose();
                //fTree = null;
            }
            base.Dispose(disposing);
        }

        #endregion

        #region Data Manipulation

        public void CollectEventValues(GEDCOMCustomEvent evt)
        {
            GKUtils.CollectEventValues(evt, fValuesCollection);
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
            GKUtils.SetRusNameParts(pName, iSurname, iName, iPatronymic);

            if (birthEvent) CreateEventEx(iRec, "BIRT", "", "");

            return iRec;
        }

        public bool DeleteRecord(GEDCOMRecord record)
        {
            bool result = false;
            if (record == null) return result;

            try {
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
                    //yield return new SearchResult(iRec);
                    result.Add(new SearchResult(rec));
                }
            }

            return result;
        }

        #endregion

        #region Individual utils

        public bool IsChildless(GEDCOMIndividualRecord iRec)
        {
            int exp = GKUtils.GetLifeExpectancy(iRec);
            return (exp != -1 && exp < 15);
        }

        public int GetRelativeYear(GEDCOMRecordWithEvents evsRec, string evSign)
        {
            return GEDCOMUtils.GetRelativeYear(evsRec, evSign);
        }

        public int FindBirthYear(GEDCOMIndividualRecord iRec)
        {
            if (iRec != null) {
                int birthDate = GEDCOMUtils.GetRelativeYear(iRec, "BIRT");
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
                int deathDate = GEDCOMUtils.GetRelativeYear(iRec, "DEAT");
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
                        if (chbDate != 0) {
                            if (maxBirth < chbDate) maxBirth = chbDate;
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

                        int days;
                        if (GKUtils.GetDaysForBirth(iRec, out days))
                        {
                            string nm = GKUtils.GetNameString(iRec, true, false);
                            nm = Culture.GetPossessiveName(nm);

                            if (days >= 0 && 3 > days) {
                                string tip;

                                if (firstTip) {
                                    tipsList.Add("#" + LangMan.LS(LSID.LSID_BirthDays));
                                    firstTip = false;
                                }

                                if (0 == days)
                                {
                                    tip = string.Format(
                                        LangMan.LS(LSID.LSID_BirthdayToday), nm);
                                }
                                else if (1 == days)
                                {
                                    tip = string.Format(
                                        LangMan.LS(LSID.LSID_BirthdayTomorrow), nm);
                                }
                                else
                                {
                                    tip = string.Format(
                                        LangMan.LS(LSID.LSID_DaysRemained),
                                        nm, days);
                                }

                                tipsList.Add(tip);
                            }
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

        private void ArcFileLoad(string targetFn, Stream toStream)
        {
            targetFn = SysUtils.NormalizeFilename(targetFn);

            using (ZipStorer zip = ZipStorer.Open(GetArcFileName(), FileAccess.Read))
            {
                ZipStorer.ZipFileEntry? entry = zip.FindFile(targetFn);
                if (entry != null) {
                    zip.ExtractStream(entry.Value, toStream);
                }
            }
        }

        private void ArcFileSave(string fileName, string sfn)
        {
            string arcFn = GetArcFileName();
            ZipStorer zip = null;

            try
            {
                if (File.Exists(arcFn)) {
                    zip = ZipStorer.Open(arcFn, FileAccess.ReadWrite);
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
                AppHub.StdDialogs.ShowError(LangMan.LS(LSID.LSID_NewDBFileNeedToSave));
            }
            return result;
        }

        public MediaStoreType GetStoreType(GEDCOMFileReference fileReference, ref string fileName)
        {
            return GKUtils.GetStoreType(fileReference, ref fileName);
        }

        public void MediaLoad(GEDCOMFileReference fileReference, out Stream stream, bool throwException)
        {
            stream = null;
            if (fileReference == null) return;

            string targetFn = "";
            MediaStoreType gst = GetStoreType(fileReference, ref targetFn);

            switch (gst) {
                case MediaStoreType.mstStorage:
                    targetFn = GetStgFolder(false) + targetFn;
                    if (!File.Exists(targetFn))
                    {
                        if (throwException) {
                            throw new MediaFileNotFoundException();
                        }

                        AppHub.StdDialogs.ShowError(LangMan.LS(LSID.LSID_ArcNotFound));
                    }
                    else {
                        stream = new FileStream(targetFn, FileMode.Open);
                    }
                    break;

                case MediaStoreType.mstArchive:
                    stream = new MemoryStream();
                    if (!File.Exists(GetArcFileName()))
                    {
                        if (throwException) {
                            throw new MediaFileNotFoundException();
                        }

                        AppHub.StdDialogs.ShowError(LangMan.LS(LSID.LSID_ArcNotFound));
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
        }

        public void MediaLoad(GEDCOMFileReference fileReference, ref string fileName)
        {
            if (fileReference == null) return;

            try
            {
                string targetFn = "";
                MediaStoreType gst = GetStoreType(fileReference, ref targetFn);

                switch (gst)
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
                                AppHub.StdDialogs.ShowError(LangMan.LS(LSID.LSID_ArcNotFound));
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
                                string newPath;
                                if (AppHub.PathReplacer.TryReplacePath(fileName, out newPath)) {
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
        }

        public bool MediaSave(GEDCOMFileReference fileReference, string fileName, MediaStoreType storeType)
        {
            if (fileReference == null) return false;

            bool result = true;

            string storeFile = Path.GetFileName(fileName);
            string storePath = "";
            string refPath = "";

            switch (GKUtils.GetMultimediaKind(GEDCOMFileReference.RecognizeFormat(fileName)))
            {
                case MultimediaKind.mkNone:
                    storePath = "unknown";
                    break;

                case MultimediaKind.mkImage:
                    storePath = "images";
                    break;

                case MultimediaKind.mkAudio:
                    storePath = "audio";
                    break;

                case MultimediaKind.mkText:
                    storePath = "texts";
                    break;

                case MultimediaKind.mkVideo:
                    storePath = "video";
                    break;
            }

            storePath = storePath + Path.DirectorySeparatorChar;

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
                        CopyFile(fileName, targetFn, false);
                    }
                    catch (IOException)
                    {
                        AppHub.StdDialogs.ShowError(LangMan.LS(LSID.LSID_FileWithSameNameAlreadyExistsInStorage));
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

        private void CopyFile(string sourceFileName, string destFileName, bool overwrite)
        {
            #if FILECOPY_EX

            IProgressController progress = AppHub.Progress;
            try {
                progress.ProgressInit(LangMan.LS(LSID.LSID_CopyingFile), 100);

                var source = new FileInfo(sourceFileName);
                var target = new FileInfo(destFileName);
                GKUtils.CopyFile(source, target, progress);
            } finally {
                progress.ProgressDone();
            }

            #else

            File.Copy(sourceFileName, destFileName, overwrite);

            #endif
        }

        public Bitmap LoadMediaImage(GEDCOMFileReference fileReference, bool throwException)
        {
            if (fileReference == null) return null;

            Bitmap result = null;
            try
            {
                Stream stm;
                MediaLoad(fileReference, out stm, throwException);

                if (stm != null)
                {
                    if (stm.Length != 0) {
                        using (Bitmap bmp = new Bitmap(stm))
                        {
                            // cloning is necessary to release the resource
                            // loaded from the image stream
                            result = (Bitmap)bmp.Clone();
                        }
                    }
                    stm.Dispose();
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

        public Bitmap LoadMediaImage(GEDCOMFileReference fileReference, int thumbWidth, int thumbHeight, ExtRect cutoutArea, bool throwException)
        {
            if (fileReference == null) return null;

            Bitmap result = null;
            try
            {
                Stream stm;
                MediaLoad(fileReference, out stm, throwException);

                if (stm != null)
                {
                    if (stm.Length != 0) {
                        using (Bitmap bmp = new Bitmap(stm))
                        {
                            bool cutoutIsEmpty = cutoutArea.IsEmpty();
                            int imgWidth = (cutoutIsEmpty) ? bmp.Width : cutoutArea.GetWidth();
                            int imgHeight = (cutoutIsEmpty) ? bmp.Height : cutoutArea.GetHeight();

                            if (thumbWidth > 0 && thumbHeight > 0) {
                                float ratio = SysUtils.ZoomToFit(imgWidth, imgHeight, thumbWidth, thumbHeight);
                                imgWidth = (int)(imgWidth * ratio);
                                imgHeight = (int)(imgHeight * ratio);
                            }

                            Bitmap newImage = new Bitmap(imgWidth, imgHeight, PixelFormat.Format24bppRgb);
                            using (Graphics graphic = Graphics.FromImage(newImage)) {
                                graphic.InterpolationMode = InterpolationMode.HighQualityBicubic;
                                graphic.SmoothingMode = SmoothingMode.HighQuality;
                                graphic.PixelOffsetMode = PixelOffsetMode.HighQuality;
                                graphic.CompositingQuality = CompositingQuality.HighQuality;

                                if (cutoutIsEmpty) {
                                    graphic.DrawImage(bmp, 0, 0, imgWidth, imgHeight);
                                } else {
                                    Rectangle destRect = new Rectangle(0, 0, imgWidth, imgHeight);
                                    Rectangle srcRect = cutoutArea.ToRectangle();
                                    graphic.DrawImage(bmp, destRect, srcRect, GraphicsUnit.Pixel);
                                }
                            }

                            result = newImage;
                        }
                    }
                    stm.Dispose();
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

        public Bitmap GetPrimaryBitmap(GEDCOMIndividualRecord iRec, int thumbWidth, int thumbHeight, bool throwException)
        {
            if (iRec == null) return null;

            Bitmap result = null;
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
                result = GEDCOMUtils.GetMultimediaLinkUID(mmLink);
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

        public void SetFileName(string fileName)
        {
            fFileName = fileName;
        }

        private void LoadProgress(object sender, int progress)
        {
            AppHub.Progress.ProgressStep(progress);
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
                if (ext == ".geds") {
                    if (!AppHub.StdDialogs.GetPassword(LangMan.LS(LSID.LSID_Password), ref pw)) {
                        AppHub.StdDialogs.ShowError(LangMan.LS(LSID.LSID_PasswordIsNotSpecified));
                        return false;
                    }
                }

                IProgressController progress = AppHub.Progress;
                progress.ProgressInit(LangMan.LS(LSID.LSID_Loading), 100);
                fTree.OnProgress += LoadProgress;
                try
                {
                    FileLoad(fileName, pw);
                    TreeTools.CheckGEDCOMFormat(fTree, fValuesCollection, progress);
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
                AppHub.StdDialogs.ShowError(LangMan.LS(LSID.LSID_LoadGedComFailed));
            }

            return result;
        }

        private void FileLoad(string fileName, string password)
        {
            if (string.IsNullOrEmpty(password)) {
                fTree.LoadFromFile(fileName);
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
                if (ext == ".geds") {
                    if (!AppHub.StdDialogs.GetPassword(LangMan.LS(LSID.LSID_Password), ref pw)) {
                        AppHub.StdDialogs.ShowError(LangMan.LS(LSID.LSID_PasswordIsNotSpecified));
                        return false;
                    }
                }

                FileSave(fileName, pw);
                result = true;
            }
            catch (UnauthorizedAccessException)
            {
                AppHub.StdDialogs.ShowError(string.Format(LangMan.LS(LSID.LSID_FileSaveError), new object[] { fileName, ": access denied" }));
            }
            catch (Exception ex)
            {
                AppHub.StdDialogs.ShowError(string.Format(LangMan.LS(LSID.LSID_FileSaveError), new object[] { fileName, "" }));
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
                        string bakFile = Path.GetFileName(fileName) + "." + SysUtils.AdjustNum(rev, 3);

                        if (!Directory.Exists(bakPath)) Directory.CreateDirectory(bakPath);
                        File.Move(fileName, bakPath + bakFile);
                    }
                    break;
            }

            // check for archive and storage, move them if the file changes location
            MoveMediaContainers(oldFileName, fileName);

            if (string.IsNullOrEmpty(password)) {
                GKUtils.PrepareHeader(fTree, fileName, GlobalOptions.Instance.DefCharacterSet, false);
                fTree.SaveToFile(fileName, GlobalOptions.Instance.DefCharacterSet);
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
                fTree.SaveToFile(rfn, GlobalOptions.Instance.DefCharacterSet);
            } catch (Exception ex) {
                Logger.LogWrite("BaseContext.CriticalSave(): " + ex.Message);
            }
        }

        private const string GEDSEC_HEADER = "GEDSECAA";
        private const byte GS_MAJOR_VER = 1;
        private const byte GS_MINOR_VER = 1;

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
                    using (DESCryptoServiceProvider cryptic = new DESCryptoServiceProvider()) {
                        cryptic.Key = pdb.CryptDeriveKey("DES", "SHA1", cryptic.KeySize, cryptic.IV);

                        using (CryptoStream crStream = new CryptoStream(fileStream, cryptic.CreateDecryptor(), CryptoStreamMode.Read))
                        {
                            fTree.LoadFromStreamExt(fileStream, crStream, fileName);
                        }

                        SCCrypt.ClearBytes(pwd);
                        SCCrypt.ClearBytes(salt);
                    }
                } finally {
                    // The project is mainly compiled under .NET 3.0,
                    // where these objects are simple, but in .NET 4.x they disposable
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
                    using (DESCryptoServiceProvider cryptic = new DESCryptoServiceProvider()) {
                        cryptic.Key = pdb.CryptDeriveKey("DES", "SHA1", cryptic.KeySize, cryptic.IV);

                        using (CryptoStream crStream = new CryptoStream(fileStream, cryptic.CreateEncryptor(), CryptoStreamMode.Write))
                        {
                            GKUtils.PrepareHeader(fTree, fileName, charSet, false);
                            fTree.SaveToStreamExt(crStream, fileName, charSet);
                            crStream.Flush();
                        }

                        SCCrypt.ClearBytes(pwd);
                        SCCrypt.ClearBytes(salt);
                    }
                } finally {
                    // The project is mainly compiled under .NET 3.0,
                    // where these objects are simple, but in .NET 4.x they disposable
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

        #endregion

        #region Undo/Redo

        public void DoUndo()
        {
            fUndoman.Undo();

            if (fViewer != null) fViewer.RefreshLists(false);
            if (fHost != null) fHost.UpdateControls(false);
        }

        public void DoRedo()
        {
            fUndoman.Redo();

            if (fViewer != null) fViewer.RefreshLists(false);
            if (fHost != null) fHost.UpdateControls(false);
        }

        public void DoCommit()
        {
            fUndoman.Commit();
            //fViewer.RefreshLists(false);
            //fHost.UpdateControls(false);
        }

        public void DoRollback()
        {
            fUndoman.Rollback();
            //fViewer.RefreshLists(false);
            //fHost.UpdateControls(false);
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
                AppHub.StdDialogs.ShowWarning(LangMan.LS(LSID.LSID_RecordIsLocked));
            }

            return result;
        }

        #endregion
    }
}
