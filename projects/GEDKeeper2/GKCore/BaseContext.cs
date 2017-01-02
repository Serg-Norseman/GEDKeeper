/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
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
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Drawing.Imaging;
using System.IO;
using System.Security.Cryptography;
using System.Text;

using Externals;
using GKCommon;
using GKCommon.GEDCOM;
using GKCommon.SmartGraph;
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

        private readonly IHost fHost;
        private readonly GEDCOMTree fTree;
        private readonly ValuesCollection fValuesCollection;
        private readonly IBaseWindow fViewer;
        private readonly ChangeTracker fUndoman;

        #endregion

        #region Public properties

        public ICulture Culture
        {
            get {
                GEDCOMLanguageID langID = this.fTree.Header.Language.Value;
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

                    case GEDCOMLanguageID.English:
                    default:
                        culture = new BritishCulture();
                        break;
                }

                return culture;
            }
        }

        public GEDCOMTree Tree
        {
            get { return this.fTree; }
        }

        public ChangeTracker Undoman
        {
            get { return this.fUndoman; }
        }

        public ValuesCollection ValuesCollection
        {
            get { return this.fValuesCollection; }
        }

        #endregion

        #region Instance control

        public BaseContext(GEDCOMTree tree, IBaseWindow viewer)
        {
            this.fTree = tree;
            this.fViewer = viewer;
            this.fHost = (viewer == null) ? null : viewer.Host;
            this.fUndoman = new ChangeTracker(this.fTree);
            this.fValuesCollection = new ValuesCollection();
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                this.fUndoman.Dispose();
            }
            base.Dispose(disposing);
        }

        #endregion

        #region Data search

        public GEDCOMSourceRecord FindSource(string sourceName)
        {
            GEDCOMSourceRecord result = null;

            int num = this.fTree.RecordsCount;
            for (int i = 0; i < num; i++)
            {
                GEDCOMRecord rec = this.fTree[i];

                if (rec is GEDCOMSourceRecord && (rec as GEDCOMSourceRecord).FiledByEntry == sourceName)
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

            int num = this.fTree.RecordsCount;
            for (int i = 0; i < num; i++)
            {
                GEDCOMRecord rec = this.fTree[i];
                if (rec is GEDCOMSourceRecord)
                {
                    sources.AddObject((rec as GEDCOMSourceRecord).FiledByEntry, rec);
                }
            }
        }
        
        #endregion

        #region Data Manipulation

        public void CollectEventValues(GEDCOMCustomEvent evt)
        {
            GKUtils.CollectEventValues(evt, this.fValuesCollection);
        }

        public GEDCOMCustomEvent CreateEventEx(GEDCOMRecordWithEvents aRec, string evSign, string evDate, string evPlace)
        {
            if (aRec == null) return null;

            GEDCOMCustomEvent result;

            if (aRec is GEDCOMIndividualRecord) {
                if (GKUtils.GetPersonEventKindBySign(evSign) == PersonEventKind.ekEvent) {
                    result = new GEDCOMIndividualEvent(this.fTree, aRec, "", "");
                } else {
                    result = new GEDCOMIndividualAttribute(this.fTree, aRec, "", "");
                }
            } else if (aRec is GEDCOMFamilyRecord) {
                result = new GEDCOMFamilyEvent(this.fTree, aRec, "", "");
            } else {
                return null;
            }

            aRec.AddEvent(result);

            result.SetName(evSign);

            if (evDate != "") {
                result.Detail.Date.ParseString(evDate);
            }

            if (evPlace != "") {
                result.Detail.Place.StringValue = evPlace;
            }

            return result;
        }

        public GEDCOMIndividualRecord CreatePersonEx(string iName, string iPatronymic, string iSurname, GEDCOMSex iSex, bool birthEvent)
        {
            GEDCOMIndividualRecord iRec = this.fTree.CreateIndividual();
            iRec.Sex = iSex;

            GEDCOMPersonalName pName = iRec.AddPersonalName(new GEDCOMPersonalName(this.fTree, iRec, "", ""));
            GKUtils.SetRusNameParts(pName, iSurname, iName, iPatronymic);

            if (birthEvent) this.CreateEventEx(iRec, "BIRT", "", "");

            return iRec;
        }

        public bool DeleteRecord(GEDCOMRecord record)
        {
            bool result = false;

            if (record != null)
            {
                try {
                    this.BeginUpdate();

                    switch (record.RecordType)
                    {
                        case GEDCOMRecordType.rtIndividual:
                            result = this.fTree.DeleteIndividualRecord(record as GEDCOMIndividualRecord);
                            break;

                        case GEDCOMRecordType.rtFamily:
                            result = this.fTree.DeleteFamilyRecord(record as GEDCOMFamilyRecord);
                            break;

                        case GEDCOMRecordType.rtNote:
                            result = this.fTree.DeleteNoteRecord(record as GEDCOMNoteRecord);
                            break;

                        case GEDCOMRecordType.rtMultimedia:
                            result = this.fTree.DeleteMediaRecord(record as GEDCOMMultimediaRecord);
                            break;

                        case GEDCOMRecordType.rtSource:
                            result = this.fTree.DeleteSourceRecord(record as GEDCOMSourceRecord);
                            break;

                        case GEDCOMRecordType.rtRepository:
                            result = this.fTree.DeleteRepositoryRecord(record as GEDCOMRepositoryRecord);
                            break;

                        case GEDCOMRecordType.rtGroup:
                            result = this.fTree.DeleteGroupRecord(record as GEDCOMGroupRecord);
                            break;

                        case GEDCOMRecordType.rtResearch:
                            result = this.fTree.DeleteResearchRecord(record as GEDCOMResearchRecord);
                            break;

                        case GEDCOMRecordType.rtTask:
                            result = this.fTree.DeleteTaskRecord(record as GEDCOMTaskRecord);
                            break;

                        case GEDCOMRecordType.rtCommunication:
                            result = this.fTree.DeleteCommunicationRecord(record as GEDCOMCommunicationRecord);
                            break;

                        case GEDCOMRecordType.rtLocation:
                            result = this.fTree.DeleteLocationRecord(record as GEDCOMLocationRecord);
                            break;
                    }
                } finally {
                    this.EndUpdate();
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

                    int num2 = family.Childrens.Count;
                    for (int j = 0; j < num2; j++)
                    {
                        GEDCOMIndividualRecord child = family.Childrens[j].Value as GEDCOMIndividualRecord;
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

                    int num2 = family.Childrens.Count;
                    for (int j = 0; j < num2; j++)
                    {
                        GEDCOMIndividualRecord child = family.Childrens[j].Value as GEDCOMIndividualRecord;

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

        #endregion

        #region Patriarchs Search

        private static int PatriarchsCompare(object item1, object item2)
        {
            return ((PatriarchObj)item1).BirthYear - ((PatriarchObj)item2).BirthYear;
        }

        public ExtList<PatriarchObj> GetPatriarchsList(int gensMin, bool datesCheck)
        {
            ExtList<PatriarchObj> patList = new ExtList<PatriarchObj>(true);

            GEDCOMTree tree = this.fTree;
            IProgressController pctl = this.fViewer;
            
            pctl.ProgressInit(LangMan.LS(LSID.LSID_PatSearch), tree.RecordsCount);

            GKUtils.InitExtCounts(tree, -1);
            try
            {
                int num = tree.RecordsCount;
                for (int i = 0; i < num; i++)
                {
                    GEDCOMRecord rec = tree[i];

                    if (rec is GEDCOMIndividualRecord)
                    {
                        GEDCOMIndividualRecord iRec = rec as GEDCOMIndividualRecord;

                        string nf, nn, np;
                        GKUtils.GetNameParts(iRec, out nf, out nn, out np);

                        int birthDate = this.FindBirthYear(iRec);
                        int descGens = GKUtils.GetDescGenerations(iRec);

                        bool res = (iRec.ChildToFamilyLinks.Count == 0);
                        res = (res && iRec.Sex == GEDCOMSex.svMale);
                        res = (res && /*nf != "" && nf != "?" &&*/ nn != "" && nn != "?");
                        res = (res && descGens >= gensMin);

                        if (datesCheck)
                        {
                            res = (res && birthDate != 0);
                        }

                        if (res)
                        {
                            PatriarchObj pObj = new PatriarchObj();
                            pObj.IRec = iRec;
                            pObj.BirthYear = birthDate;
                            pObj.DescendantsCount = GKUtils.GetDescendantsCount(iRec) - 1;
                            pObj.DescGenerations = descGens;
                            patList.Add(pObj);
                        }
                    }

                    pctl.ProgressStep();
                }

                patList.QuickSort(PatriarchsCompare);
            }
            finally
            {
                pctl.ProgressDone();
            }
            
            return patList;
        }

        public ExtList<PatriarchObj> GetPatriarchsLinks(int gensMin, bool datesCheck, bool loneSuppress)
        {
            ExtList<PatriarchObj> patList = GetPatriarchsList(gensMin, datesCheck);

            IProgressController pctl = this.fViewer;

            pctl.ProgressInit(LangMan.LS(LSID.LSID_LinksSearch), patList.Count);
            try
            {
                int num2 = patList.Count;
                for (int i = 0; i < num2; i++)
                {
                    PatriarchObj patr = patList[i];

                    for (int j = i + 1; j < num2; j++)
                    {
                        PatriarchObj patr2 = patList[j];

                        GEDCOMIndividualRecord cross;
                        bool res = TreeTools.PL_SearchDesc(patr.IRec, patr2.IRec, out cross);

                        if (res)
                        {
                            patr.HasLinks = true;
                            patr2.HasLinks = true;

                            if (cross.Sex == GEDCOMSex.svFemale) {
                                patr.Links.Add(patr2);
                            } else {
                                patr2.Links.Add(patr);
                            }
                        }
                    }

                    pctl.ProgressStep();
                }
            }
            finally
            {
                pctl.ProgressDone();
            }

            if (loneSuppress)
            {
                for (int i = patList.Count - 1; i >= 0; i--)
                {
                    PatriarchObj patr = patList[i];
                    if (!patr.HasLinks) patList.Delete(i);
                }
                patList.Pack();
            }
            
            return patList;
        }

        private static void PL_WalkDescLinks(Graph graph, PGNode prevNode, GEDCOMIndividualRecord ancestor)
        {
            for (int i = 0, count = ancestor.SpouseToFamilyLinks.Count; i < count; i++)
            {
                GEDCOMFamilyRecord family = ancestor.SpouseToFamilyLinks[i].Family;
                PGNode node = family.ExtData as PGNode;

                if (node != null && node.Type != PGNodeType.Default)
                {
                    IVertex vtx = graph.FindVertex(node.FamilyXRef);
                    if (vtx == null)
                    {
                        vtx = graph.AddVertex(node.FamilyXRef, node);
                    }

                    if (prevNode != null)
                    {
                        graph.AddDirectedEdge(prevNode.FamilyXRef, node.FamilyXRef, 1, null);
                    }

                    prevNode = node;
                }

                for (int k = 0, count2 = family.Childrens.Count; k < count2; k++)
                {
                    GEDCOMIndividualRecord child = family.Childrens[k].Value as GEDCOMIndividualRecord;
                    PL_WalkDescLinks(graph, prevNode, child);
                }
            }
        }

        public Graph GetPatriarchsGraph(int gensMin, bool datesCheck, bool loneSuppress = true)
        {
            Graph graph = new Graph();

            try
            {
                using (ExtList<PatriarchObj> patList = this.GetPatriarchsList(gensMin, datesCheck))
                {
                    GEDCOMTree tree = this.fTree;
                    IProgressController pctl = this.fViewer;

                    // init
                    GKUtils.InitExtData(tree);

                    // prepare
                    int count = patList.Count;
                    for (int i = 0; i < count; i++)
                    {
                        PatriarchObj patNode = patList[i];
                        GEDCOMIndividualRecord iRec = patNode.IRec;

                        int count2 = iRec.SpouseToFamilyLinks.Count;
                        for (int k = 0; k < count2; k++)
                        {
                            GEDCOMFamilyRecord family = iRec.SpouseToFamilyLinks[k].Family;
                            family.ExtData = new PGNode(family.XRef, PGNodeType.Patriarch, patNode.DescGenerations);
                        }
                    }

                    try
                    {
                        int patCount = patList.Count;
                        pctl.ProgressInit(LangMan.LS(LSID.LSID_LinksSearch), patCount);

                        for (int i = 0; i < patCount; i++)
                        {
                            PatriarchObj patr = patList[i];

                            for (int j = i + 1; j < patCount; j++)
                            {
                                PatriarchObj patr2 = patList[j];

                                GEDCOMFamilyRecord cross = TreeTools.PL_SearchIntersection(patr.IRec, patr2.IRec);

                                if (cross != null)
                                {
                                    PGNode node = (PGNode)cross.ExtData;

                                    if (node != null && node.Type == PGNodeType.Patriarch) {
                                        // dummy
                                    } else {
                                        int size = GKUtils.GetDescGenerations(cross.GetHusband());
                                        if (size == 0) size = 1;
                                        cross.ExtData = new PGNode(cross.XRef, PGNodeType.Intersection, size);
                                    }
                                }
                            }

                            pctl.ProgressStep();
                        }
                    }
                    finally
                    {
                        pctl.ProgressDone();
                    }

                    // create graph
                    int count3 = patList.Count;
                    for (int i = 0; i < count3; i++)
                    {
                        PatriarchObj patNode = patList[i];
                        PL_WalkDescLinks(graph, null, patNode.IRec);
                    }

                    // clear
                    GKUtils.InitExtData(tree);

                    /*if (gpl_params.aLoneSuppress) {
				for (int i = aList.Count - 1; i >= 0; i--) {
					PatriarchObj patr = aList[i] as PatriarchObj;
					if (patr.ILinks.Count == 0) aList.Delete(i);
				}
				aList.Pack();*/
                }
            }
            catch (Exception ex)
            {
                this.fHost.LogWrite("BaseContext.GetPatriarchsGraph(): " + ex.Message);
            }

            return graph;
        }
        
        #endregion

        #region Private media support

        private static string GetTreePath(string treeName)
        {
            return Path.GetDirectoryName(treeName) + Path.DirectorySeparatorChar;
        }

        private string GetArcFileName()
        {
            string treeName = this.fTree.FileName;
            string result = GetTreePath(treeName) + Path.GetFileNameWithoutExtension(treeName) + ".zip";
            return result;
        }

        private string GetStgFolder(bool create)
        {
            string treeName = this.fTree.FileName;
            string result = GetTreePath(treeName) + Path.GetFileNameWithoutExtension(treeName) + Path.DirectorySeparatorChar;
            if (!Directory.Exists(result) && create) Directory.CreateDirectory(result);
            return result;
        }

        private void ArcFileLoad(string targetFn, Stream toStream)
        {
            targetFn = targetFn.Replace('\\', '/');

            using (ZipStorer zip = ZipStorer.Open(this.GetArcFileName(), FileAccess.Read))
            {
                ZipStorer.ZipFileEntry? entry = zip.FindFile(targetFn);
                if (entry != null) {
                    zip.ExtractFile(entry.Value, toStream);
                }
            }
        }

        private void ArcFileSave(string fileName, string sfn)
        {
            string arcFn = this.GetArcFileName();
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

            bool hasArc = File.Exists(this.GetArcFileName());
            bool hasStg = Directory.Exists(this.GetStgFolder(false));

            string newPath = Path.GetDirectoryName(newFileName);
            string newName = Path.GetFileName(newFileName);

            // move the archive and the storage folder to a new location
            if (hasArc) {
                string newArc = newPath + Path.DirectorySeparatorChar + GKUtils.GetContainerName(newName, true);
                File.Move(this.GetArcFileName(), newArc);
            }

            if (hasStg) {
                string newStg = newPath + Path.DirectorySeparatorChar + GKUtils.GetContainerName(newName, false);
                Directory.Move(this.GetStgFolder(false), newStg);
            }
        }

        #endregion

        #region Public media support

        public bool CheckBasePath()
        {
            string path = Path.GetDirectoryName(this.fTree.FileName);

            bool result = (!string.IsNullOrEmpty(path));
            if (!result)
            {
                GKUtils.ShowError(LangMan.LS(LSID.LSID_NewDBFileNeedToSave));
            }
            return result;
        }

        public MediaStoreType GetStoreType(GEDCOMFileReference fileReference, ref string fileName)
        {
            if (fileReference == null) {
                throw new ArgumentNullException("fileReference");
            }

            string fileRef = fileReference.StringValue;

            fileName = fileRef;
            MediaStoreType result;

            if (fileRef.IndexOf(GKData.GKStoreTypes[2].Sign) == 0)
            {
                result = MediaStoreType.mstArchive;
                fileName = fileName.Remove(0, 4);
            }
            else
            {
                if (fileRef.IndexOf(GKData.GKStoreTypes[1].Sign) == 0)
                {
                    result = MediaStoreType.mstStorage;
                    fileName = fileName.Remove(0, 4);
                }
                else
                {
                    result = MediaStoreType.mstReference;
                }
            }

            return result;
        }

        public void MediaLoad(GEDCOMFileReference fileReference, out Stream stream, bool throwException)
        {
            stream = null;
            if (fileReference == null) return;

            string targetFn = "";
            MediaStoreType gst = this.GetStoreType(fileReference, ref targetFn);

            switch (gst) {
                case MediaStoreType.mstStorage:
                    targetFn = this.GetStgFolder(false) + targetFn;
                    if (!File.Exists(targetFn))
                    {
                        if (throwException) {
                            throw new MediaFileNotFoundException();
                        }

                        GKUtils.ShowError(LangMan.LS(LSID.LSID_ArcNotFound));
                    }
                    else {
                        stream = new FileStream(targetFn, FileMode.Open);
                    }
                    break;

                case MediaStoreType.mstArchive:
                    stream = new MemoryStream();
                    if (!File.Exists(this.GetArcFileName()))
                    {
                        if (throwException) {
                            throw new MediaFileNotFoundException();
                        }

                        GKUtils.ShowError(LangMan.LS(LSID.LSID_ArcNotFound));
                    }
                    else {
                        this.ArcFileLoad(targetFn, stream);
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
                MediaStoreType gst = this.GetStoreType(fileReference, ref targetFn);

                switch (gst)
                {
                    case MediaStoreType.mstStorage:
                        fileName = this.GetStgFolder(false) + targetFn;
                        break;

                    case MediaStoreType.mstArchive:
                        fileName = GKUtils.GetTempDir() + Path.GetFileName(targetFn);
                        FileStream fs = new FileStream(fileName, FileMode.Create);
                        try
                        {
                            if (!File.Exists(this.GetArcFileName())) {
                                GKUtils.ShowError(LangMan.LS(LSID.LSID_ArcNotFound));
                            } else {
                                targetFn = targetFn.Replace('\\', '/');
                                this.ArcFileLoad(targetFn, fs);
                            }
                        }
                        finally
                        {
                            fs.Close();
                            fs.Dispose();
                        }
                        break;

                    case MediaStoreType.mstReference:
                        fileName = targetFn;
                        break;
                }
            }
            catch (Exception ex)
            {
                this.fHost.LogWrite("BaseContext.MediaLoad_fn(): " + ex.Message);
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

            switch (GEDCOMFileReference.RecognizeFormat(fileName))
            {
                case GEDCOMMultimediaFormat.mfNone:
                case GEDCOMMultimediaFormat.mfOLE:
                case GEDCOMMultimediaFormat.mfUnknown:
                    storePath = "unknown";
                    break;

                case GEDCOMMultimediaFormat.mfBMP:
                case GEDCOMMultimediaFormat.mfGIF:
                case GEDCOMMultimediaFormat.mfJPG:
                case GEDCOMMultimediaFormat.mfPCX:
                case GEDCOMMultimediaFormat.mfTIF:
                case GEDCOMMultimediaFormat.mfTGA:
                case GEDCOMMultimediaFormat.mfPNG:
                    storePath = "images";
                    break;

                case GEDCOMMultimediaFormat.mfWAV:
                    storePath = "audio";
                    break;

                case GEDCOMMultimediaFormat.mfTXT:
                case GEDCOMMultimediaFormat.mfRTF:
                case GEDCOMMultimediaFormat.mfHTM:
                    storePath = "texts";
                    break;

                case GEDCOMMultimediaFormat.mfAVI:
                case GEDCOMMultimediaFormat.mfMPG:
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
                    this.ArcFileSave(fileName, storePath + storeFile);
                    break;

                case MediaStoreType.mstStorage:
                    refPath = GKData.GKStoreTypes[(int)storeType].Sign + storePath + storeFile;
                    try
                    {
                        string targetDir = this.GetStgFolder(true) + storePath;
                        if (!Directory.Exists(targetDir)) Directory.CreateDirectory(targetDir);

                        string targetFn = targetDir + storeFile;
                        File.Copy(fileName, targetFn, false);
                    }
                    catch (IOException)
                    {
                        GKUtils.ShowError(LangMan.LS(LSID.LSID_FileWithSameNameAlreadyExistsInStorage));
                        result = false;
                    }
                    break;
            }

            if (result) {
                fileReference.LinkFile(refPath);
            }

            return result;
        }

        public Bitmap LoadMediaImage(GEDCOMFileReference fileReference, bool throwException)
        {
            if (fileReference == null) return null;

            Bitmap result = null;
            try
            {
                Stream stm;

                this.MediaLoad(fileReference, out stm, throwException);

                if (stm != null)
                {
                    if (stm.Length != 0) {
                        using (Bitmap bmp = new Bitmap(stm))
                        {
                            // cloning is necessary to release the resource loaded from the image stream
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
                this.fHost.LogWrite("BaseContext.LoadMediaImage(): " + ex.Message);
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

                this.MediaLoad(fileReference, out stm, throwException);

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
                this.fHost.LogWrite("BaseContext.LoadMediaImage(): " + ex.Message);
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
                    result = this.LoadMediaImage(mmRec.FileReferences[0], thumbWidth, thumbHeight, cutoutArea, throwException);
                }
            }
            catch (MediaFileNotFoundException)
            {
                throw;
            }
            catch (Exception ex)
            {
                this.fHost.LogWrite("BaseContext.GetPrimaryBitmap(): " + ex.Message);
                result = null;
            }
            return result;
        }

        #endregion

        #region Files

        public void Clear()
        {
            this.fTree.Clear();
            this.fUndoman.Clear();
        }

        public void FileLoad(string fileName, string password = null)
        {
            if (string.IsNullOrEmpty(password)) {
                this.fTree.LoadFromFile(fileName);
            } else {
                this.LoadFromSecFile(fileName, password);
            }
        }

        public void FileSave(string fileName, string password = null)
        {
            string oldFileName = this.Tree.FileName;

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
                        int rev = this.Tree.Header.FileRevision;
                        string bakPath = Path.GetDirectoryName(fileName) + Path.DirectorySeparatorChar + "__history" + Path.DirectorySeparatorChar;
                        string bakFile = Path.GetFileName(fileName) + "." + SysUtils.AdjustNum(rev, 3);

                        if (!Directory.Exists(bakPath)) Directory.CreateDirectory(bakPath);
                        File.Move(fileName, bakPath + bakFile);
                    }
                    break;
            }

            // check for archive and storage, move them if the file changes location
            this.MoveMediaContainers(oldFileName, fileName);

            if (string.IsNullOrEmpty(password)) {
                GKUtils.PrepareHeader(this.fTree, fileName, GlobalOptions.Instance.DefCharacterSet, false);
                this.fTree.SaveToFile(fileName, GlobalOptions.Instance.DefCharacterSet);
            } else {
                this.SaveToSecFile(fileName, GlobalOptions.Instance.DefCharacterSet, password);
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

                DESCryptoServiceProvider cryptic = new DESCryptoServiceProvider();

                byte[] pwd = Encoding.Unicode.GetBytes(password);
                byte[] salt = SCCrypt.CreateRandomSalt(7);

                PasswordDeriveBytes pdb = new PasswordDeriveBytes(pwd, salt);
                cryptic.Key = pdb.CryptDeriveKey("DES", "SHA1", cryptic.KeySize, cryptic.IV);

                using (CryptoStream crStream = new CryptoStream(fileStream, cryptic.CreateDecryptor(), CryptoStreamMode.Read))
                {
                    this.fTree.LoadFromStreamExt(fileStream, crStream, fileName);
                }

                SCCrypt.ClearBytes(pwd);
                SCCrypt.ClearBytes(salt);
                cryptic.Clear();
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

                DESCryptoServiceProvider cryptic = new DESCryptoServiceProvider();

                byte[] pwd = Encoding.Unicode.GetBytes(password);
                byte[] salt = SCCrypt.CreateRandomSalt(7);

                PasswordDeriveBytes pdb = new PasswordDeriveBytes(pwd, salt);
                cryptic.Key = pdb.CryptDeriveKey("DES", "SHA1", cryptic.KeySize, cryptic.IV);

                using (CryptoStream crStream = new CryptoStream(fileStream, cryptic.CreateEncryptor(), CryptoStreamMode.Write))
                {
                    GKUtils.PrepareHeader(this.fTree, fileName, charSet, false);
                    this.fTree.SaveToStreamExt(crStream, fileName, charSet);
                    crStream.Flush();
                }

                SCCrypt.ClearBytes(pwd);
                SCCrypt.ClearBytes(salt);
                cryptic.Clear();
            }
        }

        #endregion

        #region Updating

        public bool IsUpdated()
        {
            return this.fTree.IsUpdated();
        }

        public void BeginUpdate()
        {
            this.fTree.BeginUpdate();
        }

        public void EndUpdate()
        {
            this.fTree.EndUpdate();
        }

        #endregion

        #region Undo/Redo

        public void DoUndo()
        {
            this.fUndoman.Undo();
            this.fViewer.RefreshLists(false);
            this.fHost.UpdateControls(false);
        }

        public void DoRedo()
        {
            this.fUndoman.Redo();
            this.fViewer.RefreshLists(false);
            this.fHost.UpdateControls(false);
        }

        public void DoCommit()
        {
            this.fUndoman.Commit();
            //this.fViewer.RefreshLists(false);
            //this.fHost.UpdateControls(false);
        }

        public void DoRollback()
        {
            this.fUndoman.Rollback();
            //this.fViewer.RefreshLists(false);
            //this.fHost.UpdateControls(false);
        }

        #endregion
    }
}
