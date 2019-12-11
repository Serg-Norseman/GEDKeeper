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

using System;
using System.Collections.Generic;
using System.IO;
using BSLib;
using GDModel;
using GKCore;
using GKCore.Interfaces;
using Xapian;

namespace GKTextSearchPlugin
{
    /// <summary>
    /// 
    /// </summary>
    public class SearchManager
    {
        private readonly object fLock;
        private readonly Plugin fPlugin;

        public SearchManager(Plugin plugin)
        {
            fPlugin = plugin;
            fLock = new object();
        }

        #region Private methods

        private static string GetSign(IBaseWindow baseWin)
        {
            return Path.GetFileNameWithoutExtension(baseWin.Context.FileName);
        }

        private static bool IsIndexedRecord(GDMRecord rec)
        {
            return !((rec is GDMLocationRecord || rec is GDMGroupRecord));
        }

        private static void SetDBLastChange(IBaseWindow baseWin, WritableDatabase database)
        {
            string dbLastchange = baseWin.Context.Tree.Header.TransmissionDateTime.ToString("yyyy.MM.dd HH:mm:ss", null);
            database.SetMetadata(GetSign(baseWin), dbLastchange);
        }

        private string GetXDBFolder()
        {
            string xdbDir = fPlugin.Host.GetAppDataPath() + "xdb";
            if (!Directory.Exists(xdbDir)) Directory.CreateDirectory(xdbDir);
            return xdbDir;
        }

        private static uint FindDocId(IBaseWindow baseWin, WritableDatabase database, string xref)
        {
            uint result;

            string key = "Q" + GetSign(baseWin) + "_" + xref;

            using (PostingIterator p = database.PostListBegin(key)) {
                if (p == database.PostListEnd(key)) {
                    result = 0; // 0 - is invalid docid (see XapianManual)
                } else {
                    result = p.GetDocId();
                }
            }

            return result;
        }

        private static bool SetDocumentContext(IBaseWindow baseWin, Document doc, TermGenerator indexer, GDMRecord rec)
        {
            StringList ctx = baseWin.GetRecordContent(rec);
            if (ctx == null) return false;

            string recLastchange = rec.ChangeDate.ToString();
            string baseSign = GetSign(baseWin);

            doc.SetData(rec.XRef);							// not edit: for link from search results to gedcom-base
            doc.AddTerm("Q" + baseSign + "_" + rec.XRef);	// not edit: specific db_rec_id - for FindDocId()
            doc.AddValue(0, recLastchange);				    // not edit: for update check
            doc.AddBooleanTerm("GDB" + baseSign);			// not edit: for filtering by database in Search()

            indexer.SetDocument(doc);
            indexer.IndexText(ctx.Text);

            return true;
        }

        private static void ReindexRecord(IBaseWindow baseWin, WritableDatabase database, TermGenerator indexer, GDMRecord record)
        {
            uint docid = FindDocId(baseWin, database, record.XRef);

            if (docid != 0) {
                // checking for needed updates
                string recLastchange = record.ChangeDate.ToString();
                string docLastchange;

                using (Document curDoc = database.GetDocument(docid)) {
                    docLastchange = curDoc.GetValue(0);
                }

                // updating a record
                if (!string.Equals(recLastchange, docLastchange)) {
                    using (Document doc = new Document())
                    {
                        if (SetDocumentContext(baseWin, doc, indexer, record))
                            database.ReplaceDocument(docid, doc);
                    }
                }
            } else {
                // only adding
                using (Document doc = new Document())
                {
                    if (SetDocumentContext(baseWin, doc, indexer, record))
                        database.AddDocument(doc);
                }
            }
        }

        #endregion

        public void ReindexBase(IBaseWindow baseWin)
        {
            if (baseWin == null) return;

            try
            {
                lock (fLock)
                {
                    using (WritableDatabase database = new WritableDatabase(GetXDBFolder(), Xapian.Xapian.DB_CREATE_OR_OPEN))
                        using (TermGenerator indexer = new TermGenerator())
                            using (Stem stemmer = new Stem("russian"))
                    {
                        indexer.SetStemmer(stemmer);

                        IProgressController progress = AppHost.Progress;
                        progress.ProgressInit(fPlugin.LangMan.LS(TLS.LSID_SearchIndexRefreshing), baseWin.Context.Tree.RecordsCount);
                        int num = baseWin.Context.Tree.RecordsCount;
                        for (int i = 0; i < num; i++)
                        {
                            GDMRecord record = baseWin.Context.Tree[i];
                            if (IsIndexedRecord(record)) ReindexRecord(baseWin, database, indexer, record);

                            progress.ProgressStep();
                        }
                        progress.ProgressDone();

                        SetDBLastChange(baseWin, database);
                    }
                }
            }
            catch (Exception ex)
            {
                Logger.LogException(ex);
            }
        }

        public void UpdateRecord(IBaseWindow baseWin, GDMRecord record)
        {
            if (baseWin == null)
                throw new ArgumentNullException("baseWin");

            if (record == null || !IsIndexedRecord(record)) return;

            try
            {
                lock (fLock)
                {
                    using (WritableDatabase database = new WritableDatabase(GetXDBFolder(), Xapian.Xapian.DB_CREATE_OR_OPEN))
                        using (TermGenerator indexer = new TermGenerator())
                            using (Stem stemmer = new Stem("russian"))
                    {
                        indexer.SetStemmer(stemmer);

                        ReindexRecord(baseWin, database, indexer, record);
                        SetDBLastChange(baseWin, database);
                    }
                }
            }
            catch (Exception ex)
            {
                Logger.LogException(ex);
            }
        }

        public void DeleteRecord(IBaseWindow baseWin, string xref)
        {
            if (baseWin == null)
                throw new ArgumentNullException("baseWin");

            try
            {
                lock (fLock)
                {
                    using (WritableDatabase database = new WritableDatabase(GetXDBFolder(), Xapian.Xapian.DB_CREATE_OR_OPEN))
                    {
                        uint docid = FindDocId(baseWin, database, xref);
                        if (docid != 0) {
                            database.DeleteDocument(docid);
                            SetDBLastChange(baseWin, database);
                        }
                    }
                }
            }
            catch (Exception ex)
            {
                Logger.LogException(ex);
            }
        }

        public class SearchEntry
        {
            public string XRef;
            public long Rank;
            public int Percent;
        }

        public List<SearchEntry> Search(IBaseWindow baseWin, string searchText)
        {
            if (baseWin == null)
                throw new ArgumentNullException("baseWin");

            const uint flags = (uint)(QueryParser.feature_flag.FLAG_PARTIAL | QueryParser.feature_flag.FLAG_WILDCARD |
                                      QueryParser.feature_flag.FLAG_PHRASE | QueryParser.feature_flag.FLAG_BOOLEAN |
                                      QueryParser.feature_flag.FLAG_LOVEHATE);
            
            List<SearchEntry> res = new List<SearchEntry>();

            try
            {
                lock (fLock)
                {
                    using (Database database = new Database(GetXDBFolder()))
                        using (Enquire enquire = new Enquire(database))
                            using (Stem stemmer = new Stem("russian"))
                                using (QueryParser qp = new QueryParser())
                    {
                        qp.SetStemmer(stemmer);
                        qp.SetDatabase(database);
                        qp.SetDefaultOp(Query.op.OP_AND);
                        qp.SetStemmingStrategy(QueryParser.stem_strategy.STEM_SOME);

                        string qs = searchText + " ged:" + GetSign(baseWin);
                        qp.AddBooleanPrefix("ged", "GDB");

                        using (Query query = qp.ParseQuery(qs, flags))
                        {
                            enquire.SetQuery(query);

                            using (MSet matches = enquire.GetMSet(0, 100))
                            {
                                MSetIterator m = matches.Begin();
                                while (m != matches.End())
                                {
                                    try
                                    {
                                        using (Document mDoc = m.GetDocument()) {
                                            SearchEntry entry = new SearchEntry();
                                            entry.XRef = mDoc.GetData();
                                            entry.Rank = m.GetRank()+1;
                                            entry.Percent = m.GetPercent();
                                            res.Add(entry);
                                        }
                                    }
                                    catch (Exception ex)
                                    {
                                        Logger.LogException(ex);
                                    }

                                    m = m.Next();
                                }
                            }
                        }
                    }
                }
            }
            catch (Exception ex)
            {
                Logger.LogException(ex);
            }

            return res;
        }
    }
}
