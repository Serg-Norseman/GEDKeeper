/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2024 by Sergey V. Zhdanovskih.
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
using System.Linq;
using BSLib;
using GDModel;
using GKCore;
using GKCore.Interfaces;
using SIODirectory = System.IO.Directory;

namespace GKTextSearchPlugin
{
#if !LUC48
    using Lucene.Net.Analysis.Standard;
    using Lucene.Net.Documents;
    using Lucene.Net.Index;
    using Lucene.Net.QueryParsers;
    using Lucene.Net.Search;
    using Lucene.Net.Store;
#else
    using Lucene.Net.Analysis.Standard;
    using Lucene.Net.Documents;
    using Lucene.Net.Index;
    using Lucene.Net.QueryParsers.Classic;
    using Lucene.Net.Search;
    using Lucene.Net.Store;
    using Lucene.Net.Util;
#endif

    /// <summary>
    /// 
    /// </summary>
    public class SearchManager
    {
        private readonly object fLock;
        private readonly Plugin fPlugin;
        private readonly StandardAnalyzer fAnalyzer;
        private readonly IndexWriter fWriter;
        private readonly IndexSearcher fSearcher;

        public SearchManager(Plugin plugin)
        {
            fPlugin = plugin;
            fLock = new object();

            var dir = FSDirectory.Open(GetIndexFolder());

#if !LUC48
            fAnalyzer = new StandardAnalyzer(Lucene.Net.Util.Version.LUCENE_30);
            fWriter = new IndexWriter(dir, fAnalyzer, IndexWriter.MaxFieldLength.UNLIMITED);
            fSearcher = new IndexSearcher(fWriter.GetReader());
#else
            fAnalyzer = new StandardAnalyzer(LuceneVersion.LUCENE_48);
            var indexConfig = new IndexWriterConfig(LuceneVersion.LUCENE_48, fAnalyzer);
            fWriter = new IndexWriter(dir, indexConfig);
            fSearcher = new IndexSearcher(fWriter.GetReader(true));
#endif
        }

        #region Private methods

        private static string GetBaseID(IBaseWindow baseWin)
        {
            return baseWin.Context.Tree.Header.File.UID;
        }

        private static bool IsIndexedRecord(GDMRecord rec)
        {
            var recType = rec.RecordType;
            return (recType != GDMRecordType.rtLocation && recType != GDMRecordType.rtGroup);
        }

        private void UpdateIndex()
        {
            fWriter.Commit();

#if !LUC48
            fWriter.Flush(false, false, false);
#else
            fWriter.Flush(triggerMerge: false, applyAllDeletes: false);
#endif
        }

        private string GetIndexFolder()
        {
            string xdbDir = fPlugin.Host.GetAppDataPath() + "ldb";
            if (!SIODirectory.Exists(xdbDir)) SIODirectory.CreateDirectory(xdbDir);
            return xdbDir;
        }

        private Document FindDocument(IBaseWindow baseWin, string uid)
        {
            //var searcher = new IndexSearcher(fWriter.GetReader());
            var query = new TermQuery(new Term(FIELD_UID, uid));
            var score = fSearcher.Search(query, 1).ScoreDocs.FirstOrDefault();
            return (score == null) ? null : fSearcher.Doc(score.Doc);
        }

        private const string FIELD_XREF = "xref";
        private const string FIELD_UID = "uid";
        private const string FIELD_TS = "ts";
        private const string FIELD_DB = "db";
        private const string FIELD_TEXT = "text";

        private Document SetDocumentContext(IBaseWindow baseWin, GDMRecord rec)
        {
            StringList ctx = baseWin.GetRecordContent(rec, RecordContentType.Quick);
            if (ctx == null) return null;

            string recLastchange = rec.ChangeDate.ToString();
            string baseID = GetBaseID(baseWin);

            var lnDoc = new Document();
#if !LUC48
            lnDoc.Add(new Field(FIELD_XREF, rec.XRef, Field.Store.YES, Field.Index.NOT_ANALYZED_NO_NORMS));
            lnDoc.Add(new Field(FIELD_UID, rec.UID, Field.Store.YES, Field.Index.NOT_ANALYZED_NO_NORMS));
            lnDoc.Add(new Field(FIELD_TS, recLastchange, Field.Store.YES, Field.Index.NOT_ANALYZED_NO_NORMS));
            lnDoc.Add(new Field(FIELD_DB, baseID, Field.Store.YES, Field.Index.NOT_ANALYZED_NO_NORMS));
            lnDoc.Add(new Field(FIELD_TEXT, ctx.Text, Field.Store.YES, Field.Index.ANALYZED));
#else
            lnDoc.Add(new StringField(FIELD_XREF, rec.XRef, Field.Store.YES));
            lnDoc.Add(new StringField(FIELD_UID, rec.UID, Field.Store.YES));
            lnDoc.Add(new StringField(FIELD_TS, recLastchange, Field.Store.YES));
            lnDoc.Add(new StringField(FIELD_DB, baseID, Field.Store.YES));
            lnDoc.Add(new TextField(FIELD_TEXT, ctx.Text, Field.Store.YES));
#endif
            return lnDoc;
        }

        private void ReindexRecord(IBaseWindow baseWin, GDMRecord record)
        {
            Document doc = FindDocument(baseWin, record.UID);

            if (doc != null) {
                // checking for needed updates
                string recLastchange = record.ChangeDate.ToString();
                string docLastchange = doc.Get(FIELD_TS);

                // updating a record
                if (!string.Equals(recLastchange, docLastchange)) {
                    if ((doc = SetDocumentContext(baseWin, record)) != null)
                        fWriter.UpdateDocument(new Term(FIELD_UID, record.UID), doc);
                }
            } else {
                // only adding
                if ((doc = SetDocumentContext(baseWin, record)) != null)
                    fWriter.AddDocument(doc);
            }
        }

        #endregion

        public void ReindexBase(IBaseWindow baseWin)
        {
            if (baseWin == null) return;

            try {
                lock (fLock) {
                    AppHost.Instance.ExecuteWork((progress) => {
                        progress.Begin(fPlugin.LangMan.LS(PLS.SearchIndexRefreshing), baseWin.Context.Tree.RecordsCount);

                        int num = baseWin.Context.Tree.RecordsCount;
                        for (int i = 0; i < num; i++) {
                            GDMRecord record = baseWin.Context.Tree[i];
                            if (IsIndexedRecord(record))
                                ReindexRecord(baseWin, record);

                            progress.Increment();
                        }

                        progress.End();
                    });

                    UpdateIndex();
                }
            } catch (Exception ex) {
                Logger.WriteError("SearchManager.ReindexBase()", ex);
            }
        }

        public void UpdateRecord(IBaseWindow baseWin, GDMRecord record)
        {
            if (baseWin == null)
                throw new ArgumentNullException("baseWin");

            if (record == null || !IsIndexedRecord(record)) return;

            try {
                lock (fLock) {
                    ReindexRecord(baseWin, record);

                    UpdateIndex();
                }
            } catch (Exception ex) {
                Logger.WriteError("SearchManager.UpdateRecord()", ex);
            }
        }

        public void DeleteRecord(IBaseWindow baseWin, GDMRecord record)
        {
            if (baseWin == null)
                throw new ArgumentNullException("baseWin");

            try {
                lock (fLock) {
                    var doc = FindDocument(baseWin, record.UID);
                    if (doc != null) {
                        fWriter.DeleteDocuments(new Term(FIELD_UID, record.UID));

                        UpdateIndex();
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("SearchManager.DeleteRecord()", ex);
            }
        }

        private class SearchEntry
        {
            public string XRef;
            public float Rank;
        }

        private List<SearchEntry> Search(IBaseWindow baseWin, string searchText, int resNum)
        {
            if (baseWin == null)
                throw new ArgumentNullException("baseWin");

            List<SearchEntry> res = new List<SearchEntry>();

            try {
                lock (fLock) {
#if !LUC48
                    var queryParser = new MultiFieldQueryParser(Lucene.Net.Util.Version.LUCENE_30, new[] { FIELD_TEXT }, fAnalyzer);
#else
                    var queryParser = new MultiFieldQueryParser(LuceneVersion.LUCENE_48, new[] { FIELD_TEXT }, fAnalyzer);
#endif

                    Query searchTermQuery = queryParser.Parse(searchText);

                    var gedQuery = new MultiPhraseQuery();
                    gedQuery.Add(new Term(FIELD_DB, GetBaseID(baseWin)));

                    BooleanQuery aggregateQuery = new BooleanQuery() {
                        { searchTermQuery, Occur.MUST },
                        { gedQuery, Occur.MUST }
                    };

                    //var reader = fWriter.GetReader();
                    //var searcher = new IndexSearcher(reader);
                    var hits = fSearcher.Search(aggregateQuery, resNum).ScoreDocs;

                    foreach (var hit in hits) {
                        var foundDoc = fSearcher.Doc(hit.Doc);

                        try {
                            SearchEntry entry = new SearchEntry();
                            entry.XRef = foundDoc.Get("xref");
                            entry.Rank = hit.Score;
                            res.Add(entry);
                        } catch (Exception ex) {
                            Logger.WriteError("SearchManager.Search.1()", ex);
                        }
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("SearchManager.Search()", ex);
            }

            return res;
        }

        public void ShowResults(IBaseWindow baseWin, string query, StringList strList, int resNum = 20)
        {
            strList.BeginUpdate();
            try {
                strList.Clear();
                var searchResults = Search(baseWin, query, resNum);

                strList.Add(string.Format(fPlugin.LangMan.LS(PLS.SearchResults) + "\r\n", searchResults.Count));

                int num = searchResults.Count;
                for (int i = 0; i < num; i++) {
                    var entry = searchResults[i];
                    GDMRecord rec = baseWin.Context.Tree.XRefIndex_Find(entry.XRef);
                    if (rec != null) {
                        strList.Add("__________________________________________________________________________________________");
                        strList.Add("");

                        strList.Add(string.Format("[b][size=+1][url={0}] {0} [/url][/size][/b]", entry.XRef));

                        StringList ctx = baseWin.GetRecordContent(rec, RecordContentType.Quick);
                        strList.AddStrings(ctx);
                        strList.Add("");
                    }
                }
            } finally {
                strList.EndUpdate();
            }
        }
    }
}
