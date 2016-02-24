using System;
using System.Collections.Generic;
using System.IO;

using BSLib;
using GKCommon.GEDCOM;
using GKCore.Interfaces;
using Xapian;

namespace GKTextSearchPlugin
{
    /// <summary>
    /// 
    /// </summary>
    public class SearchManager
	{
		private readonly object xdbLock;
		private readonly Plugin fPlugin;

		public SearchManager(Plugin plugin)
		{
			this.fPlugin = plugin;
            this.xdbLock = new object();
		}

		#region Private methods
		
		private static string GetSign(IBaseWindow aBase)
		{
			return Path.GetFileNameWithoutExtension(aBase.Tree.FileName);
		}

		private static bool IsIndexedRecord(GEDCOMRecord rec)
		{
			return !((rec is GEDCOMLocationRecord || rec is GEDCOMGroupRecord));
		}

		private static void SetDBLastChange(IBaseWindow aBase, WritableDatabase database)
		{
			string dbLastchange = aBase.Tree.Header.TransmissionDateTime.ToString("yyyy.MM.dd HH:mm:ss", null);
			database.SetMetadata(GetSign(aBase), dbLastchange);
		}

		private string GetXDBFolder()
		{
			string xdbDir = fPlugin.Host.GetAppDataPath() + "xdb";
			if (!Directory.Exists(xdbDir)) Directory.CreateDirectory(xdbDir);
			return xdbDir;
		}

		private static uint FindDocId(IBaseWindow aBase, WritableDatabase database, string xref)
		{
			uint result;

			string key = "Q" + GetSign(aBase) + "_" + xref;

			PostingIterator p = database.PostListBegin(key);
			if (p == database.PostListEnd(key)) {
				result = 0; // 0 - is invalid docid (see XapianManual)
			} else {
				result = p.GetDocId();
			}

			return result;
		}

		private static void SetDocumentContext(IBaseWindow aBase, Document doc, TermGenerator indexer, GEDCOMRecord rec)
		{
			StringList ctx = aBase.GetRecordContent(rec);
			string recLastchange = rec.ChangeDate.ToString();
			string baseSign = GetSign(aBase);

			doc.SetData(rec.XRef);							// not edit: for link from search results to gedcom-base
			doc.AddTerm("Q" + baseSign + "_" + rec.XRef);	// not edit: specific db_rec_id - for FindDocId()
			doc.AddValue(0, recLastchange);				// not edit: for update check
			doc.AddBooleanTerm("GDB" + baseSign);			// not edit: for filtering by database in Search()

			indexer.SetDocument(doc);
			indexer.IndexText(ctx.Text);
		}

		private static void ReindexRecord(IBaseWindow aBase, WritableDatabase database, TermGenerator indexer, GEDCOMRecord record)
		{
			uint docid = FindDocId(aBase, database, record.XRef);

			if (docid != 0) {
				// проверка на необходимость обновления
				string recLastchange = record.ChangeDate.ToString();

				Document curDoc = database.GetDocument(docid);
				string docLastchange = curDoc.GetValue(0);

				// обновление записи
				if (!string.Equals(recLastchange, docLastchange)) {
					using (Document doc = new Document())
					{
						SetDocumentContext(aBase, doc, indexer, record);
						database.ReplaceDocument(docid, doc);
					}
				}
			} else {
				// чистое добавление
				using (Document doc = new Document())
				{
					SetDocumentContext(aBase, doc, indexer, record);
					database.AddDocument(doc);
				}
			}
		}

		#endregion

		public void ReindexBase(IBaseWindow aBase)
		{
		    if (aBase == null) return;

			try
			{
				lock (xdbLock)
        		{
					using (WritableDatabase database = new WritableDatabase(GetXDBFolder(), Xapian.Xapian.DB_CREATE_OR_OPEN))
					using (TermGenerator indexer = new TermGenerator())
					using (Stem stemmer = new Stem("russian"))
					{
						indexer.SetStemmer(stemmer);

						aBase.ProgressInit(fPlugin.LangMan.LS(TLS.LSID_SearchIndexRefreshing), aBase.Tree.RecordsCount);
						int num = aBase.Tree.RecordsCount - 1;
						for (int i = 0; i <= num; i++)
						{
							GEDCOMRecord record = aBase.Tree[i];
							if (IsIndexedRecord(record)) ReindexRecord(aBase, database, indexer, record);

							aBase.ProgressStep();
						}
						aBase.ProgressDone();

						SetDBLastChange(aBase, database);
					}
				}
			}
			catch (Exception ex)
			{
				aBase.Host.LogWrite("SearchManager.ReindexBase(): " + ex.Message);
			}
		}

		public void UpdateRecord(IBaseWindow aBase, GEDCOMRecord record)
		{
			if (record == null || !IsIndexedRecord(record)) return;

			try
			{
				lock (xdbLock)
        		{
					using (WritableDatabase database = new WritableDatabase(GetXDBFolder(), Xapian.Xapian.DB_CREATE_OR_OPEN))
					using (TermGenerator indexer = new TermGenerator())
					using (Stem stemmer = new Stem("russian"))
					{
						indexer.SetStemmer(stemmer);

						ReindexRecord(aBase, database, indexer, record);
						SetDBLastChange(aBase, database);
					}
				}
			}
			catch (Exception ex)
			{
				aBase.Host.LogWrite("SearchManager.UpdateRecord(): " + ex.Message);
			}
		}

		public void DeleteRecord(IBaseWindow aBase, string xref)
		{
			try
			{
				lock (xdbLock)
        		{
					using (WritableDatabase database = new WritableDatabase(GetXDBFolder(), Xapian.Xapian.DB_CREATE_OR_OPEN))
					{
						uint docid = FindDocId(aBase, database, xref);
						if (docid != 0) {
							database.DeleteDocument(docid);
							SetDBLastChange(aBase, database);
						}
					}
				}
			}
			catch (Exception ex)
			{
				aBase.Host.LogWrite("SearchManager.DeleteRecord(): " + ex.Message);
			}
		}

		public class SearchEntry
		{
			public string XRef;
			public uint Rank;
			public int Percent;
		}

		public List<SearchEntry> Search(IBaseWindow aBase, string searchText)
		{
            const uint flags = (uint)(QueryParser.feature_flag.FLAG_PARTIAL | QueryParser.feature_flag.FLAG_WILDCARD |
                                      QueryParser.feature_flag.FLAG_PHRASE | QueryParser.feature_flag.FLAG_BOOLEAN |
                                      QueryParser.feature_flag.FLAG_LOVEHATE);
            
            List<SearchEntry> res = new List<SearchEntry>();

			try
			{
				lock (xdbLock)
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

						string qs = searchText + " ged:" + GetSign(aBase);
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
										SearchEntry entry = new SearchEntry();
										entry.XRef = m.GetDocument().GetData();
										entry.Rank = m.GetRank()+1;
										entry.Percent = m.GetPercent();
										res.Add(entry);
									}
									catch (Exception ex)
									{
										aBase.Host.LogWrite("SearchManager.Search(): " + ex.Message);
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
				aBase.Host.LogWrite("SearchManager.Search(): " + ex.Message);
			}

			return res;
		}

	}
}
